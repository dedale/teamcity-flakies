namespace TeamCity.Flakies

open System
open System.Text.RegularExpressions

[<AutoOpen>]
module StringExtensions =
    type String with
        member x.SplitLines() =
            x.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)

[<NoComparison>]
type FlakyTest =
    { Details: TestDetails
      History: TestHistory
      Technical: TechnicalFlakiness }

type FlakyGroup(prefix: string, flakies: FlakyTest list) =
    new(prefix, test) = FlakyGroup(prefix, [ test ])
    member __.Prefix = prefix
    member __.Items = flakies
    member __.Add flaky = FlakyGroup(prefix, flaky :: flakies)

    member __.Technical =
        flakies
        |> List.map (fun f -> f.Technical)
        |> List.groupBy id
        |> List.sortByDescending (fun (_, v) -> v.Length)
        |> List.head
        |> fst

module FlakyTestAnalyzer =
    let private isRandom (name: TestName) =
        name.Value.Contains(", ...", StringComparison.Ordinal)

    let private uniqueName (name: TestName) =
        let parts = name.Value.Split(':')
        let last = parts.[parts.Length - 1]
        parts.[parts.Length - 1] <- last.Substring(0, last.IndexOf('('))
        String.Join(':', parts)

    let private guessFlakiness (details: string) =
        let patterns =
            [ "Test exceeded Timeout value", Flakiness.UnitTestTimeout.ToDefault()
              "The operation has timed out", Flakiness.UnitTestTimeout.ToTechnical()
              "Time-out \(more than", Flakiness.UnitTestTimeout.ToTechnical()
              "There is not enough space on the disk", Flakiness.DiskFull.ToTechnical()
              "The process cannot access the file.*because it is being used by another process",
              Flakiness.FileInUse.ToDefault() ]

        let rec tryMatch (line: string) (patterns: (string * TechnicalFlakiness) list) =
            match patterns with
            | (pattern, flakiness) :: ps ->
                if Regex.IsMatch(line, pattern) then
                    Some flakiness
                else
                    tryMatch line ps
            | _ -> None

        let rec tryFind lines =
            match lines with
            | l :: ls ->
                match tryMatch l patterns with
                | Some f -> f
                | _ -> tryFind ls
            | [] -> Flakiness.Default.ToDefault()

        details.SplitLines() |> List.ofArray |> tryFind

    /// Heuristic to detect if test is fail-only and flaky:
    /// - test exists only when it fails
    /// - no successive builds (otherwise not considered flaky)
    /// - build history (since oldest failed test) is bigger than test history
    let private isFailOnly testHistoryCount (history: TestHistory) buildTypeId =
        if history.AlwaysFail && not history.Successive then
            let buildCount = testHistoryCount buildTypeId history.FirstBuildId
            history.Successes.Length < buildCount / 2
        else
            false

    let tryGuessFlakiness testHistoryCount (test: TestDetails) (history: TestHistory) (build: FailedBuild) =
        if
            history.Successes.Length = 1
            && not (List.head history.Successes)
        then
            if isRandom test.Name then
                Some(Flakiness.RandomTestName.ToDefault())
            else
                None
        elif history.IsFlaky
             || isFailOnly testHistoryCount history build.BuildTypeId then
            Some(guessFlakiness test.Details)
        else
            None

    let test
        (create: CreateIssue)
        (update: UpdateIssue)
        (details: TestDetails)
        (build: FailedBuild)
        (technical: TechnicalFlakiness)
        (state: State)
        =
        let notificationId =
            if technical.Flakiness = Flakiness.RandomTestName then
                NotificationId.Prefix(uniqueName details.Name)
            else
                NotificationId.Test(details.Id, details.Name)

        state.NewOccurrence create update build notificationId technical

    let group (create: CreateIssue) (update: UpdateIssue) (group: FlakyGroup) (build: FailedBuild) (state: State) =
        let notificationId = NotificationId.Prefix group.Prefix
        state.NewOccurrence create update build notificationId group.Technical

module BuildAnalyzer =
    let private guessFlakiness others (line: string) =
        let patterns =
            [ "error MSB4166: Child node.*exited prematurely", Flakiness.MSBuildCrash.ToTechnical()
              "error MSB4018: The.*task failed unexpectedly", Flakiness.MSBuildCrash.ToTechnical()
              "The SDK.*specified could not be found", Flakiness.MSBuildSDKNotFound.ToTechnical()
              "The paging file is too small for this operation to complete", Flakiness.OutOfMemory.ToTechnical()
              "Insufficient memory to continue the execution of the program", Flakiness.OutOfMemory.ToTechnical()
              "The process cannot access the file.*because it is being used by another process",
              Flakiness.FileInUse.ToDefault()
              "Unable to delete directory", Flakiness.FolderInUse.ToTechnical()
              "There is not enough space on the disk", Flakiness.DiskFull.ToTechnical()
              "pip install failed", Flakiness.PipFailed.ToDefault() ]

        let rec tryMatch (line: string) (patterns: (string * TechnicalFlakiness) list) =
            match patterns with
            | (pattern, flakiness) :: ps ->
                if Regex.IsMatch(line, pattern) then
                    Some flakiness
                else
                    tryMatch line ps
            | _ -> None

        match tryMatch line patterns with
        | Some f -> Set.add f others
        | _ -> others

    let run getProblems downloadLog (create: CreateIssue) (update: UpdateIssue) (build: FailedBuild) (state: State) =
        let problems = getProblems build.Id
        let isTimeout = Seq.exists (fun p -> p.Type.Value = "TC_EXECUTION_TIMEOUT") problems

        let technicals =
            if isTimeout then
                Set.add (Flakiness.BuildTimeout.ToDefault()) Set.empty
            else
                Set.empty

        let log : string = downloadLog build.Id

        let technicals =
            log.SplitLines()
            |> Seq.fold guessFlakiness technicals

        if not technicals.IsEmpty then
            let notificationId = NotificationId.BuildTypeId build.BuildTypeId

            technicals
            |> Set.fold
                (fun (state: State) technical -> state.NewOccurrence create update build notificationId technical)
                state
        else
            state
