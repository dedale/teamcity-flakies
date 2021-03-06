namespace TeamCity.Flakies

open System
open System.Xml.Linq
open System.Xml.XPath

type BuildId = BuildId of int

module BuildId =
    let value (BuildId x) = x
    let max = BuildId Int32.MaxValue

type BuildId with
    member x.Value = BuildId.value x

type BuildTypeId = BuildTypeId of string

module BuildTypeId =
    let value (BuildTypeId x) = x

type BuildTypeId with
    member x.Value = BuildTypeId.value x

type BuildStatus = Status of string

module BuildStatus =
    let value (Status x) = x

    let hasNewFailures (Status x) =
        x.Contains("new)", StringComparison.Ordinal)

type BuildStatus with
    member x.Value = BuildStatus.value x
    member x.HasNewFailures = BuildStatus.hasNewFailures x

type FailedBuild =
    { Id: BuildId
      BuildTypeId: BuildTypeId
      Status: BuildStatus }
    member x.HasNewFailures = x.Status.HasNewFailures

type TestName = TestName of string

module TestName =
    let value (TestName x) = x

type TestName with
    member x.Value = TestName.value x

type FailedTest = { Id: string; Name: TestName }

type PeriodCounter =
    { CurrentIsSuccess: bool
      Count: int }
    static member New isSuccess =
        { CurrentIsSuccess = isSuccess
          Count = 1 }

/// <summary>
/// History of a test
/// </summary>
/// <param name="firstBuildId">Oldest build id with this test.</param>
/// <param name="successes">Booleans with test history status.</param>
/// <param name="successive">Fails on successive builds.</param>
/// <param name="isFlaky">Is test flaky.</param>
type TestHistory private (firstBuildId: BuildId, successes, successive, isFlaky) =
    new(firstBuildId, successes, successive) =
        let minPeriods = 5

        let failures =
            successes
            |> List.filter (fun isSuccess -> not isSuccess)
            |> List.length

        /// Count number of periods when test is alternatively passing or failing
        let folder (counter: PeriodCounter) (isSuccess: bool) =
            if counter.CurrentIsSuccess = isSuccess then
                counter
            else
                { CurrentIsSuccess = isSuccess
                  Count = counter.Count + 1 }

        let periods =
            if successes.Length = 0 then
                0
            else
                let counter = List.fold folder (PeriodCounter.New successes.[0]) successes
                counter.Count

        let isFlaky = periods >= minPeriods && periods > failures / 2

        TestHistory(firstBuildId, successes, successive, isFlaky)

    member __.FirstBuildId = firstBuildId
    member __.Successes = successes
    member __.Successive = successive
    member __.IsFlaky = isFlaky
    member __.AlwaysFail = List.forall (fun x -> not x) successes

    static member Empty = TestHistory(BuildId.max, [], false)

type TestId = TestId of int64

module TestId =
    let value (TestId x) = x

type TestId with
    member x.Value = TestId.value x

    static member tryParse(s: string) =
        match Int64.TryParse s with
        | true, x -> TestId x |> Some
        | _ -> None

type TestDetails =
    { Id: TestId
      Name: TestName
      Details: string }

type UserLogin = UserLogin of string

module UserLogin =
    let value (UserLogin x) = x

type UserLogin with
    member x.Value = UserLogin.value x

type Assignee = { Login: UserLogin; Name: string }

type ProblemType = ProblemType of string

module ProblemType =
    let value (ProblemType x) = x

type ProblemType with
    member x.Value = ProblemType.value x

type Problem = { Type: ProblemType; Details: string }

type ProjectId = ProjectId of string

module ProjectId =
    let value (ProjectId x) = x

type ProjectId with
    member x.Value = ProjectId.value x

type ValidUrl = private Url of string

module ValidUrl =
    let value (Url url) = url

    let (|Url|_|) str =
        match Uri.TryCreate(str, UriKind.Absolute) with
        | (true, _) -> Some(str)
        | _ -> None

    let create str =
        match str with
        | Url url -> Url url
        | _ -> failwith $"'%s{str}' is not a valid url"

type ValidUrl with
    member x.Value = ValidUrl.value x

module TeamCity =

    let getRestString getString relativeUrl = getString $"app/rest/%s{relativeUrl}"

    let latestFailedBuilds getRestString (projectId: ProjectId) count =
        let count = defaultArg count 500
        printfn $"Loading latest failed builds in %s{projectId.Value} project"

        let relativeUrl =
            $"builds/?locator=affectedProject:{projectId.Value},status:FAILURE,state:finished,count:%d{count}&fields=build(id,buildTypeId,statusText)"

        let contents = getRestString relativeUrl

        let tryParse (element: XElement) =
            let buildId = element.AttributeValue "id"
            let typeId = element.AttributeValue "buildTypeId"
            let status = element.ElementContent "statusText"

            match buildId, typeId, status with
            | Some buildId, Some typeId, Some status ->
                { Id = BuildId(Int32.Parse buildId)
                  BuildTypeId = BuildTypeId typeId
                  Status = Status status.Value }
                |> Some
            | _ -> None

        XDocument.Parse(contents).XPathSelectElements("/builds/build")
        |> Seq.choose tryParse

    let newFailedTests getRestString (id: BuildId) count =
        let count = defaultArg count 500

        let relativeUrl =
            $"testOccurrences?locator=build:(id:%d{id.Value}),count:%d{count},status:FAILURE&fields=testOccurrence(id,name,firstFailed(id),mute(id))"

        let contents = getRestString relativeUrl

        let tryParse (element: XElement) =
            let testId = element.AttributeValue "id"
            let name = element.AttributeValue "name"
            let oldError = element.ElementContent "firstFailed"

            let muteId =
                match element.ElementContent "mute" with
                | Some e -> e.AttributeValue "id"
                | _ -> None

            match testId, name, oldError, muteId with
            | Some testId, Some name, None, None -> { Id = testId; Name = TestName name } |> Some
            | _ -> None

        XDocument.Parse(contents).XPathSelectElements("/testOccurrences/testOccurrence")
        |> Seq.choose tryParse

    /// Computes expected test history (for tests that only exist when failed)
    let testHistoryCount getRestString (buildTypeId: BuildTypeId) (firstBuildId: BuildId) =
        let contents = getRestString $"builds?locator=buildType:id:%s{buildTypeId.Value}&fields=build(id)"

        XDocument.Parse(contents).XPathSelectElements("/builds/build")
        |> Seq.choose (fun e -> e.AttributeValue "id")
        |> Seq.map Int32.Parse
        |> Seq.filter (fun buildId -> buildId >= firstBuildId.Value)
        |> Seq.length

    /// Explicit BuildTypeId to avoid fake flakies with 2 tests with the same name
    /// in two different build configs when it always fail in one build config (not the other)
    /// In this case, TeamCity consider the test flaky.
    /// But here we consider those different tests.
    let testHistory getRestString (id: TestId) (buildTypeId: BuildTypeId) count =
        let count = defaultArg count 500

        let relativeUrl =
            $"testOccurrences?locator=test:(id:%d{id.Value}),buildType:(id:%s{buildTypeId.Value}),count:%d{count}&fields=testOccurrence(status,build(id,number))"

        let contents = getRestString relativeUrl

        let data =
            XDocument.Parse(contents).XPathSelectElements("/testOccurrences/testOccurrence")
            |> Seq.map
                (fun e ->
                    let success =
                        match e.AttributeValue "status" with
                        | Some "SUCCESS" -> true
                        | _ -> false

                    let build = e.ElementContent "build"

                    let buildId, buildNumber =
                        match build with
                        | Some b -> b.AttributeValue "id", b.AttributeValue "number"
                        | _ -> None, None

                    {| Success = success
                       BuildId = buildId
                       BuildNumber = buildNumber |})
            |> List.ofSeq

        if data.IsEmpty then
            TestHistory.Empty
        else
            let minBuildId =
                data
                |> List.choose (fun x -> x.BuildId)
                |> List.map
                    (fun x ->
                        match Int32.TryParse x with
                        | true, x -> x
                        | _ -> 0)
                |> List.filter (fun x -> x > 0)
                |> List.min

            let numbers =
                data
                |> List.where (fun x -> not x.Success)
                |> List.choose (fun x -> x.BuildNumber)
                |> List.map (fun x -> x.Split('.', '_') |> Array.last)
                |> List.choose
                    (fun x ->
                        match Int32.TryParse x with
                        | true, x -> Some x
                        | _ -> None)
                |> List.sort

            let successive =
                if numbers.Length < 2 then
                    0
                else
                    seq { 0 .. numbers.Length - 2 }
                    |> Seq.map (fun i -> numbers.[i + 1] = numbers.[i] + 1)
                    |> Seq.length

            let successes = data |> List.map (fun x -> x.Success)
            TestHistory(BuildId minBuildId, successes, successive >= 2)

    let testDetails getRestString (id: string) =
        let contents = getRestString $"testOccurrences/%s{id}"
        let occurrence = XDocument.Parse(contents).XPathSelectElement("/testOccurrence")

        if occurrence <> null then
            let details =
                occurrence.ElementContent "details"
                |> Option.map (fun e -> e.Value)

            let test = occurrence.ElementContent "test"

            let name =
                test
                |> Option.bind (fun e -> e.AttributeValue "name")

            let testId =
                test
                |> Option.bind (fun e -> e.AttributeValue "id")
                |> Option.bind TestId.tryParse

            match testId, name, details with
            | Some testId, Some name, Some details ->
                Some
                    { Id = testId
                      Name = TestName name
                      Details = details }
            | _ -> None
        else
            None

    let assignee getRestString (id: TestId) =
        let contents =
            getRestString
                $"investigations?locator=test:(id:%d{id.Value})&fields=investigation(state,assignee(username,name))"

        let investigation = XDocument.Parse(contents).XPathSelectElement("/investigations/investigation")

        if investigation <> null then
            let state = investigation.AttributeValue "state"
            let assignee = investigation.ElementContent "assignee"

            let login =
                assignee
                |> Option.bind (fun a -> a.AttributeValue "username")

            let name =
                assignee
                |> Option.bind (fun a -> a.AttributeValue "name")

            match state, login, name with
            | Some state, Some login, Some name when state = "TAKEN" -> Some { Login = UserLogin login; Name = name }
            | _ -> None
        else
            None

    let downloadLog getString (id: BuildId) =
        getString $"downloadBuildLog.html?buildId={id.Value}"

    let problems getRestString (id: BuildId) =
        let contents =
            getRestString $"problemOccurrences?locator=build:(id:{id.Value})&fields=problemOccurrence(type,details)"

        let tryParse (element: XElement) =
            let ``type`` = element.AttributeValue "type"
            let details = element.AttributeValue "details"

            match ``type``, details with
            | Some ``type``, Some details ->
                { Type = ProblemType ``type``
                  Details = details }
                |> Some
            | _ -> None

        XDocument.Parse(contents).XPathSelectElements("/problemOccurrences/problemOccurrence")
        |> Seq.choose tryParse
