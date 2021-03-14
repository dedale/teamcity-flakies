namespace TeamCity.Flakies

open System

module FlakyMonitor =
    let private group (tests: seq<FlakyTest>) =
        let prefixed =
            tests
            |> Seq.groupBy (fun t -> t.Details.Name.Value.Split(':').[0])
            |> Seq.sortByDescending (snd >> Seq.length)
            |> List.ofSeq

        let foldTests (groups: Map<string, FlakyGroup>) (prefix, flakyTests) =
            if groups.Count = 0 then
                let group = FlakyGroup(prefix, List.ofSeq flakyTests)
                groups.Add(prefix, group)
            else
                let foldTest (groups: Map<string, FlakyGroup>) (test: FlakyTest) =
                    match groups.TryFind prefix with
                    | Some current -> groups.Remove(prefix).Add(prefix, current.Add test)
                    | _ ->
                        let known =
                            groups
                            |> Map.toSeq
                            |> Seq.tryFind
                                (fun (p, _) -> test.Details.Name.Value.Contains(p, StringComparison.OrdinalIgnoreCase))

                        match known with
                        | Some (prefix, group) -> groups.Remove(prefix).Add(prefix, group.Add test)
                        | _ -> groups.Add(prefix, FlakyGroup(prefix, test))

                Seq.fold foldTest groups flakyTests

        List.fold foldTests Map.empty prefixed

    let getFlakyTests testDetails testHistory tryGuessFlakiness log build (failedTests: seq<FailedTest>) =
        let mapping (failed: FailedTest) =
            match testDetails failed.Id with
            | Some details ->
                log $"Analyzing test {details.Id.Value}"
                let history = testHistory details.Id build.BuildTypeId

                match tryGuessFlakiness details history build with
                | Some technical ->
                    { Details = details
                      History = history
                      Technical = technical }
                    |> Some
                | _ -> None
            | _ -> None

        failedTests
        |> Seq.map mapping
        |> Seq.choose id
        |> List.ofSeq

    let analyzeTests analyzeTest analyzeGroup (groups: Map<string, FlakyGroup>) build state =
        groups
        |> Map.fold
            (fun state _ group ->
                match group.Items with
                | [ flaky ] -> analyzeTest flaky.Details flaky.History build flaky.Technical state
                | _ -> analyzeGroup group build state)
            state

    let run
        latestFailedBuilds
        newFailedTests
        getFlakyTests
        analyzeTests
        analyzeBuild
        log
        (projectId: ProjectId)
        stateFile
        =
        log $"Monitoring flakies in {projectId.Value}"
        let state = State.Load stateFile
        let failedBuilds : FailedBuild list = latestFailedBuilds projectId |> List.ofSeq

        let buildsWithNewErrors =
            failedBuilds
            |> List.filter (fun b -> b.HasNewFailures)
            |> List.sortBy (fun b -> b.Id.Value)

        let folder (state: State) (build: FailedBuild) =
            if state.Contains build.Id then
                state
            else
                // TODO continue if BuildTypeId disabled in config
                log $"Analyzing build {build.Id.Value} in {build.BuildTypeId.Value}"

                let groups =
                    newFailedTests build.Id
                    |> List.ofSeq
                    |> getFlakyTests build
                    |> group

                let state =
                    analyzeTests groups build state
                    |> analyzeBuild build
                    |> (fun (state: State) -> state.Add build.Id)

                state.Save stateFile
                state

        let state = List.fold folder state buildsWithNewErrors

        let state =
            if not failedBuilds.IsEmpty then
                let minBuild =
                    failedBuilds
                    |> List.map (fun b -> b.Id)
                    |> List.min

                state.ForgetOlderThan minBuild
            else
                state

        state.Save stateFile
        log "Done"
