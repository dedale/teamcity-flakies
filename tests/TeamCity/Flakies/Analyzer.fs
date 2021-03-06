module TeamCity.Flakies.Tests.Analyzer

open Expecto

open System
open System.Text

open TeamCity.Flakies
open TeamCity.Flakies.FileSystem
open TeamCity.Flakies.Tests.Extensions.RestApi

type FlakyTestAnalyzer =
    static member TestFlaky(flakiness, ?name, ?details) =
        let name = defaultArg name "Test"
        let details = defaultArg details ""

        let test =
            { TestDetails.Id = TestId.Random()
              Name = TestName name
              Details = details }

        let history = TestHistory.FlakyHistory()
        let testHistoryCount _ _ = failwith "Should not be called"

        let build =
            { Id = BuildId.Random()
              BuildTypeId = BuildTypeId.Random()
              Status = Status "Tests failed: 1 (1 new)" }

        let technical =
            Expect.wantSome
                (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build)
                "Expected flakiness"

        Expect.equal technical.Flakiness flakiness "Bad flakiness"
        use temp = new TempDir()
        let state = State.Load(temp.Dir.FileInDir "State.json")
        let createIssue _ _ = IssueId 1
        let updateIssue _ _ _ _ = failwith "Should not be called"
        let state = FlakyTestAnalyzer.test createIssue updateIssue test build technical state
        let notificationId = NotificationId.Test(test.Id, test.Name)
        let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
        let now = DateTime.UtcNow
        Expect.equal (history.OccurrencesSince flakiness (now.AddDays -1.)) 1 "Bad history"

let testAnalyzer =
    testList
        "Test analyzer"
        [ testCase " " <| fun _ -> Expect.isTrue true ""

          testCase "Flaky test"
          <| fun _ -> FlakyTestAnalyzer.TestFlaky(Flakiness.Default)

          testList
              "Not flaky tests"
              ([ 500, 1; 100, 20 ]
               |> List.map
                   (fun (count, failures) ->
                       testCase (sprintf "%d-%d" count failures)
                       <| fun _ ->
                           let test =
                               { TestDetails.Id = TestId.Random()
                                 Name = TestName.Random()
                                 Details = "" }

                           let testHistoryCount _ = failwith "Should not be called"
                           let history = TestHistory(BuildId 1, List.map (fun i -> i > failures) [ 1 .. count ], false)

                           let build =
                               { Id = BuildId.Random()
                                 BuildTypeId = BuildTypeId.Random()
                                 Status = Status "Tests failed: 1" }

                           Expect.isNone
                               (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build)
                               "Bad flakiness"))

          testList
              "Flaky details"
              ([ "Test exceeded Timeout value of 1800000ms", Flakiness.UnitTestTimeout
                 "System.Net.WebException : The operation has timed out", Flakiness.UnitTestTimeout
                 "There is not enough space on the disk", Flakiness.DiskFull
                 "System.IO.IOException : The process cannot access the file 'foo.bar' because it is being used by another process",
                 Flakiness.FileInUse ]
               |> List.map
                   (fun (details, flakiness) ->
                       testCase (sprintf "%s: %O" details flakiness)
                       <| fun _ -> FlakyTestAnalyzer.TestFlaky(flakiness, details = details)))

          testCase "Test random"
          <| fun _ ->
              let name = "Assembly.dll: Namespace: Class: Test"

              let test =
                  { TestDetails.Id = TestId.Random()
                    Name = TestName $"{name}(etc, ...)"
                    Details = "" }

              let history = TestHistory(BuildId 1, [ false ], false)
              let testHistoryCount _ _ = failwith "Should not be called"

              let build =
                  { Id = BuildId.Random()
                    BuildTypeId = BuildTypeId.Random()
                    Status = Status "Tests failed: 1 (1 new)" }

              let technical =
                  Expect.wantSome
                      (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build)
                      "Expected flakiness"

              let flakiness = Flakiness.RandomTestName
              Expect.equal technical.Flakiness flakiness "Bad flakiness"
              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = FlakyTestAnalyzer.test createIssue updateIssue test build technical state
              let notificationId = NotificationId.Prefix name
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince flakiness (now.AddDays -1.)) 1 "Bad history"

          testCase "Fail-only flaky"
          <| fun _ ->
              let test =
                  { TestDetails.Id = TestId.Random()
                    Name = TestName.Random()
                    Details = "" }

              let history = TestHistory(BuildId 1, [ false; false ], false)
              let testHistoryCount _ _ = 100

              let build =
                  { Id = BuildId.Random()
                    BuildTypeId = BuildTypeId.Random()
                    Status = Status "Tests failed: 1 (1 new)" }

              let technical =
                  Expect.wantSome
                      (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build)
                      "Expected flakiness"

              let flakiness = Flakiness.Default
              Expect.equal technical.Flakiness flakiness "Bad flakiness"
              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = FlakyTestAnalyzer.test createIssue updateIssue test build technical state
              let notificationId = NotificationId.Test(test.Id, test.Name)
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince flakiness (now.AddDays -1.)) 1 "Bad history"

          testCase "Fail-only successive"
          <| fun _ ->
              let test =
                  { TestDetails.Id = TestId.Random()
                    Name = TestName.Random()
                    Details = "" }

              let testHistoryCount _ = failwith "Should not be called"
              let history = TestHistory(BuildId 1, List.map (fun _ -> false) [ 1 .. 10 ], true)

              let build =
                  { Id = BuildId.Random()
                    BuildTypeId = BuildTypeId.Random()
                    Status = Status "Tests failed: 1 (1 new)" }

              Expect.isNone (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build) "Bad flakiness"

          testCase "Test new occurrence"
          <| fun _ ->
              let test =
                  { TestDetails.Id = TestId.Random()
                    Name = TestName.Random()
                    Details = "" }

              let history = TestHistory.FlakyHistory()
              let testHistoryCount _ _ = failwith "Should not be called"

              let build =
                  { Id = BuildId.Random()
                    BuildTypeId = BuildTypeId.Random()
                    Status = Status "Tests failed: 1 (1 new)" }

              let technical =
                  Expect.wantSome
                      (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build)
                      "Expected flakiness"

              let flakiness = Flakiness.Default
              Expect.equal technical.Flakiness flakiness "Bad flakiness"
              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called" //()
              let state = FlakyTestAnalyzer.test createIssue updateIssue test build technical state

              let technical =
                  Expect.wantSome
                      (FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount test history build)
                      "Expected flakiness"

              let createIssue _ _ = failwith "Should not be called"
              let updateIssue _ _ _ _ = ()
              let state = FlakyTestAnalyzer.test createIssue updateIssue test build technical state
              let notificationId = NotificationId.Test(test.Id, test.Name)
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince flakiness (now.AddDays -1.)) 2 "Bad history"

          testCase "Test group"
          <| fun _ ->
              let history = TestHistory.FlakyHistory()

              let build =
                  { Id = BuildId.Random()
                    BuildTypeId = BuildTypeId.Random()
                    Status = Status "Tests failed: 1 (1 new)" }

              let flakyTest name =
                  let details =
                      { Id = TestId.Random()
                        Name = TestName "Prefix: 1"
                        Details = "" }

                  { Details = details
                    History = history
                    Technical = Flakiness.Default.ToDefault() }

              let group =
                  FlakyGroup(
                      "Prefix",
                      [ flakyTest "Prefix: 1"
                        flakyTest "Prefix: 2" ]
                  )

              let flakiness = Flakiness.Default
              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = FlakyTestAnalyzer.group createIssue updateIssue group build state
              let notificationId = NotificationId.Prefix "Prefix"
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince flakiness (now.AddDays -1.)) 1 "Bad history" ]

let buildAnalyzer =
    testList
        "Build analyzer"
        [ testCase "Nothing flaky"
          <| fun _ ->
              let build =
                  FailedBuild.Random(status = Status "Number of tests 0 is 1 less than the provided threshold 1 (new)")

              let getProblems buildId =
                  Expect.equal buildId build.Id "Bad build id"

                  [ { Type = ProblemType "Type"
                      Details = "Details" } ]

              let downloadLog buildId =
                  Expect.equal buildId build.Id "Bad build id"
                  StringBuilder().AppendLine("Build started").AppendLine("Build finished").ToString()

              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = failwith "Should not be called"
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = BuildAnalyzer.run getProblems downloadLog createIssue updateIssue build state
              let notificationId = NotificationId.BuildTypeId build.BuildTypeId
              Expect.isNone (state.TryHistory notificationId) "No history expected"

          testCase "Build timeout"
          <| fun _ ->
              let build = FailedBuild.Random(status = Status "Execution timeout")

              let getProblems buildId =
                  Expect.equal buildId build.Id "Bad build id"

                  [ { Type = ProblemType "TC_EXECUTION_TIMEOUT"
                      Details = "Execution timeout" } ]

              let downloadLog buildId =
                  Expect.equal buildId build.Id "Bad build id"
                  "Build log"

              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = BuildAnalyzer.run getProblems downloadLog createIssue updateIssue build state
              let notificationId = NotificationId.BuildTypeId build.BuildTypeId
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince Flakiness.BuildTimeout (now.AddDays -1.)) 1 "Bad history"

          testList
              "Problem in log"
              ([ @"error MSB4166: Child node ""8"" exited prematurely", Flakiness.MSBuildCrash
                 @"error MSB4018: The ""MSBuild"" task failed unexpectedly.", Flakiness.MSBuildCrash
                 "Directory.Build.targets error MSB4236: The SDK 'Microsoft.Build.CentralPackageVersions/2.0.79' specified could not be found.",
                 Flakiness.MSBuildSDKNotFound
                 "The paging file is too small for this operation to complete", Flakiness.OutOfMemory
                 "Insufficient memory to continue the execution of the program", Flakiness.OutOfMemory
                 "FATAL : System.IO.Exception: The process cannot access the file 'foo.bar' because it is being used by another process.",
                 Flakiness.FileInUse
                 @"build.xml:666: Unable to delete directory C:\Users\AGENT\.ivy\org\component\version",
                 Flakiness.FolderInUse
                 "There is not enough space on the disk", Flakiness.DiskFull
                 "FATAL : pip install failed", Flakiness.PipFailed ]
               |> List.map
                   (fun (line, flakiness) ->
                       testCase (sprintf "%s-%O" line flakiness)
                       <| fun _ ->
                           let build =
                               FailedBuild.Random(
                                   status = Status "Number of tests 0 is 1 less than the provided threshold 1 (new)"
                               )

                           let getProblems buildId =
                               Expect.equal buildId build.Id "Bad build id"

                               [ { Type = ProblemType "Type"
                                   Details = "Details" } ]

                           let downloadLog buildId =
                               Expect.equal buildId build.Id "Bad build id"
                               $"ignored first line\n{line}"

                           use temp = new TempDir()
                           let state = State.Load(temp.Dir.FileInDir "State.json")
                           let createIssue _ _ = IssueId 1
                           let updateIssue _ _ _ _ = failwith "Should not be called"
                           let state = BuildAnalyzer.run getProblems downloadLog createIssue updateIssue build state
                           let notificationId = NotificationId.BuildTypeId build.BuildTypeId
                           let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
                           let now = DateTime.UtcNow
                           Expect.equal (history.OccurrencesSince flakiness (now.AddDays -1.)) 1 "Bad history"))

          testCase "New occurrence"
          <| fun _ ->
              let build =
                  FailedBuild.Random(status = Status "Number of tests 0 is 1 less than the provided threshold 1 (new)")

              let nextBuild =
                  { build with
                        Id = BuildId(build.Id.Value + 1) }

              let getProblems _ =
                  [ { Type = ProblemType "Type"
                      Details = "Details" } ]

              let downloadLog _ =
                  @"error MSB4166: Child node ""8"" exited prematurely"

              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = BuildAnalyzer.run getProblems downloadLog createIssue updateIssue build state
              let createIssue _ _ = failwith "Should not be called"
              let updateIssue _ _ _ _ = ()
              let state = BuildAnalyzer.run getProblems downloadLog createIssue updateIssue nextBuild state
              let notificationId = NotificationId.BuildTypeId build.BuildTypeId
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince Flakiness.MSBuildCrash (now.AddDays -1.)) 2 "Bad history"

          testCase "Unique problems"
          <| fun _ ->
              let build =
                  FailedBuild.Random(status = Status "Number of tests 0 is 1 less than the provided threshold 1 (new)")

              let getProblems buildId =
                  Expect.equal buildId build.Id "Bad build id"

                  [ { Type = ProblemType "Type"
                      Details = "Details" } ]

              let downloadLog buildId =
                  Expect.equal buildId build.Id "Bad build id"

                  StringBuilder()
                      .AppendLine("ignored")
                      .AppendLine("There is not enough space on the disk")
                      .AppendLine("ignored")
                      .AppendLine("There is not enough space on the disk")
                      .ToString()

              use temp = new TempDir()
              let state = State.Load(temp.Dir.FileInDir "State.json")
              let createIssue _ _ = IssueId 1
              let updateIssue _ _ _ _ = failwith "Should not be called"
              let state = BuildAnalyzer.run getProblems downloadLog createIssue updateIssue build state
              let notificationId = NotificationId.BuildTypeId build.BuildTypeId
              let history = Expect.wantSome (state.TryHistory notificationId) "History should exist"
              let now = DateTime.UtcNow
              Expect.equal (history.OccurrencesSince Flakiness.DiskFull (now.AddDays -1.)) 1 "Bad history" ]

let all = testList "Analyzers" [ testAnalyzer; buildAnalyzer ]
