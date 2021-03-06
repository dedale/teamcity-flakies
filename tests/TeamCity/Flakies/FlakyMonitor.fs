module TeamCity.Flakies.Tests.Monitor

open Expecto

open System

open TeamCity.Flakies
open TeamCity.Flakies.FileSystem
open TeamCity.Flakies.Tests.Extensions.RestApi

let all =
    testList
        "Flaky Monitor"
        [ testCase "Default"
          <| fun _ ->
              // Look: no mock!
              use temp = new TempDir()
              let stateFile = temp.Dir.FileInDir "State.json"
              let project = ProjectId "Project"
              let buildTypeId = BuildTypeId $"{project.Value}_Build"
              let knownBuild = FailedBuild.Random(buildTypeId = buildTypeId, status = Status "Failed (new)")

              let build =
                  { knownBuild with
                        Id = BuildId(knownBuild.Id.Value + 1) }

              let state = (State.Load stateFile).Add knownBuild.Id
              state.Save stateFile
              let test = FailedTest.Random()

              let details : TestDetails =
                  { Id = TestId.Random()
                    Name = test.Name
                    Details = "Test details" }

              let testHistory = TestHistory.FlakyHistory()
              let testTechnical = Flakiness.Default.ToDefault()
              let buildTechnical = Flakiness.DiskFull.ToTechnical()

              let latestFailedBuilds project' =
                  Expect.equal project' project "Bad project"
                  [ knownBuild; build ]

              let newFailedTests buildId' =
                  Expect.equal buildId' build.Id "Bad build id"
                  [ test ]

              let getDetails testId' =
                  Expect.equal testId' test.Id "Bad test id"
                  Some details

              let getHistory detailsId' buildTypeId' =
                  Expect.equal detailsId' details.Id "Bad details id"
                  Expect.equal buildTypeId' buildTypeId "Bad build type id"
                  testHistory

              let tryGuessFlakiness details' history' build' =
                  Expect.equal details' details "Bad details"
                  Expect.equal history' testHistory "Bad history"
                  Expect.equal build' build "Bad build"
                  Some testTechnical

              let log _ = ()
              let getFlakyTests = FlakyMonitor.getFlakyTests getDetails getHistory tryGuessFlakiness log

              let analyzeTest details' history' failed' technical' (state: State) =
                  Expect.equal details' details "Bad details"
                  Expect.equal history' testHistory "Bad history"
                  Expect.equal failed' build "Bad build"
                  Expect.equal technical' testTechnical "Bad technical"

                  let notificationId = NotificationId.Test(details.Id, details.Name)
                  let notificationHistory = NotificationHistory(notificationId, testTechnical.Flakiness, IssueId 1)
                  state.Update notificationHistory

              let analyzeGroup _ _ _ = failwith "Should not be called"
              let analyzeTests = FlakyMonitor.analyzeTests analyzeTest analyzeGroup

              let analyzeBuild failed' (state: State) =
                  Expect.equal failed' build "Bad build"
                  let notificationId = NotificationId.BuildTypeId(buildTypeId)
                  let notificationHistory = NotificationHistory(notificationId, buildTechnical.Flakiness, IssueId 2)
                  state.Update notificationHistory

              FlakyMonitor.run
                  latestFailedBuilds
                  newFailedTests
                  getFlakyTests
                  analyzeTests
                  analyzeBuild
                  log
                  project
                  stateFile

              let state = State.Load stateFile
              Expect.isTrue (state.Contains build.Id) "Build should be in state"

          testCase "lots of tests"
          <| fun _ ->
              let failures1 = 400
              let failures2 = 20
              let prefix1 = "Prefix1"
              let prefix2 = "Prefix2"
              use temp = new TempDir()
              let stateFile = temp.Dir.FileInDir "State.json"
              let project = ProjectId "Project"
              let buildTypeId = BuildTypeId $"{project.Value}_Build"

              let build =
                  FailedBuild.Random(
                      buildTypeId = buildTypeId,
                      status = Status $"Tests failed: {failures1} ({failures1} new)"
                  )

              let testHistory = TestHistory.FlakyHistory()
              let technical = Flakiness.Default.ToDefault()

              let failedTests =
                  [ for i in 1 .. failures1 - 2 do
                      (FailedTest.Random(name = $"{prefix1}: Etc: \"Test.Name.{i}\""))
                    FailedTest.Random(name = $"Extra.{prefix1}: {String.Random()}")
                    FailedTest.Random(name = $"Extra.{prefix1}: {String.Random()}")
                    for i in 1 .. failures2 do
                        FailedTest.Random(name = $"{prefix2}: Etc: \"Test.{i}\"") ]

              let testDetails =
                  failedTests
                  |> List.map
                      (fun t ->
                          t.Id,
                          { Id = TestId.Random()
                            Name = t.Name
                            Details = t.Name.Value })
                  |> Map.ofList

              let latestFailedBuilds project' =
                  Expect.equal project' project "Bad project"
                  [ build ]

              let newFailedTests buildId' =
                  Expect.equal buildId' build.Id "Bad build id"
                  failedTests

              let getDetails testId' = testDetails.[testId'] |> Some

              let getHistory (_: TestId) buildTypeId' =
                  Expect.equal buildTypeId' buildTypeId "Bad build type id"
                  testHistory

              let tryGuessFlakiness (_: TestDetails) history' build' =
                  Expect.equal history' testHistory "Bad history"
                  Expect.equal build' build "Bad build"
                  Some technical

              let log _ = ()
              let getFlakyTests = FlakyMonitor.getFlakyTests getDetails getHistory tryGuessFlakiness log

              let analyzeTest (details': TestDetails) _ _ _ =
                  failwith $"Should not be called for test {details'.Id} {details'.Name.Value}"

              let analyzeGroup (group: FlakyGroup) failed' (state: State) =
                  Expect.equal failed' build "Bad build"

                  let failures =
                      if group.Prefix = prefix1 then
                          failures1
                      else
                          failures2

                  Expect.equal group.Items.Length failures "Bad group item count"
                  let notificationId = NotificationId.Prefix group.Prefix
                  let notificationHistory = new NotificationHistory(notificationId, technical.Flakiness, IssueId 1)
                  state.Update notificationHistory

              let analyzeTests = FlakyMonitor.analyzeTests analyzeTest analyzeGroup
              let analyzeBuild _ (state: State) = state

              FlakyMonitor.run
                  latestFailedBuilds
                  newFailedTests
                  getFlakyTests
                  analyzeTests
                  analyzeBuild
                  log
                  project
                  stateFile

              let state = State.Load stateFile
              Expect.isTrue (state.Contains build.Id) "Build should be in state" ]
