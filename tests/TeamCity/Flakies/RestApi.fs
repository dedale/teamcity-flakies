module TeamCity.Flakies.Tests.RestApi

open Expecto
open System

open TeamCity.Flakies
open TeamCity.Flakies.Tests.Extensions.RestApi
open TeamCity.Flakies.Tests.Xml

open type TeamCity.Flakies.Tests.Xml.XmlContainer

let testHistory =
    testList
        "TestHistory"
        [ testCase "Typical flaky"
          <| fun _ ->
              let successes = [ 0 .. 200 ] |> List.map (fun i -> i % 7 <> 0)
              let history = TestHistory(BuildId 1, successes, false)
              Expect.isTrue history.IsFlaky "test history should be flaky"

          testCase "Long period not flaky"
          <| fun _ ->
              let successes = [ 0 .. 250 ] |> List.map (fun i -> i / 50 % 2 = 1)
              let history = TestHistory(BuildId 1, successes, true)
              Expect.isFalse history.IsFlaky "test history should not be flaky"

          testList
              "Not flaky"
              ([ 1, 1, false
                 1, 500, false
                 2, 500, false
                 20, 500, true ]
               |> List.map
                   (fun (failures, total, successive) ->
                       test $"%d{failures},%d{total},%b{successive}" {
                           let successes = [ 1 .. total ] |> List.map (fun i -> i > failures)
                           let history = TestHistory(BuildId 1, successes, successive)
                           Expect.isFalse history.IsFlaky "test history should be flaky"
                       })) ]

let restApi =
    testList
        "RestApi"
        [ testCase "Latest failed builds"
          <| fun _ ->
              let failedBuild = FailedBuild.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      "app/rest/builds/?locator=affectedProject:MyProject,status:FAILURE,state:finished,count:500&fields=build(id,buildTypeId,statusText)"
                      "Bad url"

                  Element(
                      "builds",
                      Element("build", Element("statusText", "ignored build with missing attributes")),
                      Element(
                          "build",
                          Attribute("id", failedBuild.Id.Value),
                          Attribute("buildTypeId", failedBuild.BuildTypeId.Value),
                          Element("statusText", failedBuild.Status.Value)
                      )
                  )
                  |> outerXml

              let latestFailedBuilds = TeamCity.latestFailedBuilds (TeamCity.getRestString getString)
              let builds = latestFailedBuilds (ProjectId "MyProject") None
              Expect.equal (Seq.exactlyOne builds) failedBuild "Bad build"

          testCase "New failed tests"
          <| fun _ ->
              let buildId = BuildId.Random()
              let failedTest = FailedTest.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/testOccurrences?locator=build:(id:%d{buildId.Value}),count:500,status:FAILURE&fields=testOccurrence(id,name,firstFailed(id),mute(id))"
                      "Bad url"

                  Element(
                      "testOccurrences",
                      Element("testOccurrence", Attribute("id", "ignored test with missing name")),
                      Element("testOccurrence", Attribute("name", "ignored test with missing id")),
                      Element(
                          "testOccurrence",
                          Attribute("id", String.Random()),
                          Attribute("name", String.Random()),
                          Element("firstFailed", Attribute("id", "ignored test already broken"))
                      ),
                      Element(
                          "testOccurrence",
                          Attribute("id", String.Random()),
                          Attribute("name", String.Random()),
                          Attribute("details", "ignored test muted"),
                          Element("mute", Attribute("id", "1"))
                      ),
                      Element(
                          "testOccurrence",
                          Attribute("id", failedTest.Id),
                          Attribute("name", failedTest.Name.Value)
                      )
                  )
                  |> outerXml

              let newFailedTests = TeamCity.newFailedTests (TeamCity.getRestString getString)
              let tests = newFailedTests buildId None
              Expect.equal (Seq.exactlyOne tests) failedTest "Bad test"

          testCase "Test history count"
          <| fun _ ->
              let minBuildId = BuildId.Random(1000, 2000)
              let older = random.Next(100, 200)
              let recent = random.Next(100, 200)
              let buildTypeId = BuildTypeId.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/builds?locator=buildType:id:%s{buildTypeId.Value}&fields=build(id)"
                      "Bad url"

                  Element(
                      "builds",
                      [| 1 .. older + recent |]
                      |> Array.map (fun i -> Element("build", Attribute("id", minBuildId.Value + recent - i)))
                  )
                  |> outerXml

              let testHistoryCount = TeamCity.testHistoryCount (TeamCity.getRestString getString)
              let count = testHistoryCount buildTypeId minBuildId
              Expect.equal count recent "Bad history count"

          testCase "Test history"
          <| fun _ ->
              let testId = TestId.Random()
              let buildTypeId = BuildTypeId.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/testOccurrences?locator=test:(id:%d{testId.Value}),buildType:(id:%s{buildTypeId.Value}),count:500&fields=testOccurrence(status,build(id,number))"
                      "Bad url"

                  Element(
                      "testOccurrences",
                      Element(
                          "testOccurrence",
                          Attribute("status", "SUCCESS"),
                          Element("build", Attribute("id", 4), Attribute("number", "major.minor.revision.4"))
                      ),
                      Element(
                          "testOccurrence",
                          Attribute("status", "FAILURE"),
                          Element("build", Attribute("id", 3), Attribute("number", "major.minor.revision.3"))
                      ),
                      Element(
                          "testOccurrence",
                          Attribute("status", "SUCCESS"),
                          Element("build", Attribute("id", 2), Attribute("number", "major.minor.revision.2"))
                      )
                  )
                  |> outerXml

              let testHistory = TeamCity.testHistory (TeamCity.getRestString getString)
              let history = testHistory testId buildTypeId None
              Expect.equal [ true; false; true ] history.Successes "Bad successes"
              Expect.equal 2 history.FirstBuildId.Value "Bad first build id"
              Expect.isFalse history.Successive "Bad successive"

          testCase "Empty test history"
          <| fun _ ->
              let testId = TestId.Random()
              let buildTypeId = BuildTypeId.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/testOccurrences?locator=test:(id:%d{testId.Value}),buildType:(id:%s{buildTypeId.Value}),count:500&fields=testOccurrence(status,build(id,number))"
                      "Bad url"

                  Element("testOccurrences").ToString()

              let testHistory = TeamCity.testHistory (TeamCity.getRestString getString)
              let history = testHistory testId buildTypeId None
              Expect.equal [] history.Successes "Bad successes"
              Expect.equal Int32.MaxValue history.FirstBuildId.Value "Bad first build id"
              Expect.isFalse history.Successive "Bad successive"

          testCase "Successive test history"
          <| fun _ ->
              let testId = TestId.Random()
              let buildTypeId = BuildTypeId.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/testOccurrences?locator=test:(id:%d{testId.Value}),buildType:(id:%s{buildTypeId.Value}),count:500&fields=testOccurrence(status,build(id,number))"
                      "Bad url"

                  Element(
                      "testOccurrences",
                      [| 4000, 403
                         3000, 402
                         2000, 401
                         1000, 200 |]
                      |> Array.map
                          (fun (buildId, buildCounter) ->
                              Element(
                                  "testOccurrence",
                                  Attribute("status", "FAILURE"),
                                  Element(
                                      "build",
                                      Attribute("id", buildId),
                                      Attribute("number", $"major.minor.revision.%d{buildCounter}")
                                  )
                              ))
                  )
                  |> outerXml

              let testHistory = TeamCity.testHistory (TeamCity.getRestString getString)
              let history = testHistory testId buildTypeId None
              Expect.equal [ false; false; false; false ] history.Successes "Bad successes"
              Expect.equal 1000 history.FirstBuildId.Value "Bad first build id"
              Expect.isTrue history.Successive "Bad successive"

          testCase "Test details"
          <| fun _ ->
              let now = DateTime.Now
              let time = now.TimeOfDay
              let id = $"id:%d{int time.TotalSeconds},build:(id:%d{int time.TotalMilliseconds})"
              let testId = TestId.Random()
              let testName = TestName.Random()
              let details = String.Random()

              let getString relativeUrl =
                  Expect.equal relativeUrl $"app/rest/testOccurrences/%s{id}" "Bad url"

                  Element(
                      "testOccurrence",
                      Element("details", details),
                      Element("test", Attribute("id", testId.Value), Attribute("name", testName.Value))
                  )
                  |> outerXml

              let testDetails = TeamCity.testDetails (TeamCity.getRestString getString) id

              Expect.equal
                  testDetails
                  (Some
                      { Id = testId
                        Name = testName
                        Details = details })
                  "Bad details"

          testCase "Empty test details"
          <| fun _ ->
              let now = DateTime.Now
              let time = now.TimeOfDay
              let id = $"id:%d{int time.TotalSeconds},build:(id:%d{int time.TotalMilliseconds})"

              let getString relativeUrl =
                  Expect.equal relativeUrl $"app/rest/testOccurrences/%s{id}" "Bad url"
                  Element("testOccurrence").ToString()

              let testDetails = TeamCity.testDetails (TeamCity.getRestString getString) id
              Expect.isNone testDetails "Bad details"

          testCase "Missing assignee"
          <| fun _ ->
              let id = TestId.Random()

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/investigations?locator=test:(id:%d{id.Value})&fields=investigation(state,assignee(username,name))"
                      "Bad url"

                  Element("investigations").ToString()

              let assignee = TeamCity.assignee (TeamCity.getRestString getString) id
              Expect.isNone assignee "Bad assignee"

          testCase "Get assignee"
          <| fun _ ->
              let id = TestId.Random()

              let john =
                  { Login = UserLogin "jdoe"
                    Name = "John DOE" }

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/investigations?locator=test:(id:%d{id.Value})&fields=investigation(state,assignee(username,name))"
                      "Bad url"

                  Element(
                      "investigations",
                      Element(
                          "investigation",
                          Attribute("state", "TAKEN"),
                          Element("assignee", Attribute("username", john.Login.Value), Attribute("name", john.Name))
                      )
                  )
                  |> outerXml

              let assignee = TeamCity.assignee (TeamCity.getRestString getString) id
              Expect.equal assignee (Some john) "Bad assignee"

          testCase "Get assignee given up"
          <| fun _ ->
              let id = TestId.Random()

              let john =
                  { Login = UserLogin "jdoe"
                    Name = "John DOE" }

              let getString relativeUrl =
                  Expect.equal
                      relativeUrl
                      $"app/rest/investigations?locator=test:(id:%d{id.Value})&fields=investigation(state,assignee(username,name))"
                      "Bad url"

                  Element(
                      "investigations",
                      Element(
                          "investigation",
                          Attribute("state", "GIVEN_UP"),
                          Element("assignee", Attribute("username", john.Login.Value), Attribute("name", john.Name))
                      )
                  )
                  |> outerXml

              let assignee = TeamCity.assignee (TeamCity.getRestString getString) id
              Expect.isNone assignee "Bad assignee"

          testCase "Download log"
          <| fun _ ->
              let id = BuildId.Random()
              let log = String.Random()

              let getString relativeUrl =
                  Expect.equal relativeUrl $"downloadBuildLog.html?buildId={id.Value}" "Bad url"
                  log

              let downloaded = TeamCity.downloadLog getString id
              Expect.equal downloaded log "Bad log contents"

          testCase "Get problems"
          <| fun _ ->
              let id = BuildId.Random()

              let compilationProblem =
                  { Type = ProblemType "TC_COMPILATION_ERROR"
                    Details = "Compilation error : IDontLikeCpp.vcxproj" }

              let timeoutProblem =
                  { Type = ProblemType "TC_EXECUTION_TIMEOUT"
                    Details = "Execution timeout" }

              let getString relativeUrl =
                  Expect.equal
                      $"app/rest/problemOccurrences?locator=build:(id:{id.Value})&fields=problemOccurrence(type,details)"
                      relativeUrl
                      "Bad url"

                  Element(
                      "problemOccurrences",
                      Element(
                          "problemOccurrence",
                          Attribute("type", compilationProblem.Type.Value),
                          Attribute("details", compilationProblem.Details)
                      ),
                      Element(
                          "problemOccurrence",
                          Attribute("type", timeoutProblem.Type.Value),
                          Attribute("details", timeoutProblem.Details)
                      ),
                      Element("problemOccurrence", Attribute("type", "TC_MISSING_DETAILS")),
                      Element("problemOccurrence", Attribute("details", "missing type"))
                  )
                  |> outerXml

              let problems =
                  TeamCity.problems (TeamCity.getRestString getString) id
                  |> List.ofSeq

              Expect.equal problems ([ compilationProblem; timeoutProblem ]) "Bad problems" ]

let all = testList "REST API" [ testHistory; restApi ]
