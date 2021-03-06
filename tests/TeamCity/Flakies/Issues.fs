module TeamCity.Flakies.Tests.Issues

open System

open Expecto
open Octokit

open TeamCity.Flakies
open TeamCity.Flakies.Tests.Extensions.RestApi
open TeamCity.Flakies.Tests.Extensions.Issues

let teamCityUrl = ValidUrl.create "https://teamcity.domain"

let all =
    testList
        "GitHub"
        [ testCase "New test issue"
          <| fun _ ->
              let build = FailedBuild.Random(status = Status "Failed (new)")
              let details = TestDetails.Random()
              let issue = Issue()

              let getAssignee testId' =
                  Expect.equal testId' details.Id "Bad test id"
                  None

              let createIssue (newIssue: NewIssue) =
                  Expect.equal newIssue.Title $"[Flaky] {details.Name.Value}" "Bad title"

                  let body =
                      $"The test **{details.Name.Value}** is flaky.\r\n"
                      + "\r\n"
                      + $"- Recent [test history]({teamCityUrl.Value}/project.html?tab=testDetails"
                      + $"&testNameId={details.Id.Value}&buildTypeId={build.BuildTypeId.Value}):"
                      + " Success rate: 50.0 % - Test runs: 6 total / 3 failures.\r\n"
                      + $"- Build: [link]({teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}).\r\n"
                      + $"- Build config: **{build.BuildTypeId.Value}**.\r\n"
                      + "- Flakiness: **DiskFull** (Disk is full).\r\n"
                      + "- This is a technical problem.\r\n"
                      + "- No TeamCity assignee"

                  Expect.equal newIssue.Body body "Bad body"
                  issue

              let updateIssue _ (update: IssueUpdate) =
                  Expect.isNull update.Assignees "Bad assignees"
                  Expect.equal (List.ofSeq update.Labels) [ "flaky" ] "Bad labels"

              let log _ = ()
              let history = TestHistory.FlakyHistory()
              let flakiness = Flakiness.DiskFull.ToTechnical()

              GitHub.testIssue getAssignee createIssue updateIssue log teamCityUrl details history build flakiness
              |> ignore

          testList
              "Test Descriptions"
              ([ Flakiness.FileInUse, "A file is locked by another process"
                 Flakiness.DiskFull, "Disk is full"
                 Flakiness.UnitTestTimeout, "A unit test failed because of a timeout"
                 Flakiness.RandomTestName, "Test is flaky and its name is too long for TeamCity" ]
               |> List.map
                   (fun (flakiness, description) ->
                       testCase ($"{flakiness}-{description}")
                       <| fun _ ->
                           let build = FailedBuild.Random(status = Status "Failed (new)")
                           let details = TestDetails.Random()
                           let issue = Issue()

                           let getAssignee testId' =
                               Expect.equal testId' details.Id "Bad test id"
                               None

                           let createIssue (newIssue: NewIssue) =
                               Expect.stringContains newIssue.Body description "Description not found"
                               issue

                           let updateIssue _ _ = ()
                           let log _ = ()
                           let history = TestHistory.FlakyHistory()
                           let flakiness = flakiness.ToDefault()

                           GitHub.testIssue
                               getAssignee
                               createIssue
                               updateIssue
                               log
                               teamCityUrl
                               details
                               history
                               build
                               flakiness
                           |> ignore))

          testCase "Test assignee"
          <| fun _ ->
              let build = FailedBuild.Random(status = Status "Failed (new)")
              let details = TestDetails.Random()
              let issue = Issue()

              let assignee =
                  { Login = UserLogin "ded"
                    Name = "Vianney PHILIPPE" }

              let getAssignee testId' =
                  Expect.equal testId' details.Id "Bad test id"
                  Some assignee

              let createIssue _ = issue

              let updateIssue _ (update: IssueUpdate) =
                  Expect.equal (List.ofSeq update.Assignees) [ assignee.Login.Value ] "Bad assignee"

              let log _ = ()
              let history = TestHistory.FlakyHistory()
              let flakiness = Flakiness.DiskFull.ToTechnical()

              GitHub.testIssue getAssignee createIssue updateIssue log teamCityUrl details history build flakiness
              |> ignore

          testCase "New group issue"
          <| fun _ ->
              let failures = 300
              let build = FailedBuild.Random(status = Status $"Tests failed: {failures} ({failures} new)")
              let prefix = "Prefix"
              let history = TestHistory.FlakyHistory()
              let technical = Flakiness.Default.ToDefault()

              let tests =
                  [ 1 .. failures ]
                  |> List.map
                      (fun i ->
                          let details : TestDetails =
                              { Id = TestId.Random()
                                Name = TestName $"{prefix}: \"Test.{i}\""
                                Details = "Test details" }

                          { Details = details
                            History = history
                            Technical = technical })

              let group = FlakyGroup(prefix, tests)
              let sample = List.head group.Items
              let getAssignee _ = None
              let issue = Issue()

              let createIssue (newIssue: NewIssue) =
                  Expect.equal newIssue.Title $"[Flaky] {prefix}" "Bad title"

                  let body =
                      $"{failures} tests **{prefix}** are flaky.\r\n"
                      + "\r\n"
                      + $"- Sample [test history]({teamCityUrl.Value}/project.html?tab=testDetails"
                      + $"&testNameId={sample.Details.Id.Value}&buildTypeId={build.BuildTypeId.Value}):"
                      + " Success rate: 50.0 % - Test runs: 6 total / 3 failures.\r\n"
                      + $"- Build: [link]({teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}).\r\n"
                      + $"- Build config: **{build.BuildTypeId.Value}**.\r\n"
                      + "- Flakiness: **Default**.\r\n"
                      + "- No TeamCity assignee"

                  Expect.equal newIssue.Body body "Bad body"
                  issue

              let updateIssue _ (update: IssueUpdate) =
                  Expect.isNull update.Assignees "Bad assignees"
                  Expect.equal (List.ofSeq update.Labels) [ "flaky" ] "Bad labels"

              let log _ = ()

              GitHub.groupIssue getAssignee createIssue updateIssue log teamCityUrl group build
              |> ignore

          testCase "New build issue"
          <| fun _ ->
              let build = FailedBuild.Random(status = Status "Failed (new)")
              let issue = new Issue()

              let createIssue (newIssue: NewIssue) =
                  Expect.equal newIssue.Title $"[Flaky] {build.BuildTypeId.Value}" "Bad title"

                  let body =
                      $"A flaky problem caused [this build]({teamCityUrl.Value}/"
                      + $"viewLog.html?buildId={build.Id.Value}) to fail.\r\n"
                      + "\r\n"
                      + $"- Build config: **{build.BuildTypeId.Value}**.\r\n"
                      + "- Flakiness: **BuildTimeout** (TeamCity stopped the build after timeout)."

                  Expect.equal newIssue.Body body "Bad body"
                  issue

              let updateIssue _ (update: IssueUpdate) =
                  Expect.isNull update.Assignees "Bad assignees"
                  Expect.equal (List.ofSeq update.Labels) [ "flaky" ] "Bad labels"

              let log _ = ()
              let technical = Flakiness.BuildTimeout.ToDefault()

              GitHub.buildIssue createIssue updateIssue log teamCityUrl build technical
              |> ignore

          testList
              "Build Descriptions"
              ([ Flakiness.BuildTimeout, "TeamCity stopped the build after timeout"
                 Flakiness.MSBuildCrash, "MSBuild crashed"
                 Flakiness.MSBuildSDKNotFound, "MSBuild failed to find some SDK"
                 Flakiness.OutOfMemory, "Out of memory"
                 Flakiness.FolderInUse, "A folder was locked and could not be renamed or removed"
                 Flakiness.AccessDenied, "Access denied to a resource (file, etc)"
                 Flakiness.PipFailed, "pip install failed because a requirement was not found" ]
               |> List.map
                   (fun (flakiness, description) ->
                       testCase ($"{flakiness}-{description}")
                       <| fun _ ->
                           let build = FailedBuild.Random(status = Status "Failed (new)")
                           let issue = new Issue()

                           let createIssue (newIssue: NewIssue) =
                               Expect.stringContains newIssue.Body description "Description not found"
                               issue

                           let updateIssue _ _ = ()
                           let log _ = ()
                           let technical = flakiness.ToDefault()

                           GitHub.buildIssue createIssue updateIssue log teamCityUrl build technical
                           |> ignore))

          testCase "Test new occurrence"
          <| fun _ ->
              let issueId = IssueId.Random()
              let build = FailedBuild.Random(status = Status "Failed (new)")
              let details = TestDetails.Random()
              let issue = Issue()
              let now = DateTime.UtcNow
              let notificationId = NotificationId.Test(details.Id, details.Name)

              let notificationHistory =
                  NotificationHistory(notificationId, Flakiness.Default, issueId, Some now)
                      .NewOccurrence(Flakiness.Default, now)

              let getIssue issueId' =
                  Expect.equal issueId' issueId.Value "Bad issue id"
                  issue

              let updateIssue _ = failwith "Should not be called"

              let commentIssue issueId' comment' =
                  Expect.equal issueId' issueId.Value "Bad issue id"
                  let now = now.ToString("yyyy-MM-dd")

                  let comment =
                      "New occurrence in [this build]"
                      + $"({teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}).\r\n"
                      + "Flakiness: **Default**\r\n"
                      + $"2 occurrences since {now}"

                  Expect.equal comment' comment "Bad comment"

              let technical = Flakiness.Default.ToDefault()

              GitHub.updateIssue
                  getIssue
                  updateIssue
                  commentIssue
                  teamCityUrl
                  issueId
                  build
                  technical
                  notificationHistory
              |> ignore

          testCase "Reopen issue"
          <| fun _ ->
              let issueId = IssueId.Random()
              let build = FailedBuild.Random(status = Status "Failed (new)")
              let details = TestDetails.Random()

              let issue =
                  Issue(
                      null,
                      null,
                      null,
                      null,
                      issueId.Value,
                      ItemState.Closed,
                      null,
                      null,
                      null,
                      null,
                      null,
                      null,
                      null,
                      null,
                      0,
                      null,
                      System.Nullable(),
                      DateTimeOffset.UtcNow,
                      System.Nullable(),
                      0,
                      null,
                      false,
                      null,
                      null
                  )

              let getIssue issueId' =
                  Expect.equal issueId' issueId.Value "Bad issue id"
                  issue

              let updateIssue issueId' (update: IssueUpdate) =
                  Expect.equal issueId' issueId.Value "Bad issue id"
                  Expect.equal update.State.Value ItemState.Open "Bad state"

              let commentIssue issueId' comment' =
                  Expect.equal issueId' issueId.Value "Bad issue id"

                  let comment =
                      "New occurrence in [this build]"
                      + $"({teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}).\r\n"
                      + "Flakiness: **Default**"

                  Expect.equal comment' comment "Bad comment"

              let notificationId = NotificationId.Test(details.Id, details.Name)
              let notificationHistory = NotificationHistory(notificationId, Flakiness.Default, issueId)
              let technical = Flakiness.Default.ToDefault()

              GitHub.updateIssue
                  getIssue
                  updateIssue
                  commentIssue
                  teamCityUrl
                  issueId
                  build
                  technical
                  notificationHistory
              |> ignore

          ]
