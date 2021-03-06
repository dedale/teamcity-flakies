module TeamCity.Flakies.Tests.Notifications

open System

open Expecto

open TeamCity.Flakies
open TeamCity.Flakies.FileSystem
open TeamCity.Flakies.Tests.Extensions.Notifications
open TeamCity.Flakies.Tests.Extensions.RestApi

let notificationHistory =
    testList
        "NotificationHistory"
        [ testCase "Occurrences"
          <| fun _ ->
              let now = DateTime.UtcNow
              let today = now.Date
              let history = NotificationHistory.NewTest(now = now)
              Expect.equal (history.OccurrencesSince Flakiness.Default today) 1 "Bad history"

              let history = history.NewOccurrence(Flakiness.Default, today)
              Expect.equal (history.OccurrencesSince Flakiness.Default today) 2 "Bad history"

              let tomorrow = today.AddDays 1.
              let history = history.NewOccurrence(Flakiness.Default, tomorrow)
              Expect.equal (history.OccurrencesSince Flakiness.Default today) 3 "Bad history"
              Expect.equal (history.OccurrencesSince Flakiness.Default tomorrow) 1 "Bad history"

          testCase "Other flakiness"
          <| fun _ ->
              let now = DateTime.UtcNow
              let today = now.Date
              let history = NotificationHistory.NewTest(now = now)
              let history = history.NewOccurrence(Flakiness.DiskFull, today)
              Expect.equal (history.OccurrencesSince Flakiness.Default today) 1 "Bad history for Default"
              Expect.equal (history.OccurrencesSince Flakiness.DiskFull today) 1 "Bad history for DiskFull"
              Expect.equal (history.OccurrencesSince Flakiness.FileInUse today) 0 "Bad history for FileInUse"

          testCase "Purge history"
          <| fun _ ->
              let now = DateTime.UtcNow
              let today = now.Date
              let history = NotificationHistory.NewTest(now = now)

              let history =
                  [ 1 .. 8 ]
                  |> List.fold
                      (fun (h: NotificationHistory) days ->
                          h.NewOccurrence(Flakiness.DiskFull, today.AddDays(float days)))
                      history

              Expect.equal (history.OccurrencesSince Flakiness.DiskFull today) 7 "Bad history"

          testCase "None since date"
          <| fun _ ->
              let now = DateTime.UtcNow
              let history = NotificationHistory.NewTest(now = now)
              Expect.isNone (history.TryFindSince Flakiness.AccessDenied) "Bad history" ]

let state =
    testList
        "State"
        [ testCase "Builds"
          <| fun _ ->
              use temp = new TempDir()
              let file = temp.Dir.FileInDir "State.json"
              let state = State.Load file
              let buildId = BuildId.Random()
              Expect.isFalse (state.Contains buildId) "BuildId should not be found"
              let state = state.Add buildId
              Expect.isTrue (state.Contains buildId) "BuildId should be found"
              let newId = BuildId(buildId.Value + 1)
              let state = state.Add newId
              Expect.isTrue (state.Contains buildId) "BuildId should not be removed"
              let state = state.ForgetOlderThan newId
              Expect.isFalse (state.Contains buildId) "BuildId should be removed"
              Expect.isTrue (state.Contains newId) "New id should be found"
              state.Save file
              let state = State.Load file
              Expect.isFalse (state.Contains buildId) "BuildId should not be found"
              Expect.isTrue (state.Contains newId) "New id should be found"

          testList
              "History"
              ([ NotificationHistory.NewTest(), Flakiness.Default
                 NotificationHistory.NewPrefix(), Flakiness.Default
                 NotificationHistory.NewBuild(), Flakiness.BuildTimeout ]
               |> List.map
                   (fun (history, flakiness) ->
                       testCase (sprintf "%O-%O" history.Id flakiness)
                       <| fun _ ->
                           use temp = new TempDir()
                           let file = temp.Dir.FileInDir "State.json"
                           let state = State.Load file
                           Expect.isNone (state.TryHistory history.Id) "History should not exist"
                           let state = state.Update history
                           let copy = Expect.wantSome (state.TryHistory history.Id) "History should be found"
                           Expect.equal copy history "Should be equal"
                           let next = copy.NewOccurrence flakiness
                           let state = state.Update next
                           let other = NotificationHistory.NewTest()
                           let state = state.Update other
                           state.Save file
                           let state = State.Load file
                           Expect.isSome (state.TryHistory history.Id) "History should still be there"
                           Expect.isSome (state.TryHistory other.Id) "Other should be there too")) ]

let all = testList "Notifications" [ notificationHistory; state ]
