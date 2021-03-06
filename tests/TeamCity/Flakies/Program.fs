//module TeamCity.Flakies.Tests

open Expecto

let all =
    testList
        "All"
        [ TeamCity.Flakies.Tests.FileSystem.all
          TeamCity.Flakies.Tests.RestApi.all
          TeamCity.Flakies.Tests.Notifications.all
          TeamCity.Flakies.Tests.Analyzer.all
          TeamCity.Flakies.Tests.Monitor.all
          TeamCity.Flakies.Tests.Issues.all ]

[<EntryPoint>]
let main argv = runTests defaultConfig all
