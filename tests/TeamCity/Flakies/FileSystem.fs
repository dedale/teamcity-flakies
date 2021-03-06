module TeamCity.Flakies.Tests.FileSystem

open TeamCity.Flakies.FileSystem

open Expecto

let tempDir =
    testList
        "TempDir"
        [ testCase "Create temp dir"
          <| fun _ ->
              let tempDir =
                  use temp = new TempDir()
                  Expect.isTrue temp.Dir.Exists ""
                  temp.Dir

              Expect.isFalse tempDir.Exists ""

          testCase "Temp dir name"
          <| fun _ ->
              let prefix = "WantedTempDirName"
              use temp = new TempDir(prefix)
              Expect.isTrue (temp.Dir.Name.StartsWith prefix) "" ]

let all = testList "FileSystem" [ tempDir ]
