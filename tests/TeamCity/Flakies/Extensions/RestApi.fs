module TeamCity.Flakies.Tests.Extensions.RestApi

open System

open TeamCity.Flakies

let random = Random()

type String with
    static member Random() =
        let id = Guid.NewGuid()
        id.ToString()

type BuildId with
    static member Random() = BuildId(random.Next())
    static member Random(min, max) = BuildId(random.Next(min, max))

type BuildTypeId with
    static member Random() = BuildTypeId(String.Random())

type BuildStatus with
    static member Random() = Status(String.Random())

type FailedBuild with
    static member Random(?id, ?buildTypeId, ?status) =
        { Id = defaultArg id (BuildId.Random())
          BuildTypeId = defaultArg buildTypeId (BuildTypeId.Random())
          Status = defaultArg status (BuildStatus.Random()) }

type FailedTest with
    static member Random(?id, ?name) =
        let name = defaultArg name (String.Random())

        { Id = defaultArg id (String.Random())
          Name = TestName name }

type TestId with
    static member Random() =
        let left = int64 (random.Next()) <<< 32
        let right = int64 (random.Next())
        left ||| right |> TestId

type TestName with
    static member Random() = String.Random() |> TestName

type TestHistory with
    static member FlakyHistory() =
        TestHistory(
            BuildId 1,
            [ false
              true
              false
              true
              false
              true ],
            false
        )

type TestDetails with
    static member Random() : TestDetails =
        { Id = TestId.Random()
          Name = TestName "Dummy.Test"
          Details = String.Random() }
