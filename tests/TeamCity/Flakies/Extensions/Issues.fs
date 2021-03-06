module TeamCity.Flakies.Tests.Extensions.Issues

open TeamCity.Flakies
open TeamCity.Flakies.Tests.Extensions.RestApi

type IssueId with
    static member Random() = random.Next() |> IssueId
