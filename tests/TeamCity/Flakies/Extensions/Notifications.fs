module TeamCity.Flakies.Tests.Extensions.Notifications

open System

open TeamCity.Flakies
open TeamCity.Flakies.Tests.Extensions.RestApi

type NotificationId with
    static member RandomTest() =
        NotificationId.Test(TestId.Random(), TestName.Random())

    static member RandomPrefix() = NotificationId.Prefix(String.Random())

    static member RandomBuild() =
        NotificationId.BuildTypeId(BuildTypeId.Random())

type NotificationHistory with
    static member NewTest(?flakiness, ?issueId, ?now) =
        let flakiness = defaultArg flakiness Flakiness.Default
        let issueId = defaultArg issueId (IssueId 1)
        NotificationHistory(NotificationId.RandomTest(), flakiness, issueId, now)

    static member NewPrefix(?flakiness, ?issueId, ?now) =
        let flakiness = defaultArg flakiness Flakiness.Default
        let issueId = defaultArg issueId (IssueId 1)
        NotificationHistory(NotificationId.RandomPrefix(), flakiness, issueId, now)

    static member NewBuild(?flakiness, ?issueId, ?now) =
        let flakiness = defaultArg flakiness Flakiness.Default
        let issueId = defaultArg issueId (IssueId 1)
        NotificationHistory(NotificationId.RandomBuild(), flakiness, issueId, now)
