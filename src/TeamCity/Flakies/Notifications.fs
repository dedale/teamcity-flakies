namespace TeamCity.Flakies

open System
open System.ComponentModel

open TeamCity.Flakies.FileSystem
open TeamCity.Flakies.Serialization

[<AbstractClass>]
type StringConverter<'a>() =
    inherit TypeConverter()

    override __.CanConvertFrom(context, sourceType) =
        sourceType = typedefof<string>
        || base.CanConvertFrom(context, sourceType)

    override __.CanConvertTo(context, destinationType) =
        destinationType = typedefof<string>
        || base.CanConvertTo(context, destinationType)

    override x.ConvertFrom(context, culture, value) =
        if value = null then
            nullArg <| nameof (value)
        else
            match value :?> string with
            | str when str <> null -> x.FromString str :> obj
            | _ -> base.ConvertFrom(context, culture, value)

    override x.ConvertTo(context, culture, value, destinationType) =
        if value = null then
            nullArg <| nameof (value)
        else if destinationType = typedefof<string> then
            x.ToString(value :?> 'a) :> obj
        else
            base.ConvertTo(context, culture, value, destinationType)

    abstract member FromString : string -> 'a

    abstract member ToString : 'a -> string

/// Id of a flaky notification
/// Default: for a test
/// Prefix: for a test group of a random test whose name changes at every build
/// BuildTypeId: for a build (e.g. flaky problems)
[<RequireQualifiedAccess>]
[<TypeConverter(typedefof<NotificationIdConverter>)>]
type NotificationId =
    | Test of TestId * TestName
    | Prefix of string
    | BuildTypeId of BuildTypeId

and NotificationIdConverter() =
    inherit StringConverter<NotificationId>()

    override __.FromString str =
        if str.[0] = 'T' then
            let parts = str.Substring(1).Split('|')
            let testId = TestId(Int64.Parse parts.[0])
            let testName = TestName(parts.[1])
            NotificationId.Test(testId, testName)
        elif str.[0] = 'P' then
            NotificationId.Prefix(str.Substring(1))
        else // 'B'
            NotificationId.BuildTypeId(BuildTypeId(str.Substring(1)))

    override __.ToString value =
        match value with
        | NotificationId.Test (testId, testName) -> $"T{testId.Value}|{testName.Value}"
        | NotificationId.Prefix name -> $"P{name}"
        | NotificationId.BuildTypeId x -> $"B{x.Value}"

type Flakiness =
    // Common
    | FileInUse = 0
    | DiskFull = 1
    // Tests
    | Default = 2
    | UnitTestTimeout = 3
    | RandomTestName = 4
    // Builds
    | BuildTimeout = 5
    | MSBuildCrash = 6
    | MSBuildSDKNotFound = 7
    | OutOfMemory = 8
    | FolderInUse = 9
    | AccessDenied = 10
    | PipFailed = 11

// IsTechnical: is it a technical problem
type TechnicalFlakiness =
    { Flakiness: Flakiness
      IsTechnical: bool }

[<AutoOpen>]
module FlakinessExtensions =
    type Flakiness with
        member x.ToTechnical() = { Flakiness = x; IsTechnical = true }

    type Flakiness with
        member x.ToDefault() = { Flakiness = x; IsTechnical = false }

type IssueId = IssueId of int

module IssueId =
    let value (IssueId x) = x

type IssueId with
    member x.Value = IssueId.value x

type FlakinessOccurrences = Map<Flakiness, Map<DateTime, int>>

type NotificationHistory(notificationId: NotificationId, issueId: IssueId, occurrences) =

    let maxOccurrencesDay = 7

    static member private NewOccurrences(flakiness: Flakiness, now) =
        let now = defaultArg now DateTime.UtcNow
        Map.empty.Add(flakiness, Map.empty.Add(now.Date, 1))

    new(notificationId, flakiness, issueId) = NotificationHistory(notificationId, flakiness, issueId, None)

    new(notificationId, flakiness: Flakiness, issueId: IssueId, now) =
        NotificationHistory(notificationId, issueId, NotificationHistory.NewOccurrences(flakiness, now))

    member __.Id = notificationId
    member __.IssueId = issueId

    member __.OccurrencesSince flakiness day =
        occurrences
        |> Map.filter (fun f _ -> f = flakiness)
        |> Map.toSeq
        |> Seq.collect (snd >> Map.toSeq)
        |> Seq.where (fun (d, _) -> d >= day)
        |> Seq.sumBy (fun (_, n) -> n)

    member __.NewOccurrence(flakiness, ?day) =
        let today = defaultArg day DateTime.Today

        let newOccurrences =
            match occurrences.TryFind flakiness with
            | Some byDate ->
                let newByDate =
                    match byDate.TryFind today with
                    | Some count -> byDate.Remove(today).Add(today, count + 1)
                    | _ -> byDate.Add(today, 1)

                let newByDate =
                    newByDate
                    |> Map.toSeq
                    |> Seq.where (fun (d, _) -> d <= today.AddDays(float -maxOccurrencesDay))
                    |> Seq.fold (fun (m: Map<DateTime, int>) (d, _) -> Map.remove d m) newByDate

                occurrences.Remove(flakiness).Add(flakiness, newByDate)
            | None -> occurrences.Add(flakiness, Map.empty.Add(today, 1))

        NotificationHistory(notificationId, issueId, newOccurrences)

    member __.TryFindSince flakiness =
        match occurrences.TryFind flakiness with
        | Some byDate ->
            byDate
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.min
            |> Some
        | _ -> None

type CreateIssue = FailedBuild -> TechnicalFlakiness -> IssueId

type UpdateIssue = IssueId -> FailedBuild -> TechnicalFlakiness -> NotificationHistory -> unit

type State private (builds: Set<BuildId>, histories: Map<NotificationId, NotificationHistory>) =
    private new() = State(Set.empty, Map.empty)

    static member Load(file: FilePath) : State =
        if file.Exists then
            file.ReadAllText()
            |> TextSerializer.deserialize<State>
        else
            State()

    member x.Save(file: FilePath) =
        TextSerializer.serialize x |> file.WriteAllText

    member __.Contains buildId = builds.Contains buildId

    member __.Add buildId = State(builds.Add buildId, histories)

    member __.ForgetOlderThan(buildId: BuildId) =
        let newBuilds =
            builds
            |> Set.filter (fun b -> b.Value >= buildId.Value)

        State(newBuilds, histories)

    member __.TryHistory notificationId = histories.TryFind notificationId

    member __.Update(history: NotificationHistory) =
        State(builds, histories.Remove(history.Id).Add(history.Id, history))

    member x.NewOccurrence (create: CreateIssue) update build notificationId technical =
        match x.TryHistory notificationId with
        | Some history ->
            let history = history.NewOccurrence(technical.Flakiness)
            update history.IssueId build technical history
            history
        | _ ->
            let issueId = create build technical
            NotificationHistory(notificationId, technical.Flakiness, issueId)
        |> x.Update
