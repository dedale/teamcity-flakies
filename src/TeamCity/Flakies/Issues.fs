namespace TeamCity.Flakies

open Octokit

open System
open System.Globalization
open System.Text

module GitHub =
    type Double with
        member x.ToStringInvariant format =
            x.ToString(format, CultureInfo.InvariantCulture)

    let private flakinessDescription flakiness =
        match flakiness with
        | Flakiness.FileInUse -> Some "A file is locked by another process"
        | Flakiness.DiskFull -> Some "Disk is full"
        | Flakiness.UnitTestTimeout -> Some "A unit test failed because of a timeout"
        | Flakiness.RandomTestName -> Some "Test is flaky and its name is too long for TeamCity"
        | Flakiness.BuildTimeout -> Some "TeamCity stopped the build after timeout"
        | Flakiness.MSBuildCrash -> Some "MSBuild crashed"
        | Flakiness.MSBuildSDKNotFound -> Some "MSBuild failed to find some SDK"
        | Flakiness.OutOfMemory -> Some "Out of memory"
        | Flakiness.FolderInUse -> Some "A folder was locked and could not be renamed or removed"
        | Flakiness.AccessDenied -> Some "Access denied to a resource (file, etc)"
        | Flakiness.PipFailed -> Some "pip install failed because a requirement was not found"
        | _ -> None
        |> function
        | Some description -> $" ({description})"
        | _ -> String.Empty

    let testIssue
        getAssignee
        (createIssue: NewIssue -> Issue)
        updateIssue
        log
        (teamCityUrl: ValidUrl)
        (details: TestDetails)
        (history: TestHistory)
        (build: FailedBuild)
        (technical: TechnicalFlakiness)
        =
        let newIssue = NewIssue($"[Flaky] {details.Name.Value}")
        let total = history.Successes.Length

        let successRate =
            (float (List.filter id history.Successes).Length
             / float total)
                .ToStringInvariant("P1")

        let historyUrl =
            $"{teamCityUrl.Value}/project.html?tab=testDetails"
            + $"&testNameId={details.Id.Value}"
            + $"&buildTypeId={build.BuildTypeId.Value}"

        let failures = (List.filter not history.Successes).Length
        let flakiness = technical.Flakiness
        let buildUrl = $"{teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}"
        let assignee = getAssignee details.Id

        let assignees =
            match assignee with
            | Some assignee -> [ assignee.Login.Value ]
            | _ -> []

        let body =
            StringBuilder()
                .AppendLine($"The test **{details.Name.Value}** is flaky.")
                .AppendLine()
                .Append($"- Recent [test history]({historyUrl}): ")
                .Append($"Success rate: {successRate} ")
                .AppendLine($"- Test runs: {total} total / {failures} failures.")
                .AppendLine($"- Build: [link]({buildUrl}).")
                .AppendLine($"- Build config: **{build.BuildTypeId.Value}**.")
                .AppendLine($"- Flakiness: **{flakiness}**{flakinessDescription flakiness}.")

        let body =
            if technical.IsTechnical then
                body.AppendLine("- This is a technical problem.")
            else
                body

        let body =
            match assignee with
            | Some assignee -> body.AppendLine($"- TeamCity assignee: **{assignee.Name}**")
            | _ -> body.AppendLine("- No TeamCity assignee")

        newIssue.Body <- body.ToString().Trim()
        log $"Issue body: {newIssue.Body}"
        let issue = createIssue newIssue
        let update = IssueUpdate()
        List.iter update.AddAssignee assignees

        if not assignees.IsEmpty then
            log $"""Issue assignees: {String.Join(", ", assignees)}"""
        // TODO add other custom labels
        update.AddLabel "flaky"
        log $"""Issue labels: {String.Join(", ", update.Labels)}"""
        updateIssue issue.Number update
        IssueId issue.Number

    let groupIssue
        getAssignee
        (createIssue: NewIssue -> Issue)
        updateIssue
        log
        (teamCityUrl: ValidUrl)
        (group: FlakyGroup)
        (build: FailedBuild)
        =
        let newIssue = NewIssue($"[Flaky] {group.Prefix}")
        let test = group.Items.Head
        let history = test.History

        let technicals =
            group.Items
            |> List.map (fun t -> t.Technical)
            |> Set.ofList

        let total = history.Successes.Length

        let successRate =
            (float (List.filter id history.Successes).Length
             / float total)
                .ToStringInvariant("P1")

        let historyUrl =
            $"{teamCityUrl.Value}/project.html?tab=testDetails"
            + $"&testNameId={test.Details.Id.Value}"
            + $"&buildTypeId={build.BuildTypeId.Value}"

        let failures = (List.filter not history.Successes).Length
        let buildUrl = $"{teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}"

        let teamCityAssignees =
            group.Items
            |> List.choose (fun t -> getAssignee t.Details.Id)
            |> Set.ofList

        let flakinesses = technicals |> Set.map (fun t -> t.Flakiness)

        let plural, isAre =
            if group.Items.Length > 1 then
                "s", "are"
            else
                "", "is"

        let body =
            StringBuilder()
                .AppendLine($"""{group.Items.Length} test{plural} **{group.Prefix}** {isAre} flaky.""")
                .AppendLine()
                .Append($"- Sample [test history]({historyUrl}): ")
                .Append($"Success rate: {successRate} ")
                .AppendLine($"- Test runs: {total} total / {failures} failures.")
                .AppendLine($"- Build: [link]({buildUrl}).")
                .AppendLine($"- Build config: **{build.BuildTypeId.Value}**.")

        let body =
            flakinesses
            |> Set.fold
                (fun (body: StringBuilder) f -> body.AppendLine($"- Flakiness: **{f}**{flakinessDescription f}."))
                body

        let body =
            if teamCityAssignees.Count > 0 then
                let plural =
                    if teamCityAssignees.Count > 1 then
                        "s"
                    else
                        String.Empty

                let names = String.Join(", ", teamCityAssignees |> Set.map (fun a -> a.Name))
                body.AppendLine($"- TeamCity assignee{plural}: **{names}**")
            else
                body.AppendLine("- No TeamCity assignee")

        newIssue.Body <- body.ToString().Trim()
        log $"Issue body: {newIssue.Body}"
        let issue = createIssue newIssue
        let update = IssueUpdate()
        let assignees = Set.map (fun a -> a.Login.Value) teamCityAssignees
        Set.iter update.AddAssignee assignees

        if not assignees.IsEmpty then
            log $"""Issue assignees: {String.Join(", ", assignees)}"""
        // TODO add other custom labels
        update.AddLabel "flaky"
        log $"""Issue labels: {String.Join(", ", update.Labels)}"""
        updateIssue issue.Number update
        IssueId issue.Number

    let buildIssue
        (createIssue: NewIssue -> Issue)
        updateIssue
        log
        (teamCityUrl: ValidUrl)
        (build: FailedBuild)
        (technical: TechnicalFlakiness)
        =
        let newIssue = new NewIssue($"[Flaky] {build.BuildTypeId.Value}")
        let buildUrl = $"{teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}"
        let flakiness = technical.Flakiness

        let body =
            StringBuilder()
                .AppendLine($"A flaky problem caused [this build]({buildUrl}) to fail.")
                .AppendLine()
                .AppendLine($"- Build config: **{build.BuildTypeId.Value}**.")
                .AppendLine($"- Flakiness: **{flakiness}**{flakinessDescription flakiness}.")

        let body =
            if technical.IsTechnical then
                body.AppendLine("- This is a technical problem.")
            else
                body

        newIssue.Body <- body.ToString().Trim()
        log $"Issue body: {newIssue.Body}"
        let issue = createIssue newIssue
        let update = IssueUpdate()
        update.AddLabel "flaky"
        log $"""Issue labels: {String.Join(", ", update.Labels)}"""
        updateIssue issue.Number update
        IssueId issue.Number

    let updateIssue
        (getIssue: int -> Issue)
        updateIssue
        (commentIssue: int -> string -> unit)
        (teamCityUrl: ValidUrl)
        (issueId: IssueId)
        (build: FailedBuild)
        (technical: TechnicalFlakiness)
        (notificationHistory: NotificationHistory)
        =
        let issue = getIssue issueId.Value

        if issue.State = StringEnum.op_Implicit ItemState.Closed then
            let update = IssueUpdate()
            update.State <- ItemState.Open
            updateIssue issueId.Value update

        let buildUrl = $"{teamCityUrl.Value}/viewLog.html?buildId={build.Id.Value}"

        let comment =
            StringBuilder()
                .AppendLine($"New occurrence in [this build]({buildUrl}).")
                .AppendLine($"Flakiness: **{technical.Flakiness}**{flakinessDescription technical.Flakiness}")

        let comment =
            match notificationHistory.TryFindSince technical.Flakiness with
            | Some sinceDate ->
                let occurrences = notificationHistory.OccurrencesSince technical.Flakiness sinceDate

                if occurrences > 1 then
                    let date = sinceDate.ToString("yyyy-MM-dd")
                    comment.AppendLine($"{occurrences} occurrences since {date}")
                else
                    comment
            | _ -> comment

        commentIssue issueId.Value (comment.ToString().Trim())
