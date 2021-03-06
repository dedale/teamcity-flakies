open Argu
open Octokit

open System
open System.Net.Http

open TeamCity.Flakies
open TeamCity.Flakies.FileSystem

type Arguments =
    | [<Mandatory; AltCommandLine("-tc")>] TeamCityUrl of teamCity: string
    | [<Mandatory; AltCommandLine("-p")>] TeamCityProjectId of projectId: string
    | [<Mandatory; AltCommandLine("-gh")>] GitHubUrl of gitHub: string
    | [<Mandatory; AltCommandLine("-o")>] GitHubOwner of owner: string
    | [<Mandatory; AltCommandLine("-r")>] GitHubRepo of repoName: string
    | [<Mandatory; AltCommandLine("-t")>] GitHubToken of token: string
    | [<Mandatory; AltCommandLine("-f")>] StateFile of stateFile: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | TeamCityUrl _ -> "url of TeamCity instance."
            | TeamCityProjectId _ -> "TeamCity project id to monitor for flaky tests & problems."
            | GitHubUrl _ -> "url of GitHub (Enterprise) server."
            | GitHubOwner _ -> "GitHub owner of repo."
            | GitHubRepo _ -> "name of GitHub repo where issues are created."
            | GitHubToken _ -> "personal access token for current user to GitHub repo."
            | StateFile _ -> "path of state file for persistence."

let run (args: ParseResults<Arguments>) =
    let teamCityUrl = args.GetResult TeamCityUrl |> ValidUrl.create
    let projectId = args.GetResult TeamCityProjectId |> ProjectId

    let stateFile = args.GetResult StateFile |> FilePath.New

    let gitHubUrl = args.GetResult GitHubUrl |> ValidUrl.create
    let token = args.GetResult GitHubToken
    let owner = args.GetResult GitHubOwner
    let repoName = args.GetResult GitHubRepo

    use handler = new HttpClientHandler()
    handler.UseDefaultCredentials <- true
    use httpClient = new HttpClient(handler, true)
    httpClient.Timeout <- TimeSpan.FromHours(1.)

    let getString relativeUrl =
        httpClient.GetStringAsync(teamCityUrl.Value + "/" + relativeUrl).Result

    // None = 500 results by default (tests, builds)
    let teamCityCount = None

    // TeamCity
    let getRestString = TeamCity.getRestString getString

    let latestFailedBuilds projectId =
        TeamCity.latestFailedBuilds getRestString projectId teamCityCount

    let newFailedTests buildId =
        TeamCity.newFailedTests getRestString buildId teamCityCount

    let testDetails = TeamCity.testDetails getRestString

    let testHistory testId buildTypeId =
        TeamCity.testHistory getRestString testId buildTypeId teamCityCount

    let testHistoryCount = TeamCity.testHistoryCount getRestString
    let getProblems = TeamCity.problems getRestString
    let downloadLog = TeamCity.downloadLog getString

    let getAssignee = TeamCity.assignee getRestString

    // Log
    let log line = printfn "%s" line

    // Octokit
    let gitHubClient = GitHubClient(ProductHeaderValue("Flakies"), Uri(gitHubUrl.Value))
    gitHubClient.Credentials <- Credentials token

    let octoCreate newIssue =
        gitHubClient.Issue.Create(owner, repoName, newIssue).Result

    let octoGet number =
        gitHubClient.Issue.Get(owner, repoName, number).Result

    let octoUpdate number issueUpdate =
        gitHubClient.Issue.Update(owner, repoName, number, issueUpdate).Wait()

    let octoComment number comment =
        gitHubClient.Issue.Comment.Create(owner, repoName, number, comment).Wait()

    // Issues
    let newTestIssue details history : CreateIssue =
        GitHub.testIssue getAssignee octoCreate octoUpdate log teamCityUrl details history

    let newBuildIssue : CreateIssue = GitHub.buildIssue octoCreate octoUpdate log teamCityUrl

    let newGroupIssue group (build: FailedBuild) _ : IssueId =
        GitHub.groupIssue getAssignee octoCreate octoUpdate log teamCityUrl group build

    let updateIssue : UpdateIssue = GitHub.updateIssue octoGet octoUpdate octoComment teamCityUrl

    let getFlakyTests =
        let tryGuessFlakiness = FlakyTestAnalyzer.tryGuessFlakiness testHistoryCount

        FlakyMonitor.getFlakyTests testDetails testHistory tryGuessFlakiness log

    let analyzeTests groups build state =
        let analyzeTest details history build technical state =
            FlakyTestAnalyzer.test (newTestIssue details history) updateIssue details build technical state

        let analyzeGroup group build state =
            FlakyTestAnalyzer.group (newGroupIssue group) updateIssue group build state

        FlakyMonitor.analyzeTests analyzeTest analyzeGroup groups build state

    let analyzeBuilds build state =
        BuildAnalyzer.run getProblems downloadLog newBuildIssue updateIssue build state

    FlakyMonitor.run latestFailedBuilds newFailedTests getFlakyTests analyzeTests analyzeBuilds log projectId stateFile
    0

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(programName = "TeamCity.Flakies.exe")

    let arguments =
        try
            parser.Parse argv |> Some
        with _ ->
            parser.PrintUsage() |> printfn "%s"
            None

    match arguments with
    | None -> 1
    | Some args -> run args
