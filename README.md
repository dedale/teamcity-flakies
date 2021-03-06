
# TeamCity.Flakies

Automatic monitoring of TeamCity flakies with GitHub issues.

Find flaky tests and flaky build problems in TeamCity and automatically create or update GitHub issues.

## Flaky tests

A test is flaky if it randomly fails for no reason.
The tool analyzes test history to decide it a test is flaky or not.

Typical flaky tests histories:

- Few non-successive failures
- Fail-only history (test only exists when it fails), non-successive, smaller than build history
- Single-failure history with a random test name too long for TeamCity (at every build, TeamCity considers this is a new test)

## Flaky problems

Build can have flaky problems too.

Example:
- Flaky build timeouts.
- Flaky failures (technical or not) that can be easily identified with log messages.

# F# Code

- Code is formatted with [Fantomas](https://github.com/fsprojects/fantomas)
- No interface, no mocking framework
- Test coverage is close to 100 %

NuGet dependencies:
- [Argu](https://github.com/fsprojects/Argu)
- [Json.NET](https://github.com/JamesNK/Newtonsoft.Json) (for serialization of state file)
- [Octokit](https://github.com/octokit/octokit.net)
- [Expecto](https://github.com/haf/expecto) (for tests)

# See also

- [Twitter thread](https://twitter.com/d3dal3/status/1340317307361484800)

# Ideas

- Custom assignee per TeamCity sub-project
- Customize assignee with config for some build config, test name, etc
- Detect more problems
- Also download and parse extra logs in TeamCity artifacts
- One issue per build per flakiness (currently, with one issue per build, an issue can be reopened for another problem)