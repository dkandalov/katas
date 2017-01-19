MF idea: "anonymize" commit history to completely avoid pointing fingers


graph (for open-close principle)
    y: # of commits
    x: files


find methods in a class/files which change in different commits (to find SRP violation)
(same for higher-level entities)


find code changes without appropriate test changes (most changed in the last month with least amount of tests)


how quickly someone started changing a lot of code (different files) in a project


most risky changes:
 - someone changes a file he doesn't own for the first time


- avg size of changes per committer (committer profile)
- frequency of inter-commit intervals per committer (should it be correlated to the size of commit)


methods that had been growing for the longest time (check "method_shark.rb")


complexity tolerance of committer (complexity by number of commits against the same methods)


"Active set of classe"
 classes that haven't been changed vs changed classes over the history of project


 profiles for committers by word frequency analysis in their diffs


