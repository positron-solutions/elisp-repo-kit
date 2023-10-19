## Pull Requests

**Generated** changes to the documentation and flake.lock should be kept in a
separate commit for the reviewer.  Title your commit as "generated".

Keeping these changes isolated in specific commits makes it much easier to pull
in your changes in parallel with other features.

Maintainers may harvest your changes.  We only guarantee to preserve authorship,
signature, and sign-off in the git log.

### Creating pull requests

1. Fork this repository into the personal GitHub account
1. Make changes on the personal fork
1. Remember to sign off and sign all commits in your PR branch (instructions in readme)
1. Make a Pull Request against this repository
1. **Allow maintainers to make changes to your pull request** (there's a
   checkbox)
1. Once the pull request has been approved, you will be thanked and observe your
   changes applied with authorship, signature, and sign-off in the git log

### Reminder about PR access to upstream CI

Pull-requests run with the `pull_request_target` Github Actions configuration.
Your PR can access secrets, but it run's the **upstream's** CI, not the CI
changes you are submitting.

If you need to make changes to CI itself in order to pass checks, you will need
to work with the repository's owner.  The owner will pull the branch into a
non-master branch.  There, they can trigger the CI manually with the branch's
own configuration using `workflow_dispatch` in the Actions tab.
