<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit CONTRIBUTING.org -->

Development process & infrastructure guide.


# Table of Contents

-   [Submitting Pull Requests](#org606a9eb)
-   [Development](#orgcb8ea7d)
    -   [Running tests](#org2c0c140)
    -   [Lint and byte-compile code](#org4934777)
    -   [Loading and re-loading your package](#orgea5ce35)
    -   [Re-generating Documentation](#org3edbedc)
-   [License](#org82dcc82)
-   [Developer Certificate of Origin (DCO)](#orge4b9e7a)
    -   [Sign-off](#org06a4c0a)
    -   [GPG signature](#orgd1a4d6c)
    -   [User setup for submitting changes](#orgd2c4615)
    -   [Maintaining versions](#org51f9f2c)


<a id="org606a9eb"></a>

# Submitting Pull Requests

Be sure to abide by instructions in [the pull request template](../.github/pull_request_template.md).


<a id="orgcb8ea7d"></a>

# Development

This repository was created with [elisp-repo-kit](https://github.com/positron-solutions/elisp-repo-kit/).  You can use it to streamline
development workflows.


<a id="org2c0c140"></a>

## Running tests

Run `erk-ert-project` within this project.  The tests will be
discovered, rebuilt & reloaded if necessary, and run.  There are a
few other commands to augment the [ert](https://www.gnu.org/software/emacs/manual/html_node/ert/) package.


### Running tests CI style

If you cannot reproduce a failure (or success) on CI, then you may
want to switch to using [nix](https://nixos.org/download.html) to get a reprodicible toolchain so you
can further develop with frozen versions from the nix [flake's](https://nixos.wiki/wiki/Flakes)
flake.lock.

    
    nix develop .github# # loads the devShells.default from flake.nix
    cd .github && direnv allow # same as above with file watching
    
    emacs --quick --load .github/run-shim.el -- test # graphical
    emacs --script .github/run-test.el -- test # terminal, batch style

You can *totally* run the tests locally on whatever version of Emacs you
have.  **You do not need Nix to run tests pretty close to what CI does.** CI
will use Nix to obtain Emacs & dependencies.


<a id="org4934777"></a>

## Lint and byte-compile code

This package uses [elisp-lint](https://github.com/gonewest818/elisp-lint) to detect issues with byte compiling, package
format, code structure and others.

The configuration is found inside [.github/run-shim.el](../.github/run-shim.el).  The CI run is invoked
inside of [ci.yml](../.github/workflows/ci.yml) using Emacs in script mode.  Most of the configuration is in
the run shim.

The tests are also linted, to a less restrictive standard, also found in
[run-shim.el](../.github/run-shim.el)

You can run the lints manually almost the same as running tests.

    
    nix develop .github#
    # nix develop .#emacs28
    # nix develop .#emacsGit
    emacs --script .github/run-shim.el -- lint
    emacs --script .github/run-shim.el -- lint-tests


<a id="orgea5ce35"></a>

## Loading and re-loading your package

Run `erk-reload-package` in one of your project files.  All features
in the /lisp directory will be re-compiled and loaded appropriately.

**Note**, during reloading, unloading the current module is forced.  If other
packages you use depend on the project feature, results may be unpredicatable.
This is one reason batch style testing can be preferred.


### Manual Loading & Reloading

To manually unload, run built-in command `unload-feature` and select your
package name. If you do not unload, reloading has no effect and you will see
stale behavior.

Next, add the package to your load-path and then require it or, more
directly, call `emacs-lisp-byte-compile-and-load` or
`emacs-lisp-native-compile-and-load`.


<a id="org3edbedc"></a>

## Re-generating Documentation

If you are using `elisp-repo-kit`, `erk-export-docs` will take care of
everything.  See the related commands for exporting individually and
viewing the outputs. If you enable local variables, declared at the end of
each document, saving will automatically export.

To manually export, use the appropriate org mode command:

-   `org-md-export-as-markdown`
-   `org-texinfo-export-to-info`

You can view the manual with prefix argument, `info-display-manual`, and
manually type the path to the `manual.info`.  Honestly, just use
`elisp-repo-kit`.  This stuff is tedious.

**NOTE** changes to the `manual.org` requires re-generation of other documents
 because they use `#+include:` directives.


<a id="org82dcc82"></a>

# License

This package is distributed under the terms of the [included license](./COPYING).  The CI
configuration, documentation, and scripts are MIT licensed.


<a id="orge4b9e7a"></a>

# Developer Certificate of Origin (DCO)

This project is distributed with a Developer Certificate of Origin.  By adding
a sign-off notice and GPG signature to each commit, you will provide means to
authenticate your sign-off later strengthening your attestations stated in the
DCO, upholding the overall integrity of the license coverage over the project.

A [copy of the DCO](./DCO) is distributed with this project.  Read its text to
understand the significance of configuring for sign-off.


<a id="org06a4c0a"></a>

## Sign-off

A sign-off means adding a "trailer" to your commit that looks like the
following:

    
    Signed-off-by: Random J Developer <random@developer.example.org>


<a id="orgd1a4d6c"></a>

## GPG signature

A GPG signed commit shows that the owner of the private key submitted the
changes.  Wherever signatures are recorded in chains, they can demonstrate
participation in changes elsewhere and awareness of what the submitter is
participating in.  Corroborating user's signature accross a history of works
strengthens that user's attestation provided by DCO sign-off.


<a id="orgd2c4615"></a>

## User setup for submitting changes

Follow these instructions before you get ready to submit a pull-request.

Refer to the [GitHub signing commits](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits) instructions to set up your git client
to add GPG signatures.  File issues if you run into Emacs-specific problems.
Be sure to use a Github verified email.

Because signing is intended to be a conscious process, please remember to
read and understand the [Developer Certificate of Origin](../DCO) before confinguring
your client to automatically sign-off on commits.


### Automatically add sign-off

In magit, set the `-s` switch.  Use `C-x C-s` (`transient-save`) to
preserve this switch on future uses.  (Note, this is not per-project).You
can also set the signature flag this way.


### Automatic GPG signing with per-project keys

In order to specify which projects you intend to sign with which keys, you
will want to configure your git client using path-specific configurations.

Configuing git for this can be done with the following directory structure:

    
    /home/rjdeveloper/
    ├── .gitconfig
    └── .gitconfig.d
        ├── sco-linux-projects.conf
        ├── other-projects.conf
        └── gpg-signing-projects.conf

In your root config, `.gitconfig`, add an `includeIf` directive that will
load the configuration you use for projects you intend to GPG sign commits
for.

    
    [includeIf "gitdir:/home/rjdeveloper/**/gpg-signing/**/.git"]
      path = "~/.gitconfig.d/gpg-signing-projects.conf"

In the `gpg-signing-projects.conf` add your GPG signing configuration from
earlier.  `sign` adds the GPG signature automatically.  File an issue if you
need help with multiple GPG homes or other configurations.

    
    [user]
      name = "Random J Developer"
      email = "random@developer.example.org"
      signingkey = "5FF0EBDC623B3AD4"
    
    [commit]
      sign = true
      gpgSign = true


### Manually signing & adding sign-off

If you don't like these configurations and want to individually indicate you
have read and intend to apply the DCO to your changes, these commands are
equivalent:

    git commit -s -S --message "I don't like using .gitconfig"
    
    # To clean up a commit
    git commit --amend -s -S --no-message
    
    # Combine with rebase to sign / sign-off multiple existing commits
    git rebase -i


<a id="org51f9f2c"></a>

## Maintaining versions

The Nix [flake.nix](../github/flake.nix) is where versions are declared.  The [flake.lock](../.github/flake.lock) stores a
fixed value for these declarations.  These fixed versions need periodic
update.  Nixpkgs has a new release about every six months.  You can check
their [branches](https://github.com/NixOS/nixpkgs/branches) and [tags](https://github.com/NixOS/nixpkgs/tags) to see what's current.  The effect is similar to
updating linux versions.  The `nix` command has a lot of support for
specifying versions besides just updating.

    nix flake lock --update-input nixpkgs

The `emacs-overlay`, which contains fixed versions of Emacs and snapshots of
Elisp repository package sets can be updated by running:

    nix flake lock --update-input emacs-overlay

