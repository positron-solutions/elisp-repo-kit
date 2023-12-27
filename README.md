<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<a href="https://melpa.org/#/erk"><img src="https://melpa.org/packages/erk-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/erk"><img src="https://stable.melpa.org/packages/erk-badge.svg" alt="melpa stable package"></a>
<a href="https://github.com/positron-solutions/elisp-repo-kit/actions/?workflow=CI"><img src="https://github.com/positron-solutions/elisp-repo-kit/actions/workflows/ci.yml/badge.svg" alt="CI workflow status"></a>
<a href="https://github.com/positron-solutions/elisp-repo-kit/actions/?workflow=Developer+Certificate+of+Origin"><img src="https://github.com/positron-solutions/elisp-repo-kit/actions/workflows/dco.yml/badge.svg" alt="DCO Check"></a>


# Elisp Repo Kit

This repository is a kit to start a new elisp package repository on GitHub.  The
package contained has commands to streamline elisp development.


# Quickly set up an Emacs Lisp repository on GitHub with:

-   An [elisp](https://www.youtube.com/watch?v=RQK_DaaX34Q&list=PLEoMzSkcN8oPQtn7FQEF3D7sroZbXuPZ7) package
-   CI with [GitHub Actions](https://docs.github.com/en/actions/using-jobs/using-a-matrix-for-your-jobs), configured for Darwin (MacOS) and Linux
-   Built-in Emacs info manual generation
-   [Nix](https://nixos.org/#examples) environment for obtaining dependencies or reproducibly developing CI
    locally
-   Licensing, [DCO](https://developercertificate.org/), DCO sign-off checks, PR template and [CONTRIBUTING](./CONTRIBUTING.md) instructions
-   [MELPA](https://github.com/melpa/melpa) publishing compatible

**To get started:**

Install the package and run `erk-new`, provide a directory, and
answer all the questions.


# Install ERK

    (use-package erk) ; vanilla, assuming you have MELPA configured
    
    ;; package-vc
    (package-vc-install
     '(erk :url "https://github.com/positron-solutions/elisp-repo-kit.git"
           :lisp-dir "lisp"
           :doc "doc/erk.texi"))
    
    ;; using elpaca's with explicit recipe
    (use-package erk
      :elpaca (erk :host github :repo "positron-solutions/elisp-repo-kit"))
    
    ;; straight with explicit recipe
    (use-package erk
      :straight (erk :type git :host github :repo "positron-solutions/elisp-repo-kit"))
    
    ;; or use manual load-path & require, you brave yak shaver


## Manually add just CI

Copy the .github folder and the contributing guide to your package.  Set up
your secrets for Cachix. Read the CI customization section.


# Table of Contents

-   [Creating Packages](#org55d6c6f)
-   [Using ERK for development](#org8df953f)
    -   [Loading and re-loading your package](#org832dbac)
    -   [Jumping to files](#org6cd8dde)
    -   [Run tests](#org59f2f7b)
    -   [Duplicating CI Locally](#org512e00a)
    -   [Find Files](#orgd13d7b8)
-   [File contents and structure](#orgd91f6b8)
    -   [Setting Up Your Github Repository](#org2b43980)
-   [Customizing CI](#org6f4bf92)
-   [Licensing, Developer Certificate of Origin](#org5d1d80d)
-   [Publishing to MELPA](#orgf9492cc)
    -   [Creating the recipe](#org1bcced8)
    -   [Testing package build](#orgea41cb1)
    -   [Testing stable package build](#org6975475)
    -   [MELPA Lints](#orgef6e69d)
-   [Maintaining versions](#org90c8308)
-   [Package scope and relation to other work](#org74f808b)
    -   [Dependency Management](#org7e7eab5)
    -   [Discovering and Running Tests & Lints](#orgdc8001e)
    -   [Comparisons](#orgccbe76b)
-   [Contributing](#org729e175)
-   [Footnote on FSF and Emacs Core Licensing](#orga0697b8)
-   [Shout-outs](#orgd54722e)


# Creating Packages

The simplest and intended way is to call `erk-new`.  It will first
ask you for:

-   Choose a template
-   Root directory you want to clone to
-   Package title
-   Package feature name
-   Package elisp prefix
-   Author name
-   Email address
-   GitHub user or organization

`erk-new` also calls `erk-rename-relicense` to rename all of the files, string
replace names, and re-license to GPL3.  It also changes the author and resets
the git history.  Now just follow the steps in [finish setting up](#org2b43980). Have fun!


# Using ERK for development

Elisp repo kit contains some convenience functions to reload your package and
to discover and run ert tests.  These shortcuts just make common cases faster.


## Loading and re-loading your package

`erk-reload-project-package` will unload and recompile your package if
necessary and then reload it.

`erk-reload-project-tests` is the complementary command for reloading tests.


## Jumping to files

Lots of locations come in pairs or small sets.  Jumping functions try to go
between these locations or to a sensible default location.  They are very
do-what-I-mean (DWIM).

-   `erk-jump-features` is a command that will jump between the feature and its
    corresponding test feature.  If you aren't in an elisp file, it will jump to
    the root feature file.

-   `erk-jump-defs` will try to go to the test definition and even ask insert an
    `ert-deftest` body if you forgot to write the test.  It can then jump back to
    the function definition.  If it fails, it will fall back to jumping to the
    feature.


## Run tests

**Warning**!  These commands are very likely to completely change.  If you want to
try changing them, go ahead.  Binding them is not recommended, but they do work.

-   `erk-ert-project` will discover, rebuild & reload if necessary, and run tests.
    There are a few other commands to augment the [ert](https://www.gnu.org/software/emacs/manual/html_node/ert/) package.

-   `erk-ert-rerun-this` Is not a very smart function yet, but if you are working on
    a test at point, it will run it or re-run it.


## Duplicating CI Locally

The CI configuration is all stored in [.github](./.github/).  Usually you will want
development instructions in your new repository.  The [CONTRIBUTING](./CONTRIBUTING.md) guide
contains instructions to reproduce the CI behavior.


## Find Files

Accidentally editing generated files or forgetting the path to a file is
annoying.  ERK provides a helper to find files based on their purpose.

`erk-find` will ask you to pick the file based on what it does.  It's
choices:

-   ci-dco-action
-   ci-nix-flake
-   ci-run-shim
-   ci-tests-action
-   doc-contributing
-   doc-manual
-   doc-readme

Generated files or extremely common files are not included.  For each one of
these choices, there is a corresponding command:

-   `erk-find-ci-dco-action`

-   `erk-find-ci-nix-flake`

-   `erk-find-ci-run-shim`

-   `erk-find-ci-tests-action`

-   `erk-find-doc-contributing`

-   `erk-find-find-doc-manual`

-   `erk-find-doc-readme`


# File contents and structure

*After cloning and renaming,* you will have a file tree like this:

    ├── .gitignore                        # ignores for byte compiles, autoloads etc
    │
    ├── README.md                         # this file
    ├── CONTRIBUTING.md                   # typical instructions for development
    ├── COPYING                           # a GPL3 license
    ├── DCO                               # Developer Certificate of Origin
    │
    ├── .github
    │   ├── .envrc                        # direnv integration with `nix develop`
    │   ├── flake.nix                     # dependencies for this project
    │   ├── flake.lock                    # version controlled lock of flake.nix input versions
    │   ├── run-shim.el                   # elisp script with test & lint routines
    │   ├── pull_request_template.md      # reminders for PR contributors
    │   └── workflows
    │       ├── ci.yml                    # workflow for lints and tests
    │       └── dco.yml                   # workflow to check DCO sign-offs
    │
    ├── doc
    │   ├── README.org                    # generates README.md
    │   ├── CONTRIBUTING.org              # generates CONTRIBUTING.md
    │   ├── manual.org                    # actual manual source
    │   └── erk.texi                      # generated manual for distribution
    │
    ├── lisp
    │   └── erk.el                        # the package
    │
    └── test
        └── erk-test.el                   # ERT unit tests for the package

You can use either a multi-file or flat layout for lisp.  Just name test files
`something-tests.el` and keep all lisp files in root, `/lisp` or `/test`
directories.


## Setting Up Your Github Repository

You can copy this checklist to your org agenda files:

-   [X] Create a repository (from [install](#org97f4634) instructions)
-   [ ] Create an empty GitHub repository configure it as your git remote
-   [ ] Set up your git commit signing (and verification so that it's obvious)
    **and** [sign-off](#org5d1d80d) so that it will be hypothetically [straightforward](README.md) for for FSF
    to pull in your changes if they later change to DCO instead of copyright
    assignment.
-   [ ] Sign up for [cachix](https://app.cachix.org/) and, create a binary cache with API tokens and public
    read access

-   [ ] Add repository secrets necessary for your GitHub actions
    `CACHIX_AUTH_TOKEN` and `CACHIX_CACHE_NAME` (settings -> secrets -> new
    repository secret)

-   [ ] Enable actions and add the following actions to your allowed actions list:
    
        actions/checkout@v3.2.0,
        cachix/cachix-action@v12,
        cachix/install-nix-action@v20,
        actions/setup-python@v4,
    
    **Note**, Python is used to run a DCO check script, nothing more.

-   [ ] Get your package working, pushed, actions run, and CI badges all green
-   [ ] [Publish](#orgf9492cc) to MELPA
-   [ ] Make a post on [reddit](https://reddit.com/r/emacs/) and [mastodon](https://emacs.ch/) about your new package


### Optional Steps

-   [ ] Branch protect and enable check requirements for your default branch
    (usually master).  Merge commits, verified only, and no force push are
    recommended settings.
-   [ ] Enable requiring contributors to sign-off on web-based commits

-   [ ] For **security** of your Cachix secrets (and any others), require
    Actions approval for all outside contributors.  Disabling write and
    disabling creation & approval of PR's is least privilege.

Cachix is somewhat optional.  It's free for open-source projects.  It's
about as easy to sign up and generate the token as to remove the step from
the GitHub actions, so you won't save much time by avoiding it.

If you opt out of cachix or any other binary cache, you will definitely want
to turn off tests for `emacsGit` etc because the build times are about
30min-1hr per run when a fresh Emacs must be built.


# Customizing CI

The [run-shim.el](./.github/run-shim.el) script is just provides a CLI interface for adding commands in
the [ci.yml](./.github/workflows/ci.yml) CI declaration.  Each action step just loads the shell, declared in
the [flake.nix](./.github/flake.nix) and then runs the shim in Emacs.  The shim consumes the CLI
command arguments, so you can parameterize the invocations that way.

-   If you need extra elisp dependencies during CI, add them to the `epkgs` list
    in the flake.nix.

-   If you need extra 3rd party dependencies, add them to `packages` in the call
    to `mkShell`.
-   To invoke different elisp operations, add / modify the commands in
    [run-shim.el](./.github/run-shim.el).

There's more information in [CONTRIBUTING](./CONTRIBUTING.md) about running commands locally.  You
will want this information in your new repository.


# Licensing, Developer Certificate of Origin

This project and its derivative templates are distributed with an MIT
license. `erk-new` will also run `erk-rename-relicense`, which will
automatically switch to the GPL3 license.  **The MIT license allows
re-licensing, and so this change is compatible.** If you accept non-trivial
changes to your project, it will be very hard to change to the GPL3 later, so
consider this choice.

The new repository will also come with DCO sign-off checking on PR's.  The
instructions are in the [CONTRIBUTING](./CONTRIBUTING.md) guide.  A DCO sign-off policy will give
your project a clear attestation of sufficient direct or transitive authority
from each contributor to submit changes under the terms of your project's
license.  This can only improve your legal protection from incidentally
handling copyrighted code.

The DCO choice in this repository is also meant to encourage & push stodgy
organizations whose responsibility it was to invent better processes towards
lower friction paths to contribute code.  If you fail to implement the DCO
sign-off scheme, there is less hope that the FSF will someday independently
merge changes that accumulate in your package because there will not be a
clear chain of license compliance.


# Publishing to MELPA

If you have green CI, you have already passed many requirements of publishing a
MELPA package.  **You still need to build your package and verify your recipe.**
You are going to clone melpa in order to make your PR.  You can use the clone to
verify the recipe.


## Creating the recipe

Fork MELPA personally (not for organization) and clone it to wherever you keep
your upstreams.  It's a good idea to separate upstreams from projects you
actively maintain so you can see and delete upstreams when not in use.

    
    mkdir -p upstream
    cd upstream
    git clone git@github.com:$GITHUB_USER/melpa.git  # replace $GITHUB_USER

Install package-build

    (use-package package-build)

`package-build-create-recipe` will give you something like:

    (erk :fetcher github :repo "positron-solutions/elisp-repo-kit")

The following template can be filled in and pull-requested to MELPA to publish.
You don't need to touch `:files`.  The `commit` and `branch` are optional
depending on how you version / develop / tag your releases.

Copy the recipe into `recipes/erk` inside your MELPA clone.


## Testing package build

Inside the MELPA clone root:

    
    # Builds the package
    make recipes/erk
    # Test sandbox installation (will affect ~/.emacs.d/elpa  So much for sandbox ¯\_(ツ)_/¯
    EMACS_COMMAND=$(which emacs) make sandbox INSTALL=erk


## Testing stable package build

You need a tag on your default (usually master) branch of your repo,
`positron-solutions/elisp-repo-kit`. Use `git tag -S v0.1.0` and `git push
    origin v0.1.0`.  You can also just create a release in the GitHub interface.

    
    # Test stable builds against your tags
    STABLE=t make recipes/erk


## MELPA Lints

Lastly, install [melpazoid](https://github.com/riscy/melpazoid) and call `melpazoid` on your main feature.  It does
some additional lints.  You may need to install `package-lint` if you don't have
it.  It's not declared in melpazoid's requirements.  Getting the package in Nix
is not easy yet since melpazoid is not yet on Melpa.

    
    ;; using elpaca's with explicit recipe
    (use-package melapzoid
      :elpaca (melpazoid :host github :repo "riscy/melpazoid"))
    
    ;; using straight
    (straight-use-package
     '(melpazoid :type git :host github :repo "riscy/melpazoid" :files ("melpazoid/melpazoid.el")))

If everything works, you are ready to make a pull request to MELPA.  Push your
changes and check all the boxes in the PR template except the one that requires
you to read the instructions.


# Maintaining versions

The Nix [flake.nix](./.github/flake.nix) is where versions are declared.  The [flake.lock](./.github/flake.lock) stores a
fixed value for these declarations.  These fixed versions need periodic
update.  Nixpkgs has a new release about every six months.  You can check
their [branches](https://github.com/NixOS/nixpkgs/branches) and [tags](https://github.com/NixOS/nixpkgs/tags) to see what's current.  The effect is similar to
updating linux versions.  The `nix` command has a lot of support for
specifying versions besides just updating.

    nix flake lock --update-input nixpkgs

The `emacs-overlay`, which contains fixed versions of Emacs and snapshots of
Elisp repository package sets can be updated by running:

    nix flake lock --update-input emacs-overlay


# Package scope and relation to other work

There are two functional goals of this repository:

-   Automate the annoying work necessary to set up a new repository
-   Streamline common elisp development workflows

Commands within this package will focus on cleaner integration of the tests
and lints with Emacs.  There has been a lot of work in this area, but much of
it is tangled with dependency management and sandbox creation.  Much of it is
done in languages other than elisp and focused on non-interactive workflows
with no interactive integration on top.

Providing close to out-of-box CI is a big focus.  By making it easier to
qualify changes from other users, it becomes less burdonsome to maintain
software, and by extension, less burdensom to publish and create software. The
effect is to drive creation of elisp in a way that can accelerate the flow of
elisp into Emacs itself.


## Dependency Management

This repository uses pure dependency management and then leverages it to
provide dependencies for development and CI environments.  The resulting user
experience is built around CI for reproducibility and interactive testing for
development speed.

Because most elisp dependencies can be obtained without extensive system
dependency management, many tools for testing Emacs packages provide
dependency management and loading those dependencies into a fresh Emacs
instance.  This aligns well with ad-hoc sandboxed local testing.  This was
fine in the old days of impure dependency management and dirty environments.

The [Emacs Nix Overlay](https://github.com/nix-community/emacs-overlay) and Emacs support within nixpkgs make it possible to
stating and obtaining elisp dependencies in a completely pure way.  Non-elisp
dependencies are trivially provided form nixpkgs.  Nix is extremely reliable
at dependency management, and it is no surprise that much complexity is
normalized away by just the basic behavior model of Nix.  In addition, **if
your project needs or includes additional binary dependencies or modules**,
Nix is an excellent way to provide them to CI and users.


## Discovering and Running Tests & Lints

During development, the commands provided under the `erk-` prefix make it
more convenient to reload your package and test features.  You can run the
ert tests for a project while working on multiple packages.

During CI, this repository uses an elisp shim for discovering and running
tests.  The commands within the package for convenience during development
are not depeneded upon during CI.

The CI actions obtain an environment with dependencies using Nix, so this can
also be done locally using Nix, meaning re-creating environments is available
to the user without leaning on CI.


## Comparisons

There are many tools for running Elisp tests.  Most of them are well
integrated with some dependency management.  Most of them have some published
CI integrations to draw inspiration from.  These patterns are common because
the needs arise together.


### ERK's Key Characteristics

-   Quickly spin up nearly complete starter projects
-   Fully working CI, packaging, and documentation manual scheme
-   Immediately run tests and get into the virtuous feedback loop
-   Nix dependency management, bringing both Elisp and 3rd party dependencies
    under full control

As a template for your project, **ERK leans towards being opinionated** in
order to provide complete behavior out of the box.  The trade-off of being
closer to a completed project while still being minimal is only supporting
one hosting platform, **Github, and its Github Actions CI infrastructure.** You
can adapt around this easily because it's not a complex project, but you will
do it on your own.

This project favors the **Nix dependency tooling** for extreme reproducibility
and access to a **huge** number of 3rd party dependencies on most platforms.
If you want to implement sandboxed tests or test with a specific version of
dependencies, you can do it in a pollution-free way across numerous versions
with Nix.  Everyone on most platforms can reproduce your work in a way that
doesn't pollute their system either (beyond installing Nix).


### Other Comparisons

There are many comparisons available to understand the roles of similar tools
and how they relate to each other.

-   [makem.sh](https://github.com/alphapapa/makem.sh#comparisons)
-   [Eldev](https://github.com/doublep/eldev#see-also)
-   [nomake](https://github.com/emacs-twist/nomake) Is another project with Nix work

[nix-emacs-ci](https://github.com/purcell/nix-emacs-ci) capture the work needed to provide a running Emacs to CI.  Tools
like [eldev](https://github.com/doublep/eldev#continuous-integration) and [makem.sh](https://github.com/alphapapa/makem.sh/blob/master/test.yml) have support for providing dependencies to that Emacs.
The Nix flake [in this project](./.github/flake.nix) describes both of these tasks.  Makem and Eldev
etc document Gihub workflows, but **the workflows in this repository are meant to
be used out-of-the-box after cloning**, although to be fair, there's more
decisions than actual work.

Nix-emacs-ci provides a lot of backwards-compatibility versions of Emacs.
The emacs-nix-overlay that this project employs is more forward looking,
providing `emacsGit` and sometimes other upstream branches when a big
feature like native compilation is in the pipeline.  Nix-emacs-ci is also
still using legacy Nix, without flakes.  Flakes are just nicer and the way
Nix is going.


# Contributing

For turn-key contribution to the software ecosystem that keeps you moving, see
the [funding links](https://github.com/sponsors/positron-solutions).

For code-based contribution, first decide if you want to work on this
repository or fork it to something entirely different.

The [CONTRIBUTING](./CONTRIBUTING.md) guide in this repo contains development instructions,
including singing & sign-off configuration.  You will usually want this file
in your own repositories.

Non-exhaustive list of changes that are very welcome:

-   More interactive integrations with high-value elisp development workflows
-   Running additional or better kinds of tests & lints
-   Fix bugs
-   More generic support for renaming and selecting different base repository
    templates
-   Expose trivial options where a structural choice has limited them
    unnecessarily
-   Behave the same, but with a less complicated code
-   More templates, such as those for dynamic modules or using different CI

Changes will likely be rejected if it is aimed at:

-   Non-elisp interfaces meant for invocation outside of Emacs or with scripting
    implemented in a language besides elisp.
-   Managing dependencies outside of Nix (or other pure dependency management)
-   Backwards compatibility for Emacs two versions behind next release.  Master,
    current stable release, and release - 1 are the only versions being supported
-   pre-flake Nix support
-   Guix support **if it interferes with Nix support**


# Footnote on FSF and Emacs Core Licensing

Free Software Foundation (FSF) frequently requires copyright assignment on all
code that goes into Emacs core. Many free software projects formerly requiring
copyright assignment have since switched to using a Developer Certificate of
Origin.  DCO sign-off is a practice accepted by git, GCC, and the [Linux
Kernel](https://wiki.linuxfoundation.org/dco).

Doing DCO sign-off is not the same as copyright assignment, and serves a
slightly different purpose.  DCO sign-off is an attestation from the submitter
stating that they have sufficient direct or transitive authority make their
submission under the terms of the license of the recieving project.  Copyright
assignment serves a more offensive role in the case of GPL non-compliance,
giving FSF alone legal standing.  If you don't care about FSF being able to
sue people, the DCO should suffice.

Using the DCO **may** make it easier for code in your project to be included in
Emacs core later.  **It is the intent of this choice to steer FSF towards
DCO-like solutions in order to accelerate code flow into Emacs.** Regardless of
FSF's ongoing position on use of DCO's, by requiring DCO sign-off and GPG
signature, you can be assured that changes submitted to a code base you
control are strongly attested to be covered by the license you chose.


# Shout-outs

-   [alphapapa](https://github.com/alphapapa) for being super prolific at everything, including package writing,
    documentation, and activity on various social platforms
-   [adisbladis](https://github.com/adisbladis) for the Nix overlay that makes the CI and local development so nice
-   [NobbZ](https://github.com/NobbZ) for being all over the Nix & Emacs interwebs
-   [FSF](https://www.fsf.org/) and all contributors to Emacs & packages for the Yak shaving club

