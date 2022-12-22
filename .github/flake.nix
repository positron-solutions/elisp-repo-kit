# Copyright (C) 2022 Positron Solutions

# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{
  description = "This flake provides CI & local development dependencies";

  # inputs are pinned via flake.lock.  The normal way to update an individual
  # version is:
  # nix flake lock --update-input emacs-overlay
  # Tracking can be accomplished via CI and machine commits
  inputs = {
    # stable branches are recommended for CI, regression spotting.  Testing with
    # multiple versions can be done via input overrides, such as:
    # nix build .#ci --override-input nixpkgs github:nixpkgs/nixpkgs/unstable
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # slave Emacs overlay to our nixpkgs.  This doesn't do a lot except reduce
    # the closure size and allow us to control both nixpkgs versions with one
    # declaration.
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let

        # instantaite nixpkgs with the emacs overlay applied.
        # to explore available attributes, you can instantiate nixpkgs with the emacs overlay in a nix repl:
        # pkgs = import (builtins.getFlake "nixpkgs") { system = builtins.currentSystem; overlays = [ (builtins.getFlake ("emacs-overlay")).overlay ];}
        # pkgs.emacs will tab complete
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };

        # List of Emacsen to generate development shells for
        emacsPackages = [
          "emacsUnstable"
          "emacsGit"
          "emacs28"
          "emacs"
        ];

        # let's have a development shell per Emacs!
        devShells = pkgs.lib.genAttrs emacsPackages (emacsPkg:
          pkgs.mkShell {
            packages = [
              # pkgs, contains many dependencies you can provide to your elisp
              # programs. Search for packages here:
              # https://search.nixos.org/packages

              pkgs.git # for elisp-repo-kit-clone.

              # https://github.com/nix-community/emacs-overlay
              # The emacs overlay provides up-to-date snapshots of Melpa packages.
              # These will be pure & pinned, so you need to update the flake lock
              # or use appropriate options.
              #
              # This expression builds an Emacs that loads the packages passed
              # to emacsWithPackages on startup.
              ((pkgs.emacsPackagesFor pkgs.${emacsPkg}).emacsWithPackages
                (epkgs: [
                  # List your project's dependencies here:
                  # epkgs.melpaPackages.dash
                  # epkgs.melpaStablePackages.dash
                  # epkgs.elpaPackages.dash
                  # epkgs.dash

                  # Development dependencies
                  epkgs.elpaPackages.relint
                  epkgs.melpaPackages.elisp-lint
                  # epkgs.melpaPackages.buttercup # XXX finish after #218 on buttercup
                  # epkgs.melpaPackages.elsa # XXX did not work out of the box

                  # Dependencies of elisp-repo-kit itself. These are no longer
                  # needed by your repo after cloning.
                  epkgs.elpaPackages.project
                  epkgs.melpaPackages.auto-compile
                ]))
            ];
          });
      in {
        # The output set, where all useful things go.  If your nix flake exposes
        # packages, overlays, shells, or nix utils, they can be exposed here for
        # downstream consumption.

        # Augment the devShells with a default so that `nix develop` knows what
        # to do.  Run `nix flake show` to see the results.  Per-system,
        # per-Emacs, we have a development environment avaialble.
        devShells = devShells // { default = devShells.emacsGit; };
      });
}
