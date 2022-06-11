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

EL_FILES = $(wildcard **/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)

.PHONY: clean help lint-tests lint-pkg lint flake-etadata flake-update flake-show test
.DEFAULT: help

help:
	$(info make test		- run ERT tests)
	$(info make lint		- lint, bytecode-compile package and tests)
	$(info make lint-tests		- lint, bytecode-compile tests)
	$(info make lint-pkg		- lint, bytecode-compile package)
	$(info make clean		- remove most generated files)
	$(info make flake-update	- update all nix flake.lock versions)
	$(info make flake-metadata	- show nix flake inputs tree)
	$(info make flake-show		- show nix flake outputs set)
	@printf "\n"

clean:
	@echo Deleting byte compiled files
	@rm -f $(ELC_FILES)
	@rm -rf test/clone-rename-test
	@rm -f $(wildcard **/*autoloads.el*)

lint-tests:
	@emacs --script test/elisp-repo-kit-lint-tests.el

lint-pkg:
	@emacs --script test/elisp-repo-kit-lint.el

lint: lint-tests lint-pkg

test:
	@emacs --script test/elisp-repo-kit-test-setup.el

flake-update:
	@nix flake update

flake-metadata:
	@nix flake metadata

flake-show:
	@nix flake show
