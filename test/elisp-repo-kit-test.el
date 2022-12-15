;;; elisp-repo-kit-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  <author>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Run the batch tests from root directory:
;; nix shell .#emacsGit --quick --script test/run-shim.el -- test
;; Test dependencies can be provided to the Emacsen declared inside the root
;; flake.nix.

;; For local development, dependencies should be installed by the user.  Tests
;; can be run from one of the project files using the `erk-ert-project'
;; command.

;;; Code:

(require 'ert)
(require 'elisp-repo-kit)

(ert-deftest erk--project-root-test ()
  (should (string-match-p (rx "elisp-repo-kit/" eol)
                          (erk--project-root))))

(ert-deftest erk--dir-features-test ()
  (should (equal
           '(elisp-repo-kit)
           (erk--dir-features (concat (erk--project-root) "lisp")))))

(ert-deftest erk--package-features-test ()
  (should (member 'elisp-repo-kit (erk--package-features))))

(ert-deftest erk--test-features-test ()
  (should (member 'erk-test (erk--test-features))))

(ert-deftest erk-clone-and-rename-test ()
  "Clone the repo and rename it, single step."
  (let ((rev (getenv "GITHUB_SHA"))
        (clone-root (make-temp-file "erk-clone-test-" t)))
    (erk-new
     clone-root
     "new-project" ; project-name
     "Selindis Raszagal" ; Author
     "new-shakuras" ; user-org
     "selindis.r@new-shakuras.planet" ; email
     rev) ; possibly nil
    (delete-directory clone-root)))

(provide 'elisp-repo-kit-test)
;;; elisp-repo-kit-test.el ends here.
