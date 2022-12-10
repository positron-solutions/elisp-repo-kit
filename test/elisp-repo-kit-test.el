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
;; can be run from one of the project files using the `elisp-repo-kit-project-ert'
;; command.

;;; Code:

(require 'ert)
(require 'elisp-repo-kit))

;; (declare-function erk-ert-dummy "elisp-repo-kit" ())
;; (ert-deftest erk-ert-dummy-test ()
;;   "Tests that tests are properly re-run with modified code."
;;   (should (equal (erk-ert-dummy) 8)))

(ert-deftest erk-clone-and-rename-test ()
  "Clone the repo and rename it, single step."
  (let ((rev (pop argv))
        (clone-root (make-temp-file "elisp-repo-kit" t)))
    (elisp-repo-kit-new
     clone-root
     "clone-rename-test" ; project-name
     "Selindis Raszagal" ; Author
     "new-shakuras" ; user-org
     "selindis.r@new-shakuras.planet" ; email
     rev))) ; possibly nil

(provide 'elisp-repo-kit-test)

;;; elisp-repo-kit-test.el ends here.
