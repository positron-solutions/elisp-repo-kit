;;; elisp-repo-kit.el --- write a freaking package!  -*- lexical-binding: t; -*-

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
;; These tests are run by requiring elisp-repo-kit-test-setup.

;;; Code:

(require 'elisp-repo-kit)

(ert-deftest erk-great-job-test ()
  "Tests if we are doing a great job."
  (should (string-equal (elisp-repo-kit-great-job) "You're doing a great job!")))

(ert-deftest erk-dash-dep-test ()
  "Tests that dependencies are included with the provided Emacs.
See the flake.nix for more information about providing your project dependencies
for CI & local development."
  (should (equal (elisp-repo-kit--dash-dep) '(1 4 9 16))))

(ert-deftest erk-clone-and-rename-test ()
  "Clone the repo and rename it, single step."
  (let ((rev (pop argv)))
    (elisp-repo-kit-new
     default-directory
     "clone-rename-test" ; project-name
     "Selindis Raszagal" ; Author
     "new-shakuras" ; user-org
     "selindis@new-shakuras.planet" ; email
     rev))) ; possibly nil

(provide 'elisp-repo-kit-test)

;;; elisp-repo-kit-test.el ends here.
