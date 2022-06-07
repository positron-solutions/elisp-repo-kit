;;; elisp-repo-kit-test-setup.el --- Setup and execute all tests

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

;; This package sets up load paths and then loads the test files.
;;
;; Usage:
;;
;; Always get a fresh Emacs for your test runs.  It will reload features and
;; byte compile where necessary.  The Emacs provided by the nix develop shell
;; contains the dependencies declared in the flake.nix.
;;
;;   nix develop
;;   "emacs" --quick --script test/elisp-repo-kit-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.

;;; Code:

(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)

;; This expression normalizes the behavior of --quick --load <file> and --script
;; <file> behavior.  If you don't do this, --script will see every argument
;; passed and the arguments from the Nix wrapper to set load paths.
(when (member (car argv) '("-l" "--"))
  (print "Normalizing arguments")
  (while (not (member (car argv) '("--" nil)))
    (print (format "Normalizing arguments, stripped: %s" (pop argv))))
  (pop argv))

(defvar elisp-repo-kit-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))
(setq default-directory elisp-repo-kit-test-setup-directory)

;; include all directories with lisp we need
(dolist (dir '("../lisp" "../test"))
  (let ((dir (concat elisp-repo-kit-test-setup-directory dir)))
    (add-to-list 'load-path dir)))

;; Extra dependencies required in the package are provided to CI & local
;; development by nix.  See the flake.nix for more information.

(require 'ert) ; load test dependency

(require 'elisp-repo-kit) ; load main feature

(require 'elisp-repo-kit-test) ; load each test feature

(ert t) ; run tests

;;; elisp-repo-kit-test-setup.el ends here
