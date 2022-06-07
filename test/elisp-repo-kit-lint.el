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

;; This package uses elisp-lint to look for byte compile issues or poor code
;; formatting.
;;
;; Usage:
;;
;; Always get a fresh Emacs for your test runs.  It will reload features and
;; byte compile where necessary.  The Emacs provided by the nix develop shell
;; contains the dependencies declared in the flake.nix.
;;
;;   nix develop
;;   "emacs" --quick --script test/elisp-repo-kit-test-lint.el
;;
;; elisp-lint will respect many current default settings.  Many Emacs
;; preferences are properly reflected in the lint output.

;;; Code:

(require 'elisp-lint)

;; Use this file's directory as default directory so that lisp file locations
;; are fixed with respect to this file.
(setq default-directory (file-name-directory load-file-name))
;; Add the location of elisp-repo-kit to the load path
(dolist (dir '("../lisp" "../test"))
  (let ((dir (concat default-directory dir)))
    (add-to-list 'load-path dir)))

(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)

;; 100-character column limit for lints.  If it's good enough for Linux, it's
;; good enough for us.  https://lkml.org/lkml/2020/5/29/1038
(setq-default fill-column 100)
;; Spaces
(setq-default indent-tabs-mode nil)

;; `command-line-args-left has the same effect as passing command line arguments.
(let ((command-line-args-left
       (append
        '(;; "--no-<check>
          ;; "--no-byte-compile"
          ;; "--no-checkdoc"
          ;; "--no-check-declare"
          )
        (seq-filter
         (lambda (s) (not (string-match-p ".*autoloads.*\.el$" s)))
         (file-expand-wildcards "../lisp/*.el")))))

  (message "ARGS: %s" command-line-args-left)

  ;; (setq elisp-lint-ignored-validators nil
  ;;  elisp-lint-file-validators nil
  ;;  elisp-lint-buffer-validators nil
  ;;  elisp-lint-batch-files nil)

  (elisp-lint-files-batch))

;;; elisp-repo-kit-lint.el ends here
