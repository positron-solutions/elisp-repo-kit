;;; erk-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  Positron Solutions <contact@positron.solutions>

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
;; nix shell .github#emacsGit --quick --script .github/run-shim.el -- test
;; Test dependencies can be provided to the Emacsen declared inside the root
;; flake.nix.

;; For local development, dependencies should be installed by the user.  Tests
;; can be run from one of the project files using the `erk-ert-project'
;; command.

;;; Code:

(require 'ert)
(require 'erk)

(ert-deftest erk--project-root-test ()
  (should (string-match-p (rx "elisp-repo-kit/" eol)
                          (erk--project-root))))

(ert-deftest erk--dir-features-test ()
  (should (equal
           '(erk)
           (erk--dir-features (concat (erk--project-root) "lisp")))))

(ert-deftest erk--package-features-test ()
  (should (member 'erk (erk--package-features))))

(ert-deftest erk--test-features-test ()
  (should (member 'erk-test (erk--test-features))))

(ert-deftest erk--template-github-userorg-test ()
  (should (string= (erk--template-github-userorg '(:github-path "positron-solutions/bitcoin-miner"))
                   "positron-solutions")))

(ert-deftest erk--template-github-repo-test ()
  (should (string= (erk--template-github-repo '(:github-path "positron-solutions/bitcoin-miner"))
                   "bitcoin-miner")))

(ert-deftest erk--template-feature-test ()
  (should (string= (erk--template-feature '(:github-path "positron-solutions/bitcoin-miner"))
                   "bitcoin-miner"))
  (should (string= (erk--template-feature '(:github-path "positron-solutions/bitcoin-miner"
                                                         :feature "floozly"))
                   "floozly")))

(ert-deftest erk--template-prefix-test ()
  (should (string= (erk--template-prefix '(:github-path "positron-solutions/bitcoin-miner"))
                   "bitcoin-miner"))
  (should (string= (erk--template-prefix '(:github-path "positron-solutions/bitcoin-miner"
                                                        :feature "floozly"))
                   "floozly"))
  (should (string= (erk--template-prefix '(:github-path "positron-solutions/bitcoin-miner"
                                                        :feature "floozly"
                                                        :prefix "flombow"))
                   "flombow")))

(ert-deftest erk--expand-filenames-test ()
  (should (string= (car (erk--expand-filenames '("%s-foo.el") "doo"))
                   "doo-foo.el")))

(ert-deftest erk--lisp-directory-test ()
  (should (not (string-match-p "test" (erk--lisp-directory))))
  (should (string-match-p "lisp" (erk--lisp-directory))))

(ert-deftest erk--test-directory-test ()
  (should (not (string-match-p "lisp/?$" (erk--test-directory))))
  (should (string-match-p "test" (erk--test-directory))))

(ert-deftest erk-jump-features-test ()
  ;; jump to feature when in tests
  (should
   (save-excursion
     ;;  test normally executes in a temporary buffer but `erk-jump-features'
     ;;  relies on `current-buffer'.
     (find-file (erk--project-root-feature-file))
     (erk-jump-features)
     (string-match-p "test" default-directory)))
  ;; jump to tests when in feature
  (should
   (save-excursion
     (find-file (concat (erk--test-directory) "erk-test.el"))
     (erk-jump-features)
     (string-match-p "lisp" default-directory)))
  ;; jump to feature when in root
  (should
   (save-excursion
     (find-file (concat (erk--project-root) "README.md"))
     (erk-jump-features)
     (string-match-p "lisp" default-directory))))

(ert-deftest erk-jump-defs-test ()
  (should
   (save-window-excursion
     (find-file (concat (erk--project-elisp-dir)
                        "/erk.el"))
     (save-excursion
       (goto-char (point-min))
       (search-forward "(defun erk-jump-defs")
       (erk-jump-defs)
       (string-match-p "erk-test.el" (buffer-file-name (current-buffer)))))))

(ert-deftest erk--project-elisp-dir-test ()
  (should (erk--project-elisp-dir)))

(ert-deftest erk--project-package-email-test ()
  (should (string= (erk-package-email) "contact@positron.solutions")))

(ert-deftest erk--project-package-author-test ()
   (should (string= (erk-package-author) "Positron Solutions")))

(ert-deftest erk--nodash-test ()
  (should (string= (erk--nodash "erk-") "erk")))

(ert-deftest erk--make-defun-symbol-test ()
  (should (eq (erk--make-defun-symbol 'something-test) 'something)))

(ert-deftest erk--make-test-symbol-test ()
  (should (eq (erk--make-test-symbol 'something) 'something-test)))

(ert-deftest erk--project-contains-fun-p-test ()
  (should (erk--project-contains-fun-p 'erk-new)))

(ert-deftest erk--project-contains-p-test ()
  (should (erk--project-contains-p (erk--project-root-feature-file))))

(ert-deftest erk-clone-test ()
  (let ((enable-local-variables nil)
        (clone-root (make-temp-file "erk-clone-test-" t)))
    (erk-clone (cdr (assoc 'erk-basic erk-templates))
               clone-root
               '(:title "Omg Test"
                        :prefix "ggg"
                        :email "mail@template.com"
                        :author "Huh Idk"
                        :feature "lol-pkg"
                        :user-org "zerglingscancode2"))
    (delete-directory clone-root t)))

(ert-deftest erk-new-test ()
  (let ((enable-local-variables nil)
        (erk-after-new-hook (when (require 'magit nil t)
                              '(magit-status)))
        (clone-root (make-temp-file "erk-clone-test-" t)))
    (erk-new (cdr (assoc 'erk-basic erk-templates))
             clone-root
             '(:title "Hilariously Good Package"
                      :feature "laughing-package"
                      :prefix "lol-pkg"
                      :user-org "zerglingscancode2"
                      :email "iamzerg@zergs.pwn"
                      :author "Zagara"))
    (delete-directory clone-root t)))

(provide 'erk-test)
;;; erk-test.el ends here.
