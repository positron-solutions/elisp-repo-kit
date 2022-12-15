;;; elisp-repo-kit.el --- Elisp Github repository kit  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  <author>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (project "0.7.1") (auto-compile "1.2.0") (dash "2.18.0")
;; Homepage: http://github.com/positron-solutions/elisp-repo-kit

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

;; Set up Emacs package with Gihub repository configuration, complete with
;; Actions CI, tests, lints, and a licensing scheme all ready to go.  Included
;; commands are focused on productivity, appropriate for professional
;; development in elisp.  The goal of the package is streamline authoring &
;; distributing new Emacs packages.  It provides a well-integrated but rigid
;; scheme, aka opinionated.
;;
;; The package also uses its own hosted source as a substrate for creating new
;; packages.  It will clone its source respository and then perform renaming &
;; relicensing.  Simply call `elisp-repo-kit-new' to start a new package.  The
;; README documents remaining setup steps on Github and in preparation for
;; publishing on MELPA.
;;
;; As a development aid, the package is versatile enough to work on some elisp
;; packages that were not descended from its own source.  The scope of
;; functionality is primarily to interface with linting and testing frameworks,
;; both in batch and live workflows.

;;; Code:

(require 'project) ; see flake.nix for providing dependencies for CI and local development.
(require 'auto-compile)
(require 'dash)

(defgroup elisp-repo-kit nil "Elisp repository kit.")

(defcustom elisp-repo-kit-github-package-name "elisp-repo-kit"
  "Default Github <project> for cloning templates.
If you rename this repository after forking, you need to set this
to clone from within the fork."
  :group 'elisp-repo-kit
  :type 'string)

(defcustom elisp-repo-kit-github-userorg "positron-solutions"
  "Default Github <user-or-org> for cloning templates.
If you fork this repository, you need to set this to clone it
from within the fork."
  :group 'elisp-repo-kit
  :type 'string)

(defconst elisp-repo-kit--gpl3-notice ";; This program is free software; \
you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.")
(defconst elisp-repo-kit--rename-maps ; directory file replacement-file
  '(( nil "gpl-3.0.txt" "COPYING")
    ("lisp/" "elisp-repo-kit.el" nil)
    ("test/" "elisp-repo-kit-test.el" nil)
    ("test/" "run-shim.el" nil)))
(defconst elisp-repo-kit--files-with-strings
  '("README.org"
    "lisp/elisp-repo-kit.el"
    "test/elisp-repo-kit-test.el"
    "test/run-shim.el"))

(defun erk--project-root ()
  "Return project root or buffer directory."
  (let ((project (project-current)))
    (or (if (version<= emacs-version "28.0")
            (car (with-suppressed-warnings
                     ((obsolete project-roots))
                   (funcall 'project-roots project)))
          (funcall 'project-root project))
        default-directory)))

(defun erk--reload (features dir)
  "Reload FEATURES, found in DIR."
  (dolist (feature features)
    (when (featurep feature) (unload-feature feature 'force)))
  (let ((load-path (append (list dir) load-path))
        (load-prefer-newer t)
        (auto-compile-on-load-mode t)
        (auto-compile-on-save-mode t)
        ;; ask user to save buffers in the current project
        (save-some-buffers-default-predicate 'save-some-buffers-root))
    (save-some-buffers)
    (dolist (feature features)
      (require feature))))

(defun erk--dir-features (dir)
  "Return list of features provided by elisp files in DIR.
Except autoloads."
  (let* ((package-files (directory-files dir nil (rx ".el" string-end)))
         (package-files (--reject
                         (string-match-p (rx "autoloads.el" string-end) it)
                         package-files)))
    (mapcar
     (lambda (f) (intern (string-remove-suffix ".el" f)))
     package-files)))

(defun erk--package-features ()
  "List the features defined by the project's package.
This assumes the convention of one elisp file per feature and
feature name derived file name"
  (erk--dir-features (concat (erk--project-root) "lisp" )))

(defun erk--test-features ()
  "List the features defined in project's test packages.
This assumes the convention of one elisp file per feature and
feature name derived file name"
  (erk--dir-features (concat (erk--project-root) "test" )))

;;;###autoload
(defun erk-reload-project-features ()
  "Reload the features this project provides.
The implementation assumes all packages pass package lint,
providing a feature that matches the file name.

This function should attempt not to fail.  It is infrastructure
for development, and being lenient for degenerate cases is fine."
  (interactive)
  (let* ((project-root (erk--project-root))
         (lisp-subdir (concat project-root "/lisp"))
         (project-elisp-dir (if (file-exists-p lisp-subdir) lisp-subdir
                              project-root))
         (package-features (erk--dir-features project-elisp-dir)))
    (erk--reload package-features project-elisp-dir)))

;;;###autoload
(defun erk-reload-project-tests ()
  "Reload test features that this project provides.
The implementation assumes all packages pass package lint,
providing a feature that matches the file name.

This function should attempt not to fail.  It is infrastructure
for development, and being lenient for degenerate cases is fine."
  (interactive)
  (let* ((project-root (erk--project-root))
         (lisp-subdir (concat project-root "/test"))
         (project-test-dir (if (file-exists-p lisp-subdir) lisp-subdir
                              project-root))
         (package-features (erk--dir-features project-test-dir)))
    (erk--reload package-features project-elisp-dir)))

;;;###autoload
(defun erk-ert-rerun-this-no-reload ()
  "Rerun the ert test at point, but don't reaload anything.
Use this when debugging something the tests are consuming or
debugging elisp repo kit itself, which would likely behave
unpredictably if reloaded in the middle of its own function
call."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let* ((form (funcall load-read-function (current-buffer)))
           (name (elt form 1)))
      (ert `(member (,name))))))

;;;###autoload
(defun erk-ert-rerun-this ()
  "Rerun the ert test at point.
Will reload all features and test features."
  (interactive)
  ;; TODO detect if project is dirty and ask to reload
  (erk-reload-project-features)
  (erk-reload-project-tests)
  (save-excursion
    (beginning-of-defun)
    (let* ((form (funcall load-read-function (current-buffer)))
           (name (elt form 1)))
      (ert `(member ,(list name))))))

(defun erk-ert-project-features ()
  "List the features defined by the project.
This assumes the convention of one elisp file per feature and
feature name derived file name")

(defun erk-ert-project-results-buffer ()
  "Return an ERT buffer name based on project name.")

(defun erk-ert-project-selector ()
  "Return a selector for just this project's ERT test.
This selector generates the symbols list before that selector
will run, so new features or new symbols only avaialble after
reload will not be picked up.  Run this after any necessary
feature reloading."
  (let* ((test-features (erk--test-features))
         (test-symbols (->> test-features
                            (-map #'symbol-file)
                            (--map (cdr (assoc it load-history)))
                            (-flatten-n 1)
                            (--filter (eq 'define-symbol-props (car it)))
                            (-map #'cdr)
                            (-flatten-n 2))))
    (message "test-symbols: %s" test-symbols)
    `(satisfies ,(lambda (test)
                   (member (ert-test-name test) test-symbols)))))

;;;###autoload
(defun erk-ert-project ()
  "Run all ert tests in this project."
  (interactive)
  (erk-reload-project-features)
  (erk-reload-project-tests)
  (ert (erk-ert-project-selector)))

;;;###autoload
(defun erk-modified-command ()
  "Say some stuff, but don't save this."
  (interactive)
  (message "thisfis moaod  fi lololol even more"))

(defun elisp-repo-kit--rename-package (dir old-package new-package)
  "Rename FILES in DIR.
`elisp-repo-kit--rename-map' is a list of (subdir filename
replacement-filename) triples.  When subdir is nil, it means use
DIR.  If replacement-filename is nil means replace OLD-PACKAGE
with NEW-PACKAGE, using `replace-regexp-in-string'.  DIR is the
root of where we are renaming.  Existing files will be
clobbered."
  (mapc (lambda (rename-map)
          (let ((dir (concat dir (or (pop rename-map) "")))
                (filename (pop rename-map))
                (replacement-filename (pop rename-map)))
            (let ((new-name (or replacement-filename
                                (replace-regexp-in-string old-package new-package filename))))
              (rename-file (concat dir filename) (concat dir new-name) t))))
        elisp-repo-kit--rename-maps))

(defun elisp-repo-kit--replace-strings (dir package-name author user-org email)
  "Replace values in files that need renaming or re-licensing.
DIR is where we are replacing.  PACKAGE-NAME is the new package.
AUTHOR will be used in copyright notices.  USER-ORG will be used
as the first part of the new github path.  EMAIL is shown after
AUTHOR in package headers."
  (let ((default-directory dir)
        (elisp-repo-kit-github-path (concat elisp-repo-kit-github-userorg "/"
                                            elisp-repo-kit-github-package-name))
        (github-path (concat user-org "/" package-name))
        (capitalized-package-title
         (string-join
          (mapcar #'capitalize
                  (split-string elisp-repo-kit-github-package-name "-"))
          " ")))
    (mapc
     (lambda (file)
       (with-current-buffer (find-file-noselect (concat dir file) t t)
         ;; append new author to copyright
         (print (format "visiting: %s" (buffer-file-name)))
         (when (re-search-forward ";; Copyright" nil t)
           (end-of-line)
           (insert ", " author))
         (goto-char (point-min))
         (when (re-search-forward "<author>" nil t)
           (replace-match (concat author ", <" email ">")))
         (goto-char (point-min))
         ;; replace license with GPL3 notice
         (when (re-search-forward ";; Permission \\(.\\|\n\\)*SOFTWARE.$" nil t)
           (replace-match elisp-repo-kit--gpl3-notice))
         (goto-char (point-min))
         ;; update github paths for README links
         (while (re-search-forward elisp-repo-kit-github-path nil t)
           (replace-match github-path))
         (goto-char (point-min))
         ;; update remaining package name strings
         (while (re-search-forward elisp-repo-kit-github-package-name nil t)
           (replace-match package-name))
         (goto-char (point-min))
         (while (re-search-forward capitalized-package-title nil t)
           (replace-match capitalized-package-title))
         (save-buffer 0)
         (kill-buffer)))
     elisp-repo-kit--files-with-strings)))

;;;###autoload
(defun elisp-repo-kit-clone (clone-root package-name user-org &optional rev)
  "Clone elisp-repo-kit to CLONE-ROOT and apply rename.
PACKAGE-NAME will instruct git how to name the clone.  USER-ORG
is the user or organization you will use for your Github
repository.  REV can be used to check out a specific revision.
Warning!  The revision may have lost compatibility with the
rename script.  Each rev is intended only to be able to rename
itself, as a quine and for forking as a new template repository."
  (interactive "DClone to directory:")
  (if-let ((git-bin (executable-find "git")))
      (progn
        (shell-command
         (format "cd %s; %s clone https://github.com/%s/%s.git %s"
                 clone-root git-bin
                 elisp-repo-kit-github-userorg
                 elisp-repo-kit-github-package-name
                 package-name))
        (shell-command
         (format "cd %s/%s" clone-root package-name))
        (when rev
          (shell-command (format "%s checkout %s" git-bin rev)))
        (shell-command
         (format "%s remote rm origin" git-bin))
        (shell-command
         (format "%s remote add origin git@github.com:%s/%s.git"
                 git-bin user-org package-name))
        ;; return value for renaming
        (concat clone-root "/" package-name "/"))
    (error "Could not find git executible")))

;;;###autoload
(defun elisp-repo-kit-rename-relicense (clone-dir package-name author user-org email)
  "Rename and relicense your clone of elisp-repo-kit.
CLONE-DIR is your elisp-repo-clone root.  PACKAGE-NAME should be
the long name of the package, what will show up in melpa etc.
AUTHOR will be used in copyright notices.  USER-ORG is either
your user or organization, which forms the first part of a github
repo path.  EMAIL is shown after AUTHOR in
package headers.

This command replaces all instances of:

1. package name
2. author name (appended to copyright)
3. MIT license with GPL3 license notices in package files (not test files or CI)
4. github organization
5. COPYING is changed to just the GPL3.

Then renames the files to reflect package name

Finally, MIT licenses are swapped with GPL3 license notices.
Re-licensing is fully permitted by the MIT license and intended
by the author of this repository."
  (interactive "DCloned directory: \nsPackage name: \nsAuthor: \
\nsGithub organization or username: \nsEmail: ")
  (elisp-repo-kit--replace-strings
   clone-dir package-name author user-org email)
  (elisp-repo-kit--rename-package
   clone-dir elisp-repo-kit-github-package-name package-name))

;;;###autoload
(defun elisp-repo-kit-new (clone-root package-name author user-org email &optional rev)
  "Clone elisp-repo-kit, rename, and relicense in one step.
CLONE-ROOT is where you want to clone your package to.
PACKAGE-NAME should be the long name of the package, what will
show up in melpa etc.  AUTHOR will be used in copyright notices.
USER-ORG is either your user or organization, which forms the
first part of a github repo path.  EMAIL is shown after AUTHOR in
package headers.  Optional REV is either a tag, branch or
revision used in git checkout.

See comments in `elisp-repo-kit-clone' and
`elisp-repo-kit-rename-relicense' for implementation information
and more details about argument usage.."
  (interactive "sPackage name: \nsAuthor: \nsGithub organization or username: \
\nsEmail: \nsRev tag, or branch: ")
  (elisp-repo-kit-rename-relicense
   (elisp-repo-kit-clone clone-root package-name user-org rev)
   package-name author user-org email))

(provide 'elisp-repo-kit)
;;; elisp-repo-kit.el ends here
