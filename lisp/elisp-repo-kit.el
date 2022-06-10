;;; elisp-repo-kit.el --- Write a freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  <author>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
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

;; This package is meant to be destroyed.  Delete the Lisp.  Delete the tests.
;; Create a new world for yourself.  Publish.  The commands contained here will
;; download the repo and perform renaming and relicensing.  As you may have
;; downloaded this package from MELPA, and as you have witnessed the CI passing,
;; you should already unsterstand that you have walked across the bridge.  The
;; license header checks will ensure that whatever Lisp you write or any
;; contributions it attracts may well end up in the Emacs core.

;;; Code:

(require 'dash) ; see flake.nix for providing dependencies for CI and local development.

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
(defconst elisp-repo-kit--rename-maps ; directory file hard-replace
  '(( nil "gpl-3.0.txt" "COPYING")
    ("test/" "elisp-repo-kit-test-setup.el" nil)
    ("test/" "elisp-repo-kit-test.el" nil)
    ("lisp/" "elisp-repo-kit.el" nil)))
(defconst elisp-repo-kit--files-with-strings
  '("README.org"
    "lisp/elisp-repo-kit.el"
    "test/elisp-repo-kit-lint.el"
    "test/elisp-repo-kit-lint-tests.el"
    "test/elisp-repo-kit-test.el"
    "test/elisp-repo-kit-test-setup.el"))
(defconst elisp-repo-kit--package-name "elisp-repo-kit")
(defconst elisp-repo-kit--github-path "positron-solutions/elisp-repo-kit")

(defun elisp-repo-kit--rename-package (dir old-package new-package)
  "Rename FILES in DIR.
`elisp-repo-kit--rename-map' is a list of (<subdir> name
<hard-rename>) triples.  When <subdir> is nil, it means use DIR.
If <hard-rename> is nil means replace OLD-PACKAGE with NEW-PACKAGE,
using `replace-regexp-in-string'.  DIR is the root of where we
are renaming.  Existing files will be clobbered."
  (mapc (lambda (rename-map)
          (let ((dir (concat dir (or (pop rename-map) "")))
                (name (pop rename-map))
                (hard-replace (pop rename-map)))
            (let ((new-name (or hard-replace
                                (replace-regexp-in-string old-package new-package name))))
              (rename-file (concat dir name) (concat dir new-name) t))
            (message "Used lexical variables %s %s" hard-replace rename-map)))
        elisp-repo-kit--rename-maps))

(defun elisp-repo-kit--replace-strings (dir package-name author user-org email)
  "Replace values in files that need renaming or re-licensing.
DIR is where we are reaplacing.  PACKAGE-NAME is the new package.
AUTHOR will be used in copyright notices.  USER-ORG will be used
as the first part of the new github path.  EMAIL is shown after AUTHOR in
package headers."
  (let ((default-directory dir)
        (github-path (concat user-org "/" package-name)))
    (mapc
     (lambda (file)
       (with-current-buffer (find-file-noselect (concat dir file) t t)
         ;; (insert-file-contents file 'visit)
         ;; append new author to copyright
         (message "visiting: %s" (buffer-file-name))
         (when (re-search-forward ";; Copyright.*Positron Solutions" nil t)
           (end-of-line)
           (insert (concat ", " author)))
         (goto-char (point-min))
         (when (re-search-forward "<author>" nil t)
           (replace-match (concat author ", <" email ">"))
         (goto-char (point-min))
         ;; replace license with GPL3 notice
         (when (re-search-forward ";; Permission \\(.\\|\n\\)*SOFTWARE.$" nil t)
           (replace-match elisp-repo-kit--gpl3-notice))
         (goto-char (point-min))
         ;; update github paths for README links
         (while (re-search-forward "positron-solutions/elisp-repo-kit" nil t)
           (replace-match github-path))
         (goto-char (point-min))
         ;; update remaining package name strings
         (while (re-search-forward "elisp-repo-kit" nil t)
           (replace-match package-name))
         (while (re-search-forward "Elisp Repo Kit" nil t)
           (replace-match (string-join
                           (mapcar 'capitalize (split-string "-" package-name))) " "))
         (save-buffer 0)
         (kill-buffer))))
     elisp-repo-kit--files-with-strings)))

(defun elisp-repo-kit--dash-dep ()
  "Use dash and verify our dependency is included with Emacs.
See the flake.nix for more information about providing your project dependencies
for CI & local development."
  (--map (* it it) '(1 2 3 4)))

;;;###autoload
(defun elisp-repo-kit-great-job ()
  "Tell package author they are doing a great job."
  (interactive)
  (let ((msg "You're doing a great job!"))
    (message "%s" msg)
    msg))

;;;###autoload
(defun elisp-repo-kit-clone (clone-dir &optional rev)
  "Clone elisp-repo-kit to CLONE-DIR and apply rename.

REV can be used to check out a specific revision.  Warning!  The
revision may have lost compatibility with the rename script.
Each rev is intended only to be able to rename itself, as a quine
and for forking as a new template repository."
  (interactive "DClone to directory:")
  (if-let ((git-bin (executable-find "git")))
      (progn
        (shell-command
         (format "cd %s; %s clone https://github.com/positron-solutions/elisp-repo-kit.git"
                 clone-dir git-bin))
        (when rev
          (shell-command
           (format "cd %s/elisp-repo-kit; %s checkout %s" clone-dir git-bin rev)))
        clone-dir)
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
  (interactive "DCloned directory: \nsPackage name: \nsAuthor: \nsGithub organization or username: \nsEmail: ")
  (elisp-repo-kit--replace-strings
   clone-dir package-name author user-org email)
  (elisp-repo-kit--rename-package
   clone-dir elisp-repo-kit--package-name package-name))

;;;###autoload
(defun elisp-repo-kit-new (clone-dir package-name author user-org email &optional rev)
  "Clone elisp-repo-kit, rename, and relicense in one step.
CLONE-DIR is where you want to put your new package root.
PACKAGE-NAME should be the long name of the package, what will
show up in melpa etc.  AUTHOR will be used in copyright notices.
USER-ORG is either your user or organization, which forms the
first part of a github repo path.  EMAIL is shown after AUTHOR in
package headers.  Optional REV is either a tag, branch or
revision used in git checkout.

See comments in `elisp-repo-kit-clone' and
`elisp-repo-kit-rename-relicense' for implementation information
and more details about argument usage.."
  (interactive "sPackage name: \nsAuthor: \nsGithub organization or username: \nsEmail: \nsRev tag, or branch: ")
  (elisp-repo-kit-rename-relicense
   (elisp-repo-kit-clone clone-dir rev)
   package-name author user-org email))

(provide 'elisp-repo-kit)
;;; elisp-repo-kit.el ends here
