;;; erk.el --- Elisp (GitHub) Repository Kit  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Positron Solutions <contact@positron.solutions>

;; Author:  Positron Solutions <contact@positron.solutions>
;; Keywords: convenience, programming
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1") (auto-compile "1.2.0") (dash "2.18.0") (license-templates "0.1.3"))
;; Homepage: http://github.com/positron-solutions/elisp-repo-kit

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sub-license, and/or sell copies of
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

;; Set up and develop Emacs packages, complete with Github Actions CI, tests,
;; lints, documentation generation, and a licensing scheme all ready to go.
;; Included commands are focused on productivity, appropriate for professional
;; development in elisp.  The goal of the package is streamline authoring &
;; distributing new Emacs packages.  It provides a well-integrated but rigid
;; scheme, aka opinionated.
;;
;; Simply call `erk-new' to start a new package.  ERK will clone a small
;; template project and interactively rename and relicense the project.
;; Instructions on how to host and publish your package are included in the
;; manual.
;;
;; As a development aid, the package is versatile enough to work on some elisp
;; packages not descended from its templates.  The provided functionality
;; focuses on smoothing out typical workflows.  Common actions like reloading
;; packages and navigating between source & tests are streamlined.  Processes
;; like exporting all documents are automated.

;;; Code:

;; see flake.nix for providing dependencies for CI and local development.
(require 'auto-compile)
(require 'dash)
(require 'ert)
(require 'find-func)
(require 'finder)
(require 'license-templates)
(require 'lisp-mnt)
(require 'project)
(require 'vc)
(require 'org)
(require 'info)

(eval-when-compile (require 'subr-x))

(defgroup erk nil "Elisp repository kit." :prefix 'erk :group 'programming)

(defcustom erk-templates
  '((erk-basic :github-path "positron-solutions/erk-basic"))
  "Alist of template repositories configured for cloning in `erk-new'.
Each value form is a plist with keys:

- `:github-path`: Github style path, `user-or-org/repo'.

- `:feature`: By convention, the root feature decides file names.
  If nil, uses repo in the github-path.

- `:prefix`: Elisp namespace prefix, usually the same as feature
  but sometimes it is a contraction or initialism.  If nil, uses
  feature or repo in the github-path.

- `:title`: Optional.  The package usually has a longer name
  present in it's root feature's header.  Set title to specify
  this or rely on `erk-package-title' to infer it.

Template information is likely to grow in the future to support
templates with different layouts and other requirements."
  :group 'erk
  :type '(alist :key-type symbol :value-type plist))

(defcustom erk-after-new-hook '(magit-status)
  "Action to perform after a new clone has been created.
Called with no arguments.  The cloned directory set as
`default-directory'"
  :group 'erk
  :type '(list function))

(defun erk--template-github-userorg (template)
  "Get the github repo from a TEMPLATE."
  (car (split-string (plist-get template :github-path) "/")))

(defun erk--template-github-repo (template)
  "Get the github repo from a TEMPLATE."
  (cadr (split-string (plist-get template :github-path) "/")))

(defun erk--template-feature (template)
  "Get the feature from TEMPLATE.
Falls back to github repo from :github-path."
  (or (plist-get template :feature) (erk--template-github-repo template)))

(defun erk--template-prefix (template)
  "Get the prefix from TEMPLATE.
Falls back to :prefix and then the repo from :github-path."
  (or (plist-get template :prefix) (erk--template-feature template)))

(defconst erk--gpl3-notice ";; This program is free software; \
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

;; TODO In order to work with a variety of repos with valid Elisp package
;; layouts, these variables should replaced with file discovery. Then the
;; templates should contain any custom renaming information specific to their
;; layout and content.  Finally, re-licensing should be made dynamic and able to
;; use a combination of discovery and template information to.  PR's for any of
;; these tweaks will be gladly accepted.
(defconst erk--rename-maps
  '(("lisp/" "%s.el" nil)
    ("test/" "%s-test.el" nil)
    ("doc/" "%s.texi" nil))
  "List of (DIR SRC DEST) forms.")

(defconst erk--files-with-strings
  '("doc/manual.org"
    "doc/README.org"
    "lisp/%s.el"
    "test/%s-test.el"
    ".github/run-shim.el")
  "Files for general string replacement.")

(defconst erk--files-with-copy-notices
  '("lisp/%s.el")
  "Files that need copy notice replacement.")

(defun erk--expand-filenames (files feature)
  "Render the FEATURE into the list of FILES."
  (mapcar
   (lambda (f) (format f feature))
   files))

;; TODO I mean, it's ideologically good but practically bad.  In the lazy case,
;; template authors lose motivation.  New package maintainers forget to sign up.
;; Okay, convert this to an optional string replacement and let users decide to
;; be lazy for the greater good.
(defconst erk--delete-files
  '(".github/FUNDING.yml")
  "Files that would require other accounts to migrate.")

(defconst erk--remove-strings
  '("(ERK)")
  "Strings that vanish in renaming.")

(defun erk--project-root ()
  "Return project root or buffer directory."
  (let ((project (project-current)))
    ;; TODO remove pre-28 support
    (or (if (version<= emacs-version "28.0")
            (car (with-suppressed-warnings
                     ((obsolete project-roots))
                   (funcall #'project-roots project)))
          (funcall #'project-root project))
        default-directory)))

(defun erk--lisp-directory ()
  "Heuristic to find where package files are stored."
  (let ((root (erk--project-root)))
    (if (erk--project-flat-p)
        root
      (file-name-as-directory (concat root "lisp")))))

(defun erk--test-directory ()
  "Heuristic to find where package files are stored."
  (let ((root (erk--project-root)))
    (if (erk--project-flat-p)
        root
      (file-name-as-directory (concat root "test")))))

(defun erk--project-flat-p ()
  "Heuristic to detect flat project structure."
  (let ((root (erk--project-root)))
    (not (or (file-exists-p (concat root "lisp"))
             (file-exists-p (concat root "test"))))))

(defun erk--project-contains-p (file)
  "Is FILE a member of the current project?"
  (or (string-prefix-p (erk--project-root) file)
      (member file (project-files (project-current)))))

(defun erk--project-contains-fun-p (fun)
  "Is FUN currently defined within the project?
Returns FUN if it's bound and within the project."
  (and  (symbol-function fun)
        (erk--project-contains-p
         (cdr (find-function-library fun)))
        fun))

(defun erk--reload (features dir)
  "Reload FEATURES, found in DIR."
  (dolist (feature features)
    (when (featurep feature) (unload-feature feature 'force)))
  (let ((load-path (append (list dir) load-path))
        (load-prefer-newer t)
        (auto-compile-on-load-mode t)
        (auto-compile-on-save-mode t)
        ;; ask user to save buffers in the current project
        (save-some-buffers-default-predicate #'save-some-buffers-root))
    (save-some-buffers)
    (dolist (feature features)
      (let ((elc (concat dir "/" (symbol-name feature) ".elc")))
        (unless (file-exists-p elc)
          (byte-compile-file (concat dir "/" (symbol-name feature) ".el"))))
      (require feature))))

(defun erk--dir-elisp-files (dir)
  "Return a list of the elisp files in DIR.
Ignore autoloads."
  (->>
   (directory-files dir nil (rx (literal ".el") string-end))
   (--reject (string-match-p (rx (literal "autoloads.el") line-end) it))
   ;; flycheck creates a short-lived file starting with flycheck_
   ;; Will heisenbug you.
   (--reject (string-match-p (rx line-start (literal "flycheck_")) it))))

(defun erk--dir-features (dir)
  "Return list of features provided by elisp files in DIR.
Ignore autoloads."
  (mapcar
   (lambda (f) (intern (string-remove-suffix ".el" f)))
   (erk--dir-elisp-files dir)))

(defun erk--package-features ()
  "List the features defined by the project's package.
This assumes the convention of one elisp file per feature and
feature name derived file name"
  (erk--dir-features (concat (erk--project-root) "lisp" )))

(defun erk--package-root-feature ()
  "Return the shortest feature in the package root."
  (car (sort (erk--package-features)
             (lambda (l r)
               (< (length (symbol-name l))
                  (length (symbol-name r)))))))

(defun erk--test-features ()
  "List the features defined in project's test packages.
This assumes the convention of one elisp file per feature and
feature name derived file name"
  (erk--dir-features (concat (erk--project-root) "test" )))

(defun erk--project-elisp-dir ()
  "Return the location of elisp files.
Only understands project root or root/lisp."
  (let* ((project-root (erk--project-root))
         (lisp-subdir (concat project-root "lisp")))
    (if (file-exists-p lisp-subdir) lisp-subdir
      project-root)))

;; TODO quite redundant
(defun erk--project-root-feature-file ()
  "Return the path of the root feature for the project."
  (if-let* ((project-elisp-dir (erk--project-elisp-dir))
            (package-files (directory-files project-elisp-dir nil (rx ".el" string-end)))
            (package-files (->> package-files
                                (--reject (string-match-p (rx "autoloads.el" string-end) it))))
            (root-feature-file (car (sort package-files #'string<))))
      (concat project-elisp-dir "/" root-feature-file)
    (user-error "%s doesn't seem to be an elisp project" (erk--project-root))))

;; Note, these functions are kind of redundant, but just want to consume
;; consistent interfaces internally.
(defun erk-package-author ()
  "Return the author of this project's package."
  (car (car (lm-authors (erk--project-root-feature-file)))))

(defun erk-package-email ()
  "Return the email of this project's package."
  (cdr (car (lm-authors (erk--project-root-feature-file)))))

(defun erk-package-version ()
  "Return the version of this project's package."
  (lm-version (erk--project-root-feature-file)))

(defun erk-package-title ()
  "Return the title of the package for documentation."
  (let ((file(erk--project-root-feature-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "---\\(.*?\\)-\\*-" nil t)
        (string-trim (match-string 1))))))

;;;###autoload
(defun erk-reload-project-package ()
  "Reload the features this project provides.
The implementation assumes all packages pass package lint,
providing a feature that matches the file name.

This function should attempt not to fail.  It is infrastructure
for development, and being lenient for degenerate cases is fine."
  (interactive)
  (let* ((project-root (erk--project-root))
         (lisp-subdir (concat project-root "lisp"))
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
         (lisp-subdir (concat project-root "test"))
         (project-test-dir (if (file-exists-p lisp-subdir) lisp-subdir
                             project-root))
         (package-features (--reject
                            (string-match-p (symbol-name it) (rx "-test.el" eol))
                            (erk--dir-features project-test-dir))))
    (erk--reload package-features project-test-dir)))

(defun erk-jump-features ()
  "Jump between the corresponding package and test features."
  (interactive)
  (let* ((filepath (buffer-file-name (current-buffer)))
         (filename (when filepath
                     (file-name-nondirectory filepath))))
    (if (and filename
             (string-match-p ".*\\.el" filename))
        (find-file (if (string-match "\\(.*\\)-tests?\\.el" filename)
                       (concat (erk--lisp-directory)
                               (match-string 1 filename) ".el")
                     (concat (erk--test-directory)
                             (and (string-match "\\(.*\\)\\.el" filename)
                                  (match-string 1 filename))
                             "-test.el")))
      ;; fallback, go to root feature, convenient shortcut
      ;; back into elisp files
      (find-file (erk--project-root-feature-file)))))

(defun erk--last-defname ()
  "Return previous definition and name.
Returns nil if we don't know what kind of definition it is or
what to do with it (yet).  Returns `(def . name)' form."
  (save-excursion
    (beginning-of-defun)
    (pcase-let* ((`(,def ,name)
                  (funcall load-read-function (current-buffer))))
      (cons def name))))

(defun erk--insert-test (fun buffer)
  "Insert test named TEST-SYMBOL for FUN into BUFFER."
  (pop-to-buffer buffer)
  ;; If the buffer is open and the point is near a defun already, just slam in a
  ;; test.  If not, put the test in at the end.
  (if (eq (car (erk--last-defname)) 'ert-deftest)
      (progn
        (beginning-of-defun)
        (forward-sexp))
    (progn
      (goto-char (point-max))
      (backward-sexp)
      (backward-sexp)
      (forward-sexp)))
  ;; TODO test templates configuration
  (let ((before (format  "\n\n(ert-deftest %s-test ()\n  (should (%s" fun fun))
        (after ")))"))
    (insert before)
    (save-excursion
      (insert after))
    (recenter)))

(defun erk--make-test-symbol (symbol)
  "Convert defun SYMBOL into test symbol."
  (intern (concat (symbol-name symbol) "-test" )))

(defun erk--make-defun-symbol (symbol)
  "Convert test SYMBOL into defun symbol."
  (let ((name (symbol-name symbol)))
    (when (string-match "\\(.*\\)-test\\'" name)
      (intern (match-string 1 name)))))

;; TODO Possibly add some heuristics to find improper names.  Missing test
;; suffix is one such irritant.  Check the feature.
(defun erk-jump-defs ()
  "Jump between defuns and their test defs.
For now, only the rigid pairing of an `ert-deftest' and it's
corresponding `defun' are supported."
  (interactive)
  (pcase-let* ((`(,def . ,name) (erk--last-defname)))
    (pcase def
      ;; Find the def, try reloading if it's defined from somewhere outside the
      ;; project, such as within an installed version of the package.
      (`defun
          ;; TODO Doesn't check if test is defined inside the project.  Rare,
          ;; but not if working on two versions of a project.  Let's let the
          ;; interfaces normalize a bit.
          (let* ((test-name (erk--make-test-symbol name))
                 (test (or (when (ert-test-boundp test-name) test-name)
                           (progn (erk-reload-project-tests)
                                  (when (ert-test-boundp test-name) test-name)))))
            (if test
                (progn (find-function-do-it test 'ert--test 'switch-to-buffer)
                       (forward-sexp)
                       (recenter))
              (let ((test-buffer (erk-jump-features)))
                (when (y-or-n-p (format  "%s not found.  Create? "
                                         test-name))
                  (erk--insert-test name test-buffer))))))
      (`ert-deftest
          (let* ((def-name (erk--make-defun-symbol name))
                 (def (or (erk--project-contains-fun-p def-name)
                          (progn (erk-reload-project-package)
                                 (erk--project-contains-fun-p def-name)))))
            (if def
                (progn
                  (find-function-do-it def nil 'switch-to-buffer)
                  (forward-sexp)
                  (recenter))
              (progn (erk-jump-features)))))
      (_
       (erk-jump-features)))))

(defun erk--last-test ()
  "Return test name for last thing.
Note, to expand tests, a type parameter will have to be specified
and configured by the project."
  (pcase-let* ((`(,def ,name) (erk--last-defname)))
    (pcase def
      (`defun (erk--make-test-symbol name))
      (`ert-deftest name))))

;;;###autoload
(defun erk-ert-rerun-this-no-reload ()
  "Rerun the ert test at point, but don't reload anything.
Use this when debugging with external state or debugging elisp
repo kit itself, which may behave strangely if reloaded in the
middle of a command."
  (interactive)
  ;; TODO make this work off of newer primitive
  (when-let ((name (erk--last-test)))
    (when (ert-get-test name))
    (ert `(member ,name))))

;;;###autoload
(defun erk-ert-rerun-this ()
  "Rerun the ert test at point.
Will reload all features and test features."
  (interactive)
  ;; TODO integrate ERT and commands within buffer
  (erk-reload-project-package)
  (erk-reload-project-tests)
  (save-excursion
    (beginning-of-defun)
    (let* ((form (funcall load-read-function (current-buffer)))
           (name (elt form 1)))
      (ert `(member ,name)))))

(defun erk-ert-project-results-buffer ()
  "Return an ERT buffer name based on project name.")

(defun erk--get-ert-test-symbols ()
  "Return all defined ERT test symbols.
Does not reload."
  (let ((test-features (erk--test-features)))
    (->> test-features
         (-map #'symbol-file)
         (--map (cdr (assoc it load-history)))
         (-flatten-n 1)
         (--filter (eq 'define-symbol-props (car it)))
         (-map #'cdr)
         (-flatten-n 2)
         (--filter (plist-get (symbol-plist it) 'ert--test)))))

(defun erk-ert-project-selector ()
  "Return a selector for just this project's ERT test.
This selector generates the symbols list before that selector
will run, so new features or new symbols only available after
reload will not be picked up.  Run this after any necessary
feature reloading."
  `(member ,@(erk--get-ert-test-symbols)))

;;;###autoload
(defun erk-ert-project ()
  ;; TODO select failed, automatically when there are failed figure out how to
  ;; offer up tags as options.  See `ert-select-tests', but it's just okay
  "Run Ert interactively, with selector for this project."
  (interactive)
  (erk-reload-project-package)
  (erk-reload-project-tests)
  (ert (erk-ert-project-selector)))

(defmacro erk--nze (process-form error)
  "Error if there is a non-zero exit.
PROCESS-FORM is the process call that should return zero.
ERROR is the error message."
  `(unless (eq ,process-form 0)
     (pop-to-buffer "erk-clone")
     (error ,error)))

(defun erk--rename-package (template clone-dir replacements)
  "Rename files in CLONE-DIR.
TEMPLATE is a form described in `erk-templates'.  REPLACEMENTS is
a form from `erk-new'.

`erk--rename-map' is a list of (SUBDIR SRC DEST) triples.  When
SUBDIR is nil, the path is CLONE-DIR itself.  If DEST is nil
means replace old-feature with new-feature, using
`replace-regexp-in-string'.  CLONE-DIR is the root of where we
are renaming.  Existing files will be clobbered."
  (mapc (lambda (f) (delete-file (concat (file-name-as-directory clone-dir) f)))
        erk--delete-files)

  ;; Note, dashes removed from inputs in `erk-new', but left here just in case
  ;; called from Elisp.
  (let* ((old-feature (erk--nodash (erk--template-feature template)))
         (new-feature (erk--nodash (plist-get replacements :feature)))
         (output (get-buffer-create "erk-clone"))
         (git-bin (executable-find "git"))
         (default-directory clone-dir))

    (let ((new-license (concat clone-dir "LICENSE"))
          (old-license (concat clone-dir "COPYING")))
      ;; TODO dynamic re-licensing would be nice since `license-templates'
      ;; contains a lot.  At least we're no longer worried about shipping
      ;; licenses.  There's another package with headers and yet another for
      ;; spdx identifiers.
      (license-templates-new-file "gpl-3.0" default-directory)
      (erk--nze
       (call-process "mv" nil output nil "--verbose" new-license old-license)
       (format "Could not update %s contents." old-license))
      (erk--nze
       (call-process git-bin nil output nil "add" old-license)
       (format "Could not stage %s update." old-license)))

    (mapc (lambda (rename-map)
            (let* ((dir (concat clone-dir (or (pop rename-map) "")))
                   (default-directory dir)
                   (src (format (pop rename-map) old-feature))
                   (new-filename (when-let ((file (pop rename-map)))
                                   (format
                                    file
                                    new-feature)))
                   (dest (or new-filename
                             (replace-regexp-in-string
                              (rx (literal old-feature)) new-feature src))))
              (when (file-exists-p dest)
                (erk--nze
                 (call-process git-bin nil output nil "rm" "-f" dest)
                 (format "Could not delete: %s" dest)))
              (erk--nze
               (call-process git-bin nil output nil "mv" src dest)
               (format "Could not move: %s to %s" src dest))))
          erk--rename-maps)))

(defun erk--nodash (prefix)
  "Strip dash from PREFIX if present."
  (if (string-match-p (rx "-" eol) prefix)
      (substring prefix 0 (1- (length prefix)))
    prefix))

(defun erk--prefix-match (prefix)
  "Create somewhat collision-safe regex for PREFIX."
  (rx (or whitespace punctuation bol)
      (group (literal (erk--nodash prefix)))
      (or whitespace punctuation eol)))

(defun erk--replace-strings (template clone-path replacements)
  "Replace values in files that need renaming or re-licensing.
TEMPLATE forms are from `erk-templates'.  CLONE-PATH is where we
are renaming.  The REPLACEMENTS form is described in `erk-new'."
  (let* ((default-directory clone-path)
         (old-title (or (plist-get template :title)
                        (erk-package-title)))
         (new-title (plist-get replacements :title))
         (old-github-path (plist-get template :github-path))
         (new-github-path (concat (plist-get replacements :user-org)
                                  "/"
                                  (plist-get replacements :feature)))
         (old-author (erk-package-author))
         (new-author (plist-get replacements :author))
         (old-email (erk-package-email))
         (new-email (plist-get replacements :email))
         ;; Note, dashes are stripped off of inputs in the read, but left here
         ;; in case this is called from Elisp.
         (old-feature (erk--template-feature template))
         (old-feature-rx (erk--prefix-match old-feature))
         (new-feature (erk--nodash (plist-get replacements :feature)))
         (old-prefix-rx (erk--prefix-match (erk--template-prefix template)))
         (new-prefix (erk--nodash (plist-get replacements :prefix)))
         ;; Render old-feature into files.
         (files (erk--expand-filenames erk--files-with-strings old-feature))
         (copy-notice-files (erk--expand-filenames erk--files-with-copy-notices old-feature)))
    (mapc
     (lambda (file)
       (with-current-buffer (find-file-noselect (concat clone-path file) t t)
         (print (format "visiting: %s" (buffer-file-name)))
         (mapc (lambda (s)
                 (while (re-search-forward (rx (literal s)) nil t)
                   (replace-match "")
                   (goto-char (point-min))))
               erk--remove-strings)
         ;; Try to replace strings in an order that limits the likelihood of
         ;; substring collisions.

         ;; replace title, case-insensitively
         (goto-char (point-min))
         (while (re-search-forward (rx (literal old-title)) nil t)
           (replace-match new-title nil t))
         ;; replace github paths
         (goto-char (point-min))
         (while (re-search-forward (rx (literal old-github-path)) nil t)
           (replace-match new-github-path nil t))
         ;; replace author
         (goto-char (point-min))
         (while (re-search-forward (rx (literal old-author)) nil t)
           (replace-match new-author nil t))
         ;; replace email
         (goto-char (point-min))
         (while (re-search-forward (rx (literal old-email)) nil t)
           (replace-match new-email nil t))
         ;; replace feature
         (goto-char (point-min))
         (while (re-search-forward old-feature-rx nil t)
           (replace-match new-feature nil t nil 1)) ; replace sub-expression
         ;; replace prefix
         (goto-char (point-min))
         (while (re-search-forward old-prefix-rx nil t)
           (replace-match new-prefix nil t nil 1)) ; replace sub-expression
         (when (member file copy-notice-files)
           ;; replace license with GPL3 notice
           (when (re-search-forward ";; Permission \\(.\\|\n\\)*SOFTWARE.$" nil t)
             (replace-match erk--gpl3-notice nil t)))
         (goto-char (point-min))
         (save-buffer 0)
         (kill-buffer)))
     files)))

(defun erk-clone (template clone-root replacements)
  "Clone the template repository to CLONE-ROOT and apply rename.
TEMPLATE is the same as forms in `erk-templates'.
REPLACEMENTS is the form described in `erk-new'.

The folder will be renamed to the `:feature' value in
REPLACEMENTS.  The remote will be configured to point to the
`:user-org' value in REPLACEMENTS."

  (if-let ((git-bin (executable-find "git")))
      (let ((default-directory clone-root)
            (output (get-buffer-create "erk-clone"))
            (old-github-path (plist-get template :github-path))
            (new-feature (plist-get replacements :feature))
            (user-org (plist-get replacements :user-org)))
        (erk--nze
         (call-process
          git-bin nil output nil
          "clone"
          (format "https://github.com/%s.git" old-github-path)
          (concat default-directory "/" new-feature))
         "Clone failed")
        (let* ((default-directory (concat clone-root "/" new-feature))
               (rev (plist-get template :rev))
               (rev (when rev (if (string-empty-p rev) nil rev))))
          (when rev (erk--nze
                     (call-process git-bin nil output nil "checkout" rev)
                     (format "Checkout %s failed." rev)))
          (erk--nze
           (call-process "rm" nil output nil "-rf" ".git")
           "Removing old history failed.")
          (erk--nze
           (call-process git-bin nil output nil "init") "Git initialization failed.")
          (erk--nze
           (call-process git-bin nil output nil "add" ".") "Git add all failed.")
          (erk--nze
           (call-process git-bin nil output nil "remote" "add" "origin"
                         (format "git@github.com:%s/%s.git"
                                 user-org new-feature))
           "Adding new remote failed.")
          ;; return value for renaming
          (concat clone-root "/" new-feature "/")))
    (error "Could not find git executable")))

(defun erk-rename-relicense (template clone-dir replacements)
  "Rename and re-license your new cloned template.
CLONE-DIR is the root for renaming and string
replacement.  TEMPLATE is the same forms described in
`erk-templates'.  REPLACEMENTS is the form described in
`erk-new'.

This function replaces all instances of:

1. package title

2. package feature

3. package prefix

4. author name (appended to copyright)

5. author email

6. github user-org

7. MIT license with GPL3 license notices in package files (not
test, manual, or CI though)

8. COPYING is changed to just the GPL3.

9. Documentation is re-generated, containing the new strings."
  (erk--replace-strings
   template clone-dir replacements)
  (erk--rename-package template clone-dir replacements)
  (let ((default-directory clone-dir)
        (enable-local-variables nil))
    (erk-export-docs)))

;;;###autoload
(defun erk-new (template clone-root replacements)
  "Clone an ERK style template and rename files in one step.
TEMPLATE is the same forms described in `erk-templates'.

CLONE-ROOT is where the path for the new clone will be created.

REPLACEMENTS is a plist with keys:

- `:title': The title of the package, usually found in the
  README, manual, and the first line of Elisp files.

- `:feature': By convention, all Elisp file names will include
  the root feature.  This will also be assumed to form the repo
  segment of the new Github remote path.  It's also what shows up
  in MELPA and `use-package' declarations etc.

- `:prefix': The Elisp namespace prefix, usually the same as the
  root feature, but could be an initialism or contraction.

- `:author': The new author's name.

- `:email': Author email.

- `:user-org': The Github username or organization, the first
  part of a Github style path.

- `:rev': Optional.  Either a tag, branch or revision used in git
  checkout.

See comments in `erk-clone' and `erk-rename-relicense' for
implementation information and more details about argument usage."
  (interactive
   (let*
       ((template (cdr (assoc-string
                        (completing-read "Select a template: " erk-templates)
                        erk-templates)))
        (rev (read-string
              "Rev, tag, or branch (empty implies default branch): "))
        (template (plist-put template :rev rev))
        (clone-root
         (let ((root))
           (while (not root)
             (let ((candidate
                    (directory-file-name
                     (read-directory-name "Clone root (must exist): " default-directory))))
               (if (file-directory-p candidate)
                   (setq root candidate)
                 (message "Directory must exist!")
                 (sit-for 1))))
           root))
        (title
         (read-string
          "Package full name, for documentation: "
          "My Package"))
        (feature
         (erk--nodash
          (read-string
           "Package feature, used in use-package and MELPA etc: "
           "my-feature")))
        (prefix
         (erk--nodash
          (read-string
           "Package prefix, such as mp, my-package, mypak etc: "
           "my-package")))
        (user-org (read-string "Github user or organization name: "))
        (author
         (let ((default (when (executable-find "git")
                          (string-trim
                           (shell-command-to-string "git config user.name")))))
           (read-string "Author: " default)))
        (email
         (let ((default (when (executable-find "git")
                          (string-trim
                           (shell-command-to-string "git config user.email")))))
           (read-string "Email: " default))))
     (list
      template
      clone-root
      (list
       :title title
       :feature feature
       :prefix prefix
       :user-org user-org
       :author author
       :email email))))
  (let ((cloned (erk-clone template clone-root replacements)))
    (erk-rename-relicense template cloned replacements)
    (let ((default-directory cloned))
      (run-hooks 'erk-after-new-hook))))

;;;###autoload
(defun erk-insert-package-keyword (keyword)
  "Insert package KEYWORD, from `finder-known-keywords'.
This list's name is easy to forget, so here's a shortcut."
  (interactive
   (list
    (completing-read
     "Insert package keyword:"
     finder-known-keywords
     nil t nil nil
     (lambda (item)
       (format "%s\t%s" (car item) (cdr item))))))
  (insert (format "\"%s\"" keyword)))

(defconst erk--find-paths
  '((ci-dco-action . ".github/workflows/dco.yml")
    (ci-nix-flake . ".github/flake.nix")
    (ci-run-shim . ".github/run-shim.el")
    (ci-tests-action . ".github/workflows/ci.yml")
    (doc-contributing . "doc/CONTRIBUTING.org")
    (doc-manual . "doc/manual.org")
    (doc-readme . "doc/README.org"))
  "Paths that exist in an ERK style project.")

;;;###autoload
(defun erk-find (file)
  "Find FILE within projects using erk's project structure."
  (interactive (list (completing-read "Select file" erk--find-paths)))
  (find-file (concat (erk--project-root) (cdr (assoc-string file erk--find-paths)))))

;;;###autoload
(defun erk-find-ci-dco-action ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "ci-dco-action"))

;;;###autoload
(defun erk-find-ci-nix-flake ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "ci-nix-flake"))

;;;###autoload
(defun erk-find-ci-run-shim ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "ci-run-shim"))

;;;###autoload
(defun erk-find-ci-tests-action ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "ci-tests-action"))

;;;###autoload
(defun erk-find-doc-contributing ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "doc-contributing"))

;;;###autoload
(defun erk-find-doc-manual ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "doc-manual"))

;;;###autoload
(defun erk-find-doc-readme ()
  "Shortcut for `erk-find'."
  (interactive)
  (erk-find "doc-readme"))


(defun erk--export (filename export-fun)
  "Export FILENAME to markdown using EXPORT-FUN."
  (let* ((buffer (find-buffer-visiting filename))
         (buffer (or buffer (find-file-noselect filename))))
    (set-buffer buffer)
    (when (buffer-modified-p buffer)
      (when (yes-or-no-p "Save buffer? ")
        ;; The docs all have a save hook we want to ignore
        (let ((enable-local-variables nil)) (save-buffer))))
    (save-restriction
      (widen)
      (save-excursion
        (when (region-active-p) (deactivate-mark))
        (funcall export-fun)))))

;;;###autoload
(defun erk-export-contributing (&optional preview)
  "Export the contributing doc to markdown.
With prefix argument, PREVIEW the buffer."
  (interactive "P")
  (erk--export
   (concat (erk--project-root)
           (cdr (assoc 'doc-contributing erk--find-paths)))
   #'org-md-export-to-markdown)
  (when preview
    (find-file-read-only-other-window
     (concat (erk--project-root)
             "CONTRIBUTING.md"))))

;;;###autoload
(defun erk-export-manual (&optional preview)
  "Export the manual doc to markdown.
With prefix argument, PREVIEW the buffer."
  (interactive "P")
  (erk--export
   (concat (erk--project-root)
           (cdr (assoc 'doc-manual erk--find-paths)))
   #'org-texinfo-export-to-info)
  (when preview
    (let ((exported-path (concat (erk--project-root)
                                 (format "doc/%s.info"
                                         (erk--package-root-feature)))))
      (info-initialize)
      (info-other-window (Info-find-file exported-path)
                         (generate-new-buffer-name "*info*")))))

;;;###autoload
(defun erk-export-readme (&optional preview)
  "Export the readme doc to markdown.
With prefix argument, PREVIEW the buffer."
  (interactive "P")
  (erk--export
   (concat (erk--project-root)
           (cdr (assoc 'doc-readme erk--find-paths)))
   #'org-md-export-to-markdown)
  (when preview
    (find-file-read-only-other-window
     (concat (erk--project-root)
             "README.md"))))

;;;###autoload
(defun erk-export-docs ()
  "Shortcut to export all docs."
  (interactive)
  (erk-export-contributing)
  (erk-export-readme)
  (erk-export-manual))

;;;###autoload
(defun erk-preview-contributing ()
  "Export and show the contributing."
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively #'erk-export-contributing)))

;;;###autoload
(defun erk-preview-manual ()
  "Export and show the manual."
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively #'erk-export-manual)))

;;;###autoload
(defun erk-preview-readme ()
  "Export and show the readme."
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively #'erk-export-readme)))

;; TODO initialize for tests or batch
;;;###autoload
(defun erk-temp-emacsen ()
  "Create a temporary Emacsen."
  ;; TODO enable multiple Emacsen and multiple base configs
  (interactive)
  (let* ((init-dir (make-temp-file "emacsen-" t))
         (buffer-name (format " *%s*" (file-name-nondirectory
                                       (directory-file-name init-dir))))
         (out-buffer (get-buffer-create buffer-name))
         (emacsen (executable-find "emacs")) ; TODO customize for more Emacsen
         (package-path (erk--project-root))  ; TODO prefer package over project
         ;; TODO set up $HOME to avoid stupid trash files that `no-littering'
         ;; would normally catch
         ;; TODO support no package
         (package-feature (erk--package-root-feature))
         (init-form `(progn
                       (load-theme 'modus-vivendi t) ; TODO customize
                       ;; TODO customize
                       ;; TODO use early init
                       ;; TODO switch to setopt after 28 is deprecated.
                       (setq load-prefer-newer t
                             inhibit-startup-message t
                             package-enable-at-startup nil
                             use-short-answers t
                             use-dialog-box nil
                             pixel-scroll-precision-mode 1
                             ;; Recursive minibuffer too useful when introspecting
                             enable-recursive-minibuffers t
                             default-frame-alist
                             '((tool-bar-lines . 0)
                               (menu-bar-lines . 0)
                               (internal-border-width . 0)
                               (undecorated . nil)
                               (background-color . "#000000") ; startup flicker
                               (ns-appearance . dark)         ; darwin
                               (ns-transparent-titlebar . t)  ; darwin
                               (vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil)))
                       (setq load-path ',load-path)
                       (push ,package-path load-path)
                       (require ',package-feature))))
    (start-process buffer-name out-buffer emacsen
                   "-Q"
                   "--debug-init"       ; TODO switch
                   "--init-dir" init-dir
                   "--eval" (format "%S" init-form))))

(provide 'erk)
;;; erk.el ends here.
