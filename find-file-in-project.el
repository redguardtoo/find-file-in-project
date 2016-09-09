;;; find-file-in-project.el --- Find files in a project quickly, on any OS

;; Copyright (C) 2006-2009, 2011-2012, 2015, 2016
;;   Phil Hagelberg, Doug Alcorn, Will Farrington, Chen Bin
;;
;; Version: 5.2.4
;; Author: Phil Hagelberg, Doug Alcorn, and Will Farrington
;; Maintainer: Chen Bin <chenbin.sh@gmail.com>
;; URL: https://github.com/technomancy/find-file-in-project
;; Package-Requires: ((ivy "0.7.0") (emacs "24.3"))
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program provides a couple methods for quickly finding any file
;; in a given project.  It depends on GNU find.
;;
;; Usage,
;;   - `M-x find-file-in-project-by-selected' use the selected region
;;      as the keyword to search file.  Or you need provide the keyword
;;      if no region selected.
;;   - `M-x find-directory-in-project-by-selected' use the select region
;;      to find directory.  Or you need provide the keyword if no region
;;      selected.
;;   - `M-x find-file-in-project' will start search file immediately
;;   - `M-x ffip-create-project-file' create .dir-locals.el
;;
;; A project is found by searching up the directory tree until a file
;; is found that matches `ffip-project-file'.
;; You can set `ffip-project-root-function' to provide an alternate
;; function to search for the project root.  By default, it looks only
;; for files whose names match `ffip-patterns',

;; If you have so many files that it becomes unwieldy, you can set
;; `ffip-find-options' to a string which will be passed to the `find'
;; invocation in order to exclude irrelevant subdirectories/files.
;; For instance, in a Ruby on Rails project, you are interested in all
;; .rb files that don't exist in the "vendor" directory.  In that case
;; you could set `ffip-find-options' to "-not -regex \".*vendor.*\"".

;; The variable `ffip-filename-rules' create some extra file names for
;; search when calling `find-file-in-project-by-selected'. For example,
;; When file basename `helloWorld' provided, `HelloWorld', `hello-world'
;; are added as the file name search patterns.
;; `C-h v ffip-filename-rules' to see its default value.
;;
;; All these variables may be overridden on a per-directory basis in
;; your .dir-locals.el.  See (info "(Emacs) Directory Variables") for
;; details.

;; To find in *current directory*, use `find-file-in-current-directory'
;; and `find-file-in-current-directory-by-selected'.

;; `ffip-show-diff' execute the backend from `ffip-diff-backends'.
;; The selected index is the parameter passed to `ffip-show-diff'
;; whose default value in one.
;; The output of execution is expected be in Unified Diff Format.
;; The output is inserted into *ffip-diff* buffer.
;; In the buffer, press "o/C-c C-c"/ENTER" or `M-x ffip-diff-find-file'
;; to open correspong file.
;;
;; `ffip-diff-find-file-before-hook' is called before `ffip-diff-find-file'.
;;
;; If you use evil-mode, insert below code into ~/.emacs,
;;   (defun ffip-diff-mode-hook-setup ()
;;       (evil-local-set-key 'normal "p" 'diff-hunk-prev)
;;       (evil-local-set-key 'normal "n" 'diff-hunk-next)
;;       (evil-local-set-key 'normal "P" 'diff-file-prev)
;;       (evil-local-set-key 'normal "N" 'diff-file-next)
;;       (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
;;       (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
;;   (add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

;; ivy-mode is used for filter/search UI
;; In ivy-mode, SPACE is translated to regex ".*".
;; For example, the search string "dec fun pro" is transformed into
;; regular expression "\\(dec\\).*\\(fun\\).*\\(pro\\)"
;; `C-h i g (ivy)' for more key-binding tips.
;;
;; `ffip-save-ivy-last' saves the most recent search result.
;; `ffip-ivy-resume' re-use the save result. Both requires `ivy-mode'
;; installed. You can use `ivy-resume' too.
;;
;; You can switch to ido-mode by `(setq ffip-prefer-ido-mode t)'

;; GNU Find can be installed,
;;   - through `Brew' on OS X
;;   - through `Cygwin' or `MYSYS2' on Windows.
;; Find executable will be automatically detected. But you can manually
;; specify the executable location by insert below code into ~/.emacs,
;;
;;   (if (eq system-type 'windows-nt)
;;      (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find"))
;; This program works on Windows/Cygwin/Linux/Mac Emacs.
;;
;; Windows setup is as easy as installing Cygwin into default directory on
;; ANY driver. That's all.
;;
;; See https://github.com/technomancy/find-file-in-project for advanced tips

;; Recommended binding: (global-set-key (kbd "C-x f") 'find-file-in-project)

;;; Code:

(require 'diff-mode)

(defvar ffip-filename-rules
  '(ffip-filename-identity
    (ffip-filename-dashes-to-camelcase ffip-filename-camelcase-to-dashes))
  "Rules to create extra file names for GNU Find.")

(defvar ffip-diff-find-file-before-hook nil
  "Hook run before `ffip-diff-find-file' move focus out of *ffip-diff* buffer.")

;;;###autoload
(defun ffip-diff-backend-git-show-commit ()
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (collection (split-string (shell-command-to-string git-cmd) "\n" t)))
    (ffip-completing-read "git log:"
                          collection
                          (lambda (line)
                            (shell-command-to-string (format "git show %s" (car (split-string line "|" t))))))))

;;;###autoload
(defun ffip-diff-backend-hg-show-commit ()
  (let* ((hg-cmd "hg log --template '{node|short}|{date|shortdate}|{desc|strip|firstline}|{author|user}\n'")
         (collection (split-string (shell-command-to-string hg-cmd) "\n" t)))
    (ffip-completing-read "hg log:"
                          collection
                          (lambda (line)
                            (shell-command-to-string (format "hg log -p -g -r %s" (car (split-string line "|" t))))))))

(defvar ffip-diff-backends
  '(ffip-diff-backend-git-show-commit
    "cd $(git rev-parse --show-toplevel) && git diff"
    "cd $(git rev-parse --show-toplevel) && git diff --cached"
    (car kill-ring)
    ffip-diff-backend-hg-show-commit
    "cd $(hg root) && hg diff"
    "svn diff")
  "The list of back-ends.
If back-end is string, it is run in `shell-command-to-string'.
If it's a function or lisp expression, it will be executed and return a string.

The output of execution is inserted into *ffip-diff* buffer with `ffip-diff-mode' on.
`ffip-diff-mode' inherits from `diff-mode'.")

(defvar ffip-find-executable nil "Path of GNU find.  If nil we will guess.")

(defvar ffip-project-file '(".svn" ".hg" ".git")
  "The file/directory used to locate project root.
May be set using .dir-locals.el.  Checks each entry if set to a list.")

(defvar ffip-prefer-ido-mode (not (require 'ivy nil t))
  "Use `ido-mode' instead of `ivy-mode' to display candidates.")

(defvar ffip-patterns nil
  "List of patterns to look for with `find-file-in-project'.")

(defvar ffip-match-path-instead-of-filename nil
  "Match full path instead of file name when calling `find-file-in-project-by-selected'")

(defvar ffip-prune-patterns
  '(;; VCS
    "*/.git/*"
    "*/.svn/*"
    "*/.cvs/*"
    "*/.bzr/*"
    "*/.hg/*"
    ;; project misc
    "*.log"
    "*/bin/*"
    ;; Mac
    "*/.DS_Store/*"
    ;; Ctags
    "*/tags"
    "*/TAGS"
    ;; Global/Cscope
    "*/GTAGS"
    "*/GPATH"
    "*/GRTAGS"
    "*/cscope.files"
    ;; html/javascript/css
    "*/.npm/*"
    "*/.tmp/*" ; TypeScript
    "*/.sass-cache/*" ; SCSS/SASS
    "*/.idea/*"
    "*min.js"
    "*min.css"
    "*/node_modules/*"
    "*/bower_components/*"
    ;; Images
    "*.png"
    "*.jpg"
    "*.jpeg"
    "*.gif"
    "*.bmp"
    "*.tiff"
    "*.ico"
    ;; documents
    "*.doc"
    "*.docx"
    "*.pdf"
    ;; C/C++
    "*.obj"
    "*.o"
    "*.a"
    "*.dylib"
    "*.lib"
    "*.d"
    "*.dll"
    "*.exe"
    ;; Java
    "*/.metadata*"
    "*/.gradle/*"
    "*.class"
    "*.war"
    "*.jar"
    ;; Emacs/Vim
    "*flymake"
    "*/#*#"
    ".#*"
    "*.swp"
    "*~"
    "*.elc"
    "*/.cask/*"
    ;; Python
    "*.pyc")
  "List of directory/file patterns to not descend into when listing files in `find-file-in-project'.")

(defvar ffip-find-options ""
  "Extra options to pass to `find' when using `find-file-in-project'.

Use this to exclude portions of your project: \"-not -regex \\\".*svn.*\\\"\".")

(defvar ffip-project-root nil
  "If non-nil, overrides the project root directory location.")

(defvar ffip-project-root-function nil
  "If non-nil, this function is called to determine the project root.

This overrides variable `ffip-project-root' when set.")

(defvar ffip-ivy-last-saved nil
  "Backup of `ivy-last'.  Requires ivy-mode.")

(defvar ffip-full-paths t
  "If nil only file names in search results are visible.")

(defvar ffip-debug nil "Print debug information.")

;;;###autoload
(defun ffip-project-root ()
  "Return the root of the project."
  (let ((project-root (or ffip-project-root
                          (if (functionp ffip-project-root-function)
                              (funcall ffip-project-root-function)
                            (if (listp ffip-project-file)
                                (cl-some (apply-partially 'locate-dominating-file
                                                       default-directory)
                                      ffip-project-file)
                              (locate-dominating-file default-directory
                                                      ffip-project-file))))))
    (or project-root
        (progn (message "No project was defined for the current file.")
               nil))))

(defun ffip--read-file-text (file)
  (read (decode-coding-string
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (setq buffer-file-coding-system 'binary)
           (insert-file-contents-literally file)
           (buffer-substring-no-properties (point-min) (point-max))) 'utf-8)))

(defun ffip--find-rule-to-execute (keyword f)
  "If F is a function, return it.

If F is a list, assume each element is a function.
Run each element with keyword as 1st parameter as KEYWORD and 2nd parameter as t.
If the result is true, return the function."
  (let (rlt found fn)
    (cond
     ((functionp f) (setq rlt f))

     ((listp f)
      (while (and f (not found))
        (setq fn (car f))
        (if (funcall fn keyword t)
            (setq found t)
          (setq f (cdr f))))
      (setq rlt (if found fn 'identity)))

     (t (setq rlt 'identity)))

    rlt))

;;;###autoload
(defun ffip-save-ivy-last ()
  "Save `ivy-last' into `ffip-ivy-last-saved'.  Requires `ivy-mode'."
  (interactive)
  (if (boundp 'ivy-last)
      (setq ffip-ivy-last-saved ivy-last)
    (message "Sorry. You need install `ivy-mode' first.")))

;;;###autoload
(defun ffip-ivy-resume ()
  "Wrapper of `ivy-resume'.  Resume the search saved at `ffip-ivy-last-saved'."
  (interactive)
  (let* ((ivy-last (if ffip-ivy-last-saved ffip-ivy-last-saved ivy-last))
         (default-directory (or ffip-project-root (ffip-project-root)
                                (error "No project root found"))))
    (if (fboundp 'ivy-resume)
        (ivy-resume)
      (message "Sorry. You need install `ivy-mode' first."))))

;;;###autoload
(defun ffip-filename-identity (keyword)
  "Return identical KEYWORD."
  keyword)

;;;###autoload
(defun ffip-filename-camelcase-to-dashes (keyword &optional check-only)
  "Convert KEYWORD from camel cased to dash seperated.
If CHECK-ONLY is true, only do the check."
  (let* (rlt)
    (cond
     (check-only
      (setq rlt (string-match "^[a-z0-9]+[A-Z][A-Za-z0-9]+$" keyword))
      (if ffip-debug (message "ffip-filename-camelcase-to-dashes called. check-only keyword=%s rlt=%s" keyword rlt)))
     (t
      (let* ((case-fold-search nil))
        ;; case sensitive replace
        (setq rlt (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" keyword))))

      (if (string= rlt (downcase keyword)) (setq rlt nil))

      (if (and rlt ffip-debug) (message "ffip-filename-camelcase-to-dashes called. rlt=%s" rlt))))
    rlt))

;;;###autoload
(defun ffip-filename-dashes-to-camelcase (keyword &optional check-only)
  "Convert KEYWORD from dash seperated to camel cased.
If CHECK-ONLY is true, only do the check."
  (let* (rlt)
    (cond
     (check-only
        (setq rlt (string-match "^[A-Za-z0-9]+\\(-[A-Za-z0-9]+\\)+$" keyword))
        (if ffip-debug (message "ffip-filename-dashes-to-camelcase called. check-only keyword=%s rlt=%s" keyword rlt)))
     (t
      (setq rlt (mapconcat (lambda (s) (capitalize s)) (split-string keyword "-") ""))

      (let ((first-char (substring rlt 0 1)))
       (setq rlt (concat "[" first-char (downcase first-char) "]" (substring rlt 1))))
      (if (and rlt ffip-debug) (message "ffip-filename-dashes-to-camelcase called. rlt=%s" rlt))))
    rlt))

(defun ffip--create-filename-pattern-for-gnufind (keyword)
  (let* ((rlt ""))
    (cond
     ((not keyword)
      (setq rlt ""))
     ((not ffip-filename-rules)
      (setq rlt (concat (if ffip-match-path-instead-of-filename "-iwholename" "-name")
                        " \"*"
                        keyword
                        "*\"" )))
     (t
      (dolist (f ffip-filename-rules rlt)
        (let (tmp fn)
          (setq fn (ffip--find-rule-to-execute keyword f))
          (setq tmp (funcall fn keyword))
          (when tmp
            (setq rlt (concat rlt (unless (string= rlt "") " -o")
                              " "
                              (if ffip-match-path-instead-of-filename "-iwholename" "-name")
                              " \"*"
                              tmp
                              "*\"")))))
      (unless (string= "" rlt)
        (setq rlt (concat "\\(" rlt " \\)")))))

    (if ffip-debug (message "ffip--create-filename-pattern-for-gnufind called. rlt=%s" rlt))
    rlt))

(defun ffip--win-executable-find (driver path exe)
  (let* (rlt)
    (if (executable-find (concat driver path exe))
        (setq rlt (concat driver path exe)))
    rlt))

(defun ffip--executable-find (exe)
  (let* ((rlt (if (eq system-type 'windows-nt)
                  (or
                   ;; cygwin
                   (ffip--win-executable-find "c" ":\\\\cygwin64\\\\bin\\\\" exe)
                   (ffip--win-executable-find "d" ":\\\\cygwin64\\\\bin\\\\" exe)
                   (ffip--win-executable-find "e" ":\\\\cygwin64\\\\bin\\\\" exe)
                   (ffip--win-executable-find "c" ":\\\\cygwin\\\\bin\\\\" exe)
                   (ffip--win-executable-find "d" ":\\\\cygwin\\\\bin\\\\" exe)
                   (ffip--win-executable-find "e" ":\\\\cygwin\\\\bin\\\\" exe)
                   ;; msys2
                   (ffip--win-executable-find "c" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)
                   (ffip--win-executable-find "d" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)
                   (ffip--win-executable-find "e" ":\\\\msys64\\\\usr\\\\bin\\\\" exe)
                   (ffip--win-executable-find "c" ":\\\\msys32\\\\usr\\\\bin\\\\" exe)
                   (ffip--win-executable-find "d" ":\\\\msys32\\\\usr\\\\bin\\\\" exe)
                   (ffip--win-executable-find "e" ":\\\\msys32\\\\usr\\\\bin\\\\" exe))
                ;; *nix
                (executable-find exe))))
    (unless rlt
      ;; well, `executable-find' failed
      (setq rlt exe))
    rlt))

(defun ffip--join-patterns (patterns)
  "Turn `ffip-patterns' into a string that `find' can use."
  (if ffip-patterns
      (format "\\( %s \\)" (mapconcat (lambda (pat) (format "-iwholename \"%s\"" pat))
                         patterns " -or "))
    ""))

(defun ffip--prune-patterns ()
  "Turn `ffip-prune-patterns' into a string that `find' can use."
  (mapconcat (lambda (pat) (format "-iwholename \"%s\"" pat))
             ffip-prune-patterns " -or "))

;;;###autoload
(defun ffip-completing-read (prompt collection action)
  (cond
   ((= 1 (length collection))
    ;; open file directly
    (funcall action (car collection)))
   ;; if user prefer `ido-mode' or `ivy-read' is not defined,
   ;; we use `ido-completing-read'.
   ((or ffip-prefer-ido-mode (not (fboundp 'ivy-read)))
    ;; friendly UI for ido
    (let* ((list-of-pair (consp (car collection)))
           (ido-collection (if list-of-pair
                               (mapcar 'car collection)
                             collection))
           (ido-selected (ido-completing-read prompt ido-collection)))
      (if ido-selected
          (funcall action
                   (if list-of-pair
                       (cdar (delq nil
                                   (mapcar (lambda (x)
                                             (and (string= (car x)
                                                           ido-selected)
                                                  x))
                                           collection)))
                     ido-selected)))))
   (t
    (ivy-read prompt collection
              :action action))))

;;;###autoload
(defun ffip-project-search (keyword find-directory)
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique.

If KEYWORD is string, it's the file name or file path to find file.
If KEYWORD is list, it's the list of file names."
  (let* (rlt
         (root (expand-file-name (or ffip-project-root (ffip-project-root)
                                     (error "No project root found"))))
         (default-directory (file-name-as-directory root))
         (cmd (format "%s . \\( %s \\) -prune -o -type %s %s %s %s -print"
                      (if ffip-find-executable ffip-find-executable (ffip--executable-find "find"))
                      (ffip--prune-patterns)
                      (if find-directory "d" "f")
                      (ffip--join-patterns ffip-patterns)
                      ;; When finding directory, the keyword is like:
                      ;; "proj/hello/world"
                      (cond
                       ((listp keyword)
                        ;; already got enough files names to handle
                        (let* ((ffip-filename-rules nil))
                          (ffip--create-filename-pattern-for-gnufind keyword)))
                       (t
                        (if find-directory (format "-iwholename \"*%s\"" keyword)
                          (ffip--create-filename-pattern-for-gnufind keyword))))
                      ffip-find-options)))

    (if ffip-debug (message "run cmd at %s: %s" default-directory cmd))
    (setq rlt
          (mapcar (lambda (file)
                    (if ffip-full-paths
                        (cons (replace-regexp-in-string "^\./" "" file)
                              (expand-file-name file))
                      (cons (file-name-nondirectory file)
                            (expand-file-name file))))
                  ;; #15 improving handling of directories containing space
                  (split-string (shell-command-to-string cmd) "[\r\n]+" t)))
    rlt))

(defun ffip--forward-line (lnum)
  "Forward LNUM lines"
  (if ffip-debug (message "ffip--forward-line called => %s" lnum))
  (when (and lnum (> lnum 0))
    (goto-char (point-min))
    (forward-line (1- lnum))))

;;;###autoload
(defun ffip-find-files (keyword open-another-window &optional find-directory fn)
  "The API to find files."
  (let* (project-files
         lnum
         file
         root)

    ;; extract line num if exists
    (when (and keyword (stringp keyword)
               (string-match "^\\(.*\\):\\([0-9]+\\):?$" keyword))
      (setq lnum (string-to-number (match-string 2 keyword)))
      (setq keyword (match-string 1 keyword)))

    (setq project-files (ffip-project-search keyword find-directory))
    (if (> (length project-files) 0)
        (progn
          (setq root (file-name-nondirectory
                      (directory-file-name
                       (or ffip-project-root (ffip-project-root)))))
          (ffip-completing-read
           (format "Find in %s/: " root)
           project-files
           `(lambda (file)
             ;; only one item in project files
             (if (listp file) (setq file (cdr file)))
             (if ,find-directory
                 (if ,open-another-window
                     (dired-other-window file)
                   (switch-to-buffer (dired file)))
               ;; open file
               (if ,open-another-window
                   (find-file-other-window file)
                 (find-file file))
               ;; goto line if needed
               (ffip--forward-line ,lnum)
               (if ,fn (funcall ,fn file))))))
      (message "Nothing found!"))))

(defun ffip--prepare-root-data-for-project-file (root)
  (cons 'ffip-project-root root))

;;;###autoload
(defun ffip-create-project-file ()
  "Create or Append .dir-locals.el to set up per directory.
You can move .dir-locals.el to root directory.
See (info \"(Emacs) Directory Variables\") for details."
  (interactive)
  (let* ((root (read-directory-name "Project root directory?" default-directory))
         (file (if (and root (file-exists-p root))
                   (concat (file-name-as-directory root) ".dir-locals.el"))))
    (when file
      (with-temp-buffer
        (let ((print-level nil)  (print-length nil) sexp (rlt '(a)))
          (cond
           ;; modify existing .dir-locals.el
           ((file-exists-p file)
            (let (sub-sexp new-sub-sexp)
              (setq sexp (ffip--read-file-text file))
              ;; valid .dir-locals.el
              (when sexp
                ;; the list for nil
                (setq sub-sexp (assoc nil sexp))
                (cond
                 ;; `(nil (prop1 . val1) (prop2 . val2))' exists
                 (sub-sexp
                  ;; remove (ffip-project-root . "/path/file")
                  (if (assoc 'ffip-project-root sub-sexp)
                      (setq new-sub-sexp (delete (assoc 'ffip-project-root sub-sexp) sub-sexp))
                    (setq new-sub-sexp sub-sexp))
                  (add-to-list 'new-sub-sexp (ffip--prepare-root-data-for-project-file root) t)
                  ;; update sexp
                  (setq sexp (delete sub-sexp sexp))
                  (add-to-list 'sexp new-sub-sexp))
                 (t
                  ;; add `(nil (ffip-project-root . "path/file"))'
                  (add-to-list 'sexp (list nil (ffip--prepare-root-data-for-project-file root))))))
              ))
           (t
            ;; a new .dir-locals.el
            (setq sexp (list (list nil (ffip--prepare-root-data-for-project-file root))))))
          (when sexp
            (insert (format "%S" sexp))
            (write-file file)
            (message "%s created." file)))))))

;;;###autoload
(defun ffip-current-full-filename-match-pattern-p (regex)
  "Is current full file name (including directory) match the REGEX?"
  (let* ((dir (if (buffer-file-name) (buffer-file-name) "")))
    (string-match-p regex dir)))

;;;###autoload
(defun find-file-in-project (&optional open-another-window)
  "Prompt with a completing list of all files in the project to find one.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

The project's scope is defined as the first directory containing
a `ffip-project-file' whose value is \".git\" by default.

You can override this by setting the variable `ffip-project-root'."

  (interactive "P")
  (ffip-find-files nil open-another-window))

;;;###autoload
(defun ffip-get-project-root-directory ()
  "Get the full path of project root directory."
  (expand-file-name (or ffip-project-root
                        (ffip-project-root))))

;;;###autoload
(defun find-file-in-current-directory (&optional open-another-window)
  "Like `find-file-in-project'.  But search only in current directory."
  (interactive "P")
  (let* ((ffip-project-root default-directory))
    (find-file-in-project open-another-window)))

;;;###autoload
(defun find-file-in-project-by-selected (&optional open-another-window)
  "Similar to `find-file-in-project'.
But use string from selected region to search files in the project.
If no region is selected, you need provide keyword.

Keyword could be ANY part of the file's full path and support wildcard.
For example, to find /home/john/proj1/test.js, below keywords are valid:
- test.js
- roj1/tes
- john*test

If keyword contains line number like \"hello.txt:32\" or \"hello.txt:32:\",
we will move to that line in opened file.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let* ((keyword (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (read-string "Enter keyword:"))))
    (ffip-find-files keyword open-another-window)))

;;;###autoload
(defun find-file-in-current-directory-by-selected (&optional open-another-window)
  "Like `find-file-in-project-by-selected'.  But search only in current directory."
  (interactive "P")
  (let* ((ffip-project-root default-directory))
    (find-file-in-project-by-selected open-another-window)))

;;;###autoload
(defun find-directory-in-project-by-selected (&optional open-another-window)
  "Similar to `find-file-in-project-by-selected'.
Use string from selected region to find directory in the project.
If no region is selected, you need provide keyword.

Keyword could be directory's base-name only or parent-directoy+base-name
For example, to find /home/john/proj1/test, below keywords are valid:
- test
- roj1/test
- john*test

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let* ((keyword (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (read-string "Enter keyword:"))))
    (ffip-find-files keyword open-another-window t)))

;;;###autoload
(defalias 'ffip 'find-file-in-project)


;;;###autoload
(defun ffip-diff-quit ()
  (interactive)
  ;; kill buffer instead of bury it
  (quit-window t))

;;;###autoload
(defun ffip-diff-find-file (&optional open-another-window)
  "File file(s) in current hunk."
  (interactive "P")
  (let* ((files (mapcar 'file-name-nondirectory (diff-hunk-file-names)))
        (alnum 0)
        (blnum 0))

    (save-excursion
      (diff-beginning-of-hunk t)
      (when (looking-at "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?")
        (setq alnum (string-to-number (match-string 1)))
        (setq blnum (string-to-number (match-string 3)))))

    (if (and (> (length files) 1)
             (string= (nth 0 files) (nth 1 files)))
        (ffip-find-files (nth 0 files)
                         open-another-window
                         nil
                         (lambda (opened-file)
                           ;; use line number in new file since there is only one file name candidate
                           (ffip--forward-line blnum)))
      (run-hook-with-args 'ffip-diff-find-file-before-hook)
      (ffip-find-files files
                       open-another-window
                       nil
                       (lambda (opened-file)
                         (cond
                          ((string= (file-name-nondirectory opened-file) (nth 0 files))
                           (ffip--forward-line alnum))
                          (t
                           (ffip--forward-line blnum))))))))

(defvar ffip-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map "p" 'diff-hunk-prev)
    (define-key map "n" 'diff-hunk-next)
    (define-key map "P" 'diff-file-prev)
    (define-key map "N" 'diff-file-next)
    (define-key map [remap diff-goto-source] 'ffip-diff-find-file)
    map)
  "Mode map based on `diff-mode-map'")

(define-derived-mode ffip-diff-mode diff-mode "ffip"
  "Show diff/patch."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map ffip-diff-mode-map))

(defun ffip-show-content-in-diff-mode (content)
  "Insert content into *ffip-diff* buffer."
  (cond
   ((and content (not (string= content "")))
    (let (rlt-buf)
      (if (get-buffer "*ffip-diff*")
          (kill-buffer "*ffip-diff*"))
      (setq rlt-buf (get-buffer-create "*ffip-diff*"))
      (save-current-buffer
        (switch-to-buffer-other-window rlt-buf)
        (set-buffer rlt-buf)
        (erase-buffer)
        (insert content)
        (ffip-diff-mode)
        (goto-char (point-min)))))
   (t
    (message "Output is empty!"))))

;;;###autoload
(defun ffip-show-diff (&optional num)
  "Show the diff output by excuting selected `ffip-diff-backends'.
NUM is the index selected backend from `ffip-diff-backends'.
NUM is zero based.  Its default value is zero."
  (interactive "P")
  (cond
   ((or (not num) (< num 0))
    (setq num 0))
   ((> num (length ffip-diff-backends))
    (setq num (1- (length ffip-diff-backends)))))

  (let* ((backend (nth num ffip-diff-backends))
         rlt-buf)

    (if backend
      (cond
       ;; shell command
       ((stringp backend)
        (ffip-show-content-in-diff-mode (shell-command-to-string backend)))
       ;; command
       ((functionp backend)
        (ffip-show-content-in-diff-mode (funcall backend)))
       ;; lisp exipression
       ((consp backend)
        (ffip-show-content-in-diff-mode (funcall `(lambda () ,backend))))))))

;; safe locals
(progn
  (put 'ffip-diff-backends 'safe-local-variable 'listp)
  (put 'ffip-patterns 'safe-local-variable 'listp)
  (put 'ffip-prune-patterns 'safe-local-variable 'listp)
  (put 'ffip-filename-rules 'safe-local-variable 'listp)
  (put 'ffip-match-path-instead-of-filename 'safe-local-variable 'booleanp)
  (put 'ffip-project-file 'safe-local-variable 'stringp)
  (put 'ffip-project-root 'safe-local-variable 'stringp))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
