;;; my-byte-compile.el --- syntax check the code  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Chen Bin
;;
;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;  Syntax check the code.  It's used in Emacs cli.
;;

;;; Code:

(require 'find-lisp)

(let ((files (find-lisp-find-files-internal
              "."
              (lambda (file dir)
                (and (not (file-directory-p (expand-file-name file dir)))
                     (string-match "\\.el$" file)
                     (not (string-match "\\.dir-locals\\.el" file))))
              (lambda (dir parent)
                (not (or (member dir '("." ".." ".git" ".svn" "deps" "tests"))
                         (file-symlink-p (expand-file-name dir parent))))))))
  (dolist (file files)
    (byte-compile-file file)))

;;; my-byte-compile.el ends here
