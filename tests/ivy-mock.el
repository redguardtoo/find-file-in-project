;;; ivy-mock.el --- tests for find-file-in-project

;; Copyright (C) 2016-2017 Chen Bin
;;

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;; mockup `ivy-read'
(require 'cl-lib)
(require 'ert)

(defvar ivy-read-called nil)

(cl-defun ivy-read (prompt collection
                           &key predicate require-match initial-input
                           history preselect keymap update-fn sort
                           action unwind re-builder matcher dynamic-collection caller)
  (setq ivy-read-called t)
  (message "`ivy-read' mockup is called"))
