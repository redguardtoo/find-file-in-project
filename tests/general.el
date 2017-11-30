;;; general.el --- tests for find-file-in-project

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

(require 'find-file-in-project)

(ert-deftest ffip-test-find-by-selected ()
  (let (files)
    (setq ffip-project-root default-directory)
    (setq files (mapcar 'car (ffip-project-search "ivy-mock" nil)))
    ;; (message "files=%s" files)
    (should (string-match-p "ivy-mock.el" (car files)))))


(ert-deftest ffip-test-ffip ()
  (let (files)
    (setq ffip-project-root default-directory)
    (setq files (mapcar 'car (ffip-project-search nil nil)))
    (should (> (length files) 1))
    (should (not (active-minibuffer-window)))))

(ert-deftest ffip-test-ffip-open-another ()
  (let (files
        (prefix-args '(4 (4))))
    (dolist (open-another-arg prefix-args)
      (setq ffip-project-root default-directory)
      (setq files (mapcar 'car (ffip-project-search "ivy" nil)))
      (should (= (length files) 1))
      (should (not (active-minibuffer-window)))
      (ffip-find-files "ivy" open-another-arg))))

(ert-deftest ffip-ffip-show-diff ()
  (let (files
        (ffip-diff-backends '((with-temp-buffer
                                (insert-file-contents "git-diff.diff")
                                (buffer-string)))))
    (ffip-show-diff)

    ;; the first is general.el
    (switch-to-buffer "*ffip-diff*")
    ;; the first file diff hunk
    (goto-char (point-min))
    (diff-file-next)
    (setq ivy-read-called nil)
    ;; find now
    (ffip-diff-find-file)
    (should (not ivy-read-called)) ; only one candidate
    (should (string= (file-name-nondirectory (buffer-file-name)) "general.el"))

    ;; move to the second file hunk
    (switch-to-buffer "*ffip-diff*")
    (diff-file-next)
    (setq ivy-read-called nil)
    ;; find file in the first diff hunk now
    (ffip-diff-find-file)
    (should (not ivy-read-called)) ; only one candidate
    (should (string= (file-name-nondirectory (buffer-file-name)) "ivy-mock.el"))
    ;; cleanup
    (kill-buffer "*ffip-diff*")))
