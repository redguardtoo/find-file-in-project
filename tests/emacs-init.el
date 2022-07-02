(require 'find-file-in-project)
(require 'ivy)
(ivy-mode 1)
(setq ffip-match-path-instead-of-filename t)
(run-with-idle-timer
 1
 nil
 (lambda ()
   (erase-buffer)
   (goto-char (point-min))
   (insert
    ";; Setup of this demo,\n"
    "(setq ffip-match-path-instead-of-filename t)\n\n\n"
    ";; Run \"M-x find-file-in-project-by-selected\" and input search keyword \"el\" or \"tests\".\n\n\n"
    ";; Move cursor above below paths and run \"M-x find-file-in-project-at-point\",\n\n"
    ";;   tests/ffip-tests.el ; open file directly \n"
    ";;   find-file-in-project.el:50 ; open file and jump to line 50\n")))
