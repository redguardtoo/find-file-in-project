#!/bin/sh
emacs -batch -l cl-lib -l ert -l ivy.el -l ../find-file-in-project.el  -l general.el -l windows.el -f ert-run-tests-batch-and-exit
