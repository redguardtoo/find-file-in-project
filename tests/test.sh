#!/bin/sh
cd "$(dirname "$0")"
emacs -batch -l cl-lib -l ert -l ivy-mock.el -l ../find-file-in-project.el  -l general.el -l windows.el -f ert-run-tests-batch-and-exit
