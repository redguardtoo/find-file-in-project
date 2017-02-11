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
