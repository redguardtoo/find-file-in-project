;; mockup `ivy-read'
(require 'cl-lib)
(require 'ert)

(cl-defun ivy-read (prompt collection
                           &key predicate require-match initial-input
                           history preselect keymap update-fn sort
                           action unwind re-builder matcher dynamic-collection caller)
  (message "ivy-read mockup is called"))

(provide 'ivy)