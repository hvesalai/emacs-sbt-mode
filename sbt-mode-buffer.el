;;; scala-mode-buffer.el - Functions for discovering the current sbt project
;;
;; Copyright(c) 2013 Heikki Vesalainen
;; For information on the License, see the LICENSE file

(require 'sbt-mode-project)

(defcustom sbt:buffer-name-base "*sbt*"
  "Buffer name for sbt"
  :type 'string
  :group 'sbt)

(defun sbt:buffer-name ()
  "Return the buffer name for running sbt."
  (format "%s<%s>"
          sbt:buffer-name-base
          (sbt:find-root)))

(defun sbt:require-buffer ()
  "Throw error if buffer the current buffer is not an sbt-buffer"
  (unless (derived-mode-p 'sbt-mode) 
    (error "Current buffer %s is not an sbt-buffer" (current-buffer))))

(defun sbt:mode-p ()
  "Return non-nil if the current buffer is sbt-buffer"
  (derived-mode-p 'sbt-mode))

(provide 'sbt-mode-buffer)
