;;; sbt-mode-comint.el - Support functions for comint-mode
;;
;; Copyright(c) 2013 Heikki Vesalainen
;; For information on the License, see the LICENSE file

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'sbt-mode-vars)
(require 'sbt-mode-project)
(require 'sbt-mode-buffer)

(defvar sbt:submode)
(declare-function sbt-command "sbt-mode" (command &optional focus))

(defvar ansi-color-regexp)
(when (not (boundp 'ansi-color-regexp))
  ;; `ansi-color-regexp' removed from ansi-color.el and replaced by
  ;; `ansi-color-control-seq-regexp'
  ;; commit 35ed01dfb3f811a997e26d843e9971eb6b81b125
  (setq ansi-color-regexp ansi-color-control-seq-regexp))

(defcustom sbt:sbt-history-file "target/.sbt_mode_shell_history"
  "The file to store and read shell command history from for
comint. Not the sbt's file."
  :type 'string
  :group 'sbt)

(defcustom sbt:console-history-file "target/.sbt_mode_console_history"
  "The file to store and read console command history from for
comint. Not the sbt's file."
  :type 'string
  :group 'sbt)

(defcustom sbt:sbt-prompt-regexp (concat "^\\(" ansi-color-regexp "\\)*" "\\(sbt:[^>]+\\)?>[ ]+")
  "A regular expression to match sbt shell prompt"
  :type 'string
  :group 'sbt)

(defcustom sbt:console-prompt-regexp "^scala>[ ]+"
  "A regular expression to match scala REPL prompt"
  :type 'string
  :group 'sbt)

(defcustom sbt:paste-mode-prompt-regexp "^// Entering paste mode"
  "A regular expression to detect paste-mode"
  :type 'string
  :group 'sbt)

(defcustom sbt:ansi-support t
  "See `ansi-color-for-comint-mode' in `ansi-color.el'"
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Filter" filter)
                 (const :tag "Translate" t))
  :group 'sbt)

(defcustom sbt:scroll-to-bottom-on-output nil
  "If non-nil, always scroll sbt buffer to the bottom on new output.
If nil, stop scrolling on a first error encountered or if point
is not on last line of output buffer."
  :type 'boolean
  :group 'sbt)

(defvar sbt:prompt-regexp (concat "\\(?:"
                                  sbt:sbt-prompt-regexp
                                  "\\)\\|\\(?:"
                                  sbt:console-prompt-regexp
                                  "\\)"))

(defvar sbt:quit-paste-command (if (eq system-type 'windows-nt)
                                   (kbd "C-z")
                                 (kbd "C-d"))
  "Keys for sending quit command")

(defconst sbt:history-separator ";;\n")

(defvar-local sbt:console-scala-version nil
  "Major Scala version of the running REPL, or nil if not known.")

(defvar-local sbt:pending-paste-code nil
  "Code strings waiting to be pasted once the console prompt appears.
Oldest first.")

(defvar-local sbt:pending-paste-files nil
  "Temp files sent to the REPL with :load, deleted at the next prompt.")

(defun sbt:initialize-for-comint-mode ()
  (sbt:require-buffer)
  (when (derived-mode-p 'comint-mode)

    (setq comint-process-echoes nil)
    (setq comint-scroll-to-bottom-on-output sbt:scroll-to-bottom-on-output)
    (setq comint-prompt-regexp sbt:prompt-regexp)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-preoutput-filter-functions '(sbt:move-marker-before-prompt-filter))
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom sbt:move-marker-after-prompt-filter))
    (setq ansi-color-for-comint-mode sbt:ansi-support)
    (setq comint-input-sender 'sbt:input-sender)
    (setq-local sbt:previous-history-file nil)
    (setq-local sbt:submode nil)
    (setq-local sbt:console-scala-version nil)
    (setq-local sbt:pending-paste-files nil)
    (setq-local sbt:pending-paste-code nil)
    (add-hook 'comint-output-filter-functions 'sbt:switch-submode)
    (add-hook 'comint-output-filter-functions 'sbt:ansi-filter)
    ;; Must run before `sbt:switch-submode' (prepended) so that the
    ;; version is known when the console prompt is first seen.
    (add-hook 'comint-output-filter-functions 'sbt:detect-scala-version-filter nil t)
    ;; Must run after `sbt:switch-submode' (appended) so that pasting
    ;; happens once we know we are at a console prompt.
    (add-hook 'comint-output-filter-functions 'sbt:paste-output-filter t t)
    (add-hook 'kill-buffer-hook 'sbt:delete-all-pending-paste-files nil t)
))

(defconst sbt:ansi-clear-line "M\\[2K"
  "'Ansi code' sequence sent by JLine to clear the previous
line.")

(defun sbt:input-sender (proc string)
  (sit-for 0) ; the purpose of this sit-for 0 is to let emacs show the
              ; newline that the user just inserted. Without this
              ; sometimes emacs will not give the user any feedback
              ; that the input has been sent.
  (comint-simple-send proc string)
  ;;(message "sent %s to %s" string comint-input-ring-file-name)
  (let ((comint-input-ring-separator sbt:history-separator))
    (comint-write-input-ring)))

(defun sbt:ansi-filter (input)
  (when (sbt:mode-p)
    (save-excursion
      ;; go to start of first line just inserted
      (comint-goto-process-mark)
      (goto-char (max (point-min) (- (point) (string-width input))))
      (forward-line 0)
      (while (re-search-forward sbt:ansi-clear-line nil t)
        ;; delete the ansi code and the previous line
        (delete-region (save-excursion (forward-line -1) (point)) (match-end 0))))
    input))

(defun sbt:move-marker-before-prompt-filter (input-string)
  "Move the process marker to beginning of prompt so that the
prompt will be moved with output. Also mangles the `input-string`
so that if it contains the prompt, it is moved to the end of the
input. This is needed because, especially in sbt, the output can
contain out-of-band output from other Threads that mix up the
prompt."

  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char pmark)
      (forward-line 0) ;; start of line
      (when (looking-at sbt:console-prompt-regexp)
        (set-marker pmark (- (point) 1)))))

  (let ((new-input-string 
         (if (string-match sbt:console-prompt-regexp input-string)
             (let* ((beg (match-beginning 0))
                    (before (substring input-string 0 (max 0 (- beg 1))))
                    (nl (substring input-string (max 0 (- beg 1)) beg))
                    (after (substring input-string (match-end 0)))
                    (prompt (match-string 0 input-string)))
               (concat before after nl prompt))
           input-string)))
    new-input-string))

(defun sbt:move-marker-after-prompt-filter (input-string)
  "Move the process marker to after prompt. This just reverses
what `sbt:move-marker-before-prompt-filter` did."

  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char pmark)
      (forward-line 1)
      (when (looking-at sbt:console-prompt-regexp)
        (set-marker pmark (match-end 0))))))

(defun sbt:switch-submode (input)
  (when (sbt:mode-p)
    (let ((project-root (sbt:find-root))
          (submode
           (save-excursion
             (save-match-data
               ;; go to start of last line with text inserted by comint
               (comint-goto-process-mark)
               (skip-chars-backward " \n\r\t")
               (forward-line 0)
               ;;(message "last line is %s" (thing-at-point 'line))
               (cond ((looking-at sbt:sbt-prompt-regexp) 'sbt)
                     ((looking-at sbt:console-prompt-regexp) 'console)
                     ((looking-at sbt:paste-mode-prompt-regexp) 'paste-mode)))))
          (comint-input-history-ignore "^completions\\|// completions$")
          (comint-input-ring-separator sbt:history-separator))

      (when (and submode (not (eq submode sbt:submode)))
        ;; set-up the new mode
        (setq sbt:submode submode)
        (setq comint-use-prompt-regexp (not (eq submode 'paste-mode)))
        ;; Back in sbt shell: the console (and its Scala version) is gone.
        (when (eq submode 'sbt)
          (setq sbt:console-scala-version nil))

        ;; clear the history
        (ring-resize comint-input-ring 0)
        (ring-resize comint-input-ring comint-input-ring-size)

        ;; and reload (if file found)
        (when project-root
          (setq comint-input-ring-file-name
                (cond ((eq submode 'sbt) (concat project-root sbt:sbt-history-file))
                      ((eq submode 'console) (concat project-root sbt:console-history-file))))
          (when (and comint-input-ring-file-name (file-exists-p comint-input-ring-file-name))
            (comint-read-input-ring))))))
  input)

;;;
;;; Completion functionality
;;;

(defun sbt:scala-escape-char (c)
  (cond ((= c ?\") "\\\"")
        ((= c ?\\) "\\\\")
        ((or (< c #x20) (> c #x7e)) (format "\\u%04x" c))
        (t (string c))))

(defun sbt:scala-escape-string (str)
  (mapconcat 'sbt:scala-escape-char str ""))

(defconst sbt:completions-regex "^\\[completions\\] \\(.*?\\)?$")

(defun sbt:get-completions (input)
   (sbt:require-buffer)
   (when (not (comint-check-proc (current-buffer)))
     (error "process not running in buffer %s" (current-buffer)))
   (when (or (null input) (string-match "^\\s *$" input))
     (setq input ""))
   (let ((submode
          (save-excursion
            (comint-goto-process-mark)
            (comint-bol t)
            (cond ((looking-at sbt:sbt-prompt-regexp) 'sbt)
                  ((looking-at sbt:console-prompt-regexp) 'console)
                  ('t (error "process not ready (no prompt found)"))))))
     (message "Querying completions for %s..." input)
     (setq input
           (cond ((eq submode 'sbt) (concat "completions \""
                                            (sbt:scala-escape-string input)
                                            "\""))
                 ((eq submode 'console) (concat ":completions " input))))
     (prog1
         (comint-redirect-results-list input
                                       sbt:completions-regex
                                       1)
       (message nil))))

(defun sbt:completion-at-point ()
  (sbt:require-buffer)
  (let ((point (point))
        (beg (save-excursion (comint-goto-process-mark)
                             (point)))
        (end (max (point)
                  (save-excursion (end-of-line)
                                  (skip-chars-backward " \t\n\r")
                                  (point))))
        mid)
    (goto-char beg)
    (comint-bol t)
    (if (> beg end)
        (comint-goto-process-mark)
      (cond ((or (looking-at-p sbt:sbt-prompt-regexp)
                 (looking-at-p sbt:console-prompt-regexp))
             (goto-char point)
             (let ((completions (sbt:get-completions (buffer-substring beg end))))
               (completion-in-region beg end completions `(lambda (s) (> (string-width s) 0)))))
            (t
             (goto-char point)
             "No sbt or scala prompt found before process mark")))))

(defun sbt:send-region (start end)
  (unless (comint-check-proc (sbt:buffer-name))
    (error "sbt is not running in buffer %s" (sbt:buffer-name)))
  (save-excursion
    (goto-char end)
    (skip-syntax-forward ">")
    (forward-comment (- (point-max)))
    (setq end (point)))
  (save-excursion
    (goto-char start)
    (forward-comment (point-max))
    (setq start (point)))
  (unless (> end start) (error "mark a region of code first"))
  (display-buffer (sbt:buffer-name))
  (let ((submode (buffer-local-value 'sbt:submode
                                     (get-buffer (sbt:buffer-name)))))
    (unless (or (eq submode 'console) (eq submode 'paste-mode))
      (sbt-command "console")))
  ;; TODO: Do not send region if there is an error.
  ;;
  ;; There may be compilation by (sbt-command "console")
  (let ((submode (buffer-local-value 'sbt:submode
                                     (get-buffer (sbt:buffer-name)))))
    (when (or (eq submode 'console) (eq submode 'paste-mode))
      (comint-send-region (sbt:buffer-name) start end)
      (comint-send-string (sbt:buffer-name) "\n"))))


(defcustom sbt:paste-method 'auto
  "How `sbt-paste-region' sends a region to the Scala REPL.

`auto' uses :paste for a Scala 2 REPL and :load (with a temporary
file) for a Scala 3 REPL, which has no :paste command.  The REPL
version is detected from the \"Welcome to Scala\" banner printed
when the console starts; if it is not known, :load is used.

`paste' always uses :paste, `load' always uses :load."
  :type '(choice (const :tag "Detect from REPL version" auto)
                 (const :tag "Always :paste" paste)
                 (const :tag "Always :load" load))
  :group 'sbt)

(defconst sbt:scala-welcome-regexp "Welcome to Scala \\([0-9]+\\)\\.[0-9]+"
  "Matches the banner printed by both Scala 2 and Scala 3 REPLs.
Group 1 is the major version.")

(defun sbt:detect-scala-version-filter (input)
  "Record the REPL's Scala major version from its welcome banner."
  (save-match-data
    (when (string-match sbt:scala-welcome-regexp input)
      (setq sbt:console-scala-version (string-to-number (match-string 1 input)))))
  input)

(defun sbt:delete-all-pending-paste-files ()
  "Delete every temp paste file still waiting in this buffer."
  (dolist (file sbt:pending-paste-files)
    (ignore-errors (delete-file file)))
  (setq sbt:pending-paste-files nil))

(defun sbt:paste-output-filter (input)
  "Handle deferred pasting when a console prompt appears.

First deletes temp files whose :load the REPL has now processed,
then sends any code queued by `sbt:paste-region'.  Meant to run
after `sbt:switch-submode' in `comint-output-filter-functions'."
  (when (and (eq sbt:submode 'console)
             (save-match-data (string-match sbt:console-prompt-regexp input)))
    (sbt:delete-all-pending-paste-files)
    (when sbt:pending-paste-code
      (let ((queued (nreverse sbt:pending-paste-code)))
        (setq sbt:pending-paste-code nil)
        (dolist (code queued)
          (sbt:send-paste code)))))
  input)

(defun sbt:send-paste (code)
  "Send CODE to the console in the current sbt buffer as one unit.
Uses :paste or :load according to `sbt:paste-method' and
`sbt:console-scala-version'.  Assumes the REPL is at its prompt."
  (let ((method (cond ((not (eq sbt:paste-method 'auto)) sbt:paste-method)
                      ((eq sbt:console-scala-version 2) 'paste)
                      ((eq sbt:console-scala-version 3) 'load)
                      (t (message "sbt-mode: Scala version of REPL unknown, using :load")
                         'load))))
    (if (eq method 'paste)
        (progn
          (comint-send-string (current-buffer) ":paste\n")
          (comint-send-string (current-buffer) code)
          (comint-send-string (current-buffer) "\n")
          (comint-send-string (current-buffer) sbt:quit-paste-command))
      ;; Create the file where the sbt process runs (TRAMP aware).
      (let* ((file (make-nearby-temp-file "sbt-mode-paste-" nil ".scala"))
             ;; Full path as seen by the REPL: we do not know its cwd.
             (load-path-str (expand-file-name (file-local-name file)))
             (coding-system-for-write 'utf-8-unix))
        (write-region (concat code "\n") nil file nil 'silent)
        (push file sbt:pending-paste-files)
        (comint-send-string (current-buffer) (concat ":load " load-path-str "\n"))))))

(defun sbt:paste-region (start end &optional no-exit)
  "Send region (from START to END) to the REPL as one unit.

With a Scala 2 REPL the region is sent in :paste mode.  With a
Scala 3 REPL, which has no :paste command, the region is written
to a temporary file and loaded with :load.  See `sbt:paste-method'.

If the console is not running yet it is started, and the region
is sent once the console prompt appears.

NO-EXIT is ignored and kept for backwards compatibility."
  (ignore no-exit)
  (unless (comint-check-proc (sbt:buffer-name))
    (error "sbt is not running in buffer %s" (sbt:buffer-name)))
  (save-excursion
    (goto-char end)
    (skip-syntax-forward ">")
    (forward-comment (- (point-max)))
    (setq end (point)))
  (save-excursion
    (goto-char start)
    (forward-comment (point-max))
    (setq start (point)))
  (unless (> end start) (error "mark a region of code first"))
  (display-buffer (sbt:buffer-name))
  (let ((code (buffer-substring-no-properties start end)))
    (with-current-buffer (sbt:buffer-name)
      (cond ((eq sbt:submode 'console)
             (sbt:send-paste code))
            (t
             ;; Queue the code; `sbt:paste-output-filter' sends it when
             ;; the console prompt appears.
             (push code sbt:pending-paste-code)
             (when (eq sbt:submode 'sbt)
               (sbt-command "console")))))))

(provide 'sbt-mode-comint)
