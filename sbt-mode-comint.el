;;; sbt-mode-comint.el - Support functions for comint-mode
;;
;; Copyright(c) 2013 Heikki Vesalainen
;; For information on the License, see the LICENSE file

(require 'cl)
(require 'comint)
(require 'sbt-mode-project)
(require 'sbt-mode-buffer)

(eval-when-compile
  (defvar sbt:previous-history-file))

(defcustom sbt:sbt-history-file "target/.history"
  "The .history file written by sbt. Relative to the sbt project
root. This will be loaded as the comint-input-ring on start-up"
  :type 'string
  :group 'sbt)

(defcustom sbt:console-history-file "~/.scala_history"
  "The .scala_history file written by scala. This will be loaded
as the comint-input-ring on console start-up"
  :type 'string
  :group 'sbt)

(defcustom sbt:sbt-prompt-regexp "^>[ ]*"
  "A regular expression to match sbt REPL prompt"
  :type 'string
  :group 'sbt)

(defcustom sbt:console-prompt-regexp "^scala>[ ]*"
  "A regular expression to match scala REPL prompt"
  :type 'string
  :group 'sbt)

(defcustom sbt:paste-mode-prompt-regexp "^// Entering paste mode"
  "A regular expression to detect paste-mode"
  :type 'string
  :group 'sbt)

(defcustom sbt:prompt-regexp "^\\(\\(scala\\)?>\\|[ ]+|\\)[ ]*"
  "A regular expression to match sbt and scala console prompts"
  :type 'string
  :group 'sbt)

(defcustom sbt:ansi-support 'filter
  "See `ansi-color-for-comint-mode' in `ansi-color.el'"
  :type '(choice (const :tag "Do nothing" nil)
		 (const :tag "Filter" filter)
		 (const :tag "Translate" t))
  :group 'sbt)

(defun sbt:initialize-for-comint-mode ()
  (sbt:require-buffer)
  (when (derived-mode-p 'comint-mode)

    (setq comint-process-echoes t)
    (setq comint-scroll-to-bottom-on-output t)
    (setq comint-prompt-regexp sbt:prompt-regexp)
    (setq comint-use-prompt-regexp t)
    (setq comint-prompt-read-only t)
    (setq comint-buffer-maximum-size 4096)
    (setq comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq ansi-color-for-comint-mode sbt:ansi-support)
    (setq sbt:previous-history-file nil)
    (add-hook 'comint-output-filter-functions 'sbt:switch-submode)
;    (add-hook 'comint-input-filter-functions 'sbt:enable-paste-mode)
))

(defun sbt:switch-submode (input)
  (let ((submode
         (save-excursion 
           (save-match-data 
             (comint-goto-process-mark)
             (skip-chars-backward " \n\r\t")
             (beginning-of-line)
             (cond ((looking-at sbt:sbt-prompt-regexp) 'sbt) 
                   ((looking-at sbt:console-prompt-regexp) 'console)
                   ((looking-at sbt:paste-mode-prompt-regexp) 'paste-mode))))))
    (when submode
      (setq comint-use-prompt-regexp (not (eq submode 'paste-mode)))

      (let ((comint-input-history-ignore "^completions\\|// completions$")
            (comint-input-ring-file-name
             (cond ((eq submode 'sbt) sbt:sbt-history-file)
                   ((eq submode 'console) sbt:console-history-file))))
        (when (and comint-input-ring-file-name 
                   (not (eq comint-input-ring-file-name sbt:previous-history-file)))
          (setq sbt:previous-history-file comint-input-ring-file-name)
          (comint-read-input-ring)))))
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

(defconst sbt:completions-regex "^\\[completions\\] \\(.*\\)?$")
(defconst sbt:repl-completions-string 
  "{ val input = \"$1\"; val completer = new scala.tools.nsc.interpreter.JLineCompletion($intp).completer; val completions = completer.complete(input, input.length); val prefix = input.substring(0, completions.cursor); completions.candidates.foreach(c => println(\"[completions] \" + c))} // completions")

(defun sbt:get-sbt-completions (input)
   (sbt:require-buffer)
   (when (not (comint-check-proc (current-buffer)))
     (error "sbt is not running in buffer %s" (current-buffer)))
   (when (save-excursion 
           (comint-goto-process-mark) 
           (beginning-of-line) 
           (not (looking-at-p sbt:sbt-prompt-regexp)))
     (error "sbt is not ready (no prompt found)"))
   (when (or (null input) (string-match "^\\s *$" input))
     (setq input "\"\""))
   (setq input (concat "completions " input))
   (message "Querying sbt for completions...")
   (prog1 
       (comint-redirect-results-list input
                                     sbt:completions-regex
                                     1)
     (message nil)))

(defun sbt:get-console-completions (input)
   (sbt:require-buffer)
   (when (not (comint-check-proc (current-buffer)))
     (error "sbt is not running in buffer %s" (current-buffer)))
   (when (save-excursion 
           (comint-goto-process-mark) 
           (beginning-of-line) 
           (not (looking-at-p sbt:console-prompt-regexp)))
     (error "scala console is not ready (no prompt found)"))
   (setq input (replace-regexp-in-string "\\$1"
                                         (sbt:scala-escape-string input)
                                         sbt:repl-completions-string t t))
   (message "Querying scala console for completions...")
   (prog1 
       (comint-redirect-results-list input
                                     sbt:completions-regex
                                     1)
     (message nil)))
      
(defun sbt:completion-at-point ()
  (sbt:require-buffer)
  (let ((point (point))
        (beg (save-excursion (comint-goto-process-mark)
                             (point)))
        (end (max (point) (save-excursion (end-of-line)(skip-chars-backward " \t\n\r")(point))))
        mid)
    (goto-char beg)
    (beginning-of-line)
    (if (> beg end)
        (comint-goto-process-mark)
      (cond ((looking-at-p sbt:sbt-prompt-regexp)
             (goto-char point)
             (let ((completions (sbt:get-sbt-completions (buffer-substring beg end))))
               (completion-in-region beg end completions `(lambda (s) (> (string-width s) 0)))))
            ((looking-at-p sbt:console-prompt-regexp)
             (goto-char point)
             (save-excursion
               (goto-char end)
               (unless (or (= (point) beg) (looking-back "[.,;]" (1- (point))))
                 (backward-sexp))
               (setq mid (max beg (point)))
               (ignore-errors (backward-sexp (point-max)))
               (setq beg (max beg (point))))
             (let ((completions (sbt:get-console-completions (buffer-substring beg end))))
               (completion-in-region mid end completions `(lambda (s) (> (string-width s) 0)))))
            (t
             (goto-char point)
             "No sbt or scala prompt found before process mark")))))

(provide 'sbt-mode-comint)
