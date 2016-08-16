;;; sbt-mode-hydra.el --- Custom variables for sbt-mode

;; Copyright (C) 2016 Josef Vlach
;; License: http://www.gnu.org/licenses/gpl.html

;; Homepage: https://github.com/ensime/emacs-sbt-mode
;; Keywords: languages
;; Package-Version:  0.2
;; Package-Requires: ()

;;; Commentary:
;;
;; Workflow explanation:
;; By adding this `before-save-hook' hook to your init file:
;; (add-hook 'sbt-mode-hook (lambda ()
;;                            (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)))
;; sbt will repeat (execute) last command on a save of a last modified buffer whose name match
;; one of `allowed-files-regexp'.
;;
;;; Code:

(require 'cl-lib)
(require 'hydra "hydra" 't)
(require 'comint)
(require 'sbt-mode-project)
(require 'sbt-mode-buffer)
(require 'sbt-mode)

(defvar-local sbt-hydra:current-hydra nil)
(defvar-local sbt-hydra:test-hydra-active nil)
(defvar-local sbt-hydra:hydra-previous-command nil)
(defvar-local sbt-hydra:sbt-output-cleared "")
(defvar-local sbt-hydra:sbt-test-substring "")
(defvar-local sbt-hydra:main-methods nil)
(defvar-local sbt-hydra:projects nil)               ;; dir-local
(defvar-local sbt-hydra:command-line-arguments nil) ;; dir-local
(defvar-local sbt-hydra:system-properties nil)      ;; dir-local

;; Make `sbt-hydra:projects' safe if its value is list of strings
(put 'sbt-hydra:projects 'safe-local-variable
     (lambda (projects)
       (null (memq nil (mapcar (lambda (project) (stringp project)) projects)))))

;; Make `sbt-hydra:command-line-arguments' safe if its value is
;; list of alists where keys are project names and values are command line arguments
;; For example:
;;  (
;;    ("api" . "arg1 arg2 arg3")
;;    ("core" . "arg1 arg2 arg3")
;;  )
(put 'sbt-hydra:command-line-arguments 'safe-local-variable
     (lambda (projects-and-args)
       (sbt-hydra:is-list-of-alist projects-and-args)))

;; Make `sbt-hydra:system-properties' safe if its value is
;; list of alists where keys are project names and values are lists containing strings
;; For example:
;;  (
;;   ("api" . ("-Dconfig.resource=../application.conf" "-Xmx1G"))
;;   ("core" . ("-Dpure.conf.file=myconf.conf" "-Xmx2G"))
;;  )
(put 'sbt-hydra:system-properties 'safe-local-variable
     (lambda (projects)
       (null (memq nil (mapcar (lambda (project)
                                 (and (consp project)
                                      (stringp (car project))
                                      (sbt-hydra:is-list-of-strings (cdr project)))) projects)))))

(defcustom allowed-files-regexp '(".*.scala$" "^routes$")
  "Regexp to match files when save should run last sbt command"
  :type '(repeat string)
  :group 'sbt-hydra)

(defgroup sbt-hydra nil
  "Hydra for sbt."
  :group 'sbt-hydra
  :prefix "sbt-hydra:")

(defun sbt-hydra:is-list-of-alist (list-of-alist)
  "Check if `list-of-alist' is list containing only alists where key and value are strings"
  (null (memq nil (mapcar (lambda (alist)
                            (and (consp alist)
                                 (stringp (car alist))
                                 (stringp (cdr alist)))) list-of-alist))))

(defun sbt-hydra:is-list-of-strings (list)
  "Check if `list' is list containing only strings"
  (null (memq nil (mapcar (lambda (item)
                            (stringp item)) list))))

(defun sbt-hydra:check-modified-buffers ()
  "Check modified buffers matching `allowed-files-regexp' regexps
If there is only one modified buffer then add `sbt-hydra:run-previous-command'
to run in `after-save-hook' which will run last sbt command in sbt buffer."
  (let (buffers-to-save)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (cl-loop for allowed-file-regexp being the elements of allowed-files-regexp
                 if (and (string-match allowed-file-regexp (buffer-name))
                         (buffer-modified-p))
                 do (push (buffer-name) buffers-to-save))))
    (when (eq 1 (length buffers-to-save))
      (add-hook 'after-save-hook 'sbt-hydra:run-previous-command))))

(defun sbt-hydra:sbt-buffer ()
  (let* ((current-sbt-root (sbt:find-root))
         (root-and-buffers
          (loop for process being the elements of (process-list)
                for current-process-buffer = (process-buffer process)
                when (and
                      (bufferp current-process-buffer) ;; process must have associated buffer
                      (with-current-buffer current-process-buffer
                        (and
                         (sbt:mode-p)
                         (process-live-p process)
                         (equal current-sbt-root (sbt:find-root)))))
                collect current-process-buffer into file-buffers
                finally return file-buffers)))
    (car root-and-buffers)))

(defmacro sbt-hydra:with-sbt-buffer (body)
  `(let ((sbt-buffer (sbt-hydra:sbt-buffer)))
     (if sbt-buffer
         (with-current-buffer sbt-buffer
           ,body)
       (sbt-hydra:create-new-hydra))))

;;;###autoload
(defun sbt-hydra:hydra ()
  "Show Sbt hydra for current Sbt project. If there is no hydra defined for current
Sbt project it will create one."
  (interactive)
  (if (not (macrop 'defhydra))
      (error "sbt-mode-hydra.el: No `hydra.el' available. To use `sbt-hydra:hydra' command you need to install hydra.el."))
  (unless (sbt:find-root)
    (sbt:switch-to-active-sbt-buffer))
  (sbt-hydra:with-sbt-buffer
   (if sbt-hydra:current-hydra
       (sbt-hydra:run-current-hydra)
     (sbt-hydra:create-new-hydra))))

(defun sbt-hydra:create-new-hydra ()
  (let ((res (sbt-hydra:create-hydra)))
    (cond ((stringp res)
           (message res))
          (t (message "Please wait! Sbt projects are loading. Sbt Hydra will be ready soon.")))))

(defun sbt-hydra:send-eof-if-need ()
  (when (and (stringp sbt-hydra:hydra-previous-command)
             (not (eq (string-match "/run " sbt-hydra:hydra-previous-command) nil))
             (save-excursion
               (goto-char (point-max))
               (beginning-of-line)
               (not (eq 'comint-highlight-prompt (get-text-property (point) 'face)))))
    (comint-send-eof)))

(defun sbt-hydra:run-previous-command ()
  "Run last sbt command in active sbt buffer. If last command executed
was 'run' it will try do determine if process is still running by
inspecting last line of sbt output. If it will find shell prompt then
it will execute 'run' command immediately, otherwise it will send
'eof' to sbt before running 'run' again. (This is for Play Framework
support)."
  (remove-hook 'after-save-hook 'sbt-hydra:run-previous-command)
  (sbt:switch-to-active-sbt-buffer)
  (when (sbt:find-root)
    (sbt-hydra:send-eof-if-need)
    (sbt-hydra:run-previous-sbt-command)))

(defmacro sbt-hydra:test-hydra (hydra-name hexpr)
  `(let ((heads (eval ',hexpr))
         (name (eval ',hydra-name)))
     `(defhydra ,name ()
        "
_q_ quit _o_ back _u_ testOnly _x_ clean -- -z %`sbt-hydra:sbt-test-substring
" ,@heads)))

(defmacro sbt-hydra:main-hydra (project-switcher hydra-name heads-expr)
  `(let ((heads (eval ',heads-expr))
         (name (eval ',hydra-name))
         (switcher (eval ',project-switcher)))
     `(defhydra ,name (:color red)
        ,(format "
%s
^-^----------^-^--------------^-^-------^-^---------^-^---------^-^--------^-^------^-^----------
_q_ quit     _o_ testHydra    _p_ parse _u_ testOnly _a_ repeat _n_ no-op  _i_ edit _s_ sbt-shell
_c_ compile  _y_ test:compile _t_ test  _r_ run      _l_ clean  _d_ reload _e_ eof  _h_ help
" switcher)
        ,@heads)))

(defun sbt-hydra:add-command-key (key command)
  (push key command))

(defun sbt-hydra-command:clean (project)
  `((sbt-hydra:run-project-command "clean" ,project) nil))

(defun sbt-hydra-command:test-test (project)
  `((sbt-hydra:run-project-command "test:test" ,project) nil))

(defun sbt-hydra-command:test-compile (project)
  `((sbt-hydra:run-project-command "test:compile" ,project) nil))

(defun sbt-hydra-command:compile (project)
  `((sbt-hydra:run-project-command "compile" ,project) nil))

;; run sbt command which is not expensive to run repeatedly
(defun sbt-hydra-command:no-op (project)
  `((sbt-hydra:run-project-command "name" ,project) nil))

(defun sbt-hydra-command:switch-to-sbt-buffer ()
  `((sbt:switch-to-active-sbt-buffer) nil))

(defun sbt-hydra-command:reload ()
  `((sbt-hydra:run-sbt-command "reload") nil))

(defun sbt-hydra-command:run-previous-sbt-command ()
  `((sbt-hydra:run-previous-sbt-command) nil))

(defun sbt-hydra-command:run-test-only ()
  `((sbt-hydra:run-test-only) nil))

(defun sbt-hydra-command:edit-last-command ()
  `((sbt-hydra:edit-and-run-previous-sbt-command) nil))

(defun sbt-hydra-command:run (project)
  `((sbt-hydra:run ,project) nil))

(defun sbt-hydra-command:eof ()
  `((sbt-hydra:eof) nil))

(defun sbt-hydra-command:parse-failed-test ()
  `((sbt-hydra:parse-failing-test) nil))

(defun sbt-hydra-command:test-only-hydra ()
  `((sbt-hydra:test-only-hydra-on) nil :exit t))

(defun sbt-hydra-command:quit ()
  `(nil nil))

(defun sbt-hydra-command:help ()
  `((sbt-hydra:help) nil))

(defun sbt-hydra:help ()
  (let ((name "*SBT-Hydra Help*"))
    (if (get-buffer name)
        (display-buffer (get-buffer name) #'display-buffer-pop-up-window)
      (let ((help (generate-new-buffer name)))
        (with-current-buffer help
          (insert
           "*** Sbt Hydra ***

Sbt Hydra offers convenient way for quick control of Sbt. It offers most used commands to be executed by single key.

Sbt Hydra consist of two parts. At the top is Projects section which contains list of projects from an sbt build.
At any given time one project is active. It is the one which name is in CAPITAL letters. Project can be swithed
by pressing appropriate key.

Most action provided are scoped by active project. For example key 'c' will trigger compilation of active project.

*** Project Hydra ***

Available actions:

A, B, C ...      - switch between project hydras, this part is generated dynamically on start by executing 'projects' command
                   in *sbt* buffer or it is populated by projects defined in `sbt-hydra:projects' Directory Local Variables

q - quit         - close hydra
c - compile      - execute 'compile' command for active project
y - test:compile - execute 'test:compile' command for active project
t - test         - execute 'test' command for active project
r - run          - execute 'run' command for active project
l - clean        - execute 'clean' command for active project
d - reload       - execute 'reload' command
e - eof          - send end-of-file character to the sbt prompt, useful for example for terminating running play application
i - edit         - edit last executed command and execute edited command
a - repeat       - repeat last executed command again
n - no-op        - execute 'name' command. This is convenient way how to prevent last command execute again. For example when
                   you don't want to execute test command again since it takes long time to finish
p - parse        - parse output of Sbt buffer for failed test. It must be run after 'test' command finish execution and there
                   are failed tests. In this case at the end of output is list of classes containing some failed tests.
                   This action will parse this classes names and it will create so called 'testHydra' which allow
                   run these classes only one by one.
o - testHydra    - switch to special hydra which is created by 'parse' action. More info in section 'Test Hydra'
u - testOnly     - run 'testOnly' command for active project with substring parameter (-- -z) containing text from the line point
                   is at. Works in Sbt buffer or Scala source file, but point must be at line contaning should text of the test.

*** Test Hydra ***

Available actions:

q - quit         - close hydra
o - back         - switch back to Sbt hydra
u - testOnly     - set substring (-- -z) for running one of defined test. By default use value from line point is at if this line
                   contains should text of the test
x - clean        - reset substring (-- -z) to empty string

[a] [b] [c] ...  - it run 'test:testOnly' for failed test optionally limited to (-- -z) substring. This section is generated
                   dynamically by the 'parse' action from project hydra.
")
          (goto-char (point-min))
          (read-only-mode t)
          (display-buffer help #'display-buffer-pop-up-window))))))

(defun sbt-hydra:run (project)
  (let ((main-class (cdr (assq (intern project) sbt-hydra:main-methods))))
    (cond ((eq nil main-class)
           (let ((cmd (format "show %s/mainClass" project)))
             (sbt:switch-to-active-sbt-buffer)
             (sbt-hydra:send-eof-if-need)
             (add-hook 'comint-output-filter-functions 'sbt-hydra:parse-main-class)
             (setq sbt-hydra:hydra-previous-command cmd)
             (sbt:command cmd)))
          ((equal "None" main-class)
           (message "No main class for project %s" project))
          ((equal "" main-class)
           (message "Error when getting main class for project %s. Please try again." project)
           (setq sbt-hydra:main-methods (assq-delete-all (intern project) sbt-hydra:main-methods)))
          (t
           (sbt-hydra:run-run-project-command
            (format "run %s"
                    (concat "" (cdr (assoc project sbt-hydra:command-line-arguments)))) project)))))

(defun sbt-hydra:run-sbt-command (command)
  (sbt:switch-to-active-sbt-buffer)
  (sbt:command command))

(defun sbt-hydra:run-previous-sbt-command ()
  (sbt:switch-to-active-sbt-buffer)
  (sbt:command (sbt:get-previous-command)))

(defun sbt-hydra:edit-and-run-previous-sbt-command ()
  (sbt:switch-to-active-sbt-buffer)
  (sbt:command (read-from-minibuffer "Edit sbt command: " (sbt:get-previous-command))))

(defun sbt-hydra:should-text-from-sbt-output ()
  (let ((current-line (thing-at-point 'line)))
    (when (string-match "^\\[info\\] - \\([[:word:]\\| ]+\\)[ \n]" current-line)
      (match-string-no-properties 1 current-line))))

(defun sbt-hydra:project-name-from-current-hydra ()
  (let ((hydra-name (format "%s" sbt-hydra:current-hydra)))
    (substring hydra-name 0 (- (length hydra-name) (length "/body")))))

(defun sbt-hydra:run-test-with-substring (substring)
  (sbt-hydra:run-sbt-command
   (format "%s/testOnly -- -z \"%s\""
           (sbt-hydra:project-name-from-current-hydra) substring)))

(defun sbt-hydra:run-test-only ()
  (cond ((use-region-p)
         (sbt-hydra:run-test-with-substring (buffer-substring-no-properties (region-beginning) (region-end))))
        ((string-prefix-p sbt:buffer-name-base (buffer-name))
         (let ((should-text (sbt-hydra:should-text-from-sbt-output)))
           (if should-text
               (sbt-hydra:run-test-with-substring should-text)
             (message "Current line doesn't contain name of a test. It must start with a string \"[info] - \""))))
        (t
         (save-excursion
           (let ((project (let* ((root (sbt:find-root))
                                 (buffer-name (buffer-file-name (current-buffer))) ;; not every buffer represents a file
                                 (path-from-root (when (and root buffer-name) (replace-regexp-in-string (concat ".*" (substring root 1)) "" buffer-name))))
                            (when (and path-from-root (string-match "^\\([[:word:]]*\\)/" path-from-root))
                              (match-string-no-properties 1 path-from-root)))) ;; TODO this will work only for Multi-project .sbt build definition
                 (substring (let ((line (thing-at-point 'line)))
                              (when (string-match "\"\\(.*\\)\" \\(in\\|should\\)" line)
                                (replace-regexp-in-string "\"" "" (match-string-no-properties 1 line)))))
                 (fqn (progn
                        (goto-char (point-min))
                        (when (search-forward-regexp "^package \\(.*\\)$" nil t)
                          (match-string-no-properties 1))))
                 (file-name (progn
                              ;; find first class name following by keyword `extends'
                              ;; we will assume this is a class for running the tests
                              (when (search-forward-regexp "class \\([[:word:]]+\\)\\( \\|\n\\)*extends" nil t)
                                (match-string-no-properties 1)))))
             (cond ((eq file-name nil)
                    (message "Current buffer is not *.scala file with tests."))
                   ((eq fqn nil)
                    (message (format "No package detected in %s.scala." file-name)))
                   (t
                    (sbt-hydra:run-sbt-command
                     (format "%s/testOnly %s.%s%s"
                             project fqn file-name (concat "" (format " -- -z \"%s\"" substring)))))))))))

(defun sbt-hydra:eof ()
  (sbt:switch-to-active-sbt-buffer)
  (setq sbt-hydra:hydra-previous-command "name") ;; allow eof and run command after each other
  (comint-send-eof))

(defun sbt-hydra:test-only-hydra-on ()
  (if (functionp 'sbt-test-hydra/body)
      (sbt-hydra:with-sbt-buffer
       (progn
         (setq sbt-hydra:test-hydra-active t)
         (sbt-test-hydra/body)))
    (sbt-hydra:no-test-hydra)
    ;; let's run current hydra again
    (sbt-hydra:run-current-hydra)))

(defun sbt-hydra:test-only-hydra-off ()
  (sbt-hydra:with-sbt-buffer
   (progn (setq sbt-hydra:test-hydra-active nil)
          (sbt-hydra:run-current-hydra))))

(defun sbt-hydra:run-current-hydra ()
  (sbt-hydra:with-sbt-buffer
   (if sbt-hydra:test-hydra-active
       (if (functionp 'sbt-test-hydra/body)
           (sbt-test-hydra/body)
     	(sbt-hydra:no-test-hydra))
     (funcall sbt-hydra:current-hydra))))

(defun sbt-hydra:no-test-hydra ()
  (message "No hydra defined for testOnly command"))

(defun sbt-test-hydra-command:switch-to-sbt-hydra ()
  `((sbt-hydra:test-only-hydra-off) nil :exit t))

(defun sbt-test-hydra-command:set-substring ()
  `((sbt-test-hydra:set-substring) nil))

(defun sbt-test-hydra-command:clear-substring ()
  `((sbt-test-hydra:clear-substring) nil))

(defun sbt-hydra:switch-hydra (project)
  "Switch to project hydra and remember what hydra is current one."
  (let ((project-hydra (intern (format "%s/body" project))))
    `((sbt-hydra:with-sbt-buffer
       (progn (setq sbt-hydra:current-hydra ',project-hydra)
              (,project-hydra))) nil :exit t :cmd-name ,(format "%s" project))))

(defun sbt-hydra:keys-and-projects (current-project key project-name)
  (format "_%s_ %s" key (if (equal current-project project-name)
                            (upcase project-name)
                          project-name)))

(defun sbt-hydra:project-switcher (current-project project-names)
  (let ((keys (mapcar 'char-to-string (number-sequence 65 (+ 65 (length project-names)))))
        (current-project-array (make-vector (length project-names) current-project)))
    (mapconcat 'identity (cl-mapcar 'sbt-hydra:keys-and-projects current-project-array keys project-names) " ")))

(defun sbt-hydra:generate-hydra (current-project projects)
  (let* (
         (sbt-commands (append
                        (list (sbt-hydra-command:clean current-project))
                        (list (sbt-hydra-command:compile current-project))
                        (list (sbt-hydra-command:test-test current-project))
                        (list (sbt-hydra-command:run current-project))
                        (list (sbt-hydra-command:test-compile current-project))
                        (list (sbt-hydra-command:eof))
                        (list (sbt-hydra-command:quit))
                        (list (sbt-hydra-command:no-op current-project))
                        (list (sbt-hydra-command:parse-failed-test))
                        (list (sbt-hydra-command:test-only-hydra))
                        (list (sbt-hydra-command:switch-to-sbt-buffer))
                        (list (sbt-hydra-command:reload))
                        (list (sbt-hydra-command:run-previous-sbt-command))
                        (list (sbt-hydra-command:run-test-only))
                        (list (sbt-hydra-command:edit-last-command))
                        (list (sbt-hydra-command:help))))
         (keys '("l" "c" "t" "r" "y" "e" "q" "n" "p" "o" "s" "d" "a" "u" "i" "h"))
         (sbt-commands (cl-mapcar 'sbt-hydra:add-command-key keys sbt-commands))
         (project-hydras (mapcar 'sbt-hydra:switch-hydra projects))
         (project-keys (mapcar 'char-to-string (number-sequence 65 (+ 65 (length project-hydras)))))
         (project-hydras-with-keys (cl-mapcar 'sbt-hydra:add-command-key project-keys project-hydras))
         (hydra-commands (append sbt-commands project-hydras-with-keys)))
    (sbt-hydra:main-hydra (sbt-hydra:project-switcher current-project projects) (make-symbol current-project) hydra-commands)))

(defun sbt-hydra:generate-hydras (project-names)
  (setq sbt-hydra:current-hydra (intern (format "%s/body" (car project-names)))) ;; set first hydra as current
  (let* ((projects-names-list (make-vector (length project-names) project-names))
         (hydras (cl-mapcar 'sbt-hydra:generate-hydra project-names projects-names-list)))
    (mapcar 'eval hydras)))

(defun sbt-hydra:get-system-properties (project)
  (let ((system-properties (cdr (assoc project sbt-hydra:system-properties))))
    (when system-properties (format "set fork in %s := true\nset javaOptions in (%s, run) := Seq(%s)" project project
                                    (mapconcat (lambda (system-property)
                                                 (format "\"%s\"" system-property)) system-properties ",")))))

(defun sbt-hydra:run-run-project-command (command project)
  (sbt:switch-to-active-sbt-buffer)
  (sbt-hydra:send-eof-if-need)
  (let ((cmd (format "%s/%s" project command)))
    (setq sbt-hydra:hydra-previous-command cmd)
    (let ((system-properties (sbt-hydra:get-system-properties project)))
      (when system-properties
        (sbt:command system-properties)))
    (sbt:command cmd)))

(defun sbt-hydra:run-project-command (command project)
  (sbt:switch-to-active-sbt-buffer)
  (sbt-hydra:send-eof-if-need)
  (let ((cmd (format "%s/%s" project command)))
    (setq sbt-hydra:hydra-previous-command cmd)
    (sbt:command cmd)))

(defun sbt-hydra:get-text-at-point ()
  (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
        ((string-prefix-p sbt:buffer-name-base (buffer-name))
         (let ((should-text (sbt-hydra:should-text-from-sbt-output)))
           (concat should-text "")))
        ((string-match ".scala$" (buffer-name (current-buffer)))
         (let* ((current-line (thing-at-point 'line))
                (should-text (when (string-match "\"\\(.*\\)\" \\(in\\|should\\)" current-line)
                               (replace-regexp-in-string "\"" "" (match-string-no-properties 1 current-line)))))
           (concat should-text "")))
        (t "")))

(defun sbt-test-hydra:set-substring ()
  (let ((search-term (read-from-minibuffer
                      "Run test containing substring: "
                      (sbt-hydra:get-text-at-point))))
    (message "sbt-hydra:sbt-test-substring set to %s" search-term)
    (setq sbt-hydra:sbt-test-substring search-term)))

(defun sbt-test-hydra:clear-substring ()
  (message "sbt-hydra:sbt-test-substring reset to empty string")
  (setq sbt-hydra:sbt-test-substring ""))

(defun sbt-test-hydra:test-only (command project)
  (let ((command-with-params (if (string= "" sbt-hydra:sbt-test-substring) command
                               (format "%s -- -z \"%s\"" command sbt-hydra:sbt-test-substring))))
    (sbt-hydra:run-project-command command-with-params project)))

(defconst sbt-project-regexp "^\\[info\\]\\W\\{5\\}\\(.*\\)$")
(defconst sbt-main-class-regexp "^\\[info\\][[:space:]]+\\(Some([[:word:]\\|\\.]*)\\|None\\)$")

(defun sbt-hydra:create-hydra ()
  "Create hydras for current scala project. It will create one hydra for every sbt project.
List of sbt projects is determined in two ways:
 - automatically by running `projects' command in sbt shell which will provide list all available projects.
 - by specifying projects in a `dir-locals-file'. This will provide option to specify only subset of all projects available in sbt build.
The easiest way to use second option is by running `add-dir-local-variable' command while in sbt buffer:
   M-x add-dir-local-variable RET sbt-mode RET sbt-hydra:projects RET (\"proj1\" \"proj2\")"
  (if (sbt:find-root)
      (progn
        (setq sbt-hydra:sbt-output-cleared "")
        (let ((buffer-res (sbt:switch-to-active-sbt-buffer)))
          (if (or (bufferp buffer-res)
                  (equal buffer-res "Already in sbt buffer!"))
              (progn
                ;; Existing sbt buffer
                (hack-dir-local-variables-non-file-buffer)
                (if sbt-hydra:projects
                    (sbt-hydra:generate-hydras-from-projects sbt-hydra:projects)
                  (add-hook 'comint-output-filter-functions 'sbt-hydra:parse-projects)
                  (sbt:command "projects")))
            (let ((sbt:clear-buffer-before-command nil))
              ;; New sbt buffer
              (sbt:run-sbt)
              (sbt:switch-to-active-sbt-buffer)
              (hack-dir-local-variables-non-file-buffer)
              (if sbt-hydra:projects
                  (sbt-hydra:generate-hydras-from-projects sbt-hydra:projects)
                (sbt:command "projects")
                (add-hook 'comint-output-filter-functions 'sbt-hydra:parse-projects-skip-init))))))
    "Not in sbt project. Hydra can be generated only from sbt project. See `sbt:find-root' to get more informations."))

(defun sbt-hydra:parse-main-class (sbt-output)
  (sbt-hydra:parse-sbt-output sbt-output 'sbt-hydra:get-main-class 'sbt-hydra:parse-main-class))

(defun sbt-hydra:parse-projects (sbt-output)
  (sbt-hydra:parse-sbt-output sbt-output 'sbt-hydra:get-projects 'sbt-hydra:parse-projects))

(defun sbt-hydra:parse-projects-skip-init (sbt-output)
  (sbt-hydra:parse-sbt-output-skip-init sbt-output))

(defun sbt-hydra:parse-sbt-output-skip-init (sbt-output)
  (let* ((output-cleared (replace-regexp-in-string ansi-color-regexp "" sbt-output)))
    (when (string-match sbt:prompt-regexp output-cleared)
      (remove-hook 'comint-output-filter-functions 'sbt-hydra:parse-projects-skip-init)
      (add-hook 'comint-output-filter-functions 'sbt-hydra:parse-projects))))

(defun sbt-hydra:parse-sbt-output (sbt-output f hook)
  (let* ((output-cleared (replace-regexp-in-string ansi-color-regexp "" sbt-output)))
    (setq sbt-hydra:sbt-output-cleared (concat sbt-hydra:sbt-output-cleared output-cleared))
    ;; match only if sbt prompt is very last thing in output-cleared
    (when (eq (length output-cleared)
              (when (string-match sbt:prompt-regexp output-cleared)
                (match-end 0)))
      (remove-hook 'comint-output-filter-functions hook)
      (funcall f sbt-hydra:sbt-output-cleared)
      (setq sbt-hydra:sbt-output-cleared ""))))

(defun sbt-hydra:find-main-class-matches (sbt-output)
  (let ((start-index 0)
        matches)
    (while (not (eq nil (let ((index (string-match sbt-main-class-regexp sbt-output start-index))
                              (result (match-string 1 sbt-output)))
                          (when index
                            (setq start-index (1+ index))
                            (push result matches))
                          index))))
    matches))

(defun sbt-hydra:get-main-class-for-project (sbt-output project)
  (let ((matches (sbt-hydra:find-main-class-matches sbt-output)))
    (pcase (length matches)
      (0 nil)
      (1 (car matches))
      (_ (let ((start-index (string-match (format "%s/compile:mainClass" project) sbt-output)))
           (when start-index
             (progn (string-match sbt-main-class-regexp sbt-output start-index)
                    (match-string 1 sbt-output))))))))

(defun sbt-hydra:get-main-class (sbt-output)
  (let* ((project (sbt-hydra:project-name-from-current-hydra))
         (main-class (sbt-hydra:get-main-class-for-project sbt-output project)))
    (add-to-list 'sbt-hydra:main-methods `(,(intern project) . ,(concat "" main-class)))
    (sbt-hydra:run project)))

(defun sbt-hydra:generate-hydras-from-projects (projects)
  (sbt-hydra:generate-hydras projects)
  (sbt-hydra:run-current-hydra)
  (message "Success hydra for projects %s created." projects))

(defun sbt-hydra:get-projects (sbt-output)
  (let* ((output-lines (split-string sbt-output "[\n]+" t))
         (projects-names
          (remove nil
                  (mapcar (lambda (output-line)
                            (when (string-match sbt-project-regexp output-line)
                              (match-string 1 output-line)))
                          output-lines))))
    (sbt-hydra:generate-hydras-from-projects projects-names)))

(defun sbt-test-hydra-command:test-only (project failing-test)
  `((sbt-test-hydra:test-only ,(format "test:testOnly %s" failing-test), project) ,(format "%s - %s" project failing-test)))

(defun sbt-test-hydra:generate-hydra (project failing-tests)
  (let* ((sbt-commands (append
                        (cl-mapcar 'sbt-test-hydra-command:test-only (make-vector (length failing-tests) project) failing-tests)
                        (list (sbt-hydra-command:quit))
                        (list (sbt-test-hydra-command:switch-to-sbt-hydra))
                        (list (sbt-test-hydra-command:set-substring))
                        (list (sbt-test-hydra-command:clear-substring))))
         (special-keys '("q" "o" "u" "x"))
         ;; Generate `command-keys' from character sequence a, b, c... with `special-keys' characters at the end.
         (all-commands-keys (mapcar 'char-to-string (number-sequence 97 (+ 96 (length sbt-commands)))))
         (keys (cl-remove-if-not (lambda (character) (not (member character special-keys))) all-commands-keys))
         (number-of-removed-keys (- (length all-commands-keys) (length keys)))
         (command-keys (append (nbutlast keys (- (length special-keys) number-of-removed-keys)) special-keys)))
    (cl-mapcar 'sbt-hydra:add-command-key command-keys sbt-commands)))

(defun sbt-hydra:parse-failing-test ()
  "Parse sbt buffer output and look for failed tests"
  (sbt:switch-to-active-sbt-buffer)
  (save-excursion
    (goto-char (point-max))
    (search-backward "Failed tests:")
    (forward-line)
    (let* ((start (point))
           (end (progn
                  (search-forward "sbt.TestsFailedException")
                  (forward-line 0)
                  (point)))
           (last-line (thing-at-point 'line))
           (output (buffer-substring-no-properties start end))
           (output-lines (split-string output "[\n]+" t))
           (failing-tests (mapcar (lambda (output-line)
                                    (progn
                                      (string-match "\\([[:alnum:]|[.]*\\)$" output-line)
                                      (match-string 1 output-line))) output-lines))
           (project-name (progn
                           (string-match "(\\([[:alnum:]].*\\)/" last-line)
                           (match-string 1 last-line)))
           (hydra (sbt-hydra:test-hydra 'sbt-test-hydra (sbt-test-hydra:generate-hydra project-name failing-tests))))
      (message "Test hydra successsfully created for %s failed tests." (length failing-tests))
      (eval hydra))))

(provide 'sbt-mode-hydra)

;;; sbt-mode-hydra.el ends here
