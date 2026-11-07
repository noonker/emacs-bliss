;;; bliss.el --- Porcelain for bliss, which sends your work to other machines -*- lexical-binding: t; -*-

;; Copyright (C) 2026 noonker

;; Author: noonker
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4"))
;; Keywords: tools processes
;; License: GPL-3.0-or-later

;; This file is NOT part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x bliss lists your workloads.  From there, RET attaches, r runs a
;; command, A starts an AI agent, l shows logs, k stops, and ? shows the rest.
;;
;; Nothing here blocks Emacs: building a system pack can take minutes and an
;; unreachable host can take seconds, so every call to bliss is asynchronous,
;; except when a prompt needs the list of names to complete from.
;;
;; bliss sends the environment of the process that starts it, so a workload
;; started from a buffer gets that buffer's `process-environment' and
;; `default-directory'.  That includes envrc-style buffer-local environments.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)

(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function vterm "vterm" (&optional arg))
(declare-function term-mode "term" ())
(declare-function term-char-mode "term" ())
(defvar eat-term-name)
(defvar vterm-shell)
(defvar vterm-buffer-name)
(defvar term-term-name)

(defgroup bliss nil
  "Send the current directory, environment and shell to another machine."
  :group 'tools
  :prefix "bliss-")

(defcustom bliss-command '("bliss")
  "The command that runs bliss, as a program and its first arguments."
  :type '(repeat string))

(defcustom bliss-terminal 'auto
  "The terminal that attaching uses.
`auto' picks the first of eat, vterm and term that is installed."
  :type '(choice (const :tag "First available" auto)
                 (const eat) (const vterm) (const term)))

(defconst bliss-agents '("claude" "autolith" "hermes" "pi")
  "The agents bliss supports.")

(defface bliss-running '((t :inherit success)) "A workload that is running.")
(defface bliss-pending '((t :inherit warning)) "A workload that has not started, or is unknown.")
(defface bliss-finished '((t :inherit shadow)) "A workload that has finished.")
(defface bliss-failed '((t :inherit error)) "A workload that failed or has gone.")
(defface bliss-here '((t :inherit bold)) "The workload a bare `bliss attach' would pick here.")

;;; Running bliss

(defun bliss--clean-output (text)
  "Make terminal output TEXT readable in a plain buffer."
  (let* ((text (ansi-color-apply text))
         ;; Bracketed paste, cursor and title sequences; colors are already gone.
         (text (replace-regexp-in-string "\e\\[[?0-9;]*[@-~]\\|\e\\][^\a]*\a" "" text)))
    (replace-regexp-in-string "\r\n?" "\n" text)))

(defun bliss--parse-json (text)
  "Parse the JSON in TEXT into lists and alists."
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun bliss--json (args callback on-error)
  "Run bliss with ARGS and call CALLBACK with its parsed JSON output.
On failure, call ON-ERROR with a message."
  (let ((out (generate-new-buffer " *bliss-json*"))
        (err (generate-new-buffer " *bliss-json-err*")))
    (make-process
     :name "bliss-json" :buffer out :stderr err :noquery t :connection-type 'pipe
     :command (append bliss-command args)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (if (zerop (process-exit-status proc))
                 (funcall callback (bliss--parse-json
                                    (with-current-buffer out (buffer-string))))
               (funcall on-error (string-trim (with-current-buffer err (buffer-string)))))
           (kill-buffer out)
           (let ((stderr (get-buffer-process err)))
             (when stderr (delete-process stderr)))
           (kill-buffer err)))))))

(defun bliss--json-now (&rest args)
  "Run bliss with ARGS, wait, and return its parsed JSON output, or nil on failure."
  (with-temp-buffer
    (when (eql 0 (apply #'call-process (car bliss-command) nil '(t nil) nil
                        (append (cdr bliss-command) args)))
      (bliss--parse-json (buffer-string)))))

(defun bliss--process-buffer ()
  "The buffer that collects the output of background commands."
  (let ((buffer (get-buffer-create "*bliss-process*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode) (special-mode)))
    buffer))

(defun bliss--append (buffer text)
  "Add TEXT to the end of BUFFER, following it where point was already at the end."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (follow (eobp)))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when follow (goto-char (point-max)))))))

(defun bliss--start (args &optional on-success)
  "Run bliss with ARGS in the background from the current buffer's directory.
Output goes to the process buffer, which is shown if the command fails.
Call ON-SUCCESS when it succeeds."
  (let ((buffer (bliss--process-buffer))
        (shown (string-join args " ")))
    (bliss--append buffer (format "\n$ bliss %s\n" shown))
    (message "bliss %s..." shown)
    (make-process
     :name "bliss" :buffer buffer :noquery t :connection-type 'pipe
     :command (append bliss-command args)
     :filter
     (lambda (proc text)
       (when (string-match "started \\([^ \n]+\\) on " text)
         (process-put proc 'bliss-started (match-string 1 text)))
       (bliss--append (process-buffer proc) (bliss--clean-output text)))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (bliss--refresh-list-buffers)
         (if (zerop (process-exit-status proc))
             (let ((started (process-get proc 'bliss-started)))
               (message (if started "bliss: started %s" "bliss %s: done")
                        (or started shown))
               (when on-success (funcall on-success)))
           (display-buffer (process-buffer proc))
           (message "bliss %s: failed; see *bliss-process*" shown)))))))

(defun bliss-show-process-buffer ()
  "Show the output of background commands."
  (interactive)
  (pop-to-buffer (bliss--process-buffer)))

;;; Terminals

(defun bliss--terminal-backend ()
  "The terminal to use, resolving `auto' from what is installed."
  (if (not (eq bliss-terminal 'auto))
      bliss-terminal
    (cond ((require 'eat nil t) 'eat)
          ((require 'vterm nil t) 'vterm)
          (t 'term))))

(defun bliss--make-terminal (backend title argv)
  "Start ARGV in a BACKEND terminal buffer named after TITLE, and return it.
Remote hosts rarely have the terminfo entries of Emacs's own terminals, and
tmux refuses to start without one, so each is given a name they will know."
  (pcase backend
    ('eat
     (require 'eat)
     (let ((eat-term-name "xterm-256color"))
       (apply #'eat-make title (car argv) nil (cdr argv))))
    ('vterm
     (require 'vterm)
     (let ((vterm-shell (mapconcat #'shell-quote-argument argv " "))
           (vterm-buffer-name (format "*%s*" title)))
       (vterm vterm-buffer-name)))
    (_
     (require 'term)
     (let* ((term-term-name "ansi")
            (buffer (apply #'make-term title (car argv) nil (cdr argv))))
       (with-current-buffer buffer
         (term-mode)
         (term-char-mode))
       buffer))))

(defun bliss--terminal (title args &optional fresh)
  "Run bliss with ARGS in a terminal buffer named after TITLE and show it.
A live terminal with that name is reused, unless FRESH asks for a new one."
  (let* ((title (if fresh
                    (string-trim (generate-new-buffer-name (format "*%s*" title)) "\\*" "\\*")
                  title))
         (name (format "*%s*" title))
         (existing (get-buffer name)))
    (if (and existing (process-live-p (get-buffer-process existing)))
        (pop-to-buffer existing)
      (when existing (kill-buffer existing))
      (let ((env (append '("TMUX" "TMUX_PANE" "STY") process-environment)) ; unset, for nesting
            (argv (append bliss-command args))
            buffer)
        ;; In a buffer with no local environment, this binding is the one the
        ;; new buffer sees, even when this buffer's environment was local.
        (with-temp-buffer
          (let ((process-environment env))
            (setq buffer (bliss--make-terminal (bliss--terminal-backend) title argv))))
        (let ((proc (get-buffer-process buffer)))
          (when proc
            (add-function :after (process-sentinel proc)
                          (lambda (&rest _) (bliss--refresh-list-buffers)))))
        (pop-to-buffer buffer)))))

;;; The list

(defvar-local bliss--workloads nil
  "The rows last read from `bliss list --json'.")
(defvar-local bliss--marked nil
  "Names of the marked workloads.")
(defvar-local bliss--refresh-serial 0
  "Identifies the newest refresh, so that older answers can be ignored.")

(defun bliss--age (seconds)
  "A short description of SECONDS."
  (let ((seconds (floor (max seconds 0))))
    (cond ((>= seconds 86400) (format "%dd" (/ seconds 86400)))
          ((>= seconds 3600) (format "%dh" (/ seconds 3600)))
          ((>= seconds 60) (format "%dm" (/ seconds 60)))
          (t (format "%ds" seconds)))))

(defun bliss--state-face (state)
  "The face for a workload in STATE."
  (cond ((member state '("running" "Running")) 'bliss-running)
        ((member state '("gone" "Failed")) 'bliss-failed)
        ((string-match-p "\\`exited(0)\\'\\|\\`exited\\'\\|\\`stopped\\'\\|\\`Succeeded\\'" state)
         'bliss-finished)
        ((string-prefix-p "exited(" state) 'bliss-failed)
        (t 'bliss-pending)))

(defun bliss--entry (row now)
  "The tabulated list entry for ROW, with ages measured from NOW."
  (let-alist row
    (list .name
          (vector (if .here (propertize .name 'face 'bliss-here) .name)
                  .target
                  (propertize .state 'face (bliss--state-face .state))
                  (propertize (bliss--age (- now .started)) 'bliss-started .started)
                  (abbreviate-file-name .directory)
                  (string-join .command " ")))))

(defun bliss--sort-newest (a b)
  "Whether list entry A started after list entry B."
  (> (get-text-property 0 'bliss-started (aref (cadr a) 3))
     (get-text-property 0 'bliss-started (aref (cadr b) 3))))

(defun bliss--put-marks ()
  "Show the marks, which printing the list clears."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (member (tabulated-list-get-id) bliss--marked)
        (tabulated-list-put-tag "*"))
      (forward-line 1))))

(defun bliss--render (rows)
  "Show ROWS in the current buffer."
  (setq bliss--workloads rows
        bliss--marked (seq-filter (lambda (name)
                                    (seq-some (lambda (row) (equal (alist-get 'name row) name)) rows))
                                  bliss--marked)
        tabulated-list-entries (let ((now (float-time)))
                                 (mapcar (lambda (row) (bliss--entry row now)) rows)))
  (tabulated-list-print t)
  (bliss--put-marks))

(defun bliss-refresh ()
  "Read the workloads again, without blocking Emacs."
  (interactive)
  (let ((buffer (current-buffer))
        (serial (setq bliss--refresh-serial (1+ bliss--refresh-serial))))
    (setq mode-line-process ":refreshing")
    (force-mode-line-update)
    (cl-flet ((current-p ()
                (and (buffer-live-p buffer)
                     (= serial (buffer-local-value 'bliss--refresh-serial buffer)))))
      (bliss--json
       '("list" "--json")
       (lambda (rows)
         (when (current-p)
           (with-current-buffer buffer
             (setq mode-line-process nil)
             (force-mode-line-update)
             (bliss--render rows))))
       (lambda (problem)
         (when (current-p)
           (with-current-buffer buffer
             (setq mode-line-process ":failed")
             (force-mode-line-update))
           (message "bliss list failed: %s" problem))))
      (bliss--json
       '("targets" "--json")
       (lambda (targets)
         (when (current-p)
           (with-current-buffer buffer
             (let ((default (seq-find (lambda (target) (alist-get 'default target)) targets)))
               (setq mode-name (if default
                                   (format "Bliss[%s]" (alist-get 'name default))
                                 "Bliss")))
             (force-mode-line-update))))
       #'ignore))))

(defun bliss--refresh-list-buffers ()
  "Refresh the list, if it is open."
  (let ((buffer (get-buffer "*bliss*")))
    (when buffer
      (with-current-buffer buffer (bliss-refresh)))))

(defun bliss--current-name ()
  "The workload on this line of the list, if there is one."
  (and (derived-mode-p 'bliss-list-mode) (tabulated-list-get-id)))

(defun bliss--read-workload (prompt)
  "Read a workload name with PROMPT, unless point is already on one."
  (or (bliss--current-name)
      (let* ((rows (or (bliss--json-now "list" "--json")
                       (user-error "No workloads")))
             (here (seq-find (lambda (row) (alist-get 'here row)) rows)))
        (completing-read prompt (mapcar (lambda (row) (alist-get 'name row)) rows)
                         nil t nil nil (alist-get 'name here)))))

(defun bliss--row (name)
  "The row for the workload NAME, wherever it is known."
  (let ((rows (or (and (derived-mode-p 'bliss-list-mode) bliss--workloads)
                  (bliss--json-now "list" "--json"))))
    (seq-find (lambda (row) (equal (alist-get 'name row) name)) rows)))

;;; Acting on workloads

(defun bliss-attach (name)
  "Attach to the workload NAME in a terminal."
  (interactive (list (bliss--read-workload "Attach to: ")))
  (bliss--terminal (format "bliss: %s" name) (list "attach" name)))

(defvar-local bliss--logs-name nil "The workload this buffer shows.")
(defvar-local bliss--logs-follow nil "Whether this buffer follows its workload.")

(define-derived-mode bliss-logs-mode special-mode "Bliss-Logs"
  "Shows the output of a workload."
  (setq-local revert-buffer-function
              (lambda (&rest _) (bliss--fetch-logs (current-buffer)))))

(defun bliss--fetch-logs (buffer)
  "Fill BUFFER with its workload's output."
  (with-current-buffer buffer
    (let ((old (get-buffer-process buffer))
          (inhibit-read-only t))
      (when old (delete-process old))
      (erase-buffer))
    (make-process
     :name "bliss-logs" :buffer buffer :noquery t :connection-type 'pipe
     :command (append bliss-command
                      (list "logs" bliss--logs-name)
                      (when bliss--logs-follow '("--follow")))
     :filter (lambda (proc text)
               (bliss--append (process-buffer proc) (bliss--clean-output text)))
     :sentinel #'ignore)))

(defun bliss-logs (name &optional follow)
  "Show the output of the workload NAME.  With FOLLOW, keep it coming."
  (interactive (list (bliss--read-workload "Logs of: ") current-prefix-arg))
  (let ((buffer (get-buffer-create (format "*bliss-logs: %s*" name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'bliss-logs-mode) (bliss-logs-mode))
      (setq bliss--logs-name name
            bliss--logs-follow follow)
      (bliss--fetch-logs buffer))
    (pop-to-buffer buffer)))

(defun bliss-logs-follow (name)
  "Show the output of the workload NAME and keep it coming."
  (interactive (list (bliss--read-workload "Follow logs of: ")))
  (bliss-logs name t))

(defun bliss-pull (name destination)
  "Copy the files of the workload NAME to DESTINATION, then open them."
  (let ((destination (expand-file-name destination)))
    (bliss--start (list "pull" name "--dest" destination)
                  (lambda () (dired-other-window destination)))))

(defun bliss-pull-or-visit (name)
  "Copy the files of the workload NAME back, or just open them if it runs in place."
  (interactive (list (bliss--read-workload "Pull files of: ")))
  (let ((row (bliss--row name)))
    (if (alist-get 'in_place row)
        (progn
          (message "%s works in place, so there is nothing to copy" name)
          (dired-other-window (alist-get 'directory row)))
      (bliss-pull name (read-directory-name
                        "Pull to: " nil
                        (expand-file-name (format ".bliss/pull/%s/" name)
                                          (alist-get 'directory row)))))))

(defun bliss-stop (names)
  "Stop the workloads NAMES and remove them from their targets.
Interactively, those are the marked workloads, or the one at point."
  (interactive
   (list (or (and (derived-mode-p 'bliss-list-mode) bliss--marked)
             (list (bliss--read-workload "Stop: ")))))
  (when (yes-or-no-p (format "Stop and remove %s, including its files on the target? "
                             (string-join names ", ")))
    (dolist (name names)
      (bliss--start (list "stop" name)))
    (when (derived-mode-p 'bliss-list-mode)
      (setq bliss--marked nil))))

(defun bliss-dired (name)
  "Open the directory the workload NAME was sent from."
  (interactive (list (bliss--read-workload "Directory of: ")))
  (dired-other-window (alist-get 'directory (bliss--row name))))

(defun bliss-copy-name (name)
  "Copy the workload NAME to the kill ring."
  (interactive (list (bliss--read-workload "Copy name of: ")))
  (kill-new name)
  (message "Copied %s" name))

;;; Marks

(defun bliss-mark ()
  "Mark this workload and move down."
  (interactive)
  (let ((name (or (tabulated-list-get-id) (user-error "No workload here"))))
    (cl-pushnew name bliss--marked :test #'equal)
    (tabulated-list-put-tag "*" t)))

(defun bliss-unmark ()
  "Unmark this workload and move down."
  (interactive)
  (let ((name (or (tabulated-list-get-id) (user-error "No workload here"))))
    (setq bliss--marked (delete name bliss--marked))
    (tabulated-list-put-tag " " t)))

(defun bliss-unmark-all ()
  "Unmark every workload."
  (interactive)
  (setq bliss--marked nil)
  (tabulated-list-clear-all-tags))

;;; Starting workloads

(defun bliss--read-target (prompt &optional initial history)
  "Read a target name with PROMPT, INITIAL and HISTORY."
  (let* ((targets (bliss--json-now "targets" "--json"))
         (default (seq-find (lambda (target) (alist-get 'default target)) targets)))
    (completing-read prompt (mapcar (lambda (target) (alist-get 'name target)) targets)
                     nil t initial history (alist-get 'name default))))

(defun bliss--command-line (action args rest)
  "The arguments for bliss to do ACTION, given transient ARGS and REST.
The target is a global option, so it comes before ACTION.  REST follows `--',
so that a prompt or command beginning with a dash is not taken for an option."
  (let ((targets (seq-filter (lambda (arg) (string-prefix-p "--target=" arg)) args)))
    (append targets (list action) (seq-difference args targets)
            (when rest (cons "--" rest)))))

(defun bliss--launch (action args rest)
  "Do ACTION with transient ARGS and REST, in a terminal if it needs one.
A shell, an interactive agent and `--attach' all need one.  A plan needs only
its output, and anything else runs in the background."
  (let ((command (bliss--command-line action args rest)))
    (cond ((member "--dry-run" args)
           (bliss--start command #'bliss-show-process-buffer))
          ((or (member "--attach" args) (null rest))
           (bliss--terminal (format "bliss: %s" action) command t))
          (t (bliss--start command)))))

(defun bliss-run-command (command args)
  "Run the shell COMMAND on a target, with transient ARGS."
  (interactive (list (read-shell-command "Run on target: ")
                     (transient-args 'bliss-run-menu)))
  ;; Through bash, because every pack has it and COMMAND may use pipes and quotes.
  (bliss--launch "run" args (list "bash" "-c" command)))

(defun bliss-run-shell (args)
  "Open a shell on a target, with transient ARGS."
  (interactive (list (transient-args 'bliss-run-menu)))
  (bliss--launch "run" args nil))

(defun bliss-run-plan (args)
  "Show what a run with transient ARGS would send, sending nothing."
  (interactive (list (transient-args 'bliss-run-menu)))
  (bliss--launch "run" (cons "--dry-run" args) nil))

(defun bliss-ai-prompt (prompt args)
  "Give an AI agent on a target the task PROMPT, with transient ARGS."
  (interactive (list (read-string "Task for the agent: ")
                     (transient-args 'bliss-ai-menu)))
  (when (string-empty-p prompt) (user-error "No task given"))
  (bliss--launch "ai" args (list prompt)))

(defun bliss-ai-interactive (args)
  "Start an interactive AI agent on a target, with transient ARGS."
  (interactive (list (transient-args 'bliss-ai-menu)))
  (bliss--launch "ai" args nil))

(defun bliss-ai-plan (args)
  "Show what an agent run with transient ARGS would send, sending nothing."
  (interactive (list (transient-args 'bliss-ai-menu)))
  (bliss--launch "ai" (cons "--dry-run" args) nil))

;;; Targets

(defun bliss-set-default-target (target)
  "Make TARGET the default for new workloads."
  (interactive (list (bliss--read-target "Default target: ")))
  (bliss--start (list "--target" target)))

(defun bliss-gc (target)
  "Show what is cached on TARGET and unused, then offer to remove it."
  (interactive (list (bliss--read-target "Collect garbage on: ")))
  (bliss--start
   (list "--target" target "gc" "--dry-run")
   (lambda ()
     (bliss-show-process-buffer)
     (when (yes-or-no-p (format "Remove what is listed for %s? " target))
       (bliss--start (list "--target" target "gc"))))))

;;; Menus

(transient-define-prefix bliss-run-menu ()
  "Run a command on a target."
  ["Arguments"
   ("-t" "Target" "--target=" :reader bliss--read-target)
   ("-a" "Attach after starting" "--attach")]
  ["Run"
   ("r" "Command" bliss-run-command)
   ("s" "Shell" bliss-run-shell)
   ("n" "Plan only" bliss-run-plan)])

(transient-define-prefix bliss-ai-menu ()
  "Run an AI agent on a target."
  ["Arguments"
   ("-t" "Target" "--target=" :reader bliss--read-target)
   ("-g" "Agent" "--agent=" :choices (lambda (&rest _) bliss-agents))
   ("-u" "Act without approval prompts" "--unattended")
   ("-a" "Attach after starting" "--attach")
   ("-f" "Check for a new release" "--refresh")]
  ["Start"
   ("A" "With a task" bliss-ai-prompt)
   ("i" "Interactive" bliss-ai-interactive)
   ("n" "Plan only" bliss-ai-plan)])

;;;###autoload (autoload 'bliss-dispatch "bliss" nil t)
(transient-define-prefix bliss-dispatch ()
  "Everything bliss can do."
  [["Workload"
    ("a" "Attach" bliss-attach)
    ("l" "Logs" bliss-logs)
    ("L" "Follow logs" bliss-logs-follow)
    ("p" "Pull files back" bliss-pull-or-visit)
    ("k" "Stop" bliss-stop)
    ("D" "Open its directory" bliss-dired)
    ("w" "Copy its name" bliss-copy-name)]
   ["Start"
    ("r" "Run..." bliss-run-menu)
    ("A" "AI agent..." bliss-ai-menu)]
   ["List"
    ("b" "Open the list" bliss)
    ("g" "Refresh" bliss-refresh :if-derived bliss-list-mode)
    ("m" "Mark" bliss-mark :if-derived bliss-list-mode)
    ("u" "Unmark" bliss-unmark :if-derived bliss-list-mode)
    ("U" "Unmark all" bliss-unmark-all :if-derived bliss-list-mode)]
   ["Other"
    ("t" "Set default target" bliss-set-default-target)
    ("G" "Collect garbage" bliss-gc)
    ("$" "Process output" bliss-show-process-buffer)]])

;;; The mode

(defvar bliss-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'bliss-attach)
    (define-key map (kbd "a") #'bliss-attach)
    (define-key map (kbd "l") #'bliss-logs)
    (define-key map (kbd "L") #'bliss-logs-follow)
    (define-key map (kbd "p") #'bliss-pull-or-visit)
    (define-key map (kbd "k") #'bliss-stop)
    (define-key map (kbd "D") #'bliss-dired)
    (define-key map (kbd "w") #'bliss-copy-name)
    (define-key map (kbd "r") #'bliss-run-menu)
    (define-key map (kbd "A") #'bliss-ai-menu)
    (define-key map (kbd "m") #'bliss-mark)
    (define-key map (kbd "u") #'bliss-unmark)
    (define-key map (kbd "U") #'bliss-unmark-all)
    (define-key map (kbd "g") #'bliss-refresh)
    (define-key map (kbd "t") #'bliss-set-default-target)
    (define-key map (kbd "G") #'bliss-gc)
    (define-key map (kbd "$") #'bliss-show-process-buffer)
    (define-key map (kbd "?") #'bliss-dispatch)
    (define-key map (kbd "h") #'bliss-dispatch)
    map)
  "Keymap for `bliss-list-mode'.")

(define-derived-mode bliss-list-mode tabulated-list-mode "Bliss"
  "The workloads bliss knows about.

\\{bliss-list-mode-map}"
  (setq tabulated-list-format
        [("Name" 24 t)
         ("Target" 12 t)
         ("State" 12 t)
         ("Age" 6 bliss--sort-newest)
         ("Directory" 34 t)
         ("Command" 0 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Age" . nil)
        truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (bliss-refresh)))
  (tabulated-list-init-header)
  (hl-line-mode 1))

;;;###autoload
(defun bliss ()
  "List the workloads, from the point of view of the current buffer.
New workloads start from this buffer's directory and environment."
  (interactive)
  (let ((directory default-directory)
        (environment process-environment)
        (buffer (get-buffer-create "*bliss*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'bliss-list-mode) (bliss-list-mode))
      (setq default-directory directory)
      (setq-local process-environment environment)
      (bliss-refresh))
    (pop-to-buffer buffer)))

(provide 'bliss)
;;; bliss.el ends here
