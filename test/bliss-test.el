;;; bliss-test.el --- Tests for bliss.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs against a fake bliss: a script that records its arguments and answers
;; from fixtures, so nothing touches a real target.

;;; Code:

(require 'ert)
(require 'bliss)

(defconst bliss-test--rows
  "[{\"name\":\"sunny-otter\",\"target\":\"pod\",\"state\":\"running\",\"started\":1000,\"directory\":\"/tmp/a\",\"command\":[\"make\",\"test\"],\"here\":true},
    {\"name\":\"calm-robin\",\"target\":\"box\",\"state\":\"exited(1)\",\"started\":2000,\"directory\":\"/tmp/b\",\"command\":[],\"here\":false}]")

(defvar bliss-test--dir nil)

(defun bliss-test--calls ()
  "The argument lists the fake bliss has received, one string per call."
  (let ((file (expand-file-name "calls" bliss-test--dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t)))))

(defun bliss-test--wait (predicate)
  "Spin until PREDICATE holds, failing after a few seconds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defun bliss-test--idle ()
  "Wait for every bliss process to finish."
  (bliss-test--wait
   (lambda () (not (seq-some (lambda (p) (string-prefix-p "bliss" (process-name p)))
                             (process-list))))))

(defconst bliss-test--shebang
  (concat "#!" (or (executable-find "sh") "/bin/sh") "\n")
  "First line of the fake scripts.  A Guix build has no /bin/sh.")

(defmacro bliss-test--with-fake (&rest body)
  "Run BODY with `bliss-command' pointing at a fake."
  (declare (indent 0))
  `(let* ((bliss-test--dir (make-temp-file "bliss-test" t))
          (fake (expand-file-name "bliss" bliss-test--dir))
          (bliss-command (list fake)))
     (with-temp-file (expand-file-name "rows" bliss-test--dir)
       (insert bliss-test--rows))
     (with-temp-file fake
       (insert bliss-test--shebang ""
               "dir=" bliss-test--dir "\n"
               "printf '%s\\n' \"$*\" >> \"$dir/calls\"\n"
               "case \"$*\" in\n"
               "  'list --json') cat \"$dir/rows\" ;;\n"
               "  'targets --json') echo '[{\"name\":\"pod\",\"type\":\"podman\",\"default\":true}]' ;;\n"
               "  logs*) printf '\\033[?2004h\\033[32mgreen\\033[0m\\r\\nline\\n' ;;\n"
               "  *fail*) echo 'it broke' >&2; exit 3 ;;\n"
               "  *) echo 'bliss: started merry-yak on pod' ;;\n"
               "esac\n"))
     (set-file-modes fake #o755)
     (unwind-protect (progn ,@body)
       (bliss-test--idle)
       (dolist (name '("*bliss*" "*bliss-process*" "*bliss-logs: sunny-otter*"))
         (when (get-buffer name) (kill-buffer name)))
       (delete-directory bliss-test--dir t))))

(defun bliss-test--open ()
  "Open the list and wait for its rows."
  (bliss)
  (set-buffer "*bliss*")
  (bliss-test--wait (lambda () bliss--workloads))
  (bliss-test--idle))

(defun bliss-test--goto (name)
  "Move to the row for NAME."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal (tabulated-list-get-id) name)))
    (forward-line 1))
  (should (equal (tabulated-list-get-id) name)))

;;; Output and formats

(ert-deftest bliss-test-clean-output ()
  (should (equal (bliss--clean-output "\e[?2004h\e[32mok\e[0m\r\n\e]0;title\anext\rbar")
                 "ok\nnext\nbar")))

(ert-deftest bliss-test-age ()
  (should (equal (mapcar #'bliss--age '(5 90 7200 172800 -3)) '("5s" "1m" "2h" "2d" "0s"))))

(ert-deftest bliss-test-state-faces ()
  (should (eq (bliss--state-face "running") 'bliss-running))
  (should (eq (bliss--state-face "Running") 'bliss-running))
  (should (eq (bliss--state-face "exited(0)") 'bliss-finished))
  (should (eq (bliss--state-face "exited") 'bliss-finished))
  (should (eq (bliss--state-face "exited(1)") 'bliss-failed))
  (should (eq (bliss--state-face "gone") 'bliss-failed))
  (should (eq (bliss--state-face "unknown") 'bliss-pending)))

;;; Command lines

(ert-deftest bliss-test-target-goes-before-the-action ()
  (should (equal (bliss--command-line "ai" '("--unattended" "--target=box") '("do it"))
                 '("--target=box" "ai" "--unattended" "--" "do it"))))

(ert-deftest bliss-test-prompt-starting-with-a-dash-is-not-an-option ()
  (should (equal (bliss--command-line "ai" nil '("--help me"))
                 '("ai" "--" "--help me"))))

(ert-deftest bliss-test-launch-picks-the-right-place ()
  (let (terminal started)
    (cl-letf (((symbol-function 'bliss--terminal)
               (lambda (_title args &optional _fresh) (setq terminal args)))
              ((symbol-function 'bliss--start)
               (lambda (args &optional _done) (setq started args))))
      (bliss--launch "run" nil nil)
      (should (equal terminal '("run")))
      (should-not started)
      (setq terminal nil)
      (bliss--launch "ai" '("--attach") '("task"))
      (should (equal terminal '("ai" "--attach" "--" "task")))
      (setq terminal nil)
      (bliss--launch "ai" nil '("task"))
      (should-not terminal)
      (should (equal started '("ai" "--" "task")))
      (setq started nil)
      ;; A plan never needs a terminal, even with no command.
      (bliss--launch "run" '("--dry-run") nil)
      (should-not terminal)
      (should (equal started '("run" "--dry-run"))))))

(ert-deftest bliss-test-command-with-shell-syntax-stays-one-argument ()
  (let (args)
    (cl-letf (((symbol-function 'bliss--launch)
               (lambda (_action _args rest) (setq args rest))))
      (bliss-run-command "make | tee 'a b'" nil)
      (should (equal args '("bash" "-c" "make | tee 'a b'"))))))

;;; The list

(ert-deftest bliss-test-list-shows-workloads ()
  (bliss-test--with-fake
    (bliss-test--open)
    (let ((text (buffer-string)))
      (should (string-match-p "sunny-otter +pod +running" text))
      (should (string-match-p "calm-robin +box +exited(1)" text)))
    (should (equal mode-name "Bliss[pod]"))))

(ert-deftest bliss-test-list-is-newest-first ()
  (bliss-test--with-fake
    (bliss-test--open)
    (goto-char (point-min))
    (should (equal (tabulated-list-get-id) "calm-robin"))))

(ert-deftest bliss-test-empty-list ()
  (bliss-test--with-fake
    (with-temp-file (expand-file-name "rows" bliss-test--dir) (insert "[]"))
    (bliss)
    (set-buffer "*bliss*")
    (bliss-test--idle)
    (should (null tabulated-list-entries))
    (should (null mode-line-process))))

(ert-deftest bliss-test-failed-refresh-keeps-the-rows ()
  (bliss-test--with-fake
    (bliss-test--open)
    (with-temp-file (expand-file-name "bliss" bliss-test--dir)
      (insert bliss-test--shebang "echo 'state is broken' >&2\nexit 1\n"))
    (bliss-refresh)
    (bliss-test--idle)
    (should (= (length tabulated-list-entries) 2))
    (should (equal mode-line-process ":failed"))))

(ert-deftest bliss-test-marks-survive-refresh ()
  (bliss-test--with-fake
    (bliss-test--open)
    (bliss-test--goto "sunny-otter")
    (bliss-mark)
    (should (equal bliss--marked '("sunny-otter")))
    (bliss-refresh)
    (bliss-test--idle)
    (bliss-test--goto "sunny-otter")
    (should (eq (char-after (line-beginning-position)) ?*))))

(ert-deftest bliss-test-marks-of-vanished-workloads-are-dropped ()
  (bliss-test--with-fake
    (bliss-test--open)
    (setq bliss--marked '("sunny-otter" "gone-away"))
    (bliss-refresh)
    (bliss-test--idle)
    (should (equal bliss--marked '("sunny-otter")))))

;;; Actions

(ert-deftest bliss-test-stop-asks-first ()
  (bliss-test--with-fake
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (bliss-stop '("sunny-otter")))
    (bliss-test--idle)
    (should-not (member "stop sunny-otter" (bliss-test--calls)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (bliss-stop '("sunny-otter")))
    (bliss-test--idle)
    (should (member "stop sunny-otter" (bliss-test--calls)))))

(ert-deftest bliss-test-stop-takes-the-marked-workloads ()
  (bliss-test--with-fake
    (bliss-test--open)
    (goto-char (point-min))
    (bliss-mark)
    (bliss-mark)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (call-interactively #'bliss-stop))
    (bliss-test--idle)
    (should (member "stop sunny-otter" (bliss-test--calls)))
    (should (member "stop calm-robin" (bliss-test--calls)))
    (should (null bliss--marked))))

(ert-deftest bliss-test-failure-shows-the-output ()
  (bliss-test--with-fake
    (let ((shown nil))
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) (setq shown t))))
        (bliss--start '("fail"))
        (bliss-test--idle))
      (should shown)
      (with-current-buffer "*bliss-process*"
        (should (string-match-p "it broke" (buffer-string)))))))

(ert-deftest bliss-test-success-runs-the-callback-and-refreshes ()
  (bliss-test--with-fake
    (bliss-test--open)
    (let ((before (length (bliss-test--calls))) done)
      (bliss--start '("run" "x") (lambda () (setq done t)))
      (bliss-test--idle)
      (should done)
      (should (> (length (bliss-test--calls)) (1+ before))))))

(ert-deftest bliss-test-gc-previews-then-asks ()
  (bliss-test--with-fake
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (bliss-gc "pod")
      (bliss-test--idle))
    (should (member "--target pod gc --dry-run" (bliss-test--calls)))
    (should-not (member "--target pod gc" (bliss-test--calls)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (bliss-gc "pod")
      (bliss-test--idle))
    (should (member "--target pod gc" (bliss-test--calls)))))

(ert-deftest bliss-test-logs-are-readable ()
  (bliss-test--with-fake
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (bliss-logs "sunny-otter"))
    (bliss-test--idle)
    (with-current-buffer "*bliss-logs: sunny-otter*"
      (should (equal (buffer-string) "green\nline\n")))))

(ert-deftest bliss-test-follow-asks-for-it ()
  (bliss-test--with-fake
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (bliss-logs-follow "sunny-otter"))
    (bliss-test--idle)
    (should (member "logs sunny-otter --follow" (bliss-test--calls)))))

(ert-deftest bliss-test-pull-of-an-in-place-workload-just-opens-it ()
  (bliss-test--with-fake
    (with-temp-file (expand-file-name "rows" bliss-test--dir)
      (insert "[{\"name\":\"sunny-otter\",\"target\":\"here\",\"state\":\"running\",\"started\":1,"
              "\"directory\":\"/tmp/a\",\"command\":[],\"here\":true,\"in_place\":true}]"))
    (bliss-test--open)
    (let (opened)
      (cl-letf (((symbol-function 'dired-other-window) (lambda (dir) (setq opened dir))))
        (bliss-pull-or-visit "sunny-otter"))
      (should (equal opened "/tmp/a"))
      (should-not (seq-some (lambda (call) (string-prefix-p "pull" call)) (bliss-test--calls))))))

(ert-deftest bliss-test-read-workload-prefers-the-row-at-point ()
  (bliss-test--with-fake
    (bliss-test--open)
    (bliss-test--goto "sunny-otter")
    (should (equal (bliss--read-workload "x: ") "sunny-otter"))))

(ert-deftest bliss-test-read-workload-offers-the-newest-here ()
  (bliss-test--with-fake
    (let (default)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p collection &rest rest)
                   (setq default (nth 4 rest))
                   (car collection))))
        (bliss--read-workload "x: "))
      (should (equal default "sunny-otter")))))

;;; Terminals

(ert-deftest bliss-test-terminal-runs-bliss-and-reuses-a-live-buffer ()
  (bliss-test--with-fake
    (let ((bliss-terminal 'term)
          (process-environment (cons "BLISS_TEST_ENV=carried" process-environment)))
      (with-temp-file (expand-file-name "bliss" bliss-test--dir)
        (insert bliss-test--shebang "printf '%s\\n' \"$*:$BLISS_TEST_ENV:$TERM\" >> " bliss-test--dir
                "/calls\nsleep 30\n"))
      (let* ((buffer (bliss--terminal "bliss: test" '("attach" "x"))))
        (unwind-protect
            (progn
              (should (buffer-live-p buffer))
              (bliss-test--wait #'bliss-test--calls)
              (should (equal (bliss-test--calls) '("attach x:carried:ansi")))
              (should (eq (bliss--terminal "bliss: test" '("attach" "x")) buffer)))
          (delete-process (get-buffer-process buffer))
          (kill-buffer buffer))))))

(ert-deftest bliss-test-buffer-local-environment-reaches-the-terminal ()
  (bliss-test--with-fake
    (let ((bliss-terminal 'term))
      (with-temp-file (expand-file-name "bliss" bliss-test--dir)
        (insert bliss-test--shebang "printf '%s\\n' \"$BLISS_TEST_ENV\" >> " bliss-test--dir "/calls\nsleep 30\n"))
      (with-temp-buffer
        (setq-local process-environment (cons "BLISS_TEST_ENV=local" process-environment))
        (let ((buffer (bliss--terminal "bliss: env" '("x"))))
          (unwind-protect
              (progn
                (bliss-test--wait #'bliss-test--calls)
                (should (equal (bliss-test--calls) '("local"))))
            (delete-process (get-buffer-process buffer))
            (kill-buffer buffer)))))))

(ert-deftest bliss-test-fresh-terminals-do-not-reuse ()
  (bliss-test--with-fake
    (let ((bliss-terminal 'term))
      (with-temp-file (expand-file-name "bliss" bliss-test--dir)
        (insert bliss-test--shebang "sleep 30\n"))
      (let* ((one (bliss--terminal "bliss: run" '("run") t))
             (two (bliss--terminal "bliss: run" '("run") t)))
        (unwind-protect (should-not (eq one two))
          (dolist (buffer (list one two))
            (delete-process (get-buffer-process buffer))
            (kill-buffer buffer)))))))

(provide 'bliss-test)
;;; bliss-test.el ends here
