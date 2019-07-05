;;; cling-process.el --- Communicating with the inferior Cling process -*- lexical-binding: t -*-

;; Copyright (C) 2011  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-util)
(require 'cling-session)
(require 'cling-customize)

(defconst cling-process-prompt-regex "\[cling\]$"
  "Used for delimiting command replies. 4 is End of Transmission.")

(defvar cling-reload-p nil
  "Used internally for `cling-process-loadish'.")

(defconst cling-process-greetings
  (list "Hello, Cling!"
        "The lambdas must flow."
        "Hours of hacking await!"
        "The next big Cling project is about to start!"
        "Your wish is my IO ().")
  "Greetings for when the Cling process starts up.")

(defconst cling-process-logo
  (expand-file-name "logo.svg" cling-mode-pkg-base-dir)
  "Cling logo for notifications.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing commands -- using cl 'defstruct'

(cl-defstruct cling-command
  "Data structure representing a command to be executed when with
  a custom state and three callback."
  ;; hold the custom command state
  ;; state :: a
  state
  ;; called when to execute a command
  ;; go :: a -> ()
  go
  ;; called whenever output was collected from the cling process
  ;; live :: a -> Response -> Bool
  live
  ;; called when the output from the cling process indicates that the command
  ;; is complete
  ;; complete :: a -> Response -> ()
  complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the process

(defun cling-process-compute-process-log-and-command (session hptype)
  "Compute the log and process to start command for the SESSION from the HPTYPE.
Do not actually start any process.
HPTYPE is the result of calling `'cling-process-type`' function."
  (let ((session-name (cling-session-name session)))
    (cl-ecase hptype
      ('ghci
       (append (list (format "Starting inferior cling process %s ..."
                             cling-process-path-cling)
                     session-name
                     nil)
               (apply cling-process-wrapper-function
                      (list
                       (append (cling-process-path-to-list cling-process-path-cling)
                               cling-process-args-cling)))))
      )))

(defun cling-process-path-to-list (path)
  "Convert a path (which may be a string or a list) to a list."
  (if (stringp path)
      (list path)
    path))

(defun cling-process-make (name)
  "Make an inferior Cling process."
  (list (cons 'name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun cling-process-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((session (cling-process-project-by-proc proc)))
    (when session
      (let* ((process (cling-session-process session)))
        (unless (cling-process-restarting process)
          (cling-process-log
           (propertize (format "Event: %S\n" event)
                       'face '((:weight bold))))
          (cling-process-log
           (propertize "Process reset.\n"
                       'face 'font-lock-comment-face))
          (run-hook-with-args 'cling-process-ended-functions process))))))

(defun cling-process-filter (proc response)
  "The filter for the process pipe."
  (let ((i 0))
    (cl-loop for line in (split-string response "\n")
             do (cling-process-log
                 (concat (if (= i 0)
                             (propertize "<- " 'face 'font-lock-comment-face)
                           "   ")
                         (propertize line 'face 'cling-interactive-face-compile-warning)))
             do (setq i (1+ i))))
  (let ((session (cling-process-project-by-proc proc)))
    (when session
      (if (cling-process-cmd (cling-session-process session))
          (cling-process-collect session
                                   response
                                   (cling-session-process session))))))

(defun cling-process-log (msg)
  "Effective append MSG to the process log (if enabled)."
  (when cling-process-log
    (let* ((append-to (get-buffer-create "*cling-process-log*")))
      (with-current-buffer append-to
        ;; point should follow insertion so that it stays at the end
        ;; of the buffer
        (setq-local window-point-insertion-type t)
        (let ((buffer-read-only nil))
          (insert msg "\n"))))))

(defun cling-process-project-by-proc (proc)
  "Find project by process."
  (cl-find-if (lambda (project)
                (string= (cling-session-name project)
                         (process-name proc)))
              cling-sessions))

(defun cling-process-collect (_session response process)
  "Collect input for the response until receives a prompt."
  (cling-process-set-response process
                                (concat (cling-process-response process) response))
  (while (cling-process-live-updates process))
  (when (string-match cling-process-prompt-regex
                      (cling-process-response process))
    (cling-command-exec-complete
     (cling-process-cmd process)
     (replace-regexp-in-string
      cling-process-prompt-regex
      ""
      (cling-process-response process)))
    (cling-process-reset process)
    (cling-process-trigger-queue process)))

(defun cling-process-reset (process)
  "Reset the process's state, ready for the next send/reply."
  (progn (cling-process-set-response-cursor process 0)
         (cling-process-set-response process "")
         (cling-process-set-cmd process nil)))

(defun cling-process-consume (process regex)
  "Consume a regex from the response and move the cursor along if succeed."
  (when (string-match regex
                      (cling-process-response process)
                      (cling-process-response-cursor process))
    (cling-process-set-response-cursor process (match-end 0))
    t))

(defun cling-process-send-string (process string)
  "Try to send a string to the process's process. Ask to restart if it's not running."
  (let ((child (cling-process-process process)))
    (if (equal 'run (process-status child))
        (let ((out (concat string "\n")))
          (let ((i 0))
            (cl-loop for line in (split-string out "\n")
                     do (unless (string-equal "" line)
                          (cling-process-log
                           (concat (if (= i 0)
                                       (propertize "-> " 'face 'font-lock-comment-face)
                                     "   ")
                                   (propertize line 'face 'font-lock-string-face))))
                     do (setq i (1+ i))))
          (process-send-string child out))
      (unless (cling-process-restarting process)
        (run-hook-with-args 'cling-process-ended-functions process)))))

(defun cling-process-live-updates (process)
  "Process live updates."
  (cling-command-exec-live (cling-process-cmd process)
                             (cling-process-response process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making commands

(defun cling-process-queue-without-filters (process line)
  "Queue LINE to be sent to PROCESS without bothering to look at
the response."
  (cling-process-queue-command
   process
   (make-cling-command
    :state (cons process line)
    :go (lambda (state)
          (cling-process-send-string (car state)
                                       (cdr state))))))


(defun cling-process-queue-command (process command)
  "Add a command to the process command queue."
  (cling-process-cmd-queue-add process command)
  (cling-process-trigger-queue process))

(defun cling-process-trigger-queue (process)
  "Trigger the next command in the queue to be ran if there is no current command."
  (if (and (cling-process-process process)
           (process-live-p (cling-process-process process)))
      (unless (cling-process-cmd process)
        (let ((cmd (cling-process-cmd-queue-pop process)))
          (when cmd
            (cling-process-set-cmd process cmd)
            (cling-command-exec-go cmd))))
    (progn (cling-process-reset process)
           (cling-process-set process 'command-queue nil)
           (run-hook-with-args 'cling-process-ended-functions process))))

(defun cling-process-queue-flushed-p (process)
  "Return t if command queue has been completely processed."
  (not (or (cling-process-cmd-queue process)
           (cling-process-cmd process))))

(defun cling-process-queue-flush (process)
  "Block till PROCESS' command queue has been completely processed.
This uses `accept-process-output' internally."
  (while (not (cling-process-queue-flushed-p process))
    (cling-process-trigger-queue process)
    (accept-process-output (cling-process-process process) 1)))

(defun cling-process-queue-sync-request (process reqstr)
  "Queue submitting REQSTR to PROCESS and return response blockingly."
  (let ((cmd (make-cling-command
              :state (cons nil process)
              :go `(lambda (s) (cling-process-send-string (cdr s) ,reqstr))
              :complete 'setcar)))
    (cling-process-queue-command process cmd)
    (cling-process-queue-flush process)
    (car-safe (cling-command-state cmd))))

(defun cling-process-get-repl-completions (process inputstr &optional limit)
  "Query PROCESS with `:complete repl ...' for INPUTSTR.
Give optional LIMIT arg to limit completion candidates count,
zero, negative values, and nil means all possible completions.
Returns NIL when no completions found."
  (let* ((mlimit (if (and limit (> limit 0))
                     (concat " " (number-to-string limit) " ")
                   " "))
         (reqstr (concat ":complete repl"
                         mlimit
                         (cling-string-literal-encode inputstr)))
         (rawstr (cling-process-queue-sync-request process reqstr))
         (response-status (cling-utils-repl-response-error-status rawstr)))
    (if (eq 'unknown-command response-status)
        (error
         "GHCi lacks `:complete' support (try installing GHC 7.8+ or ghci-ng)")
      (when rawstr
        ;; parse REPL response if any
        (let* ((s1 (split-string rawstr "\r?\n" t))
               (cs (mapcar #'cling-string-literal-decode (cdr s1)))
               (h0 (car s1))) ;; "<limit count> <all count> <unused string>"
          (unless (string-match
                   "\\`\\([0-9]+\\) \\([0-9]+\\) \\(\".*\"\\)\\'"
                   h0)
            (error "Invalid `:complete' response"))
          (let ((cnt1 (match-string 1 h0))
                (h1 (cling-string-literal-decode (match-string 3 h0))))
            (unless (= (string-to-number cnt1) (length cs))
              (error "Lengths inconsistent in `:complete' reponse"))
            (cons h1 cs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the process

(defun cling-process-get (process key)
  "Get the PROCESS's KEY value.
Returns nil if KEY not set."
  (cdr (assq key process)))

(defun cling-process-set (process key value)
  "Set the PROCESS's KEY to VALUE.
Returns newly set VALUE."
  (if process
      (let ((cell (assq key process)))
        (if cell
            (setcdr cell value)         ; modify cell in-place
          (setcdr process (cons (cons key value) (cdr process))) ; new cell
          value))
    (display-warning 'cling-interactive
                     "`cling-process-set' called with nil process")))

;; Wrappers using cling-process-{get,set}

(defun cling-process-set-sent-stdin (p v)
  "We've sent stdin, so let's not clear the output at the end."
  (cling-process-set p 'sent-stdin v))

(defun cling-process-sent-stdin-p (p)
  "Did we send any stdin to the process during evaluation?"
  (cling-process-get p 'sent-stdin))

(defun cling-process-set-suggested-imports (p v)
  "Remember what imports have been suggested, to avoid
re-asking about the same imports."
  (cling-process-set p 'suggested-imported v))

(defun cling-process-suggested-imports (p)
  "Get what modules have already been suggested and accepted."
  (cling-process-get p 'suggested-imported))

(defun cling-process-set-evaluating (p v)
  "Set status of evaluating to be on/off."
  (cling-process-set p 'evaluating v))

(defun cling-process-evaluating-p (p)
  "Get status of evaluating (on/off)."
  (cling-process-get p 'evaluating))

(defun cling-process-set-process (p v)
  "Set the process's inferior process."
  (cling-process-set p 'inferior-process v))

(defun cling-process-process (p)
  "Get the process child."
  (cling-process-get p 'inferior-process))

(defun cling-process-name (p)
  "Get the process name."
  (cling-process-get p 'name))

(defun cling-process-cmd (p)
  "Get the process's current command.
Return nil if no current command."
  (cling-process-get p 'current-command))

(defun cling-process-set-cmd (p v)
  "Set the process's current command."
  (cling-process-set-evaluating p nil)
  (cling-process-set-sent-stdin p nil)
  (cling-process-set-suggested-imports p nil)
  (cling-process-set p 'current-command v))

(defun cling-process-response (p)
  "Get the process's current response."
  (cling-process-get p 'current-response))

(defun cling-process-session (p)
  "Get the process's current session."
  (cling-process-get p 'session))

(defun cling-process-set-response (p v)
  "Set the process's current response."
  (cling-process-set p 'current-response v))

(defun cling-process-set-session (p v)
  "Set the process's current session."
  (cling-process-set p 'session v))

(defun cling-process-response-cursor (p)
  "Get the process's current response cursor."
  (cling-process-get p 'current-response-cursor))

(defun cling-process-set-response-cursor (p v)
  "Set the process's response cursor."
  (cling-process-set p 'current-response-cursor v))

;; low-level command queue operations

(defun cling-process-restarting (process)
  "Is the PROCESS restarting?"
  (cling-process-get process 'is-restarting))

(defun cling-process-cmd-queue (process)
  "Get the PROCESS' command queue.
New entries get added to the end of the list. Use
`cling-process-cmd-queue-add' and
`cling-process-cmd-queue-pop' to modify the command queue."
  (cling-process-get process 'command-queue))

(defun cling-process-cmd-queue-add (process cmd)
  "Add CMD to end of PROCESS's command queue."
  (cl-check-type cmd cling-command)
  (cling-process-set process
                       'command-queue
                       (append (cling-process-cmd-queue process)
                               (list cmd))))

(defun cling-process-cmd-queue-pop (process)
  "Pop the PROCESS' next entry from command queue.
Returns nil if queue is empty."
  (let ((queue (cling-process-cmd-queue process)))
    (when queue
      (cling-process-set process 'command-queue (cdr queue))
      (car queue))))


(defun cling-process-unignore-file (session file)
  "

Note to Windows Emacs hackers:

chmod is how to change the mode of files in POSIX
systems. This will not work on your operating
system.

There is a command a bit like chmod called \"Calcs\"
that you can try using here:

http://technet.microsoft.com/en-us/library/bb490872.aspx

If it works, you can submit a patch to this
function and remove this comment.
"
  (shell-command (read-from-minibuffer "Permissions command: "
                                       (concat "chmod 700 "
                                               file)))
  (cling-session-modify
   session
   'ignored-files
   (lambda (files)
     (cl-remove-if (lambda (path)
                     (string= path file))
                   files))))

(defun cling-command-exec-go (command)
  "Call the command's go function."
  (let ((go-func (cling-command-go command)))
    (when go-func
      (funcall go-func (cling-command-state command)))))

(defun cling-command-exec-complete (command response)
  "Call the command's complete function."
  (let ((comp-func (cling-command-complete command)))
    (when comp-func
      (condition-case-unless-debug e
          (funcall comp-func
                   (cling-command-state command)
                   response)
        (quit (message "Quit"))
        (error (message "Cling process command errored with: %S" e))))))

(defun cling-command-exec-live (command response)
  "Trigger the command's live updates callback."
  (let ((live-func (cling-command-live command)))
    (when live-func
      (funcall live-func
               (cling-command-state command)
               response))))

(provide 'cling-process)

;;; cling-process.el ends here
