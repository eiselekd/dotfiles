;;; cling.el --- Top-level Cling package -*- lexical-binding: t -*-

;; Copyright © 2014 Chris Done.  All rights reserved.
;;             2016 Arthur Fayzrakhmanov

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cling-mode)
(require 'cling-process)
(require 'cling-interactive-mode)
(require 'cling-repl)
(require 'cling-commands)
(require 'cling-customize)
(require 'cling-utils)
(require 'cling-string)

(defvar interactive-cling-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'cling-process-load-file)
    (define-key map (kbd "C-c C-r") 'cling-process-reload)
    (define-key map (kbd "C-c C-t") 'cling-process-do-type)
    (define-key map (kbd "C-c C-i") 'cling-process-do-info)
    (define-key map (kbd "M-.") 'cling-mode-jump-to-def-or-tag)
    (define-key map (kbd "C-c C-k") 'cling-interactive-mode-clear)
    (define-key map (kbd "C-c C-c") 'cling-process-cabal-build)
    (define-key map (kbd "C-c v c") 'cling-cabal-visit-file)
    (define-key map (kbd "C-c C-x") 'cling-process-cabal)
    (define-key map (kbd "C-c C-b") 'cling-interactive-switch)
    (define-key map (kbd "C-c C-z") 'cling-interactive-switch)
    map)
  "Keymap for using `interactive-cling-mode'.")

;;;###autoload
(define-minor-mode interactive-cling-mode
  "Minor mode for enabling cling-process interaction."
  :lighter " Interactive"
  :keymap interactive-cling-mode-map
  (add-hook 'completion-at-point-functions
            #'cling-completions-sync-repl-completion-at-point
            nil
            t))

(make-obsolete 'cling-process-completions-at-point
               'cling-completions-sync-repl-completion-at-point
               "June 19, 2015")

(defun cling-process-completions-at-point ()
  "A `completion-at-point' function using the current cling process."
  (when (cling-session-maybe)
    (let ((process (cling-process))
          symbol-bounds)
      (cond
       ;; ghci can complete module names, but it needs the "import "
       ;; string at the beginning
       ((looking-back (rx line-start
                          "import" (1+ space)
                          (? "qualified" (1+ space))
                          (group (? (char upper) ; modid
                                    (* (char alnum ?' ?.)))))
                      (line-beginning-position))
        (let ((text (match-string-no-properties 0))
              (start (match-beginning 1))
              (end (match-end 1)))
          (list start end
                (cling-process-get-repl-completions process text))))
       ;; Complete OPTIONS, a completion list comes from variable
       ;; `cling-ghc-supported-options'
       ((and (nth 4 (syntax-ppss))
           (save-excursion
             (let ((p (point)))
               (and (search-backward "{-#" nil t)
                  (search-forward-regexp "\\_<OPTIONS\\(?:_GHC\\)?\\_>" p t))))
           (looking-back
            (rx symbol-start "-" (* (char alnum ?-)))
            (line-beginning-position)))
        (list (match-beginning 0) (match-end 0) cling-ghc-supported-options))
       ;; Complete LANGUAGE, a list of completions comes from variable
       ;; `cling-ghc-supported-extensions'
       ((and (nth 4 (syntax-ppss))
           (save-excursion
             (let ((p (point)))
               (and (search-backward "{-#" nil t)
                  (search-forward-regexp "\\_<LANGUAGE\\_>" p t))))
           (setq symbol-bounds (bounds-of-thing-at-point 'symbol)))
        (list (car symbol-bounds) (cdr symbol-bounds)
              cling-ghc-supported-extensions))
       ((setq symbol-bounds (cling-ident-pos-at-point))
        (cl-destructuring-bind (start . end) symbol-bounds
          (list start end
                (cling-process-get-repl-completions
                 process (buffer-substring-no-properties start end)))))))))

;;;###autoload
(defun cling-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (cond
   ;; At a compile message, jump to the location of the error in the
   ;; source.
   ((cling-interactive-at-compile-message)
    (next-error-internal))
   ;; At the input prompt, handle the expression in the usual way.
   ((cling-interactive-at-prompt)
    (cling-interactive-handle-expr))
   ;; At any other location in the buffer, copy the line to the
   ;; current prompt.
   (t
    (cling-interactive-copy-to-prompt))))

;;;###autoload
(defun cling-session-kill (&optional leave-interactive-buffer)
  "Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer unless LEAVE-INTERACTIVE-BUFFER is not given.
3. Walk through all the related buffers and set their cling-session to nil.
4. Remove the session from the sessions list."
  (interactive)
  (cling-mode-toggle-interactive-prompt-state)
  (unwind-protect
      (let* ((session (cling-session))
             (name (cling-session-name session))
             (also-kill-buffers
              (and cling-ask-also-kill-buffers
                   (y-or-n-p
                    (format "Killing `%s'. Also kill all associated buffers?"
                            name)))))
        (cling-kill-session-process session)
        (unless leave-interactive-buffer
          (kill-buffer (cling-session-interactive-buffer session)))
        (cl-loop for buffer in (buffer-list)
                 do (with-current-buffer buffer
                      (when (and (boundp 'cling-session)
                                 (string= (cling-session-name cling-session)
                                          name))
                        (setq cling-session nil)
                        (when also-kill-buffers
                          (kill-buffer)))))
        (setq cling-sessions
              (cl-remove-if (lambda (session)
                              (string= (cling-session-name session)
                                       name))
                            cling-sessions)))
    (cling-mode-toggle-interactive-prompt-state t)))

;;;###autoload
(defun cling-interactive-kill ()
  "Kill the buffer and (maybe) the session."
  (interactive)
  (when (eq major-mode 'cling-interactive-mode)
    (cling-mode-toggle-interactive-prompt-state)
    (unwind-protect
        (when (and (boundp 'cling-session)
                   cling-session
                   (y-or-n-p "Kill the whole session?"))
          (cling-session-kill t)))
    (cling-mode-toggle-interactive-prompt-state t)))

(defun cling-session-make (name)
  "Make a Cling session."
  (when (cling-session-lookup name)
    (error "Session of name %s already exists!" name))
  (let ((session (setq cling-session
                       (list (cons 'name name)))))
    (add-to-list 'cling-sessions session)
    (cling-process-start session)
    session))

(defun cling-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file.
If `cling-process-load-or-reload-prompt' is nil, accept `default'."
  (let ((name (cling-session-default-name)))
    (unless (cling-session-lookup name)
      (cling-mode-toggle-interactive-prompt-state)
      (unwind-protect
          (if (or (not cling-process-load-or-reload-prompt)
                  (y-or-n-p (format "Start a new project named “%s”? " name)))
              (cling-session-make name))
        (cling-mode-toggle-interactive-prompt-state t)))))

;;;###autoload
(defun cling-session ()
  "Get the Cling session, prompt if there isn't one or fail."
  (or (cling-session-maybe)
      (cling-session-assign
       (or (cling-session-from-buffer)
           (cling-session-new-assume-from-cabal)
           (cling-session-choose)
           (cling-session-new)))))

;;;###autoload
(defun cling-interactive-switch ()
  "Switch to the interactive mode for this session."
  (interactive)
  (let ((initial-buffer (current-buffer))
        (buffer (cling-session-interactive-buffer (cling-session))))
    (with-current-buffer buffer
      (setq cling-interactive-previous-buffer initial-buffer))
    (unless (eq buffer (window-buffer))
      (switch-to-buffer-other-window buffer))))

(defun cling-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (cling-session-default-name))))
    (when (not (string= name ""))
      (let ((session (cling-session-lookup name)))
        (cling-mode-toggle-interactive-prompt-state)
        (unwind-protect
            (if session
                (when
                    (y-or-n-p
                     (format "Session %s already exists. Use it?" name))
                  session)
              (cling-session-make name)))
        (cling-mode-toggle-interactive-prompt-state t)))))

;;;###autoload
(defun cling-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (cling-session-assign (or (cling-session-new-assume-from-cabal)
                              (cling-session-choose)
                              (cling-session-new))))

(defun cling-process-prompt-restart (process)
  "Prompt to restart the died PROCESS."
  (let ((process-name (cling-process-name process))
        (cursor-in-echo-area t))
    (if cling-process-suggest-restart
        (progn
          (cling-mode-toggle-interactive-prompt-state)
          (unwind-protect
              (cond
               ((string-match "You need to re-run the 'configure' command."
                              (cling-process-response process))
                (cl-case (read-char-choice
                          (concat
                           "The Cling process ended. Cabal wants you to run "
                           (propertize "cabal configure"
                                       'face
                                       'font-lock-keyword-face)
                           " because there is a version mismatch. Re-configure (y, n, l: view log)?"
                           "\n\n"
                           "Cabal said:\n\n"
                           (propertize (cling-process-response process)
                                       'face
                                       'font-lock-comment-face))
                          '(?l ?n ?y))
                  (?y (let ((default-directory
                              (cling-session-cabal-dir
                               (cling-process-session process))))
                        (message "%s"
                                 (shell-command-to-string "cabal configure"))))
                  (?l (let* ((response (cling-process-response process))
                             (buffer (get-buffer "*cling-process-log*")))
                        (if buffer
                            (switch-to-buffer buffer)
                          (progn (switch-to-buffer
                                  (get-buffer-create "*cling-process-log*"))
                                 (insert response)))))
                  (?n)))
               (t
                (cl-case (read-char-choice
                          (propertize
                           (format "The Cling process `%s' has died. Restart? (y, n, l: show process log) "
                                   process-name)
                           'face
                           'minibuffer-prompt)
                          '(?l ?n ?y))
                  (?y (cling-process-start (cling-process-session process)))
                  (?l (let* ((response (cling-process-response process))
                             (buffer (get-buffer "*cling-process-log*")))
                        (if buffer
                            (switch-to-buffer buffer)
                          (progn (switch-to-buffer
                                  (get-buffer-create "*cling-process-log*"))
                                 (insert response)))))
                  (?n))))
            ;; unwind
            (cling-mode-toggle-interactive-prompt-state t)))
      (message "The Cling process `%s' is dearly departed." process-name))))

(defun cling-process ()
  "Get the current process from the current session."
  (cling-session-process (cling-session)))

;;;###autoload
(defun cling-kill-session-process (&optional session)
  "Kill the process."
  (interactive)
  (let* ((session (or session (cling-session)))
         (existing-process (get-process (cling-session-name session))))
    (when (processp existing-process)
      (cling-interactive-mode-echo session "Killing process ...")
      (cling-process-set (cling-session-process session) 'is-restarting t)
      (delete-process existing-process))))

;;;###autoload
(defun cling-interactive-mode-visit-error ()
  "Visit the buffer of the current (or last) error message."
  (interactive)
  (with-current-buffer (cling-session-interactive-buffer (cling-session))
    (if (progn (goto-char (line-beginning-position))
               (looking-at cling-interactive-mode-error-regexp))
        (progn (forward-line -1)
               (cling-interactive-jump-to-error-line))
      (progn (goto-char (point-max))
             (cling-interactive-mode-error-backward)
             (cling-interactive-jump-to-error-line)))))

(defvar xref-prompt-for-identifier nil)

;;;###autoload
(defun cling-mode-jump-to-tag (&optional next-p)
  "Jump to the tag of the given identifier.

Give optional NEXT-P parameter to override value of
`xref-prompt-for-identifier' during definition search."
  (interactive "P")
  (let ((ident (cling-ident-at-point))
        (tags-file-dir (cling-cabal--find-tags-dir))
        (tags-revert-without-query t))
    (when (and ident
               (not (string= "" (cling-string-trim ident)))
               tags-file-dir)
      (let ((tags-file-name (concat tags-file-dir "TAGS")))
        (cond ((file-exists-p tags-file-name)
               (let ((xref-prompt-for-identifier next-p))
                 (xref-find-definitions ident)))
              (t (cling-mode-generate-tags ident)))))))

;;;###autoload
(defun cling-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  (when cling-tags-on-save
    (ignore-errors (cling-mode-generate-tags))))

;;;###autoload
(defun cling-mode-tag-find (&optional _next-p)
  "The tag find function, specific for the particular session."
  (interactive "P")
  (cond
   ((elt (syntax-ppss) 3) ;; Inside a string
    (cling-mode-jump-to-filename-in-string))
   (t (call-interactively 'cling-mode-jump-to-tag))))

(defun cling-mode-jump-to-filename-in-string ()
  "Jump to the filename in the current string."
  (let* ((string (save-excursion
                   (buffer-substring-no-properties
                    (1+ (search-backward-regexp "\"" (line-beginning-position) nil 1))
                    (1- (progn (forward-char 1)
                               (search-forward-regexp "\"" (line-end-position) nil 1))))))
         (fp (expand-file-name string
                               (cling-session-cabal-dir (cling-session)))))
    (find-file
     (read-file-name
      ""
      fp
      fp))))

;;;###autoload
(defun cling-interactive-bring ()
  "Bring up the interactive mode for this session."
  (interactive)
  (let* ((session (cling-session))
         (buffer (cling-session-interactive-buffer session)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun cling-process-load-file ()
  "Load the current buffer file."
  (interactive)
  (save-buffer)
  (cling-interactive-mode-reset-error (cling-session))
  (cling-process-file-loadish (format "load \"%s\"" (replace-regexp-in-string
                                                       "\""
                                                       "\\\\\""
                                                       (buffer-file-name)))
                                nil
                                (current-buffer)))

;;;###autoload
(defun cling-process-reload ()
  "Re-load the current buffer file."
  (interactive)
  (save-buffer)
  (cling-interactive-mode-reset-error (cling-session))
  (cling-process-file-loadish "reload" t (current-buffer)))

;;;###autoload
(defun cling-process-reload-file () (cling-process-reload))

(make-obsolete 'cling-process-reload-file 'cling-process-reload
               "2015-11-14")

;;;###autoload
(defun cling-process-load-or-reload (&optional toggle)
  "Load or reload. Universal argument toggles which."
  (interactive "P")
  (if toggle
      (progn (setq cling-reload-p (not cling-reload-p))
             (message "%s (No action taken this time)"
                      (if cling-reload-p
                          "Now running :reload."
                        "Now running :load <buffer-filename>.")))
    (if  (cling-process-reload) (cling-process-load-file))))

(make-obsolete 'cling-process-load-or-reload 'cling-process-load-file
               "2015-11-14")

;;;###autoload
(defun cling-process-cabal-build ()
  "Build the Cabal project."
  (interactive)
  (cling-process-do-cabal "build")
  (cling-process-add-cabal-autogen))

;;;###autoload
(defun cling-process-cabal (p)
  "Prompts for a Cabal command to run."
  (interactive "P")
  (if p
      (cling-process-do-cabal
       (read-from-minibuffer "Cabal command (e.g. install): "))
    (cling-process-do-cabal
     (funcall cling-completing-read-function "Cabal command: "
              (append cling-cabal-commands
                      (list "build --ghc-options=-fforce-recomp"))))))

(defun cling-process-file-loadish (command reload-p module-buffer)
  "Run a loading-ish COMMAND that wants to pick up type errors\
and things like that.  RELOAD-P indicates whether the notification
should say 'reloaded' or 'loaded'.  MODULE-BUFFER may be used
for various things, but is optional."
  (let ((session (cling-session)))
    (cling-session-current-dir session)
    (when cling-process-check-cabal-config-on-load
      (cling-process-look-config-changes session))
    (let ((process (cling-process)))
      (cling-process-queue-command
       process
       (make-cling-command
        :state (list session process command reload-p module-buffer)
        :go (lambda (state)
              (cling-process-send-string
               (cadr state) (format ":%s" (cl-caddr state))))
        :live (lambda (state buffer)
                (cling-process-live-build
                 (cadr state) buffer nil))
        :complete (lambda (state response)
                    (cling-process-load-complete
                     (car state)
                     (cadr state)
                     response
                     (cl-cadddr state)
                     (cl-cadddr (cdr state)))))))))

;;;###autoload
(defun cling-process-minimal-imports ()
  "Dump minimal imports."
  (interactive)
  (unless (> (save-excursion
               (goto-char (point-min))
               (cling-navigate-imports-go)
               (point))
             (point))
    (goto-char (point-min))
    (cling-navigate-imports-go))
  (cling-process-queue-sync-request (cling-process)
                                      ":set -ddump-minimal-imports")
  (cling-process-load-file)
  (insert-file-contents-literally
   (concat (cling-session-current-dir (cling-session))
           "/"
           (cling-guess-module-name-from-file-name (buffer-file-name))
           ".imports")))

(defun cling-interactive-jump-to-error-line ()
  "Jump to the error line."
  (let ((orig-line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
    (and (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(-[0-9]+\\)?:" orig-line)
         (let* ((file (match-string 1 orig-line))
                (line (match-string 2 orig-line))
                (col (match-string 3 orig-line))
                (session (cling-interactive-session))
                (cabal-path (cling-session-cabal-dir session))
                (src-path (cling-session-current-dir session))
                (cabal-relative-file (expand-file-name file cabal-path))
                (src-relative-file (expand-file-name file src-path)))
           (let ((file (cond ((file-exists-p cabal-relative-file)
                              cabal-relative-file)
                             ((file-exists-p src-relative-file)
                              src-relative-file))))
             (when file
               (other-window 1)
               (find-file file)
               (cling-interactive-bring)
               (goto-char (point-min))
               (forward-line (1- (string-to-number line)))
               (goto-char (+ (point) (string-to-number col) -1))
               (cling-mode-message-line orig-line)
               t))))))

(provide 'cling)
;;; cling.el ends here
