;;; cling-commands.el --- Commands that can be run on the process -*- lexical-binding: t -*-

;;; Commentary:

;;; This module provides varoius `cling-mode' and `cling-interactive-mode'
;;; specific commands such as show type signature, show info, cling process
;;; commands and etc.

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

;;; Code:

(require 'cl-lib)
(require 'etags)
(require 'cling-mode)
(require 'cling-process)
(require 'cling-interactive-mode)
(require 'cling-session)
(require 'highlight-uses-mode)

(defcustom cling-mode-stylish-cling-path "stylish-cling"
  "Path to `stylish-cling' executable."
  :group 'cling
  :type 'string)

;;;###autoload
(defun cling-process-restart ()
  "Restart the inferior Cling process."
  (interactive)
  (cling-process-reset (cling-interactive-process))
  (cling-process-set (cling-interactive-process) 'command-queue nil)
  (cling-process-start (cling-interactive-session)))

(defun cling-process-start (session)
  "Start the inferior Cling process with a given SESSION.
You can create new session using function `cling-session-make'."
  (cling-interactive-mode-echo session "(Re)starting process ...")
  (let ((existing-process (get-process (cling-session-name (cling-interactive-session)))))
    (when (processp existing-process)
      (cling-interactive-mode-echo session "Restarting process ...")
      (cling-process-set (cling-session-process session) 'is-restarting t)
      (delete-process existing-process)))
  (let ((process (or (cling-session-process session)
                     (cling-process-make (cling-session-name session))))
        (old-queue (cling-process-get (cling-session-process session)
                                      'command-queue)))

    (cling-session-set-process session process)
    (cling-process-set-session process session)
    (cling-process-set-cmd process nil)
    (cling-process-set (cling-session-process session) 'is-restarting nil)
    (let ((default-directory (cling-session-cabal-dir session))
          (log-and-command (cling-process-compute-process-log-and-command session (cling-process-type))))
      (cling-session-prompt-set-current-dir session (not cling-process-load-or-reload-prompt))
      (cling-process-set-process
       process
       (progn
         (cling-process-log (propertize (format "%S" log-and-command)))
         (apply #'start-process (cdr log-and-command)))))
    (progn (set-process-sentinel (cling-process-process process) 'cling-process-sentinel)
           (set-process-filter (cling-process-process process) 'cling-process-filter))
    (cling-process-send-startup process)
    (unless (or (eq 'cabal-repl (cling-process-type))
                (eq 'cabal-new-repl (cling-process-type))
                   (eq 'stack-ghci (cling-process-type))) ;; Both "cabal repl" and "stack ghci" set the proper CWD.
      (cling-process-change-dir session
                                  process
                                  (cling-session-current-dir session)))
    (cling-process-set process 'command-queue
                         (append (cling-process-get (cling-session-process session)
                                                      'command-queue)
                                 old-queue))
    process))

(defun cling-process-send-startup (process)
  "Send the necessary start messages to cling PROCESS."
  (cling-process-queue-command
   process
   (make-cling-command
    :state process

    :go (lambda (process)
          ;; We must set the prompt last, so that this command as a
          ;; whole produces only one prompt marker as a response.
          (cling-process-send-string process ".help")
	  (cling-process-send-string process ".help")
	  )

    :live (lambda (process buffer)
            (when (cling-process-consume
                   process
                   "^\*\*\* WARNING: \\(.+\\) is writable by someone else, IGNORING!$")
              (let ((path (match-string 1 buffer)))
                (cling-session-modify
                 (cling-process-session process)
                 'ignored-files
                 (lambda (files)
                   (cl-remove-duplicates (cons path files) :test 'string=)))
                (cling-interactive-mode-compile-warning
                 (cling-process-session process)
                 (format "GHCi is ignoring: %s (run M-x cling-process-unignore)"
                         path)))))

    :complete (lambda (process _)
                (cling-interactive-mode-echo
                 (cling-process-session process)
                 (concat (nth (random (length cling-process-greetings))
                              cling-process-greetings)
                         (when cling-process-show-debug-tips
                           "
If I break, you can:
  1. Restart:           M-x cling-process-restart
  2. Configure logging: C-h v cling-process-log (useful for debugging)
  3. General config:    M-x customize-mode
  4. Hide these tips:   C-h v cling-process-show-debug-tips")))
                (with-current-buffer (cling-interactive-buffer)
                  (goto-char cling-interactive-mode-prompt-start))))))

(defun cling-commands-process ()
  "Get the Cling session, throws an error if not available."
  (or (cling-session-process (cling-session-maybe))
      (error "No Cling session/process associated with this
      buffer. Maybe run M-x cling-session-change?")))

;;;###autoload
(defun cling-process-clear ()
  "Clear the current process."
  (interactive)
  (cling-process-reset (cling-commands-process))
  (cling-process-set (cling-commands-process) 'command-queue nil))

;;;###autoload
(defun cling-process-interrupt ()
  "Interrupt the process (SIGINT)."
  (interactive)
  (interrupt-process (cling-process-process (cling-commands-process))))

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar cling-cabal-targets-history nil
  "History list for session targets.")

(defun cling-process-hayoo-ident (ident)
  "Hayoo for IDENT, return a list of modules"
  ;; We need a real/simulated closure, because otherwise these
  ;; variables will be unbound when the url-retrieve callback is
  ;; called.
  ;; TODO: Remove when this code is converted to lexical bindings by
  ;; default (Emacs 24.1+)
  (let ((url (format cling-process-hayoo-query-url (url-hexify-string ident))))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (= 200 url-http-response-status)
          (progn
            (goto-char url-http-end-of-headers)
            (let* ((res (json-read))
                   (results (assoc-default 'result res)))
              ;; TODO: gather packages as well, and when we choose a
              ;; given import, check that we have the package in the
              ;; cabal file as well.
              (cl-mapcan (lambda (r)
                           ;; append converts from vector -> list
                           (append (assoc-default 'resultModules r) nil))
                         results)))
        (warn "HTTP error %s fetching %s" url-http-response-status url)))))

(defun cling-process-hoogle-ident (ident)
  "Hoogle for IDENT, return a list of modules."
  (with-temp-buffer
    (let ((hoogle-error (call-process "hoogle" nil t nil "search" "--exact" ident)))
      (goto-char (point-min))
      (unless (or (/= 0 hoogle-error)
                  (looking-at "^No results found")
                  (looking-at "^package "))
        (while (re-search-forward "^\\([^ ]+\\).*$" nil t)
          (replace-match "\\1" nil nil))
        (cl-remove-if (lambda (a) (string= "" a))
                      (split-string (buffer-string)
                                    "\n"))))))

(defun cling-process-cling-docs-ident (ident)
  "Search with cling-docs for IDENT, return a list of modules."
  (cl-remove-if-not
   (lambda (a) (string-match "^[[:upper:]][[:alnum:]_'.]+$" a))
   (split-string
      (with-output-to-string
        (with-current-buffer
            standard-output
          (call-process "cling-docs"
                        nil             ; no infile
                        t               ; output to current buffer (that is string)
                        nil             ; do not redisplay
                        "--modules" ident)))
      "\n")))

(defun cling-process-import-modules (process modules)
  "Query PROCESS `:m +' command to import MODULES."
  (when cling-process-auto-import-loaded-modules
    (cling-process-queue-command
     process
     (make-cling-command
      :state (cons process modules)
      :go (lambda (state)
            (cling-process-send-string
             (car state)
             (format ":m + %s" (mapconcat 'identity (cdr state) " "))))))))

;;;###autoload
(defun cling-describe (ident)
  "Describe the given identifier IDENT."
  (interactive (list (read-from-minibuffer "Describe identifier: "
                                           (cling-ident-at-point))))
  (let ((results (read (shell-command-to-string
                        (concat "cling-docs --sexp "
                                ident)))))
    (help-setup-xref (list #'cling-describe ident)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (if results
              (cl-loop for result in results
                       do (insert (propertize ident 'font-lock-face
                                              '((:inherit font-lock-type-face
                                                          :underline t)))
                                  " is defined in "
                                  (let ((module (cadr (assoc 'module result))))
                                    (if module
                                        (concat module " ")
                                      ""))
                                  (cadr (assoc 'package result))
                                  "\n\n")
                       do (let ((type (cadr (assoc 'type result))))
                            (when type
                              (insert (cling-fontify-as-mode type 'cling-mode)
                                      "\n")))
                       do (let ((args (cadr (assoc 'type results))))
                            (cl-loop for arg in args
                                     do (insert arg "\n"))
                            (insert "\n"))
                       do (insert (cadr (assoc 'documentation result)))
                       do (insert "\n\n"))
            (insert "No results for " ident)))))))

;;;###autoload
(defun cling-rgrep (&optional prompt)
  "Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (cling-ident-at-point))))
    (rgrep sym
           "*.hs *.lhs *.hsc *.chs *.hs-boot *.lhs-boot"
           (cling-session-current-dir (cling-interactive-session)))))

;;;###autoload
(defun cling-process-do-info (&optional prompt-value)
  "Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer."
  (interactive "P")
  (let ((at-point (cling-ident-at-point)))
    (when (or prompt-value at-point)
      (let* ((ident (replace-regexp-in-string
                     "^!\\([A-Z_a-z]\\)"
                     "\\1"
                     (if prompt-value
                         (read-from-minibuffer "Info: " at-point)
                       at-point)))
             (modname (unless prompt-value
                        (cling-utils-parse-import-statement-at-point)))
             (command (cond
                       (modname
                        (format ":browse! %s" modname))
                       ((string= ident "") ; For the minibuffer input case
                        nil)
                       (t (format (if (string-match "^[a-zA-Z_]" ident)
                                      ":info %s"
                                    ":info (%s)")
                                  (or ident
                                      at-point))))))
        (when command
          (cling-process-show-repl-response command))))))

;;;###autoload
(defun cling-process-do-type (&optional insert-value)
  "Print the type of the given expression.

Given INSERT-VALUE prefix indicates that result type signature
should be inserted."
  (interactive "P")
  (if insert-value
      (cling-process-insert-type)
    (let* ((expr
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (cling-ident-at-point)))
           (expr-okay (and expr
                         (not (string-match-p "\\`[[:space:]]*\\'" expr))
                         (not (string-match-p "\n" expr)))))
      ;; No newlines in expressions, and surround with parens if it
      ;; might be a slice expression
      (when expr-okay
        (cling-process-show-repl-response
         (format
          (if (or (string-match-p "\\`(" expr)
                  (string-match-p "\\`[_[:alpha:]]" expr))
              ":type %s"
            ":type (%s)")
          expr))))))

;;;###autoload
(defun cling-mode-jump-to-def-or-tag (&optional _next-p)
  ;; FIXME NEXT-P arg is not used
  "Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'."
  (interactive "P")
  (if (cling-session-maybe)
        (let ((initial-loc (point-marker))
            (loc (cling-mode-find-def (cling-ident-at-point))))
          (cling-mode-handle-generic-loc loc)
          (unless (equal initial-loc (point-marker))
            (xref-push-marker-stack initial-loc)))
      (call-interactively 'cling-mode-tag-find)))

;;;###autoload
(defun cling-mode-goto-loc ()
  "Go to the location of the thing at point.
Requires the :loc-at command from GHCi."
  (interactive)
  (let ((loc (cling-mode-loc-at)))
    (when loc
      (cling-mode-goto-span loc))))

(defun cling-mode-goto-span (span)
  "Jump to the SPAN, whatever file and line and column it needs to get there."
  (xref-push-marker-stack)
  (find-file (expand-file-name (plist-get span :path)
                               (cling-session-cabal-dir (cling-interactive-session))))
  (goto-char (point-min))
  (forward-line (1- (plist-get span :start-line)))
  (forward-char (plist-get span :start-col)))

(defun cling-process-insert-type ()
  "Get the identifier at the point and insert its type.
Use GHCi's :type if it's possible."
  (let ((ident (cling-ident-at-point)))
    (when ident
      (let ((process (cling-interactive-process))
            (query (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                               ":type %s"
                             ":type (%s)")
                           ident)))
        (cling-process-queue-command
         process
         (make-cling-command
          :state (list process query (current-buffer))
          :go (lambda (state)
                (cling-process-send-string (nth 0 state)
                                             (nth 1 state)))
          :complete (lambda (state response)
                      (cond
                       ;; TODO: Generalize this into a function.
                       ((or (string-match "^Top level" response)
                            (string-match "^<interactive>" response))
                        (message "%s" response))
                       (t
                        (with-current-buffer (nth 2 state)
                          (goto-char (line-beginning-position))
                          (insert (format "%s\n" (replace-regexp-in-string "\n$" "" response)))))))))))))

(defun cling-mode-find-def (ident)
  ;; TODO Check if it possible to exploit `cling-process-do-info'
  "Find definition location of identifier IDENT.
Uses the GHCi process to find the location.  Returns nil if it
can't find the identifier or the identifier isn't a string.

Returns:

    (library <package> <module>)
    (file <path> <line> <col>)
    (module <name>)
    nil"
  (when (stringp ident)
    (let ((reply (cling-process-queue-sync-request
                  (cling-interactive-process)
                  (format (if (string-match "^[a-zA-Z_]" ident)
                              ":info %s"
                            ":info (%s)")
                          ident))))
      (let ((match (string-match "-- Defined \\(at\\|in\\) \\(.+\\)$" reply)))
        (when match
          (let ((defined (match-string 2 reply)))
            (let ((match (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)$" defined)))
              (cond
               (match
                (list 'file
                      (expand-file-name (match-string 1 defined)
                                        (cling-session-current-dir (cling-interactive-session)))
                      (string-to-number (match-string 2 defined))
                      (string-to-number (match-string 3 defined))))
               (t
                (let ((match (string-match "`\\(.+?\\):\\(.+?\\)'$" defined)))
                  (if match
                      (list 'library
                            (match-string 1 defined)
                            (match-string 2 defined))
                    (let ((match (string-match "`\\(.+?\\)'$" defined)))
                      (if match
                          (list 'module
                                (match-string 1 defined)))))))))))))))

;;;###autoload
(defun cling-mode-jump-to-def (ident)
  "Jump to definition of identifier IDENT at point."
  (interactive (list (cling-ident-at-point)))
  (let ((loc (cling-mode-find-def ident)))
    (when loc
      (cling-mode-handle-generic-loc loc))))

(defun cling-mode-handle-generic-loc (loc)
  "Either jump to or echo a generic location LOC.
Either a file or a library."
  (cl-case (car loc)
    (file (progn
              (find-file (elt loc 1))
              (goto-char (point-min))
              (forward-line (1- (elt loc 2)))
              (goto-char (+ (line-beginning-position)
                            (1- (elt loc 3))))))
    (library (message "Defined in `%s' (%s)."
                      (elt loc 2)
                      (elt loc 1)))
    (module (message "Defined in `%s'."
                     (elt loc 1)))))

(defun cling-mode-loc-at ()
  "Get the location at point.
Requires the :loc-at command from GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (cling-spanable-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (let ((reply (cling-process-queue-sync-request
                    (cling-interactive-process)
                    (save-excursion
                      (format ":loc-at %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                              reply)
                (list :path (match-string 1 reply)
                      :start-line (string-to-number (match-string 2 reply))
                      ;; ;; GHC uses 1-based columns.
                      :start-col (1- (string-to-number (match-string 3 reply)))
                      :end-line (string-to-number (match-string 4 reply))
                      ;; GHC uses 1-based columns.
                      :end-col (1- (string-to-number (match-string 5 reply))))
              (error (propertize reply 'face 'compilation-error)))
          (error (propertize "No reply. Is :loc-at supported?"
                             'face 'compilation-error)))))))

;;;###autoload
(defun cling-process-cd (&optional _not-interactive)
  ;; FIXME optional arg is not used
  "Change directory."
  (interactive)
  (let* ((session (cling-interactive-session))
         (dir (cling-session-prompt-set-current-dir session)))
    (cling-process-log
     (propertize (format "Changing directory to %s ...\n" dir)
                 'face font-lock-comment-face))
    (cling-process-change-dir session
                                (cling-interactive-process)
                                dir)))

(defun cling-session-buffer-default-dir (session &optional buffer)
  "Try to deduce a sensible default directory for SESSION and BUFFER,
of which the latter defaults to the current buffer."
  (or (cling-session-get session 'current-dir)
      (cling-session-get session 'cabal-dir)
      (if (buffer-file-name buffer)
          (file-name-directory (buffer-file-name buffer))
          "~/")))

(defun cling-session-prompt-set-current-dir (session &optional use-default)
  "Prompt for the current directory.
Return current working directory for SESSION."
  (let ((default (cling-session-buffer-default-dir session)))
    (cling-session-set-current-dir
     session
     (if use-default
         default
         (cling-utils-read-directory-name "Set current directory: " default))))
  (cling-session-get session 'current-dir))

(defun cling-process-change-dir (session process dir)
  "Change SESSION's current directory.
Query PROCESS to `:cd` to directory DIR."
  (cling-process-queue-command
   process
   (make-cling-command
    :state (list session process dir)
    :go
    (lambda (state)
      (cling-process-send-string
       (cadr state) (format ":cd %s" (cl-caddr state))))

    :complete
    (lambda (state _)
      (cling-session-set-current-dir (car state) (cl-caddr state))
      (cling-interactive-mode-echo (car state)
                                     (format "Changed directory: %s"
                                             (cl-caddr state)))))))

;;;###autoload
(defun cling-process-cabal-macros ()
  "Send the cabal macros string."
  (interactive)
  (cling-process-queue-without-filters (cling-interactive-process)
                                         ":set -optP-include -optPdist/build/autogen/cabal_macros.h"))

(defun cling-process-do-try-info (sym)
  "Get info of SYM and echo in the minibuffer."
  (let ((process (cling-interactive-process)))
    (cling-process-queue-command
     process
     (make-cling-command
      :state (cons process sym)
      :go (lambda (state)
            (cling-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":info %s" (cdr state))
               (format ":info (%s)" (cdr state)))))
      :complete (lambda (_state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (cling-mode-message-line response)))))))

(defun cling-process-do-try-type (sym)
  "Get type of SYM and echo in the minibuffer."
  (let ((process (cling-interactive-process)))
    (cling-process-queue-command
     process
     (make-cling-command
      :state (cons process sym)
      :go (lambda (state)
            (cling-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":type %s" (cdr state))
               (format ":type (%s)" (cdr state)))))
      :complete (lambda (_state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (cling-mode-message-line response)))))))

;;;###autoload
(defun cling-mode-show-type-at (&optional insert-value)
  "Show type of the thing at point or within active region asynchronously.
This function requires GHCi 8+ or GHCi-ng.

\\<cling-interactive-mode-map>
To make this function works sometimes you need to load the file in REPL
first using command `cling-process-load-file' bound to
\\[cling-process-load-file].

Optional argument INSERT-VALUE indicates that
recieved type signature should be inserted (but only if nothing
happened since function invocation)."
  (interactive "P")
  (let* ((pos (cling-command-capture-expr-bounds))
         (req (cling-utils-compose-type-at-command pos))
         (process (cling-interactive-process))
         (buf (current-buffer))
         (pos-reg (cons pos (region-active-p))))
    (cling-process-queue-command
     process
     (make-cling-command
      :state (list process req buf insert-value pos-reg)
      :go
      (lambda (state)
        (let* ((prc (car state))
               (req (nth 1 state)))
          (cling-utils-async-watch-changes)
          (cling-process-send-string prc req)))
      :complete
      (lambda (state response)
        (let* ((init-buffer (nth 2 state))
               (insert-value (nth 3 state))
               (pos-reg (nth 4 state))
               (wrap (cdr pos-reg))
               (min-pos (caar pos-reg))
               (max-pos (cdar pos-reg))
               (sig (cling-utils-reduce-string response))
               (res-type (cling-utils-repl-response-error-status sig)))

          (cl-case res-type
            ;; neither popup presentation buffer
            ;; nor insert response in error case
            ('unknown-command
             (message "This command requires GHCi 8+ or GHCi-ng. Please read command description for details."))
            ('option-missing
             (message "Could not infer type signature. You need to load file first. Also :set +c is required. Please read command description for details."))
            ('interactive-error (message "Wrong REPL response: %s" sig))
            (otherwise
             (if insert-value
                 ;; Only insert type signature and do not present it
                 (if (= (length cling-utils-async-post-command-flag) 1)
                     (if wrap
                         ;; Handle region case
                         (progn
                           (deactivate-mark)
                           (save-excursion
                             (delete-region min-pos max-pos)
                             (goto-char min-pos)
                             (insert (concat "(" sig ")"))))
                       ;; Non-region cases
                       (cling-command-insert-type-signature sig))
                   ;; Some commands registered, prevent insertion
                   (message "Type signature insertion was prevented. These commands were registered: %s"
                            (cdr (reverse cling-utils-async-post-command-flag))))
               ;; Present the result only when response is valid and not asked
               ;; to insert result
               (cling-command-echo-or-present response)))

            (cling-utils-async-stop-watching-changes init-buffer))))))))

(make-obsolete 'cling-process-generate-tags
               'cling-mode-generate-tags
               "2016-03-14")
(defun cling-process-generate-tags (&optional and-then-find-this-tag)
  "Regenerate the TAGS table.
If optional AND-THEN-FIND-THIS-TAG argument is present it is used with
function `xref-find-definitions' after new table was generated."
  (interactive)
  (let ((process (cling-interactive-process)))
    (cling-process-queue-command
     process
     (make-cling-command
      :state (cons process and-then-find-this-tag)
      :go
      (lambda (state)
        (let* ((process (car state))
               (cabal-dir (cling-session-cabal-dir
                           (cling-process-session process)))
               (command (cling-cabal--compose-hasktags-command cabal-dir)))
          (cling-process-send-string process command)))
      :complete (lambda (state _response)
                  (when (cdr state)
                    (let ((tags-file-name
                           (cling-session-tags-filename
                            (cling-process-session (car state)))))
                      (xref-find-definitions (cdr state))))
                  (cling-mode-message-line "Tags generated."))))))

(defun cling-process-add-cabal-autogen ()
  "Add cabal's autogen dir to the GHCi search path.
Add <cabal-project-dir>/dist/build/autogen/ to GHCi seatch path.
This allows modules such as 'Path_...', generated by cabal, to be
loaded by GHCi."
  (unless (or (eq 'cabal-repl (cling-process-type))
              (eq 'cabal-new-repl (cling-process-type))) ;; redundant with "cabal repl"
    (let*
        ((session       (cling-interactive-session))
         (cabal-dir     (cling-session-cabal-dir session))
         (ghci-gen-dir  (format "%sdist/build/autogen/" cabal-dir)))
      (cling-process-queue-without-filters
       (cling-interactive-process)
       (format ":set -i%s" ghci-gen-dir)))))

;;;###autoload
(defun cling-process-unignore ()
  "Unignore any ignored files.
Do not ignore files that were specified as being ignored by the
inferior GHCi process."
  (interactive)
  (let ((session (cling-interactive-session))
        (changed nil))
    (if (null (cling-session-get session 'ignored-files))
        (message "Nothing to unignore!")
      (cl-loop for file in (cling-session-get session 'ignored-files)
               do
               (cling-mode-toggle-interactive-prompt-state)
               (unwind-protect
                   (progn
                     (cl-case
                         (read-event
                          (propertize
                           (format "Set permissions? %s (y, n, v: stop and view file)"
                                   file)
                           'face
                           'minibuffer-prompt))
                       (?y
                        (cling-process-unignore-file session file)
                        (setq changed t))
                       (?v
                        (find-file file)
                        (cl-return)))
                     (when (and changed
                                (y-or-n-p "Restart GHCi process now? "))
                       (cling-process-restart)))
                 ;; unwind
                 (cling-mode-toggle-interactive-prompt-state t))))))

;;;###autoload
(defun cling-session-change-target (target)
  "Set the build TARGET for cabal REPL."
  (interactive
   (list
    (completing-read "New build target: "
                     (cling-cabal-enum-targets)
                     nil
                     nil
                     nil
                     'cling-cabal-targets-history)))
  (let* ((session cling-session)
         (old-target (cling-session-get session 'target)))
    (when session
      (cling-session-set-target session target)
      (when (not (string= old-target target))
        (cling-mode-toggle-interactive-prompt-state)
        (unwind-protect
            (when (y-or-n-p "Target changed, restart cling process?")
              (cling-process-start session)))
        (cling-mode-toggle-interactive-prompt-state t)))))

;;;###autoload
(defun cling-mode-stylish-buffer ()
  "Apply stylish-cling to the current buffer.

Use `cling-mode-stylish-cling-path' to know where to find
stylish-cling executable. This function tries to preserve
cursor position and markers by using
`cling-mode-buffer-apply-command'."
  (interactive)
  (cling-mode-buffer-apply-command cling-mode-stylish-cling-path))

(defun cling-mode-buffer-apply-command (cmd)
  "Execute shell command CMD with current buffer as input and output.
Use buffer as input and replace the whole buffer with the
output.  If CMD fails the buffer remains unchanged."
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "stylish-output"))
         (err-file (make-temp-file "stylish-error")))
        (unwind-protect
          (let* ((_errcode
                  (call-process-region (point-min) (point-max) cmd nil
                                       `((:file ,out-file) ,err-file)
                                       nil))
                 (err-file-empty-p
                  (equal 0 (nth 7 (file-attributes err-file))))
                 (out-file-empty-p
                  (equal 0 (nth 7 (file-attributes out-file)))))
            (if err-file-empty-p
                (if out-file-empty-p
                    (message "Error: %s produced no output and no error information, leaving buffer alone" cmd)
                  ;; Command successful, insert file with replacement to preserve
                  ;; markers.
                  (insert-file-contents out-file nil nil nil t))
              (progn
                ;; non-null stderr, command must have failed
                (message "Error: %s ended with errors, leaving buffer alone" cmd)
                (with-temp-buffer
                  (insert-file-contents err-file)
                  ;; use (warning-minimum-level :debug) to see this
                  (display-warning cmd
                                   (buffer-substring-no-properties (point-min) (point-max))
                                   :debug)))))
          (ignore-errors
            (delete-file err-file))
          (ignore-errors
            (delete-file out-file)))))

;;;###autoload
(defun cling-mode-find-uses ()
  "Find use cases of the identifier at point and highlight them all."
  (interactive)
  (let ((spans (cling-mode-uses-at)))
    (unless (null spans)
      (highlight-uses-mode 1)
      (cl-loop for span in spans
               do (cling-mode-make-use-highlight span)))))

(defun cling-mode-make-use-highlight (span)
  "Make a highlight overlay at the given SPAN."
  (save-window-excursion
    (save-excursion
      (cling-mode-goto-span span)
      (save-excursion
        (highlight-uses-mode-highlight
         (progn
           (goto-char (point-min))
           (forward-line (1- (plist-get span :start-line)))
           (forward-char (plist-get span :start-col))
           (point))
         (progn
           (goto-char (point-min))
           (forward-line (1- (plist-get span :end-line)))
           (forward-char (plist-get span :end-col))
           (point)))))))

(defun cling-mode-uses-at ()
  "Get the locations of use cases for the ident at point.
Requires the :uses command from GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (cling-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (let ((reply (cling-process-queue-sync-request
                    (cling-interactive-process)
                    (save-excursion
                      (format ":uses %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (let ((lines (split-string reply "\n" t)))
              (cl-remove-if
               #'null
               (mapcar (lambda (line)
                         (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                                           line)
                             (list :path (match-string 1 line)
                                   :start-line (string-to-number (match-string 2 line))
                                   ;; ;; GHC uses 1-based columns.
                                   :start-col (1- (string-to-number (match-string 3 line)))
                                   :end-line (string-to-number (match-string 4 line))
                                   ;; GHC uses 1-based columns.
                                   :end-col (1- (string-to-number (match-string 5 line))))
                           (error (propertize line 'face 'compilation-error))))
                       lines)))
          (error (propertize "No reply. Is :uses supported?"
                             'face 'compilation-error)))))))

(defun cling-command-echo-or-present (msg)
  "Present message in some manner depending on configuration.
If variable `cling-process-use-presentation-mode' is NIL it will output
modified message MSG to echo area."
  (if cling-process-use-presentation-mode
      (let ((session (cling-process-session (cling-interactive-process))))
        (cling-presentation-present session msg))
    (let ((m (cling-utils-reduce-string msg)))
      (message "%s" m))))

(defun cling-command-capture-expr-bounds ()
  "Capture position bounds of expression at point.
If there is an active region then it returns region
bounds.  Otherwise it uses `cling-spanable-pos-at-point` to
capture identifier bounds.  If latter function returns NIL this function
will return cons cell where min and max positions both are equal
to point."
  (or (when (region-active-p)
        (cons (region-beginning)
              (region-end)))
      (cling-spanable-pos-at-point)
      (cons (point) (point))))

(defun cling-command-insert-type-signature (signature)
  "Insert type signature.
In case of active region is present, wrap it by parentheses and
append SIGNATURE to original expression.  Otherwise tries to
carefully insert SIGNATURE above identifier at point.  Removes
newlines and extra whitespace in signature before insertion."
  (let* ((ident-pos (or (cling-ident-pos-at-point)
                        (cons (point) (point))))
         (min-pos (car ident-pos))
         (sig (cling-utils-reduce-string signature)))
    (save-excursion
      (goto-char min-pos)
      (let ((col (current-column)))
        (insert sig "\n")
        (indent-to col)))))

(provide 'cling-commands)
;;; cling-commands.el ends here
