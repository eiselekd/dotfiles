;;; cling-repl.el --- REPL evaluation -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

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
(require 'cling-interactive-mode)

(defun cling-interactive-handle-expr ()
  "Handle an inputted expression at the REPL."
  (let ((expr (cling-interactive-mode-input)))
    (if (string= "" (replace-regexp-in-string " " "" expr))
        ;; Just make a new prompt on space-only input
        (progn
          (goto-char (point-max))
          (insert "\n")
          (cling-interactive-mode-prompt))
      (when (cling-interactive-at-prompt)
        (cond
         ;; If already evaluating, then the user is trying to send
         ;; input to the REPL during evaluation. Most likely in
         ;; response to a getLine-like function.
         ((and (cling-process-evaluating-p (cling-interactive-process))
               (= (line-end-position) (point-max)))
          (goto-char (point-max))
          (let ((process (cling-interactive-process))
                (string (buffer-substring-no-properties
                         cling-interactive-mode-result-end
                         (point))))
            ;; here we need to go to end of line again as evil-mode
            ;; might have managed to put us one char back
            (goto-char (point-max))
            (insert "\n")
            ;; Bring the marker forward
            (setq cling-interactive-mode-result-end
                  (point-max))
            (cling-process-set-sent-stdin process t)
            (cling-process-send-string process string)))
         ;; Otherwise we start a normal evaluation call.
         (t (setq cling-interactive-mode-old-prompt-start
                  (copy-marker cling-interactive-mode-prompt-start))
            (set-marker cling-interactive-mode-prompt-start (point-max))
            (cling-interactive-mode-history-add expr)
            (cling-interactive-mode-do-expr expr)))))))

(defun cling-interactive-mode-do-expr (expr)
  (cond
   ((string-match "^:present " expr)
    (cling-interactive-mode-do-presentation (replace-regexp-in-string "^:present " "" expr)))
   (t
    (cling-interactive-mode-run-expr expr))))

(defun cling-interactive-mode-run-expr (expr)
  "Run the given expression."
  (let ((session (cling-interactive-session))
        (process (cling-interactive-process)))
    (cling-process-queue-command
     process
     (make-cling-command
      :state (list session process expr 0)
      :go (lambda (state)
            (goto-char (point-max))
            (insert "\n")
            (setq cling-interactive-mode-result-end
                  (point-max))
            (cling-process-send-string (cadr state)
                                         (cling-interactive-mode-multi-line (cl-caddr state)))
            (cling-process-set-evaluating (cadr state) t))
      :live (lambda (state buffer)
              (unless (and (string-prefix-p ":q" (cl-caddr state))
                           (string-prefix-p (cl-caddr state) ":quit"))
                (let* ((cursor (cl-cadddr state))
                       (next (replace-regexp-in-string
                              cling-process-prompt-regex
                              ""
                              (substring buffer cursor))))
                  (cling-interactive-mode-eval-result (car state) next)
                  (setf (cl-cdddr state) (list (length buffer)))
                  nil)))
      :complete
      (lambda (state response)
        (cling-process-set-evaluating (cadr state) nil)
        (unless (cling-interactive-mode-trigger-compile-error state response)
          (cling-interactive-mode-expr-result state response)))))))

(defun cling-interactive-mode-expr-result (state response)
  "Print the result of evaluating the expression."
  (let ((response
         (with-temp-buffer
           (insert response)
           (cling-interactive-mode-handle-h)
           (buffer-string))))
    (when cling-interactive-mode-eval-mode
      (unless (cling-process-sent-stdin-p (cadr state))
        (cling-interactive-mode-eval-as-mode (car state) response))))
  (cling-interactive-mode-prompt (car state)))

(defun cling-interactive-mode-eval-as-mode (session text)
  "Insert TEXT font-locked according to `cling-interactive-mode-eval-mode'."
  (with-current-buffer (cling-session-interactive-buffer session)
    (let ((inhibit-read-only t))
      (delete-region (1+ cling-interactive-mode-prompt-start) (point))
      (goto-char (point-max))
      (let ((start (point)))
        (insert (cling-fontify-as-mode text
                                         cling-interactive-mode-eval-mode))
        (when cling-interactive-mode-collapse
          (cling-collapse start (point)))))))

(provide 'cling-repl)
