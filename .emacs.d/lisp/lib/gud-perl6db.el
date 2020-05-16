(require 'gud)
(require 'rx)

;;;###autoload
(defun perl6db (command-line)
  "Run perl6db on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'perl6-debug-m
			    (concat (or (buffer-file-name) "-e 0") " "))))

  (gud-common-init command-line 'gud-perl6db/massage-args
		   'gud-perl6db/marker-filter)
  (add-hook 'comint-output-filter-functions
            'gud-perl6db/after-insert-in-gud-buffer t t)

  (set (make-local-variable 'gud-minor-mode) 'perl6db)

  (gud-def gud-break  "bp add %f %l" "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "bp rm %f %l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   ""             "\C-s" "Step one source line with display.")
  (gud-def gud-next   "s"            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "r"            "\C-r" "Run until the next breakpoint or an exception is thrown.")
  (gud-def gud-print  "p %e"          "\C-p" "Evaluate perl expression at point.")


  (setq comint-prompt-regexp "^\\([>]\\|- .*[?]\\) ")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'perl6db/mode-hook))

(defun gud-perl6db/massage-args (_file args)
  "Convert a command line as would be typed normally to run perldb
into one that invokes an Emacs-enabled debugging session."
  args)

(defvar-local gud-perl6db/last-prompt-marker nil
  "Position of the last seen perl6db prompt in the current GUD buffer.")

(defun gud-perl6db/marker-filter (string)
  "Called by GUD to find source file positions in the debugger output.
Because there is no guarantee on how many debugger output lines (or line
fragments) are contained in STRING, many implementations of FOO-marker-filter
attempt to coalesce the STRING values across multiple calls into some kind of
private accumulator variable. This design is, to put it kindly, somewhat
misguided. Instead, do the matching in a comint hook, after STRING has been
inserted into the GUD buffer."
  ;; See the call to add-hook in `perl6db'
  string)

;; https://emacs.stackexchange.com/a/18884/11005
(defvar gud-perl6db/ansi-escape-re
  '(seq (or ?\233 (and ?\e ?\[))
        (zero-or-more (char (?0 . ?\?)))
        (zero-or-more (char ?\s ?- ?\/))
        (char (?@ . ?~))))

(defun gud-perl6db/after-insert-in-gud-buffer (_string)
  "Find and mark the latest source file position in the debugger output."
  (save-excursion
    (cond
     ((and gud-perl6db/last-prompt-marker
           (marker-position gud-perl6db/last-prompt-marker))
      (goto-char (marker-position gud-perl6db/last-prompt-marker)))
     (t (goto-char (point-min))))
    ; (message (concat "Looking at " (buffer-substring (point) (point-max))))
    (when (re-search-forward
           (rx (and line-start
                    (zero-or-more (eval gud-perl6db/ansi-escape-re))
                    "+" (1+ space)
                    (group-n 1 (1+ (not space)))
                    (0+ space) "("
                    (group-n 2 (1+ digit))))
           nil t)
      (let ((filename (match-string 1))
            (line (string-to-number (match-string 2))))
        ; (message (format "Found file: %s, line: %d" filename line))
        (setq gud-last-frame (cons filename line)))
      (setq gud-perl6db/last-prompt-marker (point-marker)))))

(provide 'gud-perl6db.el)
