(require 'gud)
(require 'rx)
(require 'f)

(defcustom nqp-exec-path "nqp-m"
  "Nqp executable path."
  :type 'string
  :group 'raku)

(defcustom nqp-exec-arguments "--debug-port=9999 --debug-suspend"
  "Nqp command line arguments."
  :type 'string
  :group 'raku)

(defvar nqp-buffer-name "NQP prog"
  "Buffer name for slave nqp prog to connect to.")

(defvar gudnqp/dirs '( "~/git/rakudo/nqp" "/tmp/b" )
  "List of dirs to search for source files." )

(defun gudnqp/searchf (p)
  (cond
   ((f-absolute? p)  (f-full p))
   ((f-exists? p) p)
   (t (let* ((retvalue nil))
	(dolist (e gudnqp/dirs retvalue)
	  (let* ((lp (f-join (f-full e) p)))
	    (if (f-exists? lp)
		(setq retvalue lp))))))))

(defun nqpdb (command-line)
  "Run nqpdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'moar-remote "9999" )))

  (run-nqp)

  (gud-common-init command-line 'gud-nqpdb/massage-args
		   'gud-nqpdb/marker-filter)
  (add-hook 'comint-output-filter-functions
            'gud-nqpdb/after-insert-in-gud-buffer t t)

  (set (make-local-variable 'gud-minor-mode) 'nqpdb)

  (gud-def gud-break  "breakpoint \"%f\" %l 1 1"          "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "clearbp \"%f\" %l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step into"         "\C-s" "Step one source line with display.")
  (gud-def gud-next   "step over"         "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "resume"            "\C-r" "Run until the next breakpoint or an exception is thrown.")
  ;;(gud-def gud-print  "p %e"            "\C-p" "Evaluate perl expression at point.")


  (setq comint-prompt-regexp "^\\([>]\\|- .*[?]\\) ")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'nqpdb/mode-hook))

(defun run-nqp ()
  "Run an inferior instance of `raku' inside Emacs."
  (interactive)
  (let* ((raku-program nqp-exec-path)
         (check-proc (comint-check-proc nqp-buffer-name))
         (buffer (apply 'make-comint-in-buffer
                        nqp-buffer-name
                        check-proc
                        nqp-exec-path
                        '()
                        (append (split-string nqp-exec-arguments) `( ,(buffer-file-name) ) ))))
    ;;(with-current-buffer buffer
    ;;  (raku-repl-mode))
    (display-buffer buffer)))

;; (append (split-string "a b c") `( ,(buffer-file-name) ) )


(defun gud-nqpdb/massage-args (_file args)
  "Convert a command line as would be typed normally to run perldb
into one that invokes an Emacs-enabled debugging session."
  args)

(defvar-local gud-nqpdb/last-prompt-marker nil
  "Position of the last seen nqpdb prompt in the current GUD buffer.")

(defun gud-nqpdb/marker-filter (string)
  "Called by GUD to find source file positions in the debugger output.
Because there is no guarantee on how many debugger output lines (or line
fragments) are contained in STRING, many implementations of FOO-marker-filter
attempt to coalesce the STRING values across multiple calls into some kind of
private accumulator variable. This design is, to put it kindly, somewhat
misguided. Instead, do the matching in a comint hook, after STRING has been
inserted into the GUD buffer."
  ;; See the call to add-hook in `nqpdb'
  string)

;; https://emacs.stackexchange.com/a/18884/11005
(defvar gud-nqpdb/ansi-escape-re
  '(seq (or ?\233 (and ?\e ?\[))
        (zero-or-more (char (?0 . ?\?)))
        (zero-or-more (char ?\s ?- ?\/))
        (char (?@ . ?~))))

(defun gud-nqpdb/after-insert-in-gud-buffer (_string)
  "Find and mark the latest source file position in the debugger output."
  (save-excursion
    (cond
     ((and gud-nqpdb/last-prompt-marker
           (marker-position gud-nqpdb/last-prompt-marker))
      (goto-char (marker-position gud-nqpdb/last-prompt-marker)))
     (t (goto-char (point-min))))
    ; (message (concat "Looking at " (buffer-substring (point) (point-max))))
    (when (re-search-forward

           (rx (and line-start
		    (1+ space) "0"
		    (1+ space)
		    (1+ (not space))
		    (1+ space)
		    (group-n 1 (1+ (not ":")))
		    ":"
		    (group-n 2 (1+ digit))))
           nil t)

      (let* ((filename (match-string 1))
	     (rfilename (gudnqp/searchf filename))
	     (line (string-to-number (match-string 2))))
        (message (format "Found file: %s, line: %d, found: '%s'" filename line rfilename))

        (setq gud-last-frame (cons rfilename line))
	;;(gud-display-line rfilename line)

	)
      (setq gud-nqpdb/last-prompt-marker (point-marker)))))

(provide 'gud-nqpdb.el)
