(defun utils/debug ()
  "Debug current context."
  (interactive)
  (progn
    (cond
     ((string-match "\.pl$" (buffer-name))
      (progn
	;; note: need chmod ag-w ~/.perldb
	;; DB::emacs need to be loadable
	(when (and
	       (require 'gud nil t)
	       (require 'perldb-ui-ex nil t)
	       )
	  (call-interactively 'perldb-ui)
	  )))
     ((string-match "\.hs$" (buffer-name)) (call-interactively 'haskell-debug))
     ((string-match "\.ml$" (buffer-name)) (call-interactively 'modes/ocaml-start-debug))
     (t
      (progn
	(custom-set-variables '(gud-gdb-command-name "gdb -i=mi"))
	(call-interactively 'gdb (utils/debug-find-configure))
	)))))

(defun utils/debug-read-config (gud-config)
  (with-temp-buffer
    (insert-file-contents gud-config)
    (let* ((v nil)
	   (a (split-string (buffer-string) "\n" t)))
      (setq v (nth 0 a)) (message "Reading gdb file: [%s]" f )
      v
      )
    ))

(defun utils/debug-locate-dominating-file ()
  (let* ((source-dir (eval default-directory))) ;;(file-name-directory (buffer-file-name)))
    (locate-dominating-file source-dir "gdb.txt")))

(defun utils/debug-find-configure ()
  (let* ((gud-config-dir (utils/debug-locate-dominating-file)))
    (if gud-config-dir
	(progn
	  (let* ((f (concat gud-config-dir "gdb.txt")))
	    (concat " -i=mi --init-command=" f)))
      (if buffer-file-name
	  (concat " -i=mi " (file-name-nondirectory buffer-file-name))
	" -i=mi "
      ))))

;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-10/msg00577.html
(defadvice gdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline
	  'gud-gdb (utils/debug-find-configure))))
  (if (utils/debug-locate-dominating-file)
      (set 'default-directory (utils/debug-locate-dominating-file))))

(add-hook 'after-init-hook  'utils/debug-keybind)
(add-hook 'gud-mode-hook    'utils/debug-gud-keybind)
(add-hook 'perldb-mode-hook 'utils/debug-gud-keybind)

(setq gdb-many-windows t)

(defadvice gud-display-line (after gud-display-line-centered activate)
  "Center the line in the window"
  (when (and gud-overlay-arrow-position gdb-source-window)
    (with-selected-window gdb-source-window
      ; (marker-buffer gud-overlay-arrow-position)
      (save-restriction
        (goto-line (ad-get-arg 1))
        (recenter)))))




(provide 'utils/debug.el)
