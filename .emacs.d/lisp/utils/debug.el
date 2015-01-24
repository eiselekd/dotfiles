(defun utils/debug ()
  "Debug current context."
  (interactive)
  (progn
    (cond
     ((string-match "\.pl$" (buffer-name)) (call-interactively 'perldb))
     (t (call-interactively 'gud-gdb)))))

(defun utils/debug-read-config (gud-config)
  (with-temp-buffer
    (insert-file-contents gud-config)
    (let* ((v nil)
	   (a (split-string (buffer-string) "\n" t)))
      (setq v (nth 0 a))
      (message "Reading gud-gdb file: [%s]" f )
      v
      )
    ))

(defun utils/debug-find-configure ()
  (let* ((source-dir (file-name-directory (buffer-file-name)))
	 (gud-config-dir (locate-dominating-file source-dir "gud-gdb.txt")))
    (if gud-config-dir
	(let* ((f (concat gud-config-dir "gud-gdb.txt")))
	  (progn
	    (message (format "Using gud-gdb file: [%s]" f))
	    (utils/debug-read-config f)
	    ))
      (file-name-nondirectory buffer-file-name)
      )))

;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-10/msg00577.html
(defadvice gud-gdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline
	  'gud-gdb (utils/debug-find-configure)))))

(add-hook 'after-init-hook 'utils/debug-keybind)
(add-hook 'perldb-mode-hook 'utils/debug-perl-keybind)

(provide 'utils/debug.el)
