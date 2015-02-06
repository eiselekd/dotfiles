(defun utils/debug ()
  "Debug current context."
  (interactive)
  (progn
    (cond
     ((string-match "\.pl$" (buffer-name)) (call-interactively 'perldb))
     (t
      (progn
	(custom-set-variables '(gud-gdb-command-name "gdb -i=mi"))
	(call-interactively 'gdb (utils/debug-find-configure)))))))

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
      (file-name-nondirectory buffer-file-name)
      )))

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

(provide 'utils/debug.el)
