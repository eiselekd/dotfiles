;; note: http://www.emacswiki.org/emacs/CompileCommand

(defun utils/compilation-mode-hook ()
  "Cofind-gmpilation mode hook.")

(defun utils/compilation-filter-hook ()
  "Hook for filtering compilation output."
  ;; Temporarily make buffer writable.
  (let ((inhibit-read-only t))
    ;; Colorize compilation output.
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun utils/compile ()
  "Compile current context."
  (interactive)
  (let* ((c (utils/mode-compile-get-command)))
    (if (get-buffer "*compilation*") ; If old compile window exists
	(progn 
	  (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
	  (kill-buffer "*compilation*") ; and kill the buffers
	  )
      )
    (cond
     (c
      (progn
	(set 'compile-command c)
	(call-interactively 'compile)))
     ;;((fboundp 'mode-compile) (call-interactively 'mode-compile))
     (t (call-interactively 'compile)))))

(defun utils/compile-read-config (config)
  (with-temp-buffer
    (insert-file-contents config)
    (let* ((v nil)
	   (a (split-string (buffer-string) "\n" t)))
      (nth 0 a)
      )))

(defun utils/mode-compile-get-command ()
  (let* ((source-dir (eval default-directory)) ;;(file-name-directory (buffer-file-name)))
	 (gud-config-dir (locate-dominating-file source-dir "compile.txt")))
    (message "c-mode-common-hook, search for compile.txt in %s: %s" source-dir gud-config-dir )
    (if gud-config-dir
	(progn 
	  (message "Reading compile file: [%s]" gud-config-dir )
	  (let* ((f (concat gud-config-dir "compile.txt")))
	    (message "Reading compile file: [%s]" (utils/compile-read-config f) )
	    (utils/compile-read-config f))))))

(defun utils/mode-compile-init ()
  "Initialize mode-compile."

  (message "[*] Initialize mode-compile" )
  
  (setq-default
   ;; Set a sane compilation frame name.
   mode-compile-other-frame-name "*compilation*"
   ;; Run make with low priority and use multiple processes.
   mode-compile-make-program "nice make"
   mode-compile-default-make-options "-k -j")

  (add-hook
   'c-mode-hook
   (lambda ()
     (let* ((c (utils/mode-compile-get-command)))
       (if c
	   (progn
	     (message "Reading compile file: [%s]" c )
	     (set (make-local-variable 'compile-command) c))))))

  
	   ;;     if ( )
	       
	   
	   ;; (file-name-nondirectory buffer-file-name)
	   ;; ))))
  

	      
	   ;;    (unless (or (file-exists-p \"makefile\")
	   ;; 		  (file-exists-p \"Makefile\"))
	   ;; 	(set (make-local-variable 'compile-command)
	   ;; 	     (concat \"make -k \"
	   ;; 		     (if buffer-file-name
	   ;; 			 (shell-quote-argument
	   ;; 			  (file-name-sans-extension buffer-file-name))))))))
  
  ;; (after-load 'mode-compile
  ;;   (with-executable 'clang
  ;;     (add-to-list 'cc-compilers-list "clang")
  ;;     (add-to-list 'c++-compilers-list "clang++")))
  )

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory. looking for a file with name file-to-find.  Returns the path to it or nil if not found."
  (cl-labels
      ((find-file-r
	(path)
	(let* ((parent (file-name-directory path))
	       (possible-file (concat parent file-to-find)))
	  (cond
	   ((file-exists-p possible-file) possible-file) ; Found
	   ;; The parent of ~ is nil and the parent of / is itself.
	   ;; Thus the terminating condition for not finding the file
	   ;; accounts for both.
	   ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
	   (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(defun project-root ()
  (let ((my-tags-file (find-file-upwards "COMPILE_CMD")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))
  
;;(concat "cd " (project-root) " && scons")

(defun utils/next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun utils/previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
    )

(defun utils/compile-init ()
  "Initialize compile module."
  (message "")
  (setq-default
   ;; Prevent input in compilation buffer.
   compilation-disable-input nil
   ;; Automatically scroll output.
   compilation-scroll-output t
   ;; Save the current buffer on compilation.
   mode-compile-always-save-buffer-p t)

  ;; Add compilation mode hook.
  (add-hook 'compilation-mode-hook 'utils/compilation-mode-hook)
  ;; Add compilation filter hook.
  (add-hook 'compilation-filter-hook 'utils/compilation-filter-hook)

  ;; (if (file-exists-p *.cedet-root.el*)
  ;;     (progn 
  ;; 	(load-file *.cedet-root.el*)
  ;; 	(when
  ;; 	    (and
  ;; 	     (require 'ede nil t)
  ;; 	     (require 'eieio nil t))
  ;; 	  (progn
  ;; 	    (global-ede-mode t)
  ;; 	    (message "[*] cedet loaded"))
  ;; 	  )
  ;; 	)
  ;;   )
  (when (require 'mode-compile nil t)
    (utils/mode-compile-init))
  )

(utils/compile-init)

(provide 'utils/compile.el)
