
(defun utils/compilation-mode-hook ()
  "Cofind-gmpilation mode hook.")


(defun utils/compilation-filter-hook ()
  "Hook for filtering compilation output."
  ;; Temporarily make buffer writable.
  (let ((inhibit-read-only t))
    ;; Colorize compilation output.
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun utils/ede-project (path)
  "Get the EDE project for PATH."
  (with-feature 'ede/cpp-root
    (let ((project (ede-current-project (expand-file-name path))))
      (when (and (featurep 'ede/cpp-root)
                 (ede-cpp-root-project-p project))
        project))))

(defun utils/compile ()
  "Compile current context."
  (interactive)
  (let ((ede-proj (utils/ede-project (path-abs-buffer))))
    (cond
     (ede-proj (project-compile-project ede-proj
                                        (read-string "Build command: "
                                                     (oref ede-proj compile-command))))
     ((fboundp 'mode-compile) (call-interactively 'mode-compile))
     (t (call-interactively 'compile)))))


(defun utils/mode-compile-init ()
  "Initialize mode-compile."
  (setq-default
   ;; Set a sane compilation frame name.
   mode-compile-other-frame-name "*compilation*"
   ;; Run make with low priority and use multiple processes.
   mode-compile-make-program "nice make"
   mode-compile-default-make-options "-k -j")

  (after-load 'mode-compile
    (with-executable 'clang
      (add-to-list 'cc-compilers-list "clang")
      (add-to-list 'c++-compilers-list "clang++"))))


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

  (if (file-exists-p *.cedet-root.el*)
      (progn 
	(load-file *.cedet-root.el*)
	(when (require 'ede nil t)
	  (message "[*] cedet loaded"))
	)
    )
  (when (require 'mode-compile nil t)
    (utils/mode-compile-init))
  )

(defun utils/compile-keybind ()
    (global-set-key (kbd "<f10>") 'utils/compile))
  
(utils/compile-init)

(provide 'utils/compile.el)
