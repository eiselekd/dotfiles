;; http://emacswiki.org/emacs/ElispCookbook#toc29
;;(utils/flycheck-search-linux-makefile)

(defun utils/flycheck-search-linux-makefile ()
  "Search for linux top `Makefile' "
  (labels
      ((find-makefile-file-r (path)
	(let* ((parent (file-name-directory path))
	       (file (concat parent "Makefile")))
	  (cond
	   ((file-exists-p file)
	    (progn
	      (with-temp-buffer
		(insert-file-contents file)
		(if (string-match "VERSION = [0-9]+[[:space:]]*PATCHLEVEL" (buffer-string))
		    (throw 'found-it parent)
		  (find-makefile-file-r (directory-file-name parent))
		  ))))
	   ((equal path parent) (throw 'found-it nil))
	   (t (find-makefile-file-r (directory-file-name parent)))))))
    (if (buffer-file-name)
        (catch 'found-it 
          (find-makefile-file-r (buffer-file-name)))
      (error "buffer is not visiting a file"))))

(flycheck-define-checker utils/flycheck-linux-makefile-checker
  "Linux source checker"
  :command
  (
   "make" "C=1" "-C" (eval (utils/flycheck-search-linux-makefile))
   (eval (concat (file-name-sans-extension (file-relative-name buffer-file-name (utils/flycheck-search-linux-makefile))) ".o"))
   )
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
	(let* ((fn (flycheck-error-filename err))
	       (rn0 (file-relative-name fn default-directory)) ; flycheck-fix-error-filename converted to absolute, revert
	       (rn1 (expand-file-name rn0 (utils/flycheck-search-linux-makefile))) ; make absolute relative to "make -C dir"
	       (ef (file-relative-name rn1 default-directory)) ; relative to source
	       )
	  (setf (flycheck-error-filename err) ef)
	  )))
    errors)
  :modes (c-mode c++-mode)
  )

;;'(flycheck-c/c++-gcc-executable "/usr/local/bin/gcc-4.9")
;;make-local-variable

(defun utils/flycheck-mode-hook ()
  "Flycheck mode hook."
  (make-variable-buffer-local 'flycheck-linux-makefile)
  (setq flycheck-linux-makefile (utils/flycheck-search-linux-makefile))
  (if flycheck-linux-makefile
      (flycheck-select-checker 'utils/flycheck-linux-makefile-checker))
  (utils/flycheck-local-keybind ))

(defun utils/flymake-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
  (utils/flymake-local-keybind ))

(defun utils/projmake-start ()
  "Projmake start."
  ;;; (Bindings) ;;;
  (interactive)
  (progn
    (when (require 'projmake-mode nil t)
      (progn 
	(projmake-mode)
	(projmake-discover/search-load-project)
	(projmake-mode/on)
	(utils/projmake-local-keybind )))))

(defun utils/flycheck-color-mode-line-init ()
  "Initialize flycheck color mode."
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; "Initialize flycheck."
(defun utils/flycheck-init ()
  "Initialize flymake"
  (setq-default
   ;; Wait five seconds before starting checker
   flycheck-idle-change-delay 1)
  
  ;; Enable flycheck globally.
  ;;(global-flycheck-mode t)
  
  (after-load 'popwin
    ;; Use popwin for Flycheck error list.
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config))

  (after-load 'popwin
    (push '(flymode-error-list-mode :stick t) popwin:special-display-config))

  
  (add-hook 'flycheck-mode-hook 'utils/flycheck-mode-hook)
  (add-hook 'flymake-mode-hook 'utils/flymake-mode-hook)
  (message "[*] Flycheck hook setup")
  )

(add-hook 'after-init-hook 'utils/flycheck-init)

  

;; (defadvice flymake-find-buildfile
;;     (around advice-find-makefile-separate-obj-dir
;; 	    activate compile)
  ;; "Look for buildfile in a separate build directory"
  ;; (let* ((source-dir (ad-get-arg 1))
  ;; 	 (bld-dir (ac-build-dir source-dir)))
  ;;   (ad-set-arg 1 bld-dir)
  ;;   ad-do-it))

;; (defun ac-find-configure (source-dir)
;;   (locate-dominating-file source-dir "configure"))

;; (defvar project-build-root nil
;;   "top build directory of the project")

;; (defun ac-build-dir (source-dir)
;;   "find the build directory for the given source directory"
;;   (condition-case nil
;;       (let* ((topdir (ac-find-configure source-dir))
;; 	     (subdir (file-relative-name (file-name-directory source-dir) topdir))
;; 	     (blddir (concat (file-name-as-directory project-build-root) subdir)))
;; 	blddir)
;;         (error source-dir)))

(provide 'utils/flycheck.el)

