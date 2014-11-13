
(defun utils/flycheck-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
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


(after-load 'flymake
  (defadvice flymake-simple-make-init
      (around advice-flymake-simple-make-init)
    (ad-set-arg 1 nil)
    (ad-set-arg 2 nil)
    ad-do-it))
  

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

