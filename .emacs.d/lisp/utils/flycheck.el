
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
	(projmake-search-load-project)
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

(provide 'utils/flycheck.el)

