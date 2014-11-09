
(defun utils/flycheck-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
  (utils/flycheck-lokal-keybind ))

(defun utils/flycheck-color-mode-line-init ()
  "Initialize flycheck color mode."
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(defun utils/flycheck-init ()
  "Initialize flycheck."
  (setq-default
   ;; Wait five seconds before starting checker
   flycheck-idle-change-delay 0.2)

  ;; Enable flycheck globally.
  (global-flycheck-mode t)
  
  (after-load 'popwin
    ;; Use popwin for Flycheck error list.
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config))

  (add-hook 'flycheck-mode-hook 'utils/flycheck-mode-hook))

(when (require 'flycheck nil t)
  (utils/flycheck-init))

(provide 'utils/flycheck.el)
