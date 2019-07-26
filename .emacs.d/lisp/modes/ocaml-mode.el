
(and (require 'cl)

     (use-package tuareg
       :config
       (add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
       ;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
       (setq auto-mode-alist
	     (append '(("\\.ml[ily]?$" . tuareg-mode)
		       ("\\.topml$" . tuareg-mode))
		     auto-mode-alist)))

     ;; Merlin configuration
     (use-package merlin
       :config
       (add-hook 'tuareg-mode-hook 'merlin-mode)
       (add-hook 'merlin-mode-hook #'company-mode)
       (setq merlin-error-after-save nil))

     ;; utop configuration

     (use-package utop
       :config
       (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
       (add-hook 'tuareg-mode-hook 'utop-minor-mode)
       ))


(provide 'modes/ocaml-mode.el)
