
(defun modes/ocaml-mode-safe-vars ()
  (interactive)
  (progn
    (message "[+] set safe variables for ocaml")
    (put 'compilation-read-command 'safe-local-variable #'booleanp)
    (put 'buffer-file-coding-system 'safe-local-variable (lambda (_) t))
  ))

(defun modes/ocaml-mode-start ()
  (progn
    (and
     (require 'cl)
     (require 'tuareg)
     (require 'merlin)
     (require 'utop)

     (modes/ocaml-mode-safe-vars)

     (add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
     (add-hook 'tuareg-mode-hook 'merlin-mode)
     ;;(add-hook 'merlin-mode-hook #'company-mode)
     (setq merlin-command "ocamlmerlin")
     (setq merlin-error-after-save nil))

    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)

    (tuareg-mode)

    (local-set-key (kbd "ESC .") (lambda () (interactive)
				   (progn
				     (xref-push-marker-stack)
				     (call-interactively 'merlin-locate))))

    ;;(require 'flycheck-ocaml)
    (setq merlin-error-after-save 't )

    (require 'ocamldebug)

    ))

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . modes/ocaml-mode-start)
		("\\.topml$" . modes/ocaml-mode-start))
	      auto-mode-alist))

(provide 'modes/ocaml-mode.el)
