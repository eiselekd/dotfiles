(require 'racket-mode)
(require 'racket-xp)

;; raco pkg install racket-langserver

(defun my-racket-mode-hook () "Hooks for Web mode."
       (progn
	 (message "[+] racket mode")
	 (flycheck-mode)
	 (flycheck-select-checker 'racket)
	 (message "[+] racket xp mode")
	 ;;(racket-language-server)
	 (require 'eldoc)
	 (require 'lsp-racket)
	 (require 'lsp-ui)
	 (require 'lsp-modeline)
	 (require 'company)
	 (require 'racket-mode)
	 (require 'racket-debug)

	 (company-mode)

	 (lsp)
	 (lsp-ui-mode)
	 (global-set-key (kbd "M-?")  'racket-xp-describe)

	 )
       )

(add-hook 'racket-mode-hook  'my-racket-mode-hook)

(provide 'modes/racket.el)
