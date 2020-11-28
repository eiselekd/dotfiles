(require 'racket-mode)

(defun my-racket-mode-hook () "Hooks for Web mode."
       (progn
	 (message "[+] racket mode")


	 (flycheck-mode)
	 (flycheck-select-checker 'racket)

	 )
       )

(add-hook 'racket-mode-hook  'my-racket-mode-hook)

(provide 'modes/racket.el)
