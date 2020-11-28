(require 'racket-mode)

(defun my-racket-mode-hook () "Hooks for Web mode."
       (progn
	 (message "[+] racket mode")
	 )
       )

(add-hook 'racket-mode-hook  'my-racket-mode-hook)

(provide 'modes/racket.el)
