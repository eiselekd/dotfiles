(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (progn

	      (when (require 'which-func)
		(which-func-mode 1))
	      )))

(provide 'modes/elisp-mode.el)
