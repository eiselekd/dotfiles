(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (progn
	      (show-paren-mode)
	      (orgstruct++-mode)
	      (setq orgstruct-heading-prefix-regexp ";;")

	      (when (require 'which-func)
		(which-func-mode 1))

	      )))

(provide 'modes/elisp-mode.el)
