(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (progn
	      (show-paren-mode)
	      ;;(when (require 'org)
		;;(orgstruct++-mode))
	      ;;(setq orgstruct-heading-prefix-regexp ";;")

	      (when (require 'which-func)
		(which-func-mode 1))

	      )))

(provide 'modes/elisp-mode.el)
