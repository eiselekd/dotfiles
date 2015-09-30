
(add-hook 'c++-mode-hook
	  (lambda ()
	    (progn (add-to-list 'write-file-functions 'delete-trailing-whitespace)
		   (if (eq system-type 'cygwin) ;; use helm for !=cygwin
		       (ggtags-mode))
		   (global-set-key (kbd "M-(")  'hs-hide-block)
		   (global-set-key (kbd "M-)")  'hs-show-block)

		   (hs-org/minor-mode)
		   (global-set-key (kbd "M-h")  'hs-org/minor-mode)

		   )))

(add-hook 'c-mode-hook
	  (lambda ()
	    (progn

	      (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	      (if (eq system-type 'cygwin) ;; use helm for !=cygwin
		  (ggtags-mode))

	      (hs-org/minor-mode)
	      (global-set-key (kbd "M-h")  'hs-org/minor-mode)

	      )))

(provide 'modes/c-mode.el)
