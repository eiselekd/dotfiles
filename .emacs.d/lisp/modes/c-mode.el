
(add-hook 'c++-mode-hook
	  (lambda ()
	    (progn (add-to-list 'write-file-functions 'delete-trailing-whitespace)
		   (ggtags-mode)
		   )))
(add-hook 'c-mode-hook
	  (lambda ()
	    (progn
	      (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	      (ggtags-mode)
	      )))

(provide 'modes/c-mode.el)
