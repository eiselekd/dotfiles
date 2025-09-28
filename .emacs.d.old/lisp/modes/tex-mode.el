
(add-hook 'tex-mode-hook
          (lambda ()
	    (progn
	      (message "[+] tex-mode" )
	      (when (require 'latex-preview-pane nil t)
		(progn
		  (message "[+] latex-preview-pane loaded" )
		  (latex-preview-pane-enable)
		  )))))

(provide 'modes/tex-mode.el)
