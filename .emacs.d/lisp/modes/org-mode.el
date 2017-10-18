
(add-hook 'org-mode-hook
          (lambda ()
	    
	    (when (require 'ox-twbs nil t)
	      (progn
		(message "[+] ox-twbs")
		))))

(provide 'modes/org-mode.el)
