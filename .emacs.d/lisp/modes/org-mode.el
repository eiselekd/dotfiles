
(add-hook 'org-mode-hook
          (lambda ()
	    (progn
	      (message "[+] org-version: '%s'" org-version)
	      (require 'ox)
	      (require 'ox-html)
	      (when (require 'ox-reveal nil t)
		(progn
		  
		  (setq org-reveal-root (concat  "file://" *.emacs.d.dir* "lisp/modes/reveal.js"  ))
		  (message "[+] ox-reveal: '%s'" org-reveal-root)
		  ))
	      (when (require 'ox-twbs nil t)
		(progn
		  (message "[+] ox-twbs")
		  )))))
	  
(provide 'modes/org-mode.el)
