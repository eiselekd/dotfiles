
(add-hook 'org-mode-hook
          (lambda ()
	    (progn
	      (message "[+] org-version: '%s'" org-version)
	      (require 'ox)
	      (require 'ox-html)
	      (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

	      (when (require 'ox-reveal nil t)
		(progn
		  
		  (setq org-reveal-root (concat  "file://" *.emacs.d.dir* "lisp/modes/reveal.js"  ))
		  (message "[+] ox-reveal: '%s'" org-reveal-root)
		  
		  (global-set-key (kbd "<f7>")  'org-beamer-export-to-pdf)
		  (global-set-key (kbd "S-<f7>")  'org-reveal-export-to-html)
		  (global-set-key (kbd "<f8>")  'org-html-export-to-html)
		  (global-set-key (kbd "S-<f8>")  'org-twbs-export-to-html)
		  
		  ))
	      (when (require 'ox-twbs nil t)
		(progn
		  (message "[+] ox-twbs")
		  )))))
	  
(provide 'modes/org-mode.el)
