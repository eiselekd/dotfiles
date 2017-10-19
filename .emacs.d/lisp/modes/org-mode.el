(defun ck/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "latex") (string= lang "plantuml"))))

(add-hook 'org-mode-hook
          (lambda ()
	    (progn
	      (message "[+] org-version: '%s'" org-version)
	      (require 'ox)
	      (require 'ox-html)
	      (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

	      ;; active Org-babel languages
	      (org-babel-do-load-languages
	       'org-babel-load-languages
	       '(;; other Babel languages
		 (plantuml . t)))

	      (setq org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)

	      (setq org-plantuml-jar-path (concat  *.emacs.d.dir* "lisp/modes/plantuml.jar"  ))
	      (message "[+] org-plantuml: '%s'" org-plantuml-jar-path)

	      ;; (setq org-plantuml-jar-path (shell-command-to-string "cygpath --window /home/eiselekd/.emacs.d/lisp/modes/plantuml.jar"))
	      
	      

	      
	      (global-set-key (kbd "S-<f9>")  'org-toggle-inline-images)
	      
	      (global-set-key (kbd "<f7>")  'org-beamer-export-to-pdf)
	      (global-set-key (kbd "<f8>")  'org-html-export-to-html)
	      
	      (when (require 'ox-reveal nil t)
		(progn
		  (setq org-reveal-root (concat  "file://" *.emacs.d.dir* "lisp/modes/reveal.js"  ))
		  (message "[+] ox-reveal: '%s'" org-reveal-root)
		  (global-set-key (kbd "S-<f7>")  'org-reveal-export-to-html)
		  ))
	      (when (require 'ox-twbs nil t)
		(progn
		  (message "[+] ox-twbs")
		  (global-set-key (kbd "S-<f8>")  'org-twbs-export-to-html)
		  ))

	      )))

(provide 'modes/org-mode.el)
