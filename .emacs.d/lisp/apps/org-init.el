;; http://doc.norang.ca/org-mode.html

(add-hook 'after-init-hook (lambda ()
			     (progn
			       (require 'apps/org.el))))

(provide 'apps/org-init.el)
