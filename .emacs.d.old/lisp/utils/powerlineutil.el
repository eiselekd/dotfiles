(global-set-key
 (kbd "M-P")
 (lambda ()(interactive)
   (progn
     (require 'powerline) ;; status line
     (powerline-default-theme)
     (setq powerline-default-separator 'arrow))))

(provide 'utils/powerlineutil.el)
