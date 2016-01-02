
(defun vcs/magit-init ()
  "Initialize Magit."
  (message (format "[*] %s magit init" (timestamp_str)))
  )


(defun vcs/git-init ()
  "Initialize Git support."
  
  (when (require 'magit nil t)
    (vcs/magit-init))
  )

;; switch to load on M-g
;;(with-executable 'git
;;		 (vcs/git-init))

(provide 'vcs/git.el)
