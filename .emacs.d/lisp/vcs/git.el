
(defun vcs/magit-init ()
  "Initialize Magit."
  (message (format "[*] magit init"))
  )


(defun vcs/git-init ()
  "Initialize Git support."
  
  (when (require 'magit nil t)
    (vcs/magit-init))
  )

(with-executable 'git
		 (vcs/git-init))

(provide 'vcs/git.el)
