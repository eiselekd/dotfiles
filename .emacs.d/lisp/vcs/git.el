
(defun vcs/magit-init ()
  "Initialize Magit."
  (message (format "[*] magit init"))
  )


(defun vcs/git-init ()
  "Initialize Git support."
  ;;; (Packages) ;;;
  (when (require 'magit nil 'noerror)
    (vcs/magit-init))
  )

(with-executable 'git
		 (vcs/git-init))

(provide 'vcs/git.el)
