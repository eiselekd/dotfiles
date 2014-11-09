
(defun user/magit-init ()
  "Initialize Magit."
  (message "")
  )


(defun vcs/git-init ()
  "Initialize Git support."
  ;;; (Packages) ;;;
  (when (require 'magit nil 'noerror)
    (user/magit-init))
  )

(with-executable 'git
		 (vcs/git-init))

(provide 'vcs/git.el)
