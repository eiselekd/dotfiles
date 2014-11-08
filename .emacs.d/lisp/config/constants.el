(defun getenv-or (env value)
  (if (getenv env)
      (getenv env)
    value))

(defun path-join (root &rest dirs)
  (if (not dirs)
      root
    (apply 'path-join
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")
(defconst *user-data-directory*
  (getenv-or "XDG_DATA_HOME"
             (path-join *user-home-directory* ".local" "share" "emacs")))

(defconst *has-gdb* (executable-find "gdb"))
(defconst *has-cscope* (executable-find "cscope"))
(defconst *has-ctags* (executable-find "ctags"))
(defconst *has-git* (executable-find "git"))
(defconst *has-cleartool* (and (executable-find "cleartool")
			       (eq (call-process-shell-command "cleartool" nil nil nil "quit") 0)))

(provide 'config/constants.el)
