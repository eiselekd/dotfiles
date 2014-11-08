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

(defun load-all-files-from-dir (dir)
  "Load all Emacs Lisp files in DIR."
  (dolist (f (directory-files dir))
    (when (and
           (file-directory-p (path-join dir f))
           (not (string= "." f))
           (not (string= ".." f)))
      (load-all-files-from-dir (path-join dir f)))
    (when (and
           (not (file-directory-p (path-join dir f)))
           (not (string= ".#" (substring f 0 2)))
           (string= ".el" (substring f (- (length f) 3))))
      (load-file (path-join dir f)))))

(defmacro try-eval (fn &optional finally)
  "Safely evaluate expression FN and run FINALLY after."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@finally))

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; --------------------------------------------------------


(defconst *is-nt*    (string-equal system-type "windows-nt"))
(defconst *is-mac*   (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
(defconst *is-24.3* (not (version< emacs-version "24.3")))
(defconst *is-win*  window-system)

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
