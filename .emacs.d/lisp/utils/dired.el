
(add-hook 'dired-mode-hook 'utils/diredhook)

(defun utils/diredhook ()
  (progn 
    (message "open dired-subtree" (timestamp_str))
    (require 'dired-subtree)
    (when (not (display-graphic-p))
      (setq dired-subtree-use-backgrounds nil))
    (local-set-key (kbd "TAB") 'dired-subtree-toggle)

    (defvar grep-and-find-map (make-sparse-keymap))
    (define-key global-map "\C-f" grep-and-find-map)
    (define-key global-map "\C-fh" 'utils/dired-grep-rec)
    (define-key global-map "\C-fg" 'find-grep-dired)
    (define-key global-map "\C-fn" 'find-name-dired)

    ;;(define-key global-map "\C-xf" 'utils/dired-grep-rec-curdir)
    ;;(global-set-key "\C-xf" 'utils/dired-grep-rec-curdir )

    (local-set-key (kbd "M-f")    'utils/dired-grep-rec-curdir)

    (local-set-key (kbd "<left>") 'dired-subtree-up)
    (local-set-key (kbd "<right>") 'dired-subtree-down)

    ))

(defun utils/dired-grep-rec ()
  (interactive)
  (let* ( (dir-name (or (ignore-errors (dired-get-filename nil))
			(buffer-file-name)
			default-directory)))
    (message "recursive grep in dirname: %s " dir-name)
    (sleep-for 1) 
    (helm-do-grep-1 (list dir-name) t nil '("*"))))

(defun utils/dired-grep-rec-curdir ()
  (interactive)
  (require 'helm-files)
  (require 'helm-grep)
  (let* ( (dir-name (or (buffer-file-name)
			default-directory)))
    (prepareHelm)
    (message "recursive grep in dirname: %s " dir-name)
    (sleep-for 1) 
    (helm-do-grep-1 (list dir-name) t nil '("*"))))

;;(setq dired-listing-switches "-aBhl  --group-directories-first")

(defun dired-sort-size ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun dired-sort-extension ()
  "Dired sort by extension."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun dired-sort-ctime ()
  "Dired sort by create time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ct")))

(defun dired-sort-utime ()
  "Dired sort by access time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ut")))

(defun dired-sort-time ()
  "Dired sort by time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-name ()
  "Dired sort by name."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "")))


(defun dired-sort-criteria (criteria)
  "sort-dired by different criteria by Robert Gloeckner "
  (interactive
   (list
    (or (completing-read "criteria [name]: "
			 '("size(S)" "extension(X)" "creation-time(ct)"
			   "access-time(ut)" "time(t)" "name()"))
	"")))
  (string-match ".*(\\(.*\\))" criteria)
  (dired-sort-other
   (concat dired-listing-switches
	      (match-string 1 criteria))))



(provide 'utils/dired.el)
 
