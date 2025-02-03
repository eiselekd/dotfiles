
(defun utils/etags-find-tag-simple ()
  (or (getenv "ETAGSROOT")
      (locate-dominating-file default-directory "TAGS")
      (if (not (yes-or-no-p "File TAGS not found. Run 'etags-update.pl? "))
          (user-error "Abort")
        (let* ((tagroot (read-directory-name "Root Directory: "))
	       (default-directory tagroot)
	       (tagfile (concat default-directory "TAGS"))
	       )
	  (setq tags-file-name tagfile)
	  (message "[+] file to generate: '%s'" tags-file-name)
	  (shell-command (concat "touch " tags-file-name))
	  ))))

(defun utils/etags-tag-prepare (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)

  (require 'xref nil t)
  (require 'etags-table nil t)
  (require 'etags-select nil t)
  (require 'etag nil t)
  (require 'helm-tags nil t)
  (require 'etags-update nil t)
  (require 'helm-gtags)
  (setq tags-revert-without-query t)
  (setq etags-table-search-up-depth 6)
  (etags-table-recompute)
  (message "[*] etags-table loaded: %s" tags-table-list)
  ;; for etags-update select tags-file-name
  (setq tags-file-name (nth 0 tags-table-list))
  (message "[*] etags-table pick : %s" tags-file-name)
  
  )

(defun utils/etags-search-tags-file ()
  (if (null tags-file-name)
      (progn
	(etags-update-mode)
	(utils/etags-find-tag-simple)
	)))


(provide 'utils/etags.el)
