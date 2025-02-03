
(defun utils/etags-tag-prepare (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)

  (require 'xref nil t)
  (require 'etags-table nil t)
  (require 'etags-select nil t)
  (require 'etag nil t)
  (require 'helm-tags nil t)
  (require 'etags-update nil t)
  (setq etags-table-search-up-depth 6)
  (etags-table-recompute)
  (message "[*] etags-table loaded: %s" tags-table-list)
  (setq tags-file-name (nth 0 tags-table-list))
  (message "[*] etags-table pick : %s" tags-file-name)

  )

(provide 'utils/etags.el)
