
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
	ad-do-it
      (error (and (buffer-modified-p)
		  (not (ding))
		  (y-or-n-p "Buffer is modified, save it? ")
		  (save-buffer))
	     (utils/er-refresh-ctags extension)
	     ad-do-it))))

(defun utils/er-refresh-ctags (&optional extension)
  "Run ctags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "find . | grep \".*\\.\\(h\\|c\\)\" |  xargs ctags -u -e "))
  (shell-command (format "ctags -u -e *.%s" (or extension "c")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(provide 'utils/ctags.el)
