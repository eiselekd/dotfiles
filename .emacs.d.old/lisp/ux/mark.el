
(defun apps/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
	  (goto-char pos)
	(setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
	(set-marker (mark-marker) pos)
	(setq mark-ring (nbutlast mark-ring))
	(goto-char (marker-position (car (last mark-ring))))))))

(provide 'ux/mark.el)
