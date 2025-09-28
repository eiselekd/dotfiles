(defvar saved-mode-line-rflog nil)

(defun toggle-mode-line ()		;
  "toggle mode lie"
  (interactive)

  (if saved-mode-line-rflog
      (setq-default mode-line-format saved-mode-line-rflog
            saved-mode-line-rflog nil)
    (setq-default saved-mode-line-rflog mode-line-format
          mode-line-format nil))
  (force-mode-line-update t))

;; (setq-default mode-line-format nil)

(provide 'utils/hidebars.el)
