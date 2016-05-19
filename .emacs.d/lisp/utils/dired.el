
(add-hook 'dired-mode-hook 'utils/diredhook)

(defun utils/diredhook ()
  (progn 
    (message "[*] %s require dired-subtree" (timestamp_str))
    (require 'dired-subtree)
    (when (not (display-graphic-p))
      (setq dired-subtree-use-backgrounds nil))
    (local-set-key (kbd "TAB") 'dired-subtree-toggle)
    ))



(provide 'utils/dired.el)
 
