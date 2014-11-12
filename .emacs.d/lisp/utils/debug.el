
(defun utils/debug ()
  "Debug current context."
  (interactive)
  (progn
    (cond
     ((string-match "\.pl$" (buffer-name)) (call-interactively 'perldb))
     (t (call-interactively 'gud-gdb)))))

(add-hook 'after-init-hook 'utils/debug-keybind)
(add-hook 'perldb-mode-hook 'utils/debug-perl-keybind)

(provide 'utils/debug.el)
