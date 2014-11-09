
(defun utils/debug ()
  "Debug current context."
  (interactive)
  (progn
    (cond
     (t (call-interactively 'gud-gdb)))))

(add-hook 'after-init-hook 'utils/debug-keybind)

(provide 'utils/debug.el)
