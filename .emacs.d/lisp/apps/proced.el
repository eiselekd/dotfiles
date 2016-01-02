
(defun apps/proced-init ()
  "Initialize proced."
  (message (format "[*] %s proced init" (timestamp_str)))
  (setq-default
   ;; Show all processes.
   proced-filter 'all
   ;; Default display mode.
   proced-format 'tiny)
  
  (after-load 'proced
    (add-many-to-list
     'proced-format-alist
     '(min pid tree (args comm))
     '(tiny pid tree user pcpu pmem (args comm))))
  )

(unless (eq system-type 'darwin)
  (apps/proced-init))

(provide 'apps/proced.el)
;;; proced.el ends here
