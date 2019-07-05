;; https://commercialhaskell.github.io/intero/


(defun modes/run-cling ()
  (progn
    (require 'cling)
    (cling-session-new)
    (interactive-cling-mode)

    )
  )

(provide 'modes/cling-mode.el)
