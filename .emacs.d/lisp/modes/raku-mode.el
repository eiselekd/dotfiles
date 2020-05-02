;;(require 'use-package)
(require 'raku-mode)

(defun modes/raku-mode-start ()
  (message "[+] start flycheck raku")
  (flycheck-mode)
  (require 'flycheck-raku)
  (flycheck-select-checker 'raku)

  )

(add-hook 'raku-mode-hook #'modes/raku-mode-start)

(provide 'modes/raku-mode.el)
