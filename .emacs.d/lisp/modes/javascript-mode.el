
;;"Hooks for javascript mode."
(defun my-javascript-mode-hook ()
  (interactive)
  (progn
    ;;(message "[+] my-javascript-mode-hook")
    (setq js-indent-level 4)
    (setq indent-tabs-mode nil)
    (require 'js2-mode)
    (require 'indium)
    ))

(add-hook 'js-mode-hook  'my-javascript-mode-hook)

(provide 'modes/javascript-mode.el)
