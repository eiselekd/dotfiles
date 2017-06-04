
(defun my-javascript-mode-hook () "Hooks for javascript mode."
       (progn
	 (message "[+] my-javascript-mode-hook");
	 (setq js-indent-level 4)
	 (setq indent-tabs-mode gnil)
	 )
       )

(add-hook 'js-mode-hook  'my-javascript-mode-hook)

(provide 'modes/javascript-mode.el)
