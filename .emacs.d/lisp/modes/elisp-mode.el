(defun modes/elisp-mode-describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
	 (help-xref-following t)
	 (description (save-window-excursion
			(with-temp-buffer
			  (help-mode)
			  (help-xref-interned thing)
			  (buffer-string)))))
    (popup-tip description
	       :point (point)
	       :around t
	       :height 20
	       :scroll-bar t
	       :margin t)))


(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (progn
	      (message (format "[*] execute emacs-lisp-mode-hook"))
	      (show-paren-mode)
	      (require 'popup)
	      (local-set-key  (kbd "M-;") 'modes/elisp-mode-describe-thing-in-popup)
	      ;;(when (require 'org)
		;;(orgstruct++-mode))
	      ;;(setq orgstruct-heading-prefix-regexp ";;")
	      (message (format "[*] setup etags update"))
	      (when
		  (require 'utils/etags.el nil t)
		(utils/etags-tag-prepare)
		(etags-update-mode))

	      (when (require 'which-func)
		(if (fboundp 'which-func-mode)
		    (which-func-mode 1)))

	      )))

(provide 'modes/elisp-mode.el)
