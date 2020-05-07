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
	      (show-paren-mode)

	      (require 'popup)
	      (global-set-key  (kbd "M-;") 'modes/elisp-mode-describe-thing-in-popup)

	      ;;(when (require 'org)
		;;(orgstruct++-mode))
	      ;;(setq orgstruct-heading-prefix-regexp ";;")

	      (when (require 'which-func)
		(which-func-mode 1))

	      )))

(provide 'modes/elisp-mode.el)
