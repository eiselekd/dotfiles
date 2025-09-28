;;https://github.com/rust-lang/rust-mode
;;https://github.com/racer-rust/emacs-racer

;;rustup toolchain add nightly
;;rustup component add rust-src
;;cargo +nightly install racer

(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(defun modes/rust-hook ()
  (progn
    (message "[+] rust enable flycheck")
    (flycheck-mode)

    (when  (and (require 'racer nil t) (require 'company nil t))
      (progn
	(racer-mode)
	(company-mode)
	(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
	(setq company-tooltip-align-annotations t)
	))

    (when  (require 'cargo nil t)
      (progn
	(cargo-minor-mode)
	))


  ))

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook 'modes/rust-hook)

(provide 'modes/rust-mode.el)
