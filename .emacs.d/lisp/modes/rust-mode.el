;;https://github.com/rust-lang/rust-mode

(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(provide 'modes/rust-mode.el)
