
(use-package which-key
  :ensure t
  :config (which-key-mode)
  )

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action))
  :bind-keymap ("C-c l" . lsp-command-map)
  :config (lsp-enable-which-key-integration t)
  )

(use-package company
  :ensure t
  :hook (
         (emacs-lisp-mode . (lambda () (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode)
         )
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)

  )


(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (go-mode . company-mode))
  :bind (:map go-mode-map
              ("<f6>" . gofmt)
              ("C-c 6" . gofmt))
  :config
  (require 'lsp-go)
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t)
                          )))

;;


(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . eglot-ensure)
	 (rust-mode . company-mode))
  :mode "\\.rs\\'"
  :config
  (add-to-list 'exec-path "/home/eiselekd/.cargo/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/home/eiselekd/.cargo/bin"))
  
  ;; Use tree-sitter mode if available, otherwise fall back to rust-mode
  (when (treesit-language-available-p 'rust)
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))))




