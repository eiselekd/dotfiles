
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

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.3)
  
  ;; Disable problematic rust-cargo checker in favor of eglot
  (setq-default flycheck-disabled-checkers '(rust-cargo))
  
  ;; Customize flycheck faces to only show underlines, no background colors
  ;; Works for both GUI and terminal mode
  (with-eval-after-load 'flycheck
    ;; Force face attributes to remove any background colors
    (defun my/reset-flycheck-faces ()
      "Reset flycheck faces to remove background colors"
      (set-face-attribute 'flycheck-error nil :background 'unspecified :foreground 'unspecified :underline t :weight 'bold)
      (set-face-attribute 'flycheck-warning nil :background 'unspecified :foreground 'unspecified :underline t :slant 'italic)
      (set-face-attribute 'flycheck-info nil :background 'unspecified :foreground 'unspecified :underline t))
    
    ;; Apply immediately and after theme changes
    (my/reset-flycheck-faces)
    (add-hook 'after-init-hook #'my/reset-flycheck-faces)
    (add-hook 'flycheck-mode-hook #'my/reset-flycheck-faces)))

;; Flycheck integration with eglot
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


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

;; Minimal eglot configuration for rust-analyzer
(use-package eglot
  :ensure nil  ; Use built-in eglot
  :config
  ;; Configure eglot for rust-analyzer with workspace configuration
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  
  ;; Enable more verbose logging to debug completion issues
  (setq eglot-events-buffer-size 2000000)
  
  ;; Configure rust-analyzer settings to handle workspace issues
  (add-to-list 'eglot-workspace-configuration
               '(:rust-analyzer 
                 (:cargo (:buildScripts (:enable t))
                  :procMacro (:enable t)
                  :checkOnSave (:command "clippy"))))
  
  ;; Add company-capf for eglot completion
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (message "Eglot managed mode activated, backends: %s" company-backends)
              (setq-local company-backends
                         (add-to-list 'company-backends 'company-capf))
              (message "After adding capf, backends: %s" company-backends)))
  
  ;; Disable flymake in favor of flycheck
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1))))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . eglot-ensure)
	 (rust-mode . company-mode)
	 (rust-mode . flycheck-mode)
	 (rust-ts-mode . eglot-ensure)
	 (rust-ts-mode . company-mode)
	 (rust-ts-mode . flycheck-mode))
  :mode "\\.rs\\'"
  :config
  (add-to-list 'exec-path "/home/eiselekd/.cargo/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/home/eiselekd/.cargo/bin"))
  
  ;; Ensure company-capf is available for completion
  (add-hook 'rust-mode-hook 
            (lambda () 
              (setq-local company-backends (add-to-list 'company-backends 'company-capf))
              ;; Configure flycheck for rust-mode
              (setq-local flycheck-disabled-checkers '(rust-cargo))))
  (add-hook 'rust-ts-mode-hook 
            (lambda () 
              (setq-local company-backends (add-to-list 'company-backends 'company-capf))
              ;; Configure flycheck for rust-ts-mode  
              (setq-local flycheck-disabled-checkers '(rust-cargo))))
  
  ;; Use tree-sitter mode if available, otherwise fall back to rust-mode
  (when (treesit-language-available-p 'rust)
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))))




