
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
  
  ;; Disable fringe/margin indicators since we're using inline errors
  (setq flycheck-indication-mode nil)
  
  ;; Customize flycheck faces to only show underlines, no background colors
  ;; Works for both GUI and terminal mode
  (with-eval-after-load 'flycheck
    ;; Diagnostic function to check face attributes
    (defun my/diagnose-flycheck-faces ()
      "Show diagnostic information about flycheck faces"
      (interactive)
      (with-output-to-temp-buffer "*Flycheck Face Diagnostics*"
        (princ (format "Display type: %s\n" (if (display-graphic-p) "GUI" "TUI")))
        (princ (format "TERM: %s\n" (getenv "TERM")))
        (princ (format "Terminal: %s\n\n" (getenv "TERM_PROGRAM")))
        
        (dolist (face '(flycheck-error flycheck-warning flycheck-info default))
          (princ (format "Face: %s\n" face))
          (princ (format "  All attributes: %s\n" (face-all-attributes face)))
          (princ (format "  Background: %s\n" (face-background face nil t)))
          (princ (format "  Foreground: %s\n" (face-foreground face nil t)))
          (princ (format "  Underline: %s\n" (face-attribute face :underline)))
          (princ (format "  Inverse-video: %s\n\n" (face-attribute face :inverse-video))))))
    
    ;; Manual command to force face reset
    (defun my/fix-flycheck-faces-now ()
      "Manually trigger flycheck face reset"
      (interactive)
      (my/reset-flycheck-faces)
      (message "Flycheck faces reset! Check with M-x my/diagnose-flycheck-faces"))
    
    ;; Force face attributes to remove any background colors
    (defun my/reset-flycheck-faces ()
      "Reset flycheck faces to remove background colors"
      (let ((term-type (getenv "TERM")))
        (message "Resetting flycheck faces - Display: %s, TERM: %s" 
                 (if (display-graphic-p) "GUI" "TUI") 
                 term-type)
        ;; Both GUI and TUI mode - use underlines with no background colors
        (message "Using underlines with no background colors")
        (set-face-attribute 'flycheck-error nil 
                          :family 'unspecified :foundry 'unspecified :width 'unspecified 
                          :height 'unspecified :weight 'bold :slant 'unspecified 
                          :underline t :overline 'unspecified :strike-through 'unspecified 
                          :box 'unspecified :inverse-video 'unspecified 
                          :foreground 'unspecified :background 'unspecified 
                          :stipple 'unspecified :inherit 'unspecified)
        
        (set-face-attribute 'flycheck-warning nil 
                          :family 'unspecified :foundry 'unspecified :width 'unspecified 
                          :height 'unspecified :weight 'unspecified :slant 'italic 
                          :underline t :overline 'unspecified :strike-through 'unspecified 
                          :box 'unspecified :inverse-video 'unspecified 
                          :foreground 'unspecified :background 'unspecified 
                          :stipple 'unspecified :inherit 'unspecified)
        
        (set-face-attribute 'flycheck-info nil 
                          :family 'unspecified :foundry 'unspecified :width 'unspecified 
                          :height 'unspecified :weight 'unspecified :slant 'unspecified 
                          :underline t :overline 'unspecified :strike-through 'unspecified 
                          :box 'unspecified :inverse-video 'unspecified 
                          :foreground 'unspecified :background 'unspecified 
                          :stipple 'unspecified :inherit 'unspecified)))
    
    ;; Apply immediately and after theme changes
    (my/reset-flycheck-faces)
    (add-hook 'after-init-hook #'my/reset-flycheck-faces)
    (add-hook 'flycheck-mode-hook #'my/reset-flycheck-faces)
    
    ;; Force immediate re-application for current buffers
    (when (boundp 'flycheck-mode)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when flycheck-mode
            (my/reset-flycheck-faces)))))))

;; Flycheck integration with eglot
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; Show flycheck errors inline at end of line
(with-eval-after-load 'flycheck
  (defvar-local my/flycheck-inline-overlays nil
    "List of overlays for inline error display")
  
  (defun my/clear-inline-errors ()
    "Clear all inline error overlays in current buffer"
    (when my/flycheck-inline-overlays
      (mapc #'delete-overlay my/flycheck-inline-overlays)
      (setq my/flycheck-inline-overlays nil)))
  
  (defun my/show-errors-inline ()
    "Show flycheck errors at end of lines"
    (my/clear-inline-errors)
    (when (and flycheck-mode (flycheck-has-current-errors-p))
      (let ((errors (flycheck-overlay-errors-in (point-min) (point-max))))
        (dolist (err errors)
          (when-let* ((pos (flycheck-error-pos err))
                      (message-text (flycheck-error-message err))
                      (level (flycheck-error-level err))
                      (prefix (cond ((eq level 'error) " ⚠ ")
                                   ((eq level 'warning) " ⚠ ")
                                   (t " ℹ ")))
                      (text (concat prefix message-text)))
            (save-excursion
              (goto-char pos)
              (let ((overlay (make-overlay (line-end-position) (line-end-position))))
                (overlay-put overlay 'after-string 
                           (propertize text 'face '(:inherit shadow)))
                (push overlay my/flycheck-inline-overlays))))))))
  
  ;; Hook to update inline errors
  (add-hook 'flycheck-after-syntax-check-hook #'my/show-errors-inline)
  (add-hook 'before-change-functions 
            (lambda (&rest _) (my/clear-inline-errors)) nil t))


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
  :bind (:map rust-mode-map
              ("M-." . xref-find-definitions)
              ("M-," . xref-go-back)
              ("M-?" . xref-find-references)
              ("C-c d" . eldoc-doc-buffer))
  :config
  (add-to-list 'exec-path "/home/eiselekd/.cargo/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/home/eiselekd/.cargo/bin"))
  
  ;; Add keybindings for rust-ts-mode after it's loaded
  (with-eval-after-load 'rust-ts-mode
    (define-key rust-ts-mode-map (kbd "M-.") 'xref-find-definitions)
    (define-key rust-ts-mode-map (kbd "M-,") 'xref-go-back)
    (define-key rust-ts-mode-map (kbd "M-?") 'xref-find-references)
    (define-key rust-ts-mode-map (kbd "C-c d") 'eldoc-doc-buffer))
  
  ;; Disable fringes in rust buffers (GUI mode only)
  (defun my/disable-fringes-in-rust ()
    "Disable fringes in rust buffers"
    (when (display-graphic-p)
      (set-window-fringes (selected-window) 0 0)))
  
  ;; Ensure company-capf is available for completion
  (add-hook 'rust-mode-hook 
            (lambda () 
              (setq-local company-backends (add-to-list 'company-backends 'company-capf))
              ;; Configure flycheck for rust-mode
              (setq-local flycheck-disabled-checkers '(rust-cargo))
              ;; Disable fringes
              (my/disable-fringes-in-rust)))
  (add-hook 'rust-ts-mode-hook 
            (lambda () 
              (setq-local company-backends (add-to-list 'company-backends 'company-capf))
              ;; Configure flycheck for rust-ts-mode  
              (setq-local flycheck-disabled-checkers '(rust-cargo))
              ;; Disable fringes
              (my/disable-fringes-in-rust)))
  
  ;; Use tree-sitter mode if available, otherwise fall back to rust-mode
  (when (treesit-language-available-p 'rust)
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))))




