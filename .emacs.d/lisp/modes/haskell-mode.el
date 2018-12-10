
(autoload 'haskell-mode "haskell-mode" "haskell-mode" t)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("hs" . haskell-mode))

;;(require 'flycheck-haskell)
;; cabel update
;; cabal install ghc-mod
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(defun haskell-interactive-start ()
  (progn 
    (haskell-mode)
    (haskell-interactive-switch)))


(add-hook 'haskell-mode-hook
	  (lambda ()
	    (progn
	      (message "[+] Enter haskell-mode")
	      (when  (require 'flycheck-haskell nil t )
		(message "[+] flycheck-haskellloaded"))

	      (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
		(setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
		(add-to-list 'exec-path my-cabal-path))

	      (turn-on-haskell-indentation)

	      (custom-set-variables
					; Set up hasktags (part 2)
	       '(haskell-tags-on-save t)
					; Set up interactive mode (part 2)
	       '(haskell-process-auto-import-loaded-modules t)
	       '(haskell-process-log t)
	       '(haskell-process-suggest-remove-import-lines t)
					; Set interpreter to be "cabal repl"
	       '(haskell-process-type 'cabal-repl))

	      (require 'company nil t)
	      (require 'ghc nil t)
	      (add-to-list 'company-backends 'company-ghc)
	      
	      (require 'haskell nil t)
	      (require 'haskell-interactive-mode nil t )
	      (require 'haskell-commands nil t)
	      (require 'haskell-doc nil t)
	      (haskell-doc-mode)
	      (flycheck-mode)
	      
	      (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
	      (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
	      (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
	      (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
	      (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
	      (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
	      (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
	      (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
	      
	      (define-key haskell-mode-map (kbd "M-;")
	      	(lambda ()(interactive)
	      	  (let*
	      	      ((my-cabal-path (expand-file-name "~/.cabal/bin"))
	      	       (f (format "%s/hasktags" my-cabal-path))
	      	       (cmd (format "git clone https://github.com/MarcWeber/hasktags.git /tmp/hasktags;cd /tmp/hasktags;cabal build;cp ./dist/build/hasktags/hasktags %s" f))
	      	       )
	      	    (if (not (or (executable-find "hasktags" ) (file-exists-p f)))
	      		(progn
	      		  (message "[+] Compile hasktags: %s" cmd)
	      		  (shell-command cmd)
	      		  ))
	      	    (haskell-mode-generate-tags)
	      	    )))
	      
	      (when  (require 'haskell-cabal nil t )
		(progn
		  (message "[+] Enter haskell-cabal")
		  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
		  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
		  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
		  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


	      )))



(provide 'modes/haskell-mode.el)
