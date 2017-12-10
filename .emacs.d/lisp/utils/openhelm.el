
;; commands http://tuhdo.github.io/helm-intro.html

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the linux kernel."
  (interactive)
  (message (format "[*] detected linux mode"))
  (setq c-set-style "linux")
  (setq c-brace-offset -8)
  (setq c-default-style "linux")
  (setq c-basic-offset 8)
  (setq tab-width 8))


(defun gtag/c-hook ()
  (message (format "[*] setup hel"))

  (setq-local show-trailing-whitespace t)
					;(semantic-mode)
					;(auto-complete-mode -1)
  ;;(company-mode)
  (global-set-key (kbd "C-c C-f") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
  (setq helm-quick-update                     t ; do not display invisible candidates
	helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non&#x2013;nil
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-&lt;next&gt;/M-&lt;prior&gt;
	helm-ff-file-name-history-use-recentf t
	helm-gtags-ignore-case t
	helm-gtags-auto-update t
	helm-gtags-use-input-at-cursor t
	helm-gtags-pulse-at-cursor t
	helm-gtags-prefix-key "\C-cg"
				    helm-gtags-suggested-key-mapping t)
  (helm-gtags-mode)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "M--") 'helm-dash-at-point)

  (setq-local helm-dash-docsets '("C++"))
  ;;(add-hook 'go-mode-hook 'go-doc)

  ;;Too complex for me right now.
  ;;(helm-mode)
  ;;(define-key company-mode-map (kbd "M-h") 'company-c-headers)
  (hs-minor-mode)
  (define-key hs-minor-mode-map (kbd "C-c C-t") 'hs-toggle-hiding)
  (define-key c-mode-map (kbd "C-c C-c") 'compile)
					;(semantic-mru-bookmark-mode)
					;(define-key semantic-mode-map (kbd "M-]") 'semantic-ia-fast-jump)
					;(define-key semantic-mode-map (kbd "M-[") 'semantic-ia-fast-jump-back)
  (define-key c-mode-map (kbd "C-c C-i") 'default-c-includes)
  (ggtags-mode)
  (define-key ggtags-mode-map (kbd "M-.") nil)
  (define-key ggtags-mode-map (kbd "M-<") nil)
  (define-key ggtags-mode-map (kbd "M->") nil)
  (define-key ggtags-mode-map (kbd "M-n") nil)
  (define-key ggtags-mode-map (kbd "M-p") nil)
  (define-key ggtags-mode-map (kbd "M-,") nil)
  (define-key ggtags-mode-map (kbd "M-]") nil)
					;(define-key ggtags-mode-map (kbd "M&#x2013;") 'ggtags-find-reference)
  ;;Flycheck has issues with tramp, just FYI.
  ;;(flycheck-mode)

  (define-key c-mode-map (kbd "M--") (lambda () (interactive) (
							       message (format "[*] %s" (utils/flycheck-search-linux-makefile)))
							       ))


  (if (utils/flycheck-search-linux-makefile)
      (call-interactively 'linux-c-mode))

  )


;; https://github.com/fxfactorial/emacsd/blob/master/init.el

(defun gtag/gtag-init ()
  (when (require 'gtags nil t)
    (if (<= emacs-major-version 24)
      ( progn
	(setq tramp-verbose 6)
	(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")))
    (message (format "[*] try open helm. Note: install globals (gtags) and helm"))
    (require 'helm-config)
    (require 'helm-gtags)
    (require 'helm-dash)
    (require 'ggtags nil t)
    (message "[*] %s retired helm-internal" (timestamp_str))

    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "M-X") 'helm-M-x)
    (global-set-key (kbd "M-q") 'helm-do-grep)
    (global-set-key (kbd "M-Q") (lambda () (interactive)
				  (let ((current-prefix-arg '(10)))
				    (call-interactively 'helm-do-grep))))

    ;; Helm stuff
    (setq helm-quick-update                     t ; do not display invisible candidates
	  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-ff-file-name-history-use-recentf t
	  helm-gtags-ignore-case t
	  helm-gtags-auto-update t
	  helm-gtags-use-input-at-cursor t
	  helm-gtags-pulse-at-cursor t
	  helm-gtags-prefix-key "\C-cg"
	  helm-gtags-suggested-key-mapping t
	  )
    ;; helm-exit-idle-delay 0

    (add-hook 'c++-mode-hook 'gtag/c-hook)
    (add-hook 'c-mode-hook 'gtag/c-hook)


    ))


(gtag/gtag-init)

;;(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'utils/openhelm.el)
