
(message "[+] init.el")

(defconst *.emacs.d.dir*
  (file-name-directory (or load-file-name buffer-file-name))
  "path to .emacs.d")
(defconst *.emacs.d.lisp.dir*
  (concat *.emacs.d.dir* "lisp")
  "path to .emacs.d/lisp")
(add-to-list 'load-path *.emacs.d.lisp.dir* )
  
(load "elpaca_init.el")

(defvar $leader-key "C-c"
  "leader key used to quicky access commands.")

(defvar $mm-leader-key ","
  "leader key for major mode specific commands")

(general-create-definer $leader-set-key
  :prefix $leader-key
  :keymaps 'override)

(defun general-leader-define-key (_state keymap key def _orig-def _kargs)
  "define a new key based on leader"
  (if (eq keymap 'global)
      (eval `($leader-set-key ,key ',def))
    (eval `($leader-local-set-key :keymaps ',keymap ,key ',def))))

(use-package transient)

(use-package magit
  :defer 6
  :general
  ("M-g" 'magit-status
   ;;"C-x M-g" 'magit-dispatch
   ;;"C-c M-g" 'magit-file-dispatch

   )
  (magit-diff-mode-map
   "SPC" nil)
  (magit-mode-map
   "SPC" nil)
  :init
  ($leader-set-key
;;    "g" '(:ignore t :wk "git")
    "gg" 'magit-dispatch
    "gf" 'magit-file-dispatch)
  ;;(evil-ex-define-cmd "git" 'magit-status)
  ;; make transient not take the width of the whole frame
  (setq transient-display-buffer-action
        '(display-buffer-below-selected))
  :config
  (when t
    (setq magit-refresh-status-buffer nil)
    (setq magit-branch-direct-configure nil)
    (setq magit-commit-show-diff nil)
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source))

(require 'windmove)
(windmove-default-keybindings)
(global-set-key (kbd "M-S-<up>") 'shrink-window)
(global-set-key (kbd "M-S-<down>") 'enlarge-window)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)

(global-set-key (kbd "M-e") (lambda()(interactive) (shell default-directory)))


(setq themes-list
      `(
	("light"     . ,( lambda () (progn (load-theme 'solarized-light t)) ))
	("dark"      . ,( lambda () (progn (load-theme 'solarized-dark t)) ))
	("selenized-light"     . ,( lambda () (progn (load-theme 'solarized-selenized-light t)) ))
	("selenized-dark"      . ,( lambda () (progn (load-theme 'solarized-selenized-dark t)) ))
	("gruvbox-light"     . ,( lambda () (progn (load-theme 'solarized-gruvbox-light t)) ))
	("gruvbox-dark"      . ,( lambda () (progn (load-theme 'solarized-gruvbox-dark t)) ))
	("high contrast light"     . ,( lambda () (progn (load-theme 'solarized-light-high-contrast t)) ))
	("high contrast dark"      . ,( lambda () (progn (load-theme 'solarized-dark-high-contrast t)) ))
	("wombat"      . ,( lambda () (progn (load-theme 'solarized-wombat-dark t)) ))
	("zenburn"      . ,( lambda () (progn (load-theme 'solarized-zenburn t)) ))
	))

(use-package dash
  :ensure t
  )

(defun cycle-theme-sel ()
  (interactive)
  (require 'dash)
  (setq themes-list (-rotate 1 themes-list))
  ;;(custom-set-variables '( frame-background-mode mode))
  (message (format "[+] enable theme %s" (car (nth 0 themes-list))))
  (funcall (cdr (nth 0 themes-list))))


(use-package solarized-theme
  :bind
  (( "M-t" . cycle-theme-sel))
  :init
  (load-theme 'solarized-dark t)
  )

;; ;;;; Ivy

(use-package helm)

(use-package ivy)
(use-package counsel
  :bind
  (
   ("M-x" . counsel-M-x))
  )

;; (use-package ivy
;;   :bind (
;; ;;	 ("C-x b" . ibuffer)
;;   )
;;   :general
;;   (ivy-minibuffer-map
;;    "C-h" "DEL"
;;    "C-w" 'ivy-backward-kill-word
;;    "C-S-H" help-map
;;    "C-l" 'ivy-alt-done
;;    "<C-return>" 'ivy-immediate-done
;;    [mouse-1] 'ignore
;;    [mouse-2] 'ignore
;;    [mouse-3] 'ignore)
;;   (ivy-reverse-i-search-map
;;    "C-k" 'ivy-previous-line)
;;   (ivy-switch-buffer-map
;;    "C-k" 'ivy-previous-line
;;    "C-d" 'ivy-switch-buffer-kill)
;;   ("C-x r b" 'counsel-bookmark
;;    "C-x C-r" 'ivy-resume)
;;   (ivy-occur-grep-mode-map
;;    "SPC" nil)
;;   (minibuffer-local-map
;;    "C-c C-l" 'counsel-minibuffer-history)
;;   ;;("C-x C-b" 'ivy-switch-buffer)
;;   :init
;;   (setq ivy-height 15
;;         ivy-use-virtual-buffers 'recentf
;;         ivy-virtual-abbreviate 'abbreviate
;;         ivy-extra-directories nil
;;         ivy-use-selectable-prompt t
;;         ivy-count-format "%d/%d "
;;         ivy-re-builders-alist '((t . ivy--regex-ignore-order))
;;         ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
;;   :config
;;   (ivy-mode)
;;   ;; don't resort my functions
;;   (setq ivy-sort-matches-functions-alist '((t))))

;; (setq ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)
;;                                        (org-mode . org-level-8)))


;; (use-package hydra
;;   :ensure (:wait t))
;; (use-package ivy-hydra
;;   :after (ivy hydra))

;; (with-eval-after-load 'counsel
;;   (ivy-add-actions
;;    t
;;    '(("y" $ivy-yank "yank" $ivy-yank-all)))
;;   (ivy-add-actions
;;    'counsel-find-file
;;    '(("g" $magit-status-in-dir "git status")
;;      ("d" $async-delete-file "delete")
;;      ("y" $yank-file-name "yank" $yank-file-name-list)
;;      ("s" (lambda (x) (counsel-rg nil x)) "search")
;;      ("f" $ivy-file-jump "find")
;;      ("o" find-file-other-window "other window")
;;      ("x" (lambda (x) ($counsel-shell-pop ivy-current-prefix-arg nil x)) "shell")
;;      ("j" (lambda (x) (let ((default-directory x)) (counsel-git))) "jump"))))


;; (use-package counsel
;;   :bind (;;("C-x C-f" . counsel-find-file)
;;          ;;("C-x f" . counsel-find-file)
;;          ;;("C-x C-j" . counsel-git)
;;          ;;("C-x j" . counsel-git)
;;          ;;("C-c s" . counsel-ag)
;;          ("M-x" . counsel-M-x))
;;   :general
;;   (:definer 'leader
;;    "T" 'counsel-load-theme)
;;   :init
;;   (setq counsel-find-file-ignore-regexp (rx (or (: bos (any "#.")) (: (any "#~") eos)))
;;         counsel-bookmark-avoid-dired t)
;;   :config
;;   ;; adding --search-zip can cause the PCRE engine to hit it's line limit. add `-- -z` to search zip files
;;   (setq counsel-rg-base-command (append counsel-rg-base-command '("--max-columns-preview")))
;;   (ivy-configure 'counsel-company
;;     :display-fn 'ivy-display-function-overlay)
;;   (setq ivy-initial-inputs-alist nil))





(require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;;sort on major-mode
(setq ibuffer-default-sorting-mode 'major-mode)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ;; ("equfitter"
               ;;  (filename . "equationfitter/"))
               ("Text" (name . ".txt"))
               ("Programming C++" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 ))

               ("Source Code" ;; non C++ related stuff.
                (or
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 (mode . shell-script-mode)
                 (mode . f90-mode)
                 (mode . scheme-mode)
                 ;; etc
                 ))

               ("LaTeX"
                (or
                 (mode . tex-mode)
                 (mode . latex-mode)
                 (name . ".tex")
                 (name . ".bib")
                 ))


               ("Mail"
                (or  ;; mail-related buffers
                 (mode . message-mode)
                 (mode . mail-mode)
                 (mode . mime-mode)
		 ;;                   (mode . MIME-mode)

                 ;; etc.; all your mail related modes
                 ))

               ("Web" (or (mode . html-mode)
                          (mode . css-mode)))

               ("ERC"   (mode . erc-mode))

               ;; ("Subversion" (name . "\*svn"))
               ;; ("Magit" (name . "\*magit"))

               ("Emacs-created"
                (or
                 (name . "^\\*")))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            ;;(ibuffer-auto-mode 1)   ;auto update the buffer-list
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)

;; keep from warning, twice, about deleting buffers.
;; only warn about deleting modified buffers.
(setq ibuffer-expert t)

;; ;; Switching to ibuffer puts the cursor on the most recent buffer
;; (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
;;   "Open ibuffer with cursor pointed to most recent buffer name"
;;   (let ((recent-buffer-name (buffer-name)))
;;     ad-do-it
;;     (ibuffer-jump-to-buffer recent-buffer-name)))
;; (ad-activate 'ibuffer)
;;---------------------------------------------


(use-package casual-suite
  ;; :ensure
  ;; (:type git :host github :repo "kickingvegas/casual")
  :general
  ;;("s-." #'casual-editkit-main-tmenu)
  ;;("M-g a" #'casual-avy-tmenu)
  (:keymaps 'reb-mode-map
	  "S-." #'casual-re-builder-tmenu)
  (:keymaps 'calc-mode-map
	  "C-o" #'casual-calc-tmenu)
  (:keymaps 'dired-mode-map
	  "C-o" #'casual-dired-tmenu)
  (:keymaps 'isearch-mode-map
	  "C-o" #'casual-isearch-tmenu)
  (:keymaps 'ibuffer-mode-map
	  (kbd "C-o") #'casual-ibuffer-tmenu
	  "F" #'casual-ibuffer-filter-tmenu
	  "s" #'casual-ibuffer-sortby-tmenu)
  (:keymaps 'bookmark-bemenu-mode-map
	  "C-o" #'casual-bookmarks-tmenu)
  (:keymaps 'org-agenda-mode-map
	  "C-o" #'casual-agenda-tmenu)
  (:keymaps 'Info-mode-map
	  "C-o" #'casual-info-tmenu)
  (:keymaps 'calendar-mode-map
	  "C-o" #'casual-calendar-tmenu))


(use-package helm-dash
  :bind
  (
   ("M--" . helm-dash-at-point)))

(use-package helm-gtags
  :bind
  (
   ("M-." . helm-gtags-dwim)
   ("M-," . helm-gtags-pop-stack)
   ))
 




(defun xah-open-file-at-cursor ()
  
  "Open the file path under cursor.
   If there is text selection, uses the text selection for path.
   If the path starts with “http://”, open the URL in browser.
   Input path can be {relative, full path, URL}.
   Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
   If path does not have a file extention, automatically try with “.el” for elisp files.
   This command is similar to `find-file-at-point' but without prompting for confirmation.
   URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
    
    (interactive)
    (let ((ξpath (if (use-region-p)
		     (buffer-substring-no-properties (region-beginning) (region-end))
		   (let (p0 p1 p2)
		     (setq p0 (point))
		     ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
		     (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
		     (setq p1 (point))
		     (goto-char p0)
		     (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
		     (setq p2 (point))
		     (goto-char p0)
		     (buffer-substring-no-properties p1 p2)))))
      (if (string-match-p "\\`https?://" ξpath)
	  (browse-url ξpath)
	(progn ; not starting “http://”
	  (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
	      (progn
		(let (
		      (ξfpath (match-string 1 ξpath))
		      (ξline-num (string-to-number (match-string 2 ξpath))))
		  (if (file-exists-p ξfpath)
		      (progn
			(find-file ξfpath)
			(goto-char 1)
			(forward-line (1- ξline-num)))
		    (progn
		      (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
			(find-file ξfpath))))))
	    (progn
	      (if (file-exists-p ξpath)
		  (find-file ξpath)
		(if (file-exists-p (concat ξpath ".el"))
		    (find-file (concat ξpath ".el"))
		  (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
		    (find-file ξpath ))))))))))

;; https://google.com

(global-set-key (kbd "M-5") 'xah-open-file-at-cursor)

;; Tree-sitter setup function
(defun ensure-tree-sitter-rust ()
  "Ensure Rust tree-sitter grammar is available."
  (unless (treesit-language-available-p 'rust)
    (message "Rust tree-sitter grammar not found. Please install it manually.")
    (message "You can install it by running: M-x treesit-install-language-grammar RET rust RET")
    nil))

;; Call this during startup
(with-eval-after-load 'treesit
  (ensure-tree-sitter-rust))

(load "modes/rust.el")

(use-package savehist
  :ensure nil
  :defer 1
  :config
  (add-to-list 'savehist-additional-variables 'read-expression-history)
  (savehist-mode))


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(if (fboundp 'scroll-bar-mode)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(load "hide-mode-line.el")
(hide-mode-line)
(force-mode-line-update)
;; (use-package hide-mode-line
;;   :ensure t )

;; (add-hook 'after-init-hook
;; 	  (lambda ()(interactive)
;; 	    (progn
;; 	      (message "[+] hide-mode-line-mode")
;; 	      (require 'hide-mode-line)
;; 	      (call-interactively 'hide-mode-line-mode))))
     

(global-set-key (kbd "ESC l")
 (lambda ()(interactive)
   (progn
     (message "[+] hide-mode-line-mode")
     (hide-mode-line)
     (force-mode-line-update)
     )))

     ;; (if (bound-and-true-p hide-mode-line-mode)
     ;;     (set-fringe-style (cdr (assoc (downcase "no-fringes") fringe-styles)))
     ;;     (set-fringe-style (cdr (assoc (downcase "default") fringe-styles))))
     ;; (xterm-mouse-mode (if (bound-and-true-p hide-mode-line) 0 1))
     ;; (force-mode-line-update))))

(setq inhibit-startup-screen t)

(cua-mode)
;; scroll with cursor in place, cua mode overwrite
(global-set-key "\M-n" (lambda ()(interactive) (call-interactively 'scroll-up-line )))
(global-set-key "\M-p" (lambda ()(interactive) (call-interactively 'scroll-down-line )))


(use-package kanji-mode
    :ensure t
  )

;; start org-agenda
(global-set-key (kbd "M-j")  'goto-line)
(global-set-key (kbd "M-J") (lambda ()(interactive)
			      (progn
				(cond
				 ((string= current-input-method 'japanese) (progn (set-input-method 'japanese-hiragana)))
				 ((string= current-input-method 'japanese-hiragana) (progn (set-input-method 'japanese-katakana)))
				 ((string= current-input-method 'japanese-katakana) (progn (set-input-method 'japanese)))
				 (t
				  (progn 
				    (set-language-environment 'Japanese)
				    (set-input-method 'japanese)
				    (require 'kanji-mode)
				    (kanji-mode)
				    ))))))


(use-package aider :ensure t
  :bind
  (
   ("M-P" . aider-transient-menu)
   )
)
 

(use-package copilot :ensure t
  :bind
  ("M-p" . copilot-mode)
  (:map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("M-TAB" . 'copilot-accept-completion-by-word)
        ("M-<tab>" . 'copilot-accept-completion-by-word))

)

;; remove trailing whitespaces
(global-set-key (kbd "M-W")  'whitespace-cleanup)


;;* Startup screen
(find-file default-directory)
