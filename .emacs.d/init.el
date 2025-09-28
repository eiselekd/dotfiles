
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

(use-package transient)

(use-package magit
  :defer 6
  :general
  ("M-g" 'magit-status
   "C-x M-g" 'magit-dispatch
   "C-c M-g" 'magit-file-dispatch)
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
;; scroll with cursor in place
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

(global-set-key (kbd "M-e") (lambda()(interactive) (shell default-directory)))

(use-package solarized-theme
  :bind
  (( "M-t" . solarized-toggle-theme))
  :init
  (load-theme 'solarized-dark t)
  )



