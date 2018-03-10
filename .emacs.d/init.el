;; note: META-: :  eval
;;       ctrl-e :  eval under cursor
;; note: terminal.app old emacs: install "brew", then "brew install emacs"
;; cedet-1-1: emacs -q --no-site-file -l cedet-build.el -f cedet-build
;; note: cedet install: move object-class-fast in eieio.el to top
;; dbg: C-u M-x eval-defun
;; use lexical-binding: t
;; toggle-debug-on-error
(setq ns-right-alternate-modifier nil)

;; (add-to-list 'load-path (substitute-in-file-name "$HOME/git/benchmark-init-el"))
;; (require 'benchmark-init)
;; (require 'benchmark-init-modes)
;; (benchmark-init/activate)

;; 1: hkset load path
(if (version< emacs-version "24.3") (setq user-emacs-directory "~/.emacs.d/"))

(defun timestamp_str ()
  (let ((n (current-time)))
    (format "%d:%d" (nth 1 n) (nth 2 n))))

(defconst *.emacs.d.dir*
  (file-name-directory (or load-file-name buffer-file-name))
  "path to .emacs.d")
(defconst *.emacs.d.lisp.dir*
  (concat *.emacs.d.dir* "lisp")
  "path to .emacs.d/lisp")
(defconst *.emacs.moreconf*
  (substitute-in-file-name "$HOME/emacs"))

(message (format "[*] %s starting emacs config from %s" (timestamp_str) *.emacs.d.lisp.dir*))

;; (if (and (<= emacs-major-version 24)
;; 	 (<= emacs-minor-version 4))
;;     ;;(setq load-path (cons (expand-file-name "lib/cl" *.emacs.d.lisp.dir*  )  load-path ))
;;     (define-obsolete-function-alias 'cl-struct-define 'cl-defstruct "24.3")

;;     ;;(require 'inline)
;;     ;;(require 'cl)
;;     ;;(require 'cl-lib)
;;   )

(add-to-list 'load-path (substitute-in-file-name "$HOME/emacs"))
(require 'moreconf-erc.el nil t)
(add-to-list 'load-path *.emacs.d.lisp.dir*)
(add-to-list 'load-path (expand-file-name "lib" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "wanderlust/elmo" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "wanderlust/wl" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "flim" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "semi" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "apel" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "async" *.emacs.d.lisp.dir* ))
(add-to-list 'load-path (expand-file-name "helm" *.emacs.d.lisp.dir* )) ;; async for helm
(setq ad-redefinition-action 'accept) ;;ad-handle-definition: `tramp-read-passwd' got redefined
(add-to-list 'load-path (expand-file-name "company-mode" *.emacs.d.lisp.dir* ))

(add-to-list 'load-path (expand-file-name "lib/magit" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/haskell-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "dired-hacks" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/irony-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/company-irony" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/org-mode/lisp" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/org-mode/contrib/lisp" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/latex-preview-pane" *.emacs.d.lisp.dir*  ))

(require 'cc-mode)				      
(setq c-default-style 
      '((java-mode . "java")
	(awk-mode . "awk")
	(c-mode . "bsd")
	(c++-mode . "bsd")
	(other . "gnu")))

(if (or (>= emacs-major-version 24)
	(>= emacs-minor-version 4)) (progn

;;(set-face-attribute 'default nil :height 100)

;;  )

;; magit: /usr/bin/emacsclient.emacs24
;; (with-editor-debug)
;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient.emacs24")
;; M-x find-library cl-lib
;;(require 'cl-lib)
;;(require 'cl)
;;(require 'cl-macs)

(require 'config/constants.el)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(require 'cl)
(require 'flycheck nil t) ;; -mode
(message "[*] %s retired flycheck" (timestamp_str))
(require 'iswitchb-mode nil t)
;;(require 'projmake-mode nil t)
(message "[*] %s retired projmake-mode" (timestamp_str))
;; 2: load apps
;;(require 'vcs/git.el)
;;(require 'apps/eshell.el)
(message "[*] %s retired eshell" (timestamp_str))
;;(require 'apps/proced.el)

(setq org-agenda-files (quote ("~/todo.org" "~/git/org" )))
(require 'apps/org.el)

(message "[*] %s retired org" (timestamp_str))
(require 'ux/popups.el)
(require 'ux/mark.el)
(require 'utils/compile.el)
(message "[*] %s retired compile" (timestamp_str))
;;(require 'utils/ctags.el)
(require 'utils/openfile.el)
(require 'utils/flycheck.el)

;;(require 'haskell-mode)
;;(require 'utils/irc.el)
(message "[*] %s retired irc" (timestamp_str))


(require 'ggtags)
(if
  (or
    (eq system-type 'cygwin)
    (string-match "Microsoft"  ;; Linux subsystem for Windows
		  (with-temp-buffer
		    (shell-command "uname -r" t)
		    (goto-char (point-max))
		    (delete-char -1)
		    (buffer-string))))
    (require 'ggtags)
    (require 'utils/openhelm.el)
)
(if (or (eq system-type 'freebsd) (eq system-type 'berkeley-unix))
    (progn
      (require 'simple)
      (normal-erase-is-backspace-mode)
      )
)
(message "[*] %s retired openhelm" (timestamp_str))

;;(require 'utils/hackernews)
;;(require 'utils/helm-hackernews)
(require 'utils/debug.el)
(require 'flymake-cursor)

;; 3: (re-)define keybindings
(message (format "[*] %s set keybindings" (timestamp_str)))
(require 'config/keybindings.el)

;;(require 'ux/popups.el.el)
;; 4: configure modes
(message (format "[*] %s config modes" (timestamp_str)))
(require 'modes/c-mode.el)
(require 'modes/ruby-mode.el)
(require 'modes/lua-mode.el)
(require 'modes/haskell-mode.el)
(require 'modes/web-mode.el)
(require 'modes/javascript-mode.el)
(require 'modes/org-mode.el)
(require 'modes/tex-mode.el)

(require 'ov)

(add-to-list 'load-path (expand-file-name "fringe" *.emacs.d.dir* ))
(message (format "[*] %s try load fringe" (timestamp_str)))
(require 'fringe)

(require 'flymake)
(defun flymake-simple-make-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-inplace nil nil "Makefile" 'flymake-get-make-cmdline))

(require 'hideshow-org)
(if
    (require 'hideif-changed nil t) ;; hide-ifdef-block show-ifdef-block
    (progn
      (message "[+] >> hideif-changed loaded")
      (setq hide-ifdef-shadow nil)
      )
  (message "[+] >> hideif-changed failed")

  )

(require 'pp)



;;(when (>= emacs-major-version 24)
;;  (require 'package)
;;  (package-initialize)
;;  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;  (package-install 'slime)

;;(add-to-list 'package-archives                 '("e6h" . "http://www.e6h.org/packages/") t)
;;    (package-initialize) ;; You might already have this line
;;(add-to-list 'package-archives           '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(package-initialize)
;;(when (not package-archive-contents)  (package-refresh-contents))
;; (package-install 'flycheck)
;; (package-install 'back-button)
;; (package-install 'wanderlust)
;; (package-install 'projmake-mode)
;; (package-install 'perspective)
;; (package-install 'helm) -config)
;;  (list-packages)
;;  (require 'mode-compile)
;; (package-install 'wanderlust)
;; (package-install 'wl)
;; (package-install 'dash) (require 'dash)
;; (package-install 'magit)
;; (list-packages)
;; (require 'mode-compile)
;;  (list-packages)
;;  (require 'mode-compile)
;; (require 'install-elisp)
;; (install-elisp "https://raw.githubusercontent.com/emacsmirror/mode-compile/master/mode-compile.el
;; (install-elisp "http://www.emacswiki.org/emacs/download/flymake-cursor.el")
;; (require 'cl)
;; (require 'sr-speedbar)
;; (require 'projmake-mode)
;;
;; (require 'flymake-chromium)
;;
;;(defun my-mode-hook ()
;; (projmake-mode)
;;    (projmake-search-load-project))
;;")
;; )

;;(require 'package) ;; You might already have this line
;;(add-to-list 'package-archives '("e6h" . "http://www.e6h.org/packages/") t)
;; (package-initialize) ;; You might already have this line

(condition-case nil
    (progn
      (autoload 'wl "wl" "Wanderlust" t))
  (error
   (message "Wanderlust not loaded")))

(require 'windmove)

;;(desktop-save-mode 1)
;;(when (fboundp 'winner-mode)
;;      (winner-mode 1))

;; (setq wg-prefix-key (kbd "C-c w"))
;; (setq wg-morph-on nil)
;; (setq wg-query-for-save-on-emacs-exit nil)
;; (require 'workgroups)
;; (workgroups-mode 1)
;; (setq wg-no-confirm t
;;       wg-file "~/.emacs.d/.workgroups"
;;       wg-use-faces nil
;;       wg-switch-on-load nil)
;; (add-hook 'kill-emacs-hook
;; 	  (lambda ()
;; 	     (condition-case nil
;; 		 (wg-save wg-file)
;; 	       (error nil))))
;; (wg-load wg-file)

(require 'back-button nil t)

(require 'remember)

(require 'utils/dired.el)

;;(org-remember-insinuate)

))

;;
;;(require 'perspective)
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(safe-local-variable-values
   (quote
    ((py-indent-offset . 4)
     (flycheck-clang-include-path "." "inc")
     (flycheck-gcc-include-path "." "inc")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq visible-bell 1)

;; try load local dotfiles.lo
(if (file-exists-p "~/.emacs.config")
		   (load "~/.emacs.config"))
