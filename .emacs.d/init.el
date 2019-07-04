;; note: META-: :  eval
;;       ctrl-e :  eval under cursor
;; note: terminal.app old emacs: install "brew", then "brew install emacs"
;; cedet-1-1: emacs -q --no-site-file -l cedet-build.el -f cedet-build
;; note: cedet install: move object-class-fast in eieio.el to top
;; dbg: C-u M-x eval-defun
;; use lexical-bindutfing: t
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

(add-to-list 'load-path (expand-file-name "lib/ghc-mod" *.emacs.d.lisp.dir* ))
(add-to-list 'load-path (expand-file-name "lib/magit" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/transient" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/haskell-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "dired-hacks" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/irony-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/company-irony" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/org-mode/lisp" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/org-mode/contrib/lisp" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/latex-preview-pane" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/hydra" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/use-package" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/js/Indium" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/js/js2-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/js/js2-refactor.el" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/js/multiple-cursors.el" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/js/yasnippet" *.emacs.d.lisp.dir*  ))

;; printf "\xe2\x80\xa6"
;; UTF-8 support
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(require 'cc-mode)
(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(c-mode . "bsd")
	(c++-mode . "bsd")
	(other . "gnu")))

;;(when (require 'quilt nil t)
;;  (progn
;;    (message "[*] quilt loaded")
;;    ))

(if (or (>= emacs-major-version 24)
	(>= emacs-minor-version 4))
    (progn

      ;;(set-face-attribute 'default nil :height 100)

      ;;  )

      ;; magit: /usr/bin/emacsclient.emacs24
      ;; (with-editor-debug)
      ;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient.emacs24")
      ;; M-x find-library cl-lib
      ;;(require 'cl-lib)
      ;;(require 'cl)
      ;;(require 'cl-macs)
      ;;(require 'magit)

      (require 'config/constants.el)
      (require 'nhexl-mode)

      (require 'ansi-color)
      (defun display-ansi-colors ()
	(interactive)
	(ansi-color-apply-on-region (point-min) (point-max)))

      ;;(if window-system
      	  (progn
      	    (require 'powerline) ;; status line
      	    (powerline-default-theme)
      	    (setq powerline-default-separator 'arrow))
	;;)

      ;;(require 'powerline) ;; status line
      ;;(powerline-default-theme)
      ;;(setq powerline-default-separator 'utf-8)

      ;;"Get the current default separator. Always returns utf-8 in non-gui mode."

      (require 'cl)
      (require 'flycheck nil t) ;; -mode
      (message "[*] %s retired flycheck" (timestamp_str))
      (require 'iswitchb-mode nil t)

      (require 'cl-lib)
      (require 'dash)

      ;;(-filter (lambda (p) (member (process-name p) '("haskell"))) (process-list))
      (defun noquery-process ()
       	(mapcar
       	 (lambda (p) (set-process-query-on-exit-flag p nil))
       	 (-filter (lambda (p) (member (process-name p) '("haskell"))) (process-list))))
      (defun his-tracing-function (orig-fun &rest args)
	(noquery-process)
	(message "[+] about to call save-buffers-kill-terminal")
	(let ((res (apply orig-fun args)))
	  (message "[+] save-buffers-kill-terminal exit")
	  res))
      (advice-add 'save-buffers-kill-terminal :around #'his-tracing-function)

      ;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
      ;; 	"Prevent annoying \"Active processes exist\" query when you quit Emacs."
      ;; 	(cl-letf (((symbol-function #'process-list)
      ;; 		   (lambda ()
      ;; 		     ()
      ;; 		     (-filter (lambda (p) (not (member (process-name p) '("haskell")))) (process-list))
      ;; 		     )))
      ;; 	  ad-do-it))

      ;;(require 'projmake-mode nil t)
      ;;(message "[*] %s retired projmake-mode" (timestamp_str))
      ;; 2: load apps
      ;;(require 'vcs/git.el)
      ;;(require 'apps/eshell.el)
      ;;(message "[*] %s retired eshell" (timestamp_str))
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

      (require 'haskell-mode)
      ;;(require 'utils/irc.el)
      ;;(message "[*] %s retired irc" (timestamp_str))

      (defun prepareHelm ()
	(require 'ggtags)
	(if
	    (or
	     (eq system-type 'cygwin)
	     (string-match
	      "Microsoft"  ;; Linux subsystem for Windows
	      (with-temp-buffer
		(shell-command "uname -r" t)
		(goto-char (point-max))
		(delete-char -1)
		(buffer-string))))
	    (require 'ggtags)
	  (require 'utils/openhelm.el)
	  (defun my-helm-pipe-grep-match (fun &rest args)
	    (let* ((patterns (split-string helm-pattern))
		   (helm-grep-default-command
		    (cl-reduce (lambda (grep pat)
				 (concat grep " | grep --color=always " pat))
			       (cdr patterns)
			       :initial-value (replace-regexp-in-string "%p" (car patterns) helm-grep-default-command))))
	      (apply fun args)))
	  (advice-add 'helm-grep--prepare-cmd-line :around 'my-helm-pipe-grep-match)
	  (defun helm-do-grep (&optional arg)
	    (interactive "P")
	    (helm-do-grep-1 (list default-directory) arg))))

      (if (or (eq system-type 'freebsd) (eq system-type 'berkeley-unix))
	  (progn
	    (require 'simple)
	    (normal-erase-is-backspace-mode)
	    ))

      (message "[*] %s retired openhelm" (timestamp_str))

      ;;(require 'utils/hackernews)
      ;;(require 'utils/helm-hackernews)
      (require 'utils/debug.el)
      (require 'flymake-cursor)
      (require 'ov)

      (add-to-list 'load-path (expand-file-name "fringe" *.emacs.d.dir* ))
      (message (format "[*] %s try load fringe" (timestamp_str)))
      (require 'fringe)

      ;;(require 'flymake)
      ;;(defun flymake-simple-make-init ()
      ;;  (flymake-simple-make-init-impl 'flymake-create-temp-inplace nil nil "Makefile" 'flymake-get-make-cmdline))



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

      ;; (require 'magit)

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

      ;; (condition-case nil
      ;;     (progn
      ;;       (autoload 'wl "wl" "Wanderlust" t))
      ;;   (error
      ;;    (message "Wanderlust not loaded")))


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


      ;;(org-remember-insinuate)

      ))

(require 'hideshow-org)
(if (require 'hideif-changed nil t) ;; hide-ifdef-block show-ifdef-block
    (progn
      (message "[+] >> hideif-changed loaded")
      (setq hide-ifdef-shadow nil)
      )
  (message "[+] >> hideif-changed failed"))
(require 'pp)

(require 'utils/dired.el)
(global-set-key (kbd "M-f")  'utils/dired-grep-rec-curdir)

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
(global-set-key (kbd "ESC M-h") (lambda ()(interactive) (progn (haskell-interactive-start))))
;;(require 'modes/web-mode.el)
(require 'modes/javascript-mode.el)
(require 'modes/org-mode.el)
;;(require 'modes/tex-mode.el)
;;(ido-mode 1)

(require 'windmove)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; hide modeline when only one frame
(autoload 'hide-mode-line "hide-mode-line" nil t)
(hide-mode-line)

;;;; ======================== themes ===============================
;;;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

;; (if
;;     (not (display-graphic-p))
;;     (progn
;;       (add-to-list 'custom-theme-load-path (expand-file-name "themes/emacs-color-theme-solarized" *.emacs.d.dir*  ))
;;       (load                                (expand-file-name "themes/emacs-color-theme-solarized/solarized-theme.el" *.emacs.d.dir*  ))
;;       (setq frame-background-mode 'dark)
;;       (load-theme 'solarized t))
;;   (progn
;;     (add-to-list 'custom-theme-load-path (expand-file-name "themes/solarized-emacs" *.emacs.d.dir*  ))
;;     (add-to-list 'load-path              (expand-file-name "themes/solarized-emacs" *.emacs.d.dir*  ))
;;     (load-theme 'solarized-dark t)
;;   ))


(setq solarized-distinct-fringe-background t)
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(add-to-list 'custom-theme-load-path (expand-file-name "themes/emacs-color-theme-solarized" *.emacs.d.dir*  ))
(load                                (expand-file-name "themes/emacs-color-theme-solarized/solarized-theme.el" *.emacs.d.dir*  ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("fc5ad2db8ba71ce0c0d989de5cf60f8dbe9562b0356901d370b5c3440b316475" default)))
 '(frame-background-mode mode)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (dash yasnippet zerodark-theme use-package magit counsel-projectile bash-completion auto-package-update))))

(put 'downcase-region 'disabled nil)


;; (add-hook
;;  'after-make-frame-functions
;;  (lambda (frame)
;;    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;      (message (format "[*] mode %s background" mode))
;;      (set-frame-parameter frame 'background-mode mode)
;;      (set-terminal-parameter frame 'background-mode mode))
;;    (enable-theme 'solarized)))

(defun set-dark-light-theme (mode)
  (message (format "[*] mode %s background" mode))
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'background-mode mode)
    (set-terminal-parameter frame 'background-mode mode)
    (custom-set-variables '( frame-background-mode mode)))
  (enable-theme 'solarized))

;;(add-to-list 'custom-theme-load-path (expand-file-name "themes/solarized-emacs" *.emacs.d.dir*  ))
;;(add-to-list 'load-path              (expand-file-name "themes/solarized-emacs" *.emacs.d.dir*  ))
;;       (add-to-list 'custom-theme-load-path (expand-file-name "themes/emacs-color-theme-solarized" *.emacs.d.dir*  ))
;;       (load                                (expand-file-name "themes/emacs-color-theme-solarized/solarized-theme.el" *.emacs.d.dir*  ))

;;(load-theme 'solarized t)
;;(load-theme 'solarized-dark t)

(defun toggle-dark-light-theme ()
   (interactive)
   (if (eq active-theme 'light)
       (setq active-theme 'dark)
     (setq active-theme 'light))
   (set-dark-light-theme active-theme))

(setq active-theme 'dark)
(set-dark-light-theme active-theme)

(global-set-key
 (kbd "ESC t")
 (lambda ()(interactive)
   (progn (toggle-dark-light-theme))))

(global-set-key
 (kbd "ESC T")
 (lambda ()(interactive)
   (progn
     (hide-mode-line)
     ( force-mode-line-update))))



;; (add-hook
;;  'after-make-frame-functions
;;  (lambda (frame)
;;    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;      (message (format "[*] mode %s background" mode))
;;      (set-frame-parameter frame 'background-mode mode)
;;      (set-terminal-parameter frame 'background-mode mode))
;;    (enable-theme 'solarized)))


(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;;(tool-bar-mode 0)

;;;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;; ======================== themes ===============================


(setq visible-bell 1)

;; try load local dotfiles.lo
(if (file-exists-p "~/.emacs.config")
    (load "~/.emacs.config"))


(setq inhibit-startup-screen t)


;; http://ivanmalison.github.io/dotfiles/#tile
(require 'tile)
;; https://github.com/abo-abo/hydra
(require 'hydra)
(require 'use-package)

(set-default 'truncate-lines t)

(use-package tile
  :bind ("C-c t" . imalison:hydra-tile/body)
  :config
  (progn
    (defvar imalison:tall-tile-strategy (tile-split-n-tall 3))
    (defvar imalison:wide-tile-strategy tile-wide)
    (defvar imalison:master-tile-strategy (tile-argument-buffer-fetcher
                                           :layout tile-master-left))
    (require 'hydra)
    (defhydra imalison:hydra-tile
      nil
      "tile"
      ("t" (tile :strategy imalison:tall-tile-strategy))
      ("w" (tile :strategy imalison:wide-tile-strategy))
      ("m" (tile :strategy imalison:master-tile-strategy))
      ("s" tile-select)
      ("0" (tile :strategy tile-one))
      ("n" tile)
      ("l" winner-undo))
    (setq tile-cycler
          (tile-strategies :strategies
                           (list imalison:tall-tile-strategy
                                 imalison:master-tile-strategy
                                 imalison:wide-tile-strategy
                                 tile-one)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; (setq helm-debug 't)
