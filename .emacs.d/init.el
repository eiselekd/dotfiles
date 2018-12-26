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

(add-to-list 'load-path (expand-file-name "lib/ghc-mod" *.emacs.d.lisp.dir* ))
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

;;(when (require 'quilt nil t)
;;  (progn
;;    (message "[*] quilt loaded")
;;    ))

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
;;(require 'magit)

(require 'config/constants.el)
(require 'nhexl-mode)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(require 'cl)
(require 'flycheck nil t) ;; -mode
(message "[*] %s retired flycheck" (timestamp_str))
(require 'iswitchb-mode nil t)
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

;;(require 'haskell-mode)
;;(require 'utils/irc.el)
(message "[*] %s retired irc" (timestamp_str))

(defun prepareHelm ()
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
      (helm-do-grep-1 (list default-directory) arg))


    )



  )





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

(global-set-key
 (kbd "ESC M-h")
 (lambda ()(interactive) (progn
			   (haskell-interactive-start))))

;;(require 'modes/web-mode.el)
;;(require 'modes/javascript-mode.el)
(require 'modes/org-mode.el)
;;(require 'modes/tex-mode.el)

;;(ido-mode 1)

(require 'ov)

(add-to-list 'load-path (expand-file-name "fringe" *.emacs.d.dir* ))
(message (format "[*] %s try load fringe" (timestamp_str)))
(require 'fringe)

;;(require 'flymake)
;;(defun flymake-simple-make-init ()
;;  (flymake-simple-make-init-impl 'flymake-create-temp-inplace nil nil "Makefile" 'flymake-get-make-cmdline))

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

(require 'windmove)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

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
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(frame-background-mode (quote dark))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((verilog-library-directories "." "../../cortexm7/verilog")
     (py-indent-offset . 4)
     (flycheck-clang-include-path "." "inc")
     (flycheck-gcc-include-path "." "inc"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#ff7f00")
     (60 . "#ffbf00")
     (80 . "#b58900")
     (100 . "#ffff00")
     (120 . "#ffff00")
     (140 . "#ffff00")
     (160 . "#ffff00")
     (180 . "#859900")
     (200 . "#aaff55")
     (220 . "#7fff7f")
     (240 . "#55ffaa")
     (260 . "#2affd4")
     (280 . "#2aa198")
     (300 . "#00ffff")
     (320 . "#00ffff")
     (340 . "#00ffff")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(add-to-list 'custom-theme-load-path (expand-file-name "emacs-color-theme-solarized" *.emacs.d.lisp.dir*  ))

;;;; ======================== themes ===============================
;;;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(add-to-list 'custom-theme-load-path (expand-file-name "themes/emacs-color-theme-solarized" *.emacs.d.dir*  ))
(add-to-list 'load-path              (expand-file-name "themes/emacs-color-theme-solarized" *.emacs.d.dir*  ))

(add-to-list 'custom-theme-load-path (expand-file-name "themes/solarized-emacs" *.emacs.d.dir*  ))
(add-to-list 'load-path              (expand-file-name "themes/solarized-emacs" *.emacs.d.dir*  ))

(setq solarized-distinct-fringe-background t)
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)




;; (if (display-graphic-p)
;;     (progn
;;       (add-to-list 'custom-theme-load-path (expand-file-name "emacs-color-theme-solarized" *.emacs.d.lisp.dir*  ))
;;       (load-theme 'solarized-dark t)
;;       )
;;   (progn 
;;     ;;(load-theme 'zenburn t)
;;     (add-to-list 'custom-theme-load-path (expand-file-name "solarized-emacs" *.emacs.d.lisp.dir*  ))
;;     (load-theme 'solarized t)
;;     ))


;;(require 'color-theme)
;;(require 'color-theme-solarized)
;;(color-theme-initialize)


(if (not (display-graphic-p))
    (progn
      (load (expand-file-name "themes/emacs-color-theme-solarized/solarized-theme.el" *.emacs.d.dir*  ))
      (setq frame-background-mode 'dark)
      (load-theme 'solarized t))
  (load-theme 'solarized-dark t)
  )

;; (if (display-graphic-p)
;;     (progn
;;       (add-to-list 'custom-theme-load-path (expand-file-name "emacs-color-theme-solarized" *.emacs.d.lisp.dir*  ))
;;       (load-theme 'solarized-dark t)
;;       )
;;   (progn 
;;     ;;(load-theme 'zenburn t)
;;     (add-to-list 'custom-theme-load-path (expand-file-name "solarized-emacs" *.emacs.d.lisp.dir*  ))
;;     (load-theme 'solarized t)
;;     ))


  

;;;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;; ======================== themes ===============================


(setq visible-bell 1)

;; try load local dotfiles.lo
(if (file-exists-p "~/.emacs.config")
		   (load "~/.emacs.config"))


(setq inhibit-startup-screen t)

;; (add-hook
;;  'after-make-frame-functions
;;  (lambda (frame)
;;    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;      (message (format "[*] mode %s background" mode))
;;      (set-frame-parameter frame 'background-mode mode)
;;      (set-terminal-parameter frame 'background-mode mode))
;;    (enable-theme 'solarized)))
