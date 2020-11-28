
;;* Init

;;; Local Variables:
;;; End:

;; -*- orgstruct-heading-prefix-regexp: ";;" -*-

;; note: META-: :  eval
;;       ctrl-e :  eval under cursor
;; note: terminal.app old emacs: install "brew", then "brew install emacs"
;; cedet-1-1: emacs -q --no-site-file -l cedet-build.el -f cedet-build
;; note: cedet install: move object-class-fast in eieio.el to top
;; dbg: C-u M-x eval-defun
;; use lexical-bindutfing: t
;; toggle-debug-on-error
(setq ns-right-alternate-modifier nil)
(setq start-time (current-time))

;; (add-to-list 'load-path (substitute-in-file-name "$HOME/git/benchmark-init-el"))
;; (require 'benchmark-init)
;; (require 'benchmark-init-modes)
;; (benchmark-init/activate)

;; 1: hkset load path
(if (version< emacs-version "24.3") (setq user-emacs-directory "~/.emacs.d/"))

(defun timestamp_str ()
  (let ((n (time-subtract (current-time) start-time)))
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
(add-to-list 'load-path (expand-file-name "evil" *.emacs.d.lisp.dir* )) ;; async for helm
(add-to-list 'load-path (expand-file-name "lib/swiper" *.emacs.d.lisp.dir* )) ;; async for helm
(setq ad-redefinition-action 'accept) ;;ad-handle-definition: `tramp-read-passwd' got redefined
(add-to-list 'load-path (expand-file-name "company-mode" *.emacs.d.lisp.dir* ))

(add-to-list 'load-path (expand-file-name "lib/ghc-mod" *.emacs.d.lisp.dir* ))
(add-to-list 'load-path (expand-file-name "lib/magit" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/tuareg" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/merlin" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/transient" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/haskell-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" *.emacs.d.lisp.dir* ))
(add-to-list 'load-path (expand-file-name "lib/eglot" *.emacs.d.lisp.dir*  ))
(if (version<= emacs-version "26.3")
    (add-to-list 'load-path (expand-file-name "lib-26.3" *.emacs.d.lisp.dir*  )))
(add-to-list 'load-path (expand-file-name "lib/lsp-ui" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/lsp-haskell" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/cling-mode" *.emacs.d.lisp.dir*  ))
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
(add-to-list 'load-path (expand-file-name "lib/realgud" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/raku-mode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/polymode" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "lib/poly-markdown" *.emacs.d.lisp.dir* ))
(add-to-list 'load-path (expand-file-name "lib/racket-mode" *.emacs.d.lisp.dir* ))


;;* Encoding

;; printf "\xe2\x80\xa6"
;; UTF-8 support
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;* Modes
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

;;* Programming
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
	;;)

      ;;(require 'powerline) ;; status line
      ;;(powerline-default-theme)
      ;;(setq powerline-default-separator 'utf-8)

      ;;"Get the current default separator. Always returns utf-8 in non-gui mode."

      ;;(require 'cl)
      ;;(require 'flycheck nil t) ;; -mode
      (require 'iswitchb-mode nil t)

      ;;(require 'cl-lib)
      ;;(require 'dash)

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
      ;;(require 'apps/org.el)

      (message "[*] %s retired org" (timestamp_str))
      (require 'ux/popups.el)
      (require 'ux/mark.el)
      (require 'utils/compile.el)
      (message "[*] %s retired compile" (timestamp_str))
      ;;(require 'utils/ctags.el)
      (require 'utils/openfile.el)
      (message "[*] %s retired utils/openfile" (timestamp_str))
      ;;(require 'flycheck nil t) ;; -mode
      (require 'utils/flycheck-init.el)
      (message "[*] %s retired flycheck-init" (timestamp_str))
      (require 'utils/spell.el)
      (message "[*] %s retired spell" (timestamp_str))
      (require 'utils/tty.el)
      (message "[*] %s retired tty" (timestamp_str))
      (require 'utils/irc.el)


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
      (require 'fringe)
      (message (format "[*] %s retire fringe" (timestamp_str)))

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
      (message "[*] %s retire remember" (timestamp_str))


      ;;(org-remember-insinuate)

      ))

(require 'hideshow-org)
(message "[*] %s retire hide-show" (timestamp_str))
(if (require 'hideif-changed nil t) ;; hide-ifdef-block show-ifdef-block
    (progn
      (message "[+] %s hideif-changed loaded" (timestamp_str))
      (setq hide-ifdef-shadow nil)
      )
  (message "[+] >> hideif-changed failed"))
(require 'pp)
(message "[*] %s retire pp" (timestamp_str))

(require 'utils/dired.el)
(message "[*] %s retire dired" (timestamp_str))
(require 'utils/occur.el)
(require 'utils/occur-util.el)
;;(require 'color-occur)
(global-set-key (kbd "M-f")  'utils/dired-grep-rec-curdir)
(global-set-key (kbd "M-Ä")  (lambda ()(interactive) (call-interactively 'utils/occur-multi)))
(global-set-key (kbd "M-m")  'menu-bar-mode)

;; 3: (re-)define keybindings
(message (format "[*] %s set keybindings" (timestamp_str)))
(require 'config/keybindings.el)
(message "[*] %s retire keybinding" (timestamp_str))

;;(require 'ux/popups.el.el)
;; 4: configure modes
(message (format "[*] %s config modes" (timestamp_str)))
(require 'modes/c-mode.el)
(message "[*] %s retire c-mode" (timestamp_str))
(require 'modes/ruby-mode.el)
(message "[*] %s retire ruby-mode" (timestamp_str))
(require 'modes/lua-mode.el)
(message "[*] %s retire lua-mode" (timestamp_str))
(require 'modes/haskell-mode.el)
(message "[*] %s retire haskell-mode" (timestamp_str))
(require 'modes/cling-mode.el)
(message "[*] %s retire cling-mode" (timestamp_str))
(require 'modes/rust-mode.el)
(message "[*] %s retire rust-mode" (timestamp_str))
(require 'modes/elisp-mode.el)
(global-set-key (kbd "ESC M-h") (lambda ()(interactive) (progn (haskell-interactive-start))))
(message "[*] %s retire elisp-mode" (timestamp_str))
;;(require 'modes/web-mode.el)
(require 'modes/javascript-mode.el)
(message "[*] %s retire javascript-mode" (timestamp_str))
(require 'modes/org-mode.el)
(message "[*] %s retire org-mode" (timestamp_str))
(require 'modes/l8-mode.el)
(message "[*] %s retire l8-mode" (timestamp_str))
(require 'modes/ocaml-mode.el)
(message "[*] %s retire ocaml-mode" (timestamp_str))
(require 'modes/raku-mode.el)
(message "[*] %s retire raku-mode" (timestamp_str))
(require 'modes/tex-mode.el)
;;(ido-mode 1)
(message (format "[*] %s retired config modes" (timestamp_str)))


(require 'windmove)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)



;; hide modeline when only one frame
(autoload 'hide-mode-line "hide-mode-line" nil t)
;;(hide-mode-line)

;; ctrl-x ctrl-b
(require 'utils/buffer.el)

;;* Themes
;;;; ======================== themes ===============================

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" *.emacs.d.dir*  ))
;;(load-theme 'cyberpunk t)

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

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-ghc-show-info t)
;;  '(custom-safe-themes
;;    '("5cce533073e34bfd8ea173887b2566b2b5165309231bdd6088ea92ee76ce114b" "fc5ad2db8ba71ce0c0d989de5cf60f8dbe9562b0356901d370b5c3440b316475" default))
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t)
;;  '(haskell-process-suggest-remove-import-lines t)
;;  '(haskell-process-type 'cabal-repl)
;;  '(haskell-tags-on-save t)
;;  '(package-selected-packages
;;    '(dash yasnippet zerodark-theme use-package magit counsel-projectile bash-completion auto-package-update)))

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
  (message (format "[*] mode %s background" (timestamp_str) mode))
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'background-mode mode)
    (set-terminal-parameter frame 'background-mode mode)
    ;;(custom-set-variables '( frame-background-mode mode))

    )
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

(require 'dash)
(setq themes-list
      `(
	("light"     . ,( lambda () (progn (set-dark-light-theme 'light)) ))
	("dark"      . ,( lambda () (progn (set-dark-light-theme 'dark)) ))
	("cyberpunk" . ,( lambda () (progn (load-theme 'cyberpunk t) )))
	))

(defun cycle-theme-sel ()
  (interactive)
  (setq themes-list (-rotate 1 themes-list))
  ;;(custom-set-variables '( frame-background-mode mode))
  (message (format "[+] enable theme %s" (car (nth 0 themes-list))))
  (funcall (cdr (nth 0 themes-list))))

;;(setq active-theme 'light)
(setq active-theme 'dark)
(set-dark-light-theme active-theme)

(global-set-key (kbd "ESC t") 'cycle-theme-sel)
(global-set-key (kbd "ESC M-t") 'xterm-mouse-mode)
(global-set-key (kbd "ESC T")
 (lambda ()(interactive)
   (progn
     (hide-mode-line)
     (xterm-mouse-mode (if (bound-and-true-p hide-mode-line) 0 1))
     ( force-mode-line-update))))

;;(xterm-mouse-mode 0)
;;(xterm-mouse-mode 1)

;; (add-hook
;;  'after-make-frame-functions
;;  (lambda (frame)
;;    (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;      (message (format "[*] mode %s background" mode))
;;      (set-frame-parameter frame 'background-mode mode)
;;      (set-terminal-parameter frame 'background-mode mode))
;;    (enable-theme 'solarized)))


;;(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;;(tool-bar-mode 0)

;;;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;; ======================== themes ===============================
(message "[*] %s start loading theme" (timestamp_str))

;;* Layout
(setq visible-bell 1)

;; try load local dotfiles.lo
(if (file-exists-p "~/.emacs.config")
    (load "~/.emacs.config"))
(if (file-exists-p "~/.emacs.dispatch")
    (load "~/.emacs.dispatch"))



(setq inhibit-startup-screen t)


;; http://ivanmalison.github.io/dotfiles/#tile
;;(require 'tile)
;; https://github.com/abo-abo/hydra
;;(require 'hydra)
;;(require 'use-package)

(set-default 'truncate-lines t)

;; (use-package tile
;;   :bind ("C-c t" . imalison:hydra-tile/body)
;;   :config
;;   (progn
;;     (defvar imalison:tall-tile-strategy (tile-split-n-tall 3))
;;     (defvar imalison:wide-tile-strategy tile-wide)
;;     (defvar imalison:master-tile-strategy (tile-argument-buffer-fetcher
;;                                            :layout tile-master-left))
;;     (require 'hydra)
;;     (defhydra imalison:hydra-tile
;;       nil
;;       "tile"
;;       ("t" (tile :strategy imalison:tall-tile-strategy))
;;       ("w" (tile :strategy imalison:wide-tile-strategy))
;;       ("m" (tile :strategy imalison:master-tile-strategy))
;;       ("s" tile-select)
;;       ("0" (tile :strategy tile-one))
;;       ("n" tile)
;;       ("l" winner-undo))
;;     (setq tile-cycler
;;           (tile-strategies :strategies
;;                            (list imalison:tall-tile-strategy
;;                                  imalison:master-tile-strategy
;;                                  imalison:wide-tile-strategy
;;                                  tile-one)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)
(message "[*] %s retire wsbutler" (timestamp_str))

;;(global-whitespace-mode)
;;(setq whitespace-style '(face trailing lines tabs big-indent))

;; (progn
;;  ;; Make whitespace-mode with very basic background coloring for whitespaces.
;;   ;; http://ergoemacs.org/emacs/whitespace-mode.html
;;   (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

;;   ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
;;   (setq whitespace-display-mappings
;;         ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
;;         '(
;;           (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;           (newline-mark 10 [182 10]) ; LINE FEED,
;;           (tab-mark 9 [9655 9] [92 9]) ; tab
;;           )))

(setq-default show-trailing-whitespace t)
;;(use-package ws-butler
;;  :init
;;  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; (setq helm-debug 't)


;;* Startup screen
(find-file default-directory)

;;* Powerline
;; (dired-mode)
;; (tmm-menubar-mouse )

(require 'utils/powerlineutil.el)

;;(define-key global-map [mode-line mouse-1] 'my-press-me)
;;(xterm-mouse-mode 1)
;;(setq x-select-enable-clipboard t)

;;(use-package minions
;;  :config (minions-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot utop merlin tuareg))
 '(safe-local-variable-values '((whitespace-line-column . 80))))


(setq vc-follow-symlinks 't)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(message "[*] %s init finish" (timestamp_str))

;; replace default mode for .lex
(cl-remove "\\.lex\\'" auto-mode-alist :test 'equal :key 'car)
(cl-remove "\\.y\\'" auto-mode-alist :test 'equal :key 'car)

(autoload 'flex-mode "flex-mode" "Autoload flexmode." t)
(autoload 'bison-mode "bison-mode" "Autoload bisonmode." t)
(autoload 'racket-mode "modes/racket-mode" "Autoload racketmode." t)
(add-to-list 'auto-mode-alist '("\\.lex\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(put 'scroll-left 'disabled nil)

(setq smerge-command-prefix "\C-cv")

;;(global-set-key (kbd "C-v") 'clipboard-yank)

(setq x-select-enable-primary  nil)
(setq x-select-enable-clpiboard 't)

(global-set-key (kbd "C-M-h") (lambda ()(interactive)
				(progn
				  (require 'which-key)
				  (which-key-mode))))

;;(global-set-key (kbd "C-M-h") (lambda ()(interactive)
;;				(progn
;;				  (require 'showkey)
;;				  (showkey-log-mode))))

;; (require 'evil)

(require 'modes/poly.el)
(require 'zoom-window)
(global-set-key (kbd "C-L")  'zoom-window-zoom)

(global-set-key (kbd "M-E")
		(lambda ()(interactive)
		  (progn
		    (require 'evil)
		    (evil-mode))))
