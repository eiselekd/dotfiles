;; note: META-: :  eval
;;       ctrl-e :  eval under cursor
;; note: terminal.app old emacs: install "brew", then "brew install emacs"
;; cedet-1-1: emacs -q --no-site-file -l cedet-build.el -f cedet-build
;; note: cedet install: move object-class-fast in eieio.el to top
(setq ns-right-alternate-modifier nil)

;; 1: hkset load path
(if (version< emacs-version "24.3") (setq user-emacs-directory "~/.emacs.d/"))

(defconst *.emacs.d.dir*
  (file-name-directory (or load-file-name buffer-file-name))
  "path to .emacs.d")
(defconst *.emacs.d.lisp.dir*
  (concat *.emacs.d.dir* "lisp")
  "path to .emacs.d/lisp")
(message (format "[*] starting emacs config from %s" *.emacs.d.lisp.dir*))
(add-to-list 'load-path *.emacs.d.lisp.dir*)
(add-to-list 'load-path (expand-file-name "lib" *.emacs.d.lisp.dir*  ))
(add-to-list 'load-path (expand-file-name "wanderlust/elmo" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "wanderlust/wl" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "flim" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "semi" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "apel" *.emacs.d.dir* ))
(add-to-list 'load-path (expand-file-name "helm" *.emacs.d.lisp.dir* ))

(require 'config/constants.el)

(require 'cl)
(require 'flycheck nil t) ;; -mode
(require 'iswitchb-mode nil t)
(require 'projmake-mode nil t)
;; 2: load apps
(require 'vcs/git.el)
(require 'apps/eshell.el)
(require 'apps/proced.el)
(require 'apps/org.el)
(require 'ux/popups.el)
(require 'ux/mark.el)
(require 'utils/compile.el)
(require 'utils/debug.el)
(require 'utils/ctags.el)
(require 'utils/openfile.el)
(require 'utils/openhelm.el)
(require 'flymake-cursor)
(require 'utils/flycheck.el)

;; 3: (re-)define keybindings
(message (format "[*] set keybindings"))
(require 'config/keybindings.el)

;;(require 'ux/popups.el.el)
;; 4: configure modes
(message (format "[*] config modes"))
(require 'modes/c-mode.el)

(add-to-list 'load-path (expand-file-name "fringe" *.emacs.d.dir* ))
(message (format "[*] try load fringe"))
(require 'fringe)

(require 'flymake)
(defun flymake-simple-make-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-inplace nil nil "Makefile" 'flymake-get-make-cmdline))

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

(setq wg-prefix-key (kbd "C-c w"))
(setq wg-morph-on nil)
(setq wg-query-for-save-on-emacs-exit nil)
(require 'workgroups)
(workgroups-mode 1)
(setq wg-no-confirm t
      wg-file "~/.emacs.d/.workgroups"
      wg-use-faces nil
      wg-switch-on-load nil)
(add-hook 'kill-emacs-hook
	  (lambda ()
	     (condition-case nil
		 (wg-save wg-file)
	       (error nil))))
(wg-load wg-file)

(require 'back-button nil t)

(require 'remember)
;;(org-remember-insinuate)





;;
;;(require 'perspective)
;;
