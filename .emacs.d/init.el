
(setq ns-right-alternate-modifier nil)

;; 1: hkset load path
(if (version< emacs-version "24.3") (setq user-emacs-directory "~/.emacs.d/"))

(defconst *.emacs.d.lisp.dir*
  (concat (file-name-directory (or load-file-name buffer-file-name)) "lisp")
  "path to .emacs.d/lisp")
(message (format "[*] starting emacs config from %s" *.emacs.d.lisp.dir*))
(add-to-list 'load-path *.emacs.d.lisp.dir*)
(add-to-list 'load-path (expand-file-name "lib" *.emacs.d.lisp.dir*  ))
(require 'config/constants.el)

(require 'cl)
;; 2: load apps
(require 'vcs/git.el)
(require 'apps/eshell.el)
(require 'apps/proced.el)
(require 'ux/popups.el)
(require 'utils/compile.el)

;; 3: (re-)define keybindings
(message (format "[*] set keybindings"))
(require 'config/keybindings.el)

;;(require 'ux/popups.el.el)

;; 4: configure modes
(message (format "[*] config modes"))
(require 'modes/c-mode.el)


;;(when (>= emacs-major-version 24)
;;  (require 'package)
;;  (package-initialize)
;;  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives           '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(package-initialize)
;;(when (not package-archive-contents)  (package-refresh-contents))
;;  (list-packages)
;;  (require 'mode-compile)
;; (require 'install-elisp)
;; (install-elisp "https://raw.githubusercontent.com/emacsmirror/mode-compile/master/mode-compile.el" )
;;  )
