

;; 1: hkset load path
(if (version< emacs-version "24.3") (setq user-emacs-directory "~/.emacs.d/"))

(defconst *.emacs.d.lisp.dir*
  (concat (file-name-directory (or load-file-name buffer-file-name)) "lisp")
  "path to .emacs.d/lisp")
(message (format "[*] starting emacs config from %s" *.emacs.d.lisp.dir*))
(add-to-list 'load-path *.emacs.d.lisp.dir*)
(add-to-list 'load-path (expand-file-name "lib" *.emacs.d.lisp.dir*  ))
(require 'config/constants.el)

;; 2: (re-)define keybindings
(message (format "[*] set keybindings"))
(require 'config/keybindings.el)

;; 2: load apps
(require 'vcs/git.el)
(require 'apps/eshell.el)
(require 'apps/proced.el)

;;(require 'ux/popups.el.el)

;; 3: configure modes
(message (format "[*] config modes"))
(require 'modes/c-mode.el)


