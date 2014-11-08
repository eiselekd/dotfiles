;; 1: set load path
(defconst *.emacs.d.lisp.dir*
  (expand-file-name "lisp" user-emacs-directory)
  "path to .emacs.d/lisp")
(message (format "[*] starting emacs config from %s" *.emacs.d.lisp.dir*))
(add-to-list 'load-path *.emacs.d.lisp.dir*)
(add-to-list 'load-path (expand-file-name "lib" *.emacs.d.lisp.dir*  ))
(require 'config/constants.el)


;; 2: (re-)define keybindings
(message (format "[*] set keybindings"))
(require 'config/keybindings.el)

;; 3: configure modes
(message (format "[*] config modes"))
(require 'modes/c-mode.el)

