;;(define-key input-decode-map "\e[1;2A" [S-up])
;;(if (equal "xterm" (tty-type))
  ;;  (define-key function-key-map "\e[1;2A" [S-up])
;;  )

(defadvice terminal-init-xterm (after select-shift-up activate)

  (define-key function-key-map "\e[[D" [M-up])                                         (define-key function-key-map "\e[[B" [M-down])                                                                                                                                                                                                                                    
  (define-key function-key-map "\e[1;9C" [M-right])
  (define-key function-key-map "\e[1;2A" [S-up])
  
  (define-key function-key-map "\e[1;2D" [S-left])  
  (define-key function-key-map "\e[1;2C" [S-right])  
  (define-key function-key-map "\e[1;2B" [S-down])  
  ;;(define-key function-key-map "\e[1;2A" [S-up])  
  (define-key function-key-map "\e[1;2F" [S-end])  
  (define-key function-key-map "\e[1;2H" [S-home])
  )

(global-set-key [S-up]  'scroll-down)

;;(global-set-key [M-up]  'scroll-up)
;;(global-set-key [M-down]  'scroll-down)


;; (add-hook 'term-setup-hook                                                                                                                                                                         
;; '(lambda ()                                                                                                                                                                                      
;;  (define-key function-key-map "\e[1;9A" [M-up])                                                                                                                                                
;;  (define-key function-key-map "\e[1;9B" [M-down])                                                                                                                                              
;;  (define-key function-key-map "\e[1;9C" [M-right])                                                                                                                                             
;;  (define-key function-key-map "\e[1;2A" [S-up])                                                                                                                                                
;;  (define-key function-key-map "\e[1;9D" [M-left]))) 

;; (if (equal "xterm" (tty-type))  (define-key input-decode-map "\e[1;2A" [S-up]))

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

;; 3: configure modes
(message (format "[*] config modes"))
(require 'modes/c-mode.el)

