
;; (install-elisp "http://www.emacswiki.org/emacs/download/apropos-fn%2bvar.el" )
;; (require 'apropos-fn-var)
;; (apropos-variable  "-mode-map$" )
;; (apropos-variable "function-key-map")
;; note: show keybinding "c-h k", "m-x where-is", "c-h m",
;; note: alt(option) key in mac term enable as META: Menue->Terminal->Preferences => check "option as meta" 
;; note: scroll on terminal+macbook: fn + up|down

;; ctrl-c ctrl-v ctrl-y
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy nil) ;; Non-standard Windows behaviour

;; terminal escape hell
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key function-key-map "\e[[D" [M-up])                                         
  (define-key function-key-map "\e[[B" [M-down])
  (define-key function-key-map "\e[1;9C" [M-right])
  (define-key function-key-map "\e[1;2A" [S-up])
  (define-key function-key-map "\e[1;2D" [S-left])  
  (define-key function-key-map "\e[1;2C" [S-right])  
  (define-key function-key-map "\e[1;2B" [S-down])  
  (define-key function-key-map "\e[1;2F" [S-end])  
  (define-key function-key-map "\e[1;2H" [S-home])
  )

;; mavigate windows with arrow
(windmove-default-keybindings 'meta)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;;(global-set-key [M-up]  'scroll-up)
;;(global-set-key [M-down]  'scroll-down)
;; mark and clipboard:
;;(global-set-key (kbd "C-SPC") 'set-mark-command) 
;;(global-set-key (kbd "C-w")   'kill-region) 
;;(global-set-key (kbd "M-w")   'kill-ring-save) 
;;(global-set-key (kbd "C-y")   'yank) 
;; navigating:
;;(global-set-key (kbd "<down>")    'next-line) 
;;(global-set-key (kbd "<up>")      'previous-line) 
;;(global-set-key (kbd "<left>")    'backward-char) 
;;(global-set-key (kbd "<right>")   'forward-char)
;; undo
;;(global-set-key (kbd "C-_") 'undo) 
;;(define-key input-decode-map "\e[1;5C" [(control right)])
;;(global-set-key [M-right] 'forward-word) 
;;(global-set-key [M-left]  'backward-word)
;;(global-set-key [M-up]    'scroll-up) 
;;(global-set-key [M-down]  'scroll-down)
;;(global-set-key (kbd "ESC-<up>")  'scroll-up) 
;; (global-set-key [M-up] 'scroll-up)
;; (global-set-key [M-down] 'scroll-down)
;; (global-set-key [(meta up)] 'transpose-line-up_)
;; (global-set-key [(meta down)] 'transpose-line-down)
;;(global-set-key (kbd "M-<LEFT>")  'backwardkfuir-word2_) 
;;(global-set-key (kbd "M-<RIGHT>") 'forward-word2_) 
;;(global-set-key (kbd "C-<up>")   'scroll-up2_) 
;;(global-set-key (kbd "C-<down>") 'scroll-down2_) 
;;(global-set-key (kbd "<f1>") 'shell)



(provide 'config/keybindings.el)


;; test
