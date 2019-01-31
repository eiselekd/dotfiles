;; http://www.nongnu.org/emacs-tiny-tools/keybindings/index-body.html
;; (install-elisp "http://www.emacswiki.org/emacs/download/apropos-fn%2bvar.el" )
;; (require 'apropos-fn-var)
;; (apropos-variable  "-mode-map$" )
;; (apropos-variable "function-key-map")
;; note: show keybinding "c-h k", "m-x where-is", "c-h m",
;; note: alt(option) key in mac term enable as META: Menue->Terminal->Preferences => check "option as meta"
;; note: scroll on terminal+macbook: fn + up|down

;; debug: "c-h l" : read key buffer
;; debug: "c-h k" <key> : keybind
;; debug: "c-h c" <key> : keybind
;; http://unix.stackexchange.com/questions/79374/are-there-any-linux-terminals-which-can-handle-all-key-combinations/79561#79561

;; ctrl-c ctrl-v ctrl-y
(cua-mode t)
(cua-selection-mode 1)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy nil) ;; Non-standard Windows behaviour

;; terminal escape hell
(defadvice terminal-init-xterm (after select-shift-up activate)

  (message (format "[*] term keycode define"))

  (define-key function-key-map "\e[[D" [M-up])
  (define-key function-key-map "\e[[B" [M-down])
  (define-key function-key-map "\e[1;9C" [M-right])

  (define-key function-key-map "\e[1;2A" [S-up])
  (define-key function-key-map "\e[1;2B" [S-down])
  (define-key function-key-map "\e[1;2C" [S-right])
  (define-key function-key-map "\e[1;2D" [S-left])

  ;; define this for mac : alt-shift + up,down,left,right
  (define-key function-key-map "\e[1;4A" [M-S-up])
  (define-key function-key-map "\e[1;4D" [M-S-left])
  (define-key function-key-map "\e[1;4C" [M-S-right])
  (define-key function-key-map "\e[1;4B" [M-S-down])

  (define-key function-key-map "\e[1;2F" [S-end])
  (define-key function-key-map "\e[1;2H" [S-home])
  )

  
;; mavigate windows with arrow
;;(windmove-default-keybindings 'meta)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; start egit
(global-set-key (kbd "M-e")  'shell)
(global-set-key (kbd "M-E")  (lambda()(interactive)
			       (ansi-term )))
;; start proced
(global-set-key (kbd "M-p")  'proced)

;; start proced
(global-set-key (kbd "M-h")  (lambda ()(interactive) (haskell-interactive-start)))

;; start proced
(global-set-key (kbd "M-d")  (lambda ()(interactive) (toggle-debug-on-error)))
;; start org-agenda
(global-set-key (kbd "M-a")  'org-agenda)
;; start erc
(global-set-key (kbd "M-I") 'utils/irc-djcb-erc-start-or-switch) ;; ERC
;; start org-agenda
(global-set-key (kbd "M-j")  'goto-line)

;;search
(global-set-key (kbd "C-c o") 'occur)


;; start magit
(global-set-key (kbd "M-G")  (lambda ()(interactive)
			       (progn
				 (with-executable 'git
				   (when (and
					  (require 'magit nil t)
					  (require 'utils/magit-util.el nil t))
				     (utils/magit-commit-all)
				     )))))
(global-set-key (kbd "M-g")  (lambda ()(interactive)
			       (progn
				 ;;(setq with-editor-file-name-history-exclude 1)
				 (with-executable 'git
				   (when (require 'magit nil t)
				     (magit-status))))))



;; interpose helm
(global-set-key (kbd "M-Q")  (lambda ()(interactive)
			       (progn
				 (prepareHelm)
				 (let ((current-prefix-arg '(10)))
				    (call-interactively 'helm-do-grep)))))
(global-set-key (kbd "M-.")  (lambda ()(interactive)
			       (progn
				 (prepareHelm)
				 (helm-gtags-dwim))))

;; start find-tag
;(global-set-key (kbd "M-?")  (lambda ()(interactive)(find-tag (thing-at-point 'word))))
;; open file under cursor
(global-set-key (kbd "M-5")  'xah-open-file-at-cursor)
;; goto function under cursor
(global-set-key (kbd "M-6")  'find-function-at-point)
;; goto workspace 1
(global-set-key (kbd "M-1")  (lambda ()(interactive)(wg-switch-to-workgroup (wg-get-workgroup 'name "w1"))))
;; goto workspace 2
(global-set-key (kbd "M-2")  (lambda ()(interactive)(wg-switch-to-workgroup (wg-get-workgroup 'name "w2"))))

(global-set-key (kbd "M-s")
		(lambda ()(interactive)
		  (progn
		    (when (require 'quilt nil t)
		      (progn
			(message "[*] quilt loaded")
			)))))

;; flymake
;;(global-set-key (kbd "M-F")  'utils/projmake-start)
(global-set-key (kbd "M-F")  'flymake-mode)

;; flycheck
;;(if (not (eq system-type 'darwin))
    ;; termninal send \033 f (Meta - f)
;;    (global-set-key (kbd "M-f")  'flycheck-mode))
(global-set-key (kbd "ESC M-c")  'flycheck-mode-verbose)
(global-set-key (kbd "ESC M-C")  'flycheck-mode-verbose-select)
(global-set-key (kbd "M-c")  'flycheck-mode)
(global-set-key (kbd "M-C")  (lambda ()(interactive) (call-interactively 'utils/projmake-sethere)))

;;(lambda ()(interactive)
;;			       (progn
;;				 (require 'flycheck nil t)
;;				 (require 'utils/flycheck.el)
;;				 'flycheck-mode)))
;;
(global-set-key (kbd "M-n")  (lambda ()(interactive)
			       (progn
				 (eww "https://news.ycombinator.com/")
				 )))

;; compile errors
(global-set-key (kbd "<f5>") 'utils/previous-error)
(global-set-key (kbd "<f6>") 'utils/next-error)

(global-set-key (kbd "ESC <f10>") 'menu-bar-open)

(defun utils/compile-keybind ()
    (global-set-key (kbd "<f9>") 'utils/compile))
(defun utils/debug-keybind ()
    (global-set-key (kbd "<f10>") 'utils/debug))
(defun utils/debug-gud-keybind ()

;;  (global-set-key (kbd "<f3>") 'gud-next)
  ;;(global-set-key (kbd "<f3>") 'gud-cont)
		(global-set-key (kbd "<f4>") 'gud-finish)
		(global-set-key (kbd "<f12>") 'gud-break)
		(define-key gud-mode-map (kbd "<up>") 'comint-previous-input)
		(define-key gud-mode-map (kbd "<down>") 'comint-next-input))




(defun utils/isgud ()
  (let ((mode major-mode))
    (or (string= mode 'gud-mode)
	(string= mode 'gdb-locals-mode)
	(string= mode 'gdb-inferior-io-mode)
	(string= mode 'gdb-frames-mode)
	(string= mode 'gdb-breakpoints-mode)
	(and (or (string= mode 'c-mode)
		 (string= mode 'c++-mode))
	     (get-buffer "*gud*")
	     ))))

(global-set-key (kbd "<f1>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F1 in major mode %s" mode)
				 (cond ((string= mode 'haskell-mode) (haskell/eval-haskell))
				       ((string= mode 'org-mode) (org-agenda))
				       ((utils/isgud) (progn
							(switch-to-buffer "*gud*")
							(gdb-restore-windows)
							(gud-refresh)
							))
				       (t (progn
					    (org-agenda)))
				       )
				 )))

(global-set-key (kbd "<f2>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F2 in major mode %s" mode)
				 (cond ((string= mode 'haskell-mode) (haskell-process-do-type))
				       ((or (string= mode 'org-mode)
					    (string= mode 'org-agenda-mode)) (call-interactively 'bh/org-sparse-tree))
				       ((utils/isgud) (call-interactively 'gud-step))
				       (t (progn t))
				       )
				 )))

(global-set-key (kbd "<f4>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F4 in major mode %s" mode)
				 (cond ((string= mode 'haskell-mode) (call-interactively 'haskell-hoogle))
				       ((string= mode 'org-mode)
					(call-interactively 'org-columns))
				       ((string= mode 'org-agenda-mode) (call-interactively 'org-agenda-columns))
				       (t (progn t))
				       )
				 )))

(global-set-key (kbd "<f5>") (lambda ()(interactive)
			       (let ((mode major-mode))
			       	 (message "[*] F5 in major mode %s" mode)
				 (cond ((utils/isgud) (call-interactively 'gud-step))  ;;(utils/isgud)
			       	       (t (progn t))
			       	       )
			       	 )))

(global-set-key (kbd "<f6>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F6 in major mode %s" mode)
				 (cond ((utils/isgud) (call-interactively 'gud-next))  ;;(utils/isgud)
				       (t (progn t))
				       )
				 )))

(global-set-key (kbd "<f7>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F7 in major mode %s" mode)
				 (cond ((or (string= mode 'org-mode)
					    (string= mode 'org-agenda-mode)) (call-interactively 'org-capture))
				       ((utils/isgud) (call-interactively 'gud-finish))
				       (t (progn t))
				       )
				 )))

(global-set-key (kbd "<f8>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F8 in major mode %s" mode)
				 (cond ((utils/isgud) (call-interactively 'gud-cont))
				       (t (progn t))
				       )
				 )))


(utils/compile-keybind)

(defun utils/flycheck-local-keybind ()
  "Flycheck key bindings."
;;  (message "[*] bind f5/f6 for flycheck")
  (local-set-key (kbd "<f5>") 'flycheck-list-errors)
  (local-set-key (kbd "<f6>") 'flycheck-next-error))

(defun utils/flymake-local-keybind ()
  "Flymake key bindings."
;;  (message "[*] bind f6 for flymake")
  (local-set-key (kbd "<f5>") 'flymake-goto-prev-error)
  (local-set-key (kbd "<f6>") 'flymake-goto-next-error))


(defun utils/projmake-local-keybind ()
  "Flymake key bindings."
;;  (message "[*] bind f6 for flymake")
  (local-set-key (kbd "<f5>") 'flymake-goto-prev-error)
  (local-set-key (kbd "<f6>") 'flymake-goto-next-error))


(global-set-key (kbd "M-S-<up>") 'shrink-window)
(global-set-key (kbd "M-S-<down>") 'enlarge-window)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)


(global-set-key (kbd "ESC <left>") 'pop-global-mark)
(global-set-key (kbd "ESC <right>") 'apps/unpop-to-mark-command)

(global-set-key (kbd "M-z") 'repeat)

(global-set-key "\C-x\C-b" 'buffer-menu)

(global-set-key (kbd "C--")
		(lambda ()(interactive)
		  (text-scale-adjust -1)))

(global-set-key (kbd "C-+")
		(lambda ()(interactive)
		  (text-scale-adjust 1)))

;;(global-set-key (kbd "C-DEL>") 'backward-kill-word)

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

;;(require 'back-button)

(provide 'config/keybindings.el)


;; test
