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
;; debug: "c-h v" : describe variable
;; http://unix.stackexchange.com/questions/79374/are-there-any-linux-terminals-which-can-handle-all-key-combinations/79561#79561

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Events.html#Input-Events
;; https://emacs.stackexchange.com/questions/22611/is-there-a-canonical-way-of-representing-key-combinations-in-elisp-what-is-it
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html#Translation-Keymaps
;; https://github.com/halhenke/doctor/blob/master/my%20emacs%20and%20keybinding%20dilemma.org
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; https://tldp.org/HOWTO/pdf/Keyboard-and-Console-HOWTO.pdf

;; ctrl-c ctrl-v ctrl-y
(cua-mode t)
(cua-selection-mode 1)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
;;(setq cua-keep-region-after-copy nil) ;; Non-standard Windows behaviour
;;(define-key cua-global-keymap [(control w)] 'cua-copy-region)


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
(global-set-key (kbd "M-e")   (lambda ()(interactive)
			       (progn
				 (let* ((mode major-mode))
				   (cond
				    ((string= mode 'racket-mode)
				     (progn
				       (racket-run)))
				    (t (shell)
				       ))))))


(global-set-key (kbd "M-E")  (lambda()(interactive)
			       (require 'utils/shell.el)
			       (utils/shell)
			       ))
;; start proced
(global-set-key (kbd "M-p")  'proced)

;; start hyperbold
(global-set-key (kbd "M-h") (lambda ()(interactive)
			      (progn
				(when (require 'hyperbole)
				  (message "hyperbold loaded")))))


(global-set-key (kbd "M-d")  (lambda ()(interactive) (toggle-debug-on-error)))
;; start org-agenda
(global-set-key (kbd "M-a")  'org-agenda)
;; start erc
(global-set-key (kbd "M-I") (lambda ()(interactive)
			      (progn
				(when (require 'utils/irc.el)
				  (utils/irc-djcb-erc-start-or-switch))))) ;; ERC

;; ispell cycle
(global-set-key (kbd "M-i") 'cycle-ispell-languages) ;; ERC

;; start org-agenda
(global-set-key (kbd "M-j")  'goto-line)
(global-set-key (kbd "M-J") (lambda ()(interactive)
			      (progn
				(set-language-environment 'Japanese)
				(set-input-method 'japanese)
				(require 'kanji-mode)
				(kanji-mode)
				)))

;; remove trailing whitespaces
(global-set-key (kbd "M-W")  'whitespace-cleanup)

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
				   (when (require 'utils/magit-util.el nil t)
				     (utils/magit-status))))))  ;;(org-agenda)

;; interpose helm
(global-set-key (kbd "M-Q")  (lambda ()(interactive)
			       (progn
				 (prepareHelm)
				 (let ((current-prefix-arg '(10)))
				    (call-interactively 'helm-do-grep)))))
(global-set-key (kbd "M-.")  (lambda ()(interactive)
			       (progn
				 (let* ((mode major-mode))
				   (cond
				    ((string= mode 'emacs-lisp-mode)
				     (progn
				       (xref-push-marker-stack)
				       (call-interactively 'find-function-at-point)))
				    ((string= mode 'haskell-mode)
				     (progn
				       (let* ((backend (xref-find-backend))
					      (id (xref-backend-identifier-at-point backend)))
					 ;;(xref--find-definitions identifier nil))))
					 ;;(xref-find-definitions)

					 ;; (defadvice xref-find-definitions (before c-tag-file activate)
					 ;;   "Automatically create tags file."
					 ;;   (let ((tag-file (concat default-directory "TAGS")))
					 ;;     (unless (file-exists-p tag-file)
					 ;;       (shell-command "etags *.[ch] -o TAGS 2>/dev/null"))
					 ;;     (visit-tags-table tag-file)))


					 (call-interactively 'xref-find-definitions ))))
				    (t (progn
					(prepareHelm)
					(helm-gtags-dwim))))))))




;; start find-tag
;(global-set-key (kbd "M-?")  (lambda ()(interactive)(find-tag (thing-at-point 'word))))
;; open file under cursor
(global-set-key (kbd "M-5")  'xah-open-file-at-cursor)
;; goto function under cursor
(global-set-key (kbd "M-6")  'find-function-at-point)
(global-set-key (kbd "M-7")  'whitespace-cleanup)
(global-set-key (kbd "M-&")
		(lambda () (interactive)
		  (describe-variable (variable-at-point))))
;; goto workspace 1
(global-set-key (kbd "M-1")  (lambda ()(interactive)(wg-switch-to-workgroup (wg-get-workgroup 'name "w1"))))
;; goto workspace 2
(global-set-key (kbd "M-2")  (lambda ()(interactive)(wg-switch-to-workgroup (wg-get-workgroup 'name "w2"))))

(global-set-key (kbd "M-q")
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

;; minions menue withh appends
(global-set-key (kbd "ESC ^") (lambda ()(interactive) (progn (minions-minor-modes-menu))))

;;(lambda ()(interactive)
;;			       (progn
;;				 (require 'flycheck nil t)
;;				 (require 'utils/flycheck.el)
;;				 'flycheck-mode)))
;;
(global-set-key (kbd "M-N")  (lambda ()(interactive)
			       (progn
				 (when (require 'utils/cling-term.el nil t)
				   (progn
				     (cling-term "cling" "cling")

				     )))))

(global-set-key (kbd "ESC M-N")  (lambda ()(interactive)
			       (progn
				 (when (require 'utils/cling.el nil t)
				   (progn
				     (utils/run-cling)

				     )))))

(global-set-key (kbd "M-n")  (lambda ()(interactive)
			       (progn
				 (eww "https://news.ycombinator.com/")
				 )))

;; compile errors
(global-set-key (kbd "<f5>") 'utils/previous-error)
(global-set-key (kbd "<f6>") 'utils/next-error)
(global-set-key (kbd "S-<f5>") 'flycheck-previous-error)
(global-set-key (kbd "S-<f6>") 'flycheck-next-error)


(global-set-key (kbd "ESC <f10>") (lambda ()(interactive)
				    (progn
				      (menu-bar-mode)
				      (call-interactively 'menu-bar-open)
				      )))

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



(defun active-minor-modes ()
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode)
	    (condition-case nil
		(if (and (symbolp mode) (symbol-value mode))
		    (add-to-list 'active-modes mode))
	      (error nil) ))
          minor-mode-list)
    minor-mode-list))

;;(member 'gdb-many-windows (active-minor-modes))
;;(member 3 '(1 2 3))

(defun utils/gud-buffer ()
  (let ((bnames (mapcar (function buffer-name) (buffer-list))))
    (seq-filter (apply-partially #'string-prefix-p "*gud-" ) bnames)
    ))

(defun utils/gud-perl-buffer ()
  (let ((bnames (mapcar (function buffer-name) (buffer-list))))
    (seq-filter (apply-partially #'string-suffix-p ".pl" ) bnames)
    ))

(defun utils/isgud ()
  ;;(message "test c++ mode: %s" (or (string= mode 'c-mode)
  ;;(string= mode 'c++-mode)))
  ;;(message "test c++ mode and buffer: %s" 	    (and (or (string= mode 'c-mode)
  ;;					     (string= mode 'c++-mode))
  ;;							 (utils/gud-buffer)))

  (let ((mode
	 major-mode)
	(r nil))
    (if (or (string= mode 'gud-mode)
	    (string= mode 'gdb-locals-mode)
	    (string= mode 'gdb-inferior-io-mode)
	    (string= mode 'gdb-frames-mode)
	    (string= mode 'gdb-breakpoints-mode)
	    (utils/gud-buffer)
	    ;; (and (or (string= mode 'perl-mode)
	    ;; 	     (string= mode 'c-mode)
	    ;; 	     (string= mode 'c++-mode))
	    ;; 	 (utils/gud-buffer)
	    ;; 	 ;;(get-buffer "*gud*")
	    ;; 	 )
	    )
	(setq r t)
      )
    r))


(global-set-key (kbd "<f1>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F1 in major mode %s" mode)
				 (cond ((string= mode 'haskell-mode) (haskell/eval-haskell))
				       ((string= mode 'raku-mode) (modes/raku-repl-start))
				       ((string= mode 'org-mode) (org-agenda))
				       ((utils/isgud)
					(cond ((or (utils/gud-perl-buffer)
						   (string= mode 'perl-mode))
					       (call-interactively 'perldb-many-windows))
					      (t (progn
						   (switch-to-buffer "*gud*")
						   (gdb-restore-windows)
						   (gud-refresh)
						   ))))
				       ((modes/ocamldebug-have-debugbuffer)
					(progn
					  (modes/ocamldebug-switchto-debugbuffer)
					  ))

				       (t (progn
					    (when (require 'utils/magit-util.el nil t)
					      (utils/magit-status))))  ;;(org-agenda)
				       )
				 )))

(global-set-key (kbd "<f2>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F2 in major mode %s" mode)
				 (cond ((string= mode 'haskell-mode) (haskell-process-do-type))
				       ((or (string= mode 'org-mode)
					    (string= mode 'org-agenda-mode)) (call-interactively 'bh/org-sparse-tree))
				       ((utils/isgud) (call-interactively 'gud-step))
				       (t (progn
					    (when (and
						   (require 'utils/tty.el nil t)
						   (require 'ttylogmode nil t))
					      (tty-dispatch)
					      )))
				 ))))

(global-set-key (kbd "<f3>") (lambda ()(interactive)
			       (dired default-directory)
			       ))

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
				 (cond ((string= mode 'tuareg-mode) (lambda ()(interactive) (ocamldebug-call "step")))
				       ((utils/isgud)
					(progn
					  (message "[*] gud-step")
					  (call-interactively 'gud-step)))  ;;(utils/isgud)
			       	       (t (progn
					    (message "[*] F5 unbound: isgud: %s, " (utils/isgud))
					    t))
			       	       )
			       	 )))

(global-set-key (kbd "<f6>") (lambda ()(interactive)
			       (let ((mode major-mode))
				 (message "[*] F6 in major mode %s" mode)
				 (cond ((string= mode 'tuareg-mode) (lambda ()(interactive) (ocamldebug-call "next")))
				       ((utils/isgud)
					(progn
					  (message "[*] gud-next")
					  (call-interactively 'gud-next)))  ;;(utils/isgud)
				       (t (progn
					    (message "[*] F6 unbound")
					    t)))
				 )
			       ))

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

(global-set-key (kbd "ESC H") (lambda ()(interactive) (find-file "~/git/dotfiles/README.md")))

(global-set-key (kbd "ESC <left>") 'pop-global-mark)
(global-set-key (kbd "ESC <right>") 'apps/unpop-to-mark-command)

(global-set-key (kbd "M-z") 'repeat)

(global-set-key (kbd "C-x B") 'buffer-menu)

(global-set-key (kbd "C--")
		(lambda ()(interactive)
		  (text-scale-adjust -1)))

(global-set-key (kbd "C-+")
		(lambda ()(interactive)
		  (text-scale-adjust 1)))

;;(global-set-key (kbd "C-DEL>") 'backward-kill-word)

(global-set-key [M-up]  'scroll-up)
(global-set-key [M-down]  'scroll-down)
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


(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

;; test

(provide 'config/keybindings.el)
