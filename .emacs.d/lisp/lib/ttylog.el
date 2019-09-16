;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Events.html#Input-Events
;; https://emacs.stackexchange.com/questions/22611/is-there-a-canonical-way-of-representing-key-combinations-in-elisp-what-is-it
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html#Translation-Keymaps
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote-Patterns.html
;; (serial-term "/dev/ttyUSB0" 115200)
;; (ttylog-mode)
;; (require 'magit)
;; (magit)
;; (magit-status-setup-buffer "/home/eiselekd/git/dotfiles")
;; ( gdb )
(require 'term)
(require 'transient)
(require 'ttylog-test)
(require 'magit)
(require 'magit-section)
(require 'f)
(require 'cl-lib)
;; ( gdb-setup-windows )
;;(magit-init "/tmp" )

(defvar ttyexpect-tick 2.0)
(defvar ttyexpect-global-stop nil)
(defvar ttypexpect-mutex nil)
(defvar ttypexpect-condvar nil)
(defvar ttypexpect-str '("" ""))
(defvar ttyexpect-default-thread (current-thread))
(setq ttypexpect-str '("" ""))

(setq ttypexpect-mutex   (make-mutex))
(setq ttypexpect-condvar (make-condition-variable ttypexpect-mutex))
;;(setq ttyexpect-global-stop t)

(defun ttyexpect-tickthread-create ()
  (make-thread
   (lambda ()
     (progn
       (message "0: time-emulation ...")
       (while (not ttyexpect-global-stop)
	 (with-mutex ttypexpect-mutex
	   (sleep-for ttyexpect-tick)
	   ;;(message "0: tick ...")
       	   (condition-notify ttypexpect-condvar)
	   ))
       (message "0: exit time-emulation ...")
       )
     ) "tickthread" ))

(defun ttyexpect-tickthread-present ()
  (let* (
	 (v (seq-filter (
			 lambda (a)
				(progn
				  (string= "tickthread" (thread-name a)))) (all-threads)))
	 )
    (>= (length v) 1)))

(defun ttyexpect-tickthread-create-test ()
  (if (not (ttyexpect-tickthread-present))
      (ttyexpect-tickthread-create)
    (message "0: time-emulation already running ...")
    ))
;;(ttyexpect-tickthread-create)
;;(ttyexpect-tickthread-create-test)
;;(ttyexpect-tickthread-present)

(defun ttyexpect-prepare-mutex ()
  (if (featurep 'threads)
      (progn
	(if (not (string= "mutex" (type-of ttypexpect-mutex)))
	    (setq ttypexpect-mutex   (make-mutex)))
	(if (not (string= "condition-variable" (type-of ttypexpect-condvar)))
	    (setq ttypexpect-condvar (make-condition-variable ttypexpect-mutex)))))
  )


(defun ttypexpect-str-get (idx) (elt ttypexpect-str idx ))

(defun ttypexpect-rec (idx str)
  (with-mutex ttypexpect-mutex
    (setf (nth idx ttypexpect-str) (concat (nth idx ttypexpect-str) str))
    (condition-notify ttypexpect-condvar)))

(defun ttypexpect-sync (idx)
  (with-mutex ttypexpect-mutex
    (setf (nth idx ttypexpect-str) "")
    (condition-notify ttypexpect-condvar)))

(defun ttypexpect_wait_for_console (idx  sendstr consoleout iterwait timeout)
  (message "%d: ttypexpect_wait_for_console: " idx sendstr)
  )

(defun ttylog_newline (str)
  (replace-regexp-in-string "[\n\r]" "\\\\n" str))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Calculations.html
(defun ttypexpect (v  &optional timeout ) ;; &optional timeout
  (message "[+]: ttypexpect %s timeout: %s" v timeout)
  (catch 'found
    (let* (
	   (doloop t)
	   (hastimeout nil)
	   (iv0 (seq-filter
		 (lambda (e)
		   (let* ((v (elt e 0))
			  (p (and (symbolp v) (string= (symbol-name v) "TIMEOUT"))))
		     (if (not p)
			 (setq hastimeout t))
		     (not p))) v))
	   (n0 (float-time))
	   (n1 (+ n0 (if (eq timeout nil) 100000 timeout))))
      (message "   : Filtered vector: %s, timeout: %s" iv0 n0)
      (while doloop
	(with-mutex ttypexpect-mutex
	  (let* ((iv1 (mapcar
		       (lambda (v)
			 (let* ((idx (elt v 0))
				(str (ttypexpect-str-get idx))
				(r   (elt v 1))
				(i   (string-match r str))
				(j   (match-end 0))
				)
			   (message "   : Match %s : %s" (ttylog_newline str) r )
			   `( ,i ,idx ,(elt v 2) ,j)
			   )) iv0))
		 (iv2 (seq-filter (lambda (e) (not (eq (elt e 0) nil))) iv1))
		 (iv2c (copy-sequence iv2))
		 (iv3 (sort iv2 (lambda (a b) (< (elt a 0)(elt b 0))) ))
		 )
	    (message "iv0: sorted : %s" iv0)
	    (message "iv1: sorted : %s" iv1)
	    (message "iv2: sorted : %s" iv2c)
	    (message "iv3: sorted : %s l: %d" iv3 (length iv3))

	    (if (>= (length iv3) 1)
		(let* (
		       (e (elt iv3 0))
		       (c (elt e 2))
		       (stridx (elt e 3))
		       (idx (elt e 1))
		       )
		  (setf (nth idx ttypexpect-str) (substring (nth idx ttypexpect-str) stridx))
		  (throw 'found c)
		  )
	      (if (and hastimeout (<= n1 (float-time)))
		  (throw 'found 'TIMEOUT)
		))
	    (condition-wait ttypexpect-condvar))))
      )
    )
  )

;;(>= (+ (float-time) 0.0001) (float-time))

(defun ttyexpect_test0 ()
  (ttypexpect-rec 0 "Dadaism \n test\n Usermacro x/assignstr")
  (ttypexpect-rec 0 "Dadaism \n test\n Usermacro x/assignstr\nAnother")
  (let ((c (ttypexpect [ [0 "Another" FOUNDAnother ] [0 "Usermacro x/assign" FOUND ]  [ TIMEOUT ] ] 1.0 )))
    (pcase c
      ('TIMEOUT 1)
      ('FOUND 2)
      ('FOUNDAnother 3)
      ))



    )


;;(mapcar '1+ '[2 4 6])


;;	(with-mutex ttypexpect-mutex
	  ;;    (condition-wait cv))
	  ;;(catch 'loop

 ;;  (catch 'loop
 ;;    (dolist (item v)
 ;;      (when (ttymatch-locked (elem 0 item) )
 ;;        (throw 'loop (cdr item)))))))


 ;; (info "(elisp) Examples of Catch")
 ;;  (with-mutex ttypexpect-mutex

 ;;    (condition-wait cv))
 ;; )

(defun ttypexpect-hiz-run ()
  (interactive)
  (make-thread
   (lambda ()
     (progn
       (message "%d: ttypexpect-hiz-run" 0)
       (ttypexpect-sync 0)
       (ttypexpect_wait_for_console 0 "\n" "HiZ>" 1 5)
       (ttypexpect_send 0 "?\n")
       (let ((c (ttypexpect [ [0 "Usermacro x/assign" 'FOUND] ['TIMEOUT] ] 1.0 )))
	 (cond
	  ((= c 0)
	   (message " > Found Usermacro x/assign")
	   )
	  ( t      t)))

       (ttypexpect_wait_for_console 0 "\n" "HiZ>" 1 5)
       ))))

(defun ttypexpect-idx-to-bufname (idx)
  (if (= idx 0)
      "/dev/ttyUSB0"
    "/dev/ttyUSB1"))

(defun ttypexpect_send (idx str)
  (let ((bufname (ttypexpect-idx-to-bufname idx)))
    (if (get-buffer-process bufname)
	(progn
	  (message ">: %s" str)
	  (with-current-buffer bufname
	    (term-send-raw-string str)))
      (message "°: no tty idx %d" idx))))


(defun ttypexpect-hiz ()
  (interactive)
  (ttypexpect_send 0 "\n"))

(defvar-local ttylog-curline "")
(defvar-local ttylog-logfile nil)

(defface ttylog-font-gold
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'magit-faces)

(defface ttylog-font-green
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'magit-faces)

(defface ttylog-font-orange
  '((t :foreground "orange"))
  "Face for signatures that have expired."
  :group 'magit-faces)

(defvar ttylog-face-map '( ( "/dev/ttyUSB0" . ttylog-font-gold ) ;;
			   ( "/dev/ttyUSB1" . ttylog-font-green ))
  )

(defvar ttylog-bufidx '( ( "/dev/ttyUSB0" . 0 )
			 ( "/dev/ttyUSB1" . 1 ))
  )

(defun ttylog-face-for (n)
  (progn
    (if (eq "/dev/ttyUSB0"  n)
	'ttylog-font-gold
      'ttylog-font-green)))

(defadvice term-emulate-terminal (before term-emulate-terminal-before (proc str) activate)
  (progn
    (let ((pn (process-name proc))
	  (buffer (process-buffer proc)))
      (with-current-buffer buffer

	;; todo: register list of chunks instead:
	(setq ttylog-curline (concat ttylog-curline str))
	;;(message "%s: '%s'" pn ttylog-curline)
	(let* ((lstr ttylog-curline)
	       (b (get-buffer "*ttylog*" )))
	  (ttylog-rec (buffer-name buffer) str )
	  (when ttylog-logfile
	    (f-append-text str 'utf-8 ttylog-logfile))
	  )))))

(defun addLines (str)
  (magit-insert-section (status)
    (magit-insert-section (status)
      (magit-insert-heading "Root")
      (insert (propertize str 'face 'ttylog-font-green)))))

;;(insert (propertize str 'face 'ttylog-font-green))
;;(insert (propertize str 'face 'ttylog-font-gold))
(defun my-scroll-buffer-to-bottom (buf)
  (with-current-buffer buf
    (set-window-point
     (get-buffer-window (current-buffer) 'visible)
     (point-max))))

(defun ttylog-rec ( bufname str )
  (with-current-buffer bufname

    ;; todo: register list of chunks instead:
    (setq ttylog-curline (concat ttylog-curline str))

    ;;(message "%s: '%s'" pn ttylog-curline)
    (let* ((lstr ttylog-curline)
	   (b (get-buffer "*ttylog*"))
	   (inhibit-read-only t))
      (with-current-buffer b
	(let ((posend (= (point) (point-max))))
	  (save-excursion
            (goto-char (point-max))
	    (insert (propertize str 'face (ttylog-face-for bufname))))

	  ;;(my-scroll-buffer-to-bottom (current-buffer) )
	  (if posend
	      (progn
		(goto-char (point-max))
		(my-scroll-buffer-to-bottom (current-buffer)))

	    ;; (progn
	    ;; 	(cl-dolist (window (get-buffer-window-list nil nil t))
	    ;; 	  (set-window-point window (point-max))))

	    ;;(set-window-point b (point-max))
	    )))

      (ttypexpect-rec 0 str)
      )
    ))

(defun ttylog-generate-new-buffer (mode &optional value)
  (let* ((name "*ttylog*")
         (buffer (generate-new-buffer name)))
    buffer))

(defun setup-ttylog-buffer ()
  (let ((buffer (ttylog-generate-new-buffer 'ttylog-mode))
	(major-mode 'ttylog-mode))
    (with-current-buffer buffer
      (funcall 'ttylog-mode)
      (remove-dos-eol)
      (setq-default show-trailing-whitespace nil)
      )
    (display-buffer buffer)
    ))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun setup-ttylog ()
  (progn
    (setup-ttylog-buffer)
    (ttyexpect-prepare-mutex)
    (ttyexpect-tickthread-create)
    ))

(defun tty-uart-prepare-attach (w n)
  (progn
    (set-window-buffer w (get-buffer n))
    (set-window-dedicated-p w t)
    ))

(defun tty-uart-connect (&rest args)
  (interactive) ;;  (and current-prefix-arg (list "--3way"))
  (message "Connect to MP and VIP uart")

  (condition-case nil
      (serial-term "/dev/ttyUSB0" 115200)
    (error (get-buffer-create "/dev/ttyUSB0")))
  (condition-case nil
      (serial-term "/dev/ttyUSB1" 115200)
    (error (get-buffer-create "/dev/ttyUSB1")))

  (setq-default show-trailing-whitespace nil)

  (display-buffer "/dev/ttyUSB0")
  (display-buffer "/dev/ttyUSB1")
  (setup-ttylog)

  (switch-to-buffer "/dev/ttyUSB0")
  (delete-other-windows)

  (let ((wintty (split-window-below (/ (* (window-height) 3) 4 )))
	(winusb1 (split-window-right))
	)
    (tty-uart-prepare-attach winusb1 "/dev/ttyUSB1")
    (tty-uart-prepare-attach wintty "*ttylog*")
    )

  )


(defvar ttylog-base-map
  (let ((map (make-keymap))) ;;
    (suppress-keymap map t)
    (define-key map  (kbd "C-i") 'magit-section-toggle)
    (define-key map  [backtab]   'magit-section-cycle-global)
    map)
  "Parent keymap for all keymaps of modes derived from `ttylog-base-mode'.")


(defvar ttylog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ttylog-base-map)
    (define-key map "i" 'ttylog-dispatch)
    (define-key map "g" 'magit-section-toggle)
    (define-key map "G" 'magit-section-cycle-global)
    map)
  "Keymap for `ttylog-mode-map'.")


(defun tty-test-setup ()
  (interactive)
  (if (eq (current-local-map) ttylog-mode-map)
      (message "ttylog-mode-map is active")))

;;;###autoload (autoload 'ttylog-dispatch "ttylog" nil t)
(define-transient-command ttylog-dispatch ()
  "Invoke a Magit command from a list of available commands."
  ["tty commands"
   [("C" "Connect"          tty-uart-connect)
    ("r" "run hiz test"     ttypexpect-hiz-run)
    ("t" "test uart input"  tty-test-map)
    ("v" "test setup"       tty-test-setup)
    ]
   ]
  ["Applying changes"
   :if-derived magit-mode
   [("a" "Apply"          magit-apply)]]
  ["Essential commands"
   :if-derived ttylog-mode
   ("<tab>" "toggle section at point"  magit-section-toggle)]
  )


(put 'ttylog-mode 'mode-class 'special)

;;fundamental-mode
(define-derived-mode ttylog-mode special-mode "ttylog-mode"
  "Parent major mode from which ttylog major modes inherit.
Magit is documented in info node `(magit)'."
  :group 'ttylog-modes
  (buffer-disable-undo)
  (setq truncate-lines t)
  ;;(setq buffer-read-only nil)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (setq left-margin-width 2 right-margin-width 0)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (hack-dir-local-variables-non-file-buffer)
  )


(current-thread)
(all-threads)

(provide 'ttylog)
