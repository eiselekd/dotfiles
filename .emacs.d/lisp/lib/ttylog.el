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
(require 'magit)
(require 'magit-section)
(require 'f)
(require 'cl-lib)
(require 'ht)
(require 'transient)

(defvar ttyexpect-tick 0.2)
(defvar ttyexpect-global-stop nil)
(defvar ttypexpect-mutex nil)
(defvar ttypexpect-condvar nil)
(defvar ttypexpect-str '("" ""))
(defvar ttyexpect-default-thread (current-thread))
(setq   ttypexpect-str '("" ""))
(defvar ttypexpect-bufname-logname nil)
(setq   ttypexpect-bufname-logname (make-hash-table :test 'equal)) ;;
(defvar ttypexpect-buftype nil)
(setq   ttypexpect-buftype (make-hash-table :test 'equal)) ;;
;;(type-of ttypexpect-bufname-logname)



(defun ttyexpect-log (str)
  (message "%s" str)
  (f-append-text (format "%s\n" str) 'utf-8 "/tmp/tty.log"))

(defun ttyexpect-datalog-open (n)
  (puthash n (concat "/data/ttylog/" (replace-regexp-in-string "/" "_" n) "-" (format-time-string "%Y-%m-%d_%H-%M-%S")) ttypexpect-bufname-logname))

;; (setq ttypexpect-bufname-logname (make-hash-table :test 'equal))
;; (puthash "a" 1 ttypexpect-bufname-logname)
;; (gethash "a" ttypexpect-bufname-logname)
;; (ttyexpect-datalog-open "/dev/ttyUSB0")


(defun ttyexpect-datalog-get (n)
  (gethash n ttypexpect-bufname-logname))
;; (ttyexpect-datalog-get "/dev/ttyUSB0")



;;(setq ttypexpect-mutex   (make-mutex))
;;(setq ttypexpect-condvar (make-condition-variable ttypexpect-mutex))
;;(setq ttyexpect-global-stop t)

(defun ttyexpect-tickthread-create ()
  (ttyexpect-prepare-mutex)
  (make-thread
   (lambda ()
     (progn
       (message "0: time-emulation ...")
       (while (not ttyexpect-global-stop)
	 (sleep-for ttyexpect-tick)
	 (with-mutex ttypexpect-mutex
	   ;;(message "0: tick ...")
       	   (condition-notify ttypexpect-condvar)
	   ))
       (message "0: exit time-emulation ...")
       )
     ) "tickthread" ))

(defun ttyexpect-tickthread-present ()
  (let*
      ((v (seq-filter
	   (lambda (a)
	     (progn
	       (string= "tickthread" (thread-name a)))) (all-threads))))
    (>= (length v) 1)))

(defun ttyexpect-tickthread-create-test ()
  (ttyexpect-prepare-mutex)
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

(defun ttypexpect-map-type (n)
  (gethash n ttypexpect-buftype))

(defun ttypexpect-str-get (idx)
  (ttyexpect-log (format "ttypexpect-str-get %s :'%s'" idx (ttylog_newline (elt ttypexpect-str idx ))))
  (if (stringp idx)
      (progn
	(setq idx (ttypexpect-map-type idx))
	(ttyexpect-log (format "ttypexpect-str-get map to %d" idx ))
	))
  (if (null idx)
      ""
      (elt ttypexpect-str idx )))

(defun ttypexpect-rec (idx str &optional sync)
  (with-mutex ttypexpect-mutex
    (ttyexpect-log (format  "[%d] : => '%s'" idx (ttylog_newline str) ))
    (setf (nth idx ttypexpect-str) (concat (nth idx ttypexpect-str) str))
    (if (not (null sync))
	(setf (nth idx ttypexpect-str) str))
    (condition-notify ttypexpect-condvar)))

(defun ttypexpect_sync (idx)
  ttypexpect-rec (idx "" 1))

(defun ttypexpect_wait_for_console (idx  sendstr consoleout iterwait timeout)
  (message "%d: ttypexpect_wait_for_console: " idx sendstr)
  )

(defun ttylog_newline (str)
  (replace-regexp-in-string "[\n\r]" "\\\\n" str))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Calculations.html
(defun ttypexpect (v  &optional timeout ) ;; &optional timeout
  (ttyexpect-log (format "[+]: ttypexpect %s timeout: %s" v timeout))
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
      (ttyexpect-log (format "   : filtered vector: %s, timeout: %s, hastimeout: %s" iv0 n0 hastimeout))
      (ttyexpect-tickthread-create-test)
      (while doloop
	(ttyexpect-log "[.] ttypexpect-mutex " )
	(with-mutex ttypexpect-mutex
	  (ttyexpect-log "[.] ttypexpect-mutex taken: " )
	  (let* ((iv1 (mapcar
		       (lambda (v)
			 (let* ((idx (elt v 0))
				(str (ttypexpect-str-get idx))
				(r   (elt v 1))
				(i   (string-match r str))
				(j   (and (not (null i)) (match-end 0)))  ;; todo: check zero
				)
			   (ttyexpect-log (format  "   : Match %s : %s" (ttylog_newline str) r ))
			   `( ,i ,idx ,(elt v 2) ,j)
			   )) iv0))
		 (iv2 (seq-filter (lambda (e) (not (eq (elt e 0) nil))) iv1))
		 (iv2c (copy-sequence iv2))
		 (iv3 (sort iv2 (lambda (a b) (< (elt a 0)(elt b 0))) ))
		 )
	    (ttyexpect-log (format "iv0: sorted : %s" iv0))
	    (ttyexpect-log (format "iv1: sorted : %s" iv1))
	    (ttyexpect-log (format "iv2: sorted : %s" iv2c))
	    (ttyexpect-log (format "iv3: sorted : %s l: %d" iv3 (length iv3)))

	    (if (>= (length iv3) 1)
		(let* (
		       (e (elt iv3 0))
		       (c (elt e 2))
		       (stridx (elt e 3))
		       (idx (elt e 1))
		       )
		  (setf (nth idx ttypexpect-str) (substring (nth idx ttypexpect-str) stridx))
		  (ttyexpect-log (format "[+] : Found %s move stridx %d: %s" c stridx (ttylog_newline (nth idx ttypexpect-str))))
		  (throw 'found c)
		  )
	      (progn
		(ttyexpect-log (format " wait after miss: %s" (float-time)))
		(if (and hastimeout (<= n1 (float-time)))
		    (progn
		      (ttyexpect-log (format "[+] : timout"))
		      (throw 'found 'TIMEOUT))
		  )))
	    (ttyexpect-log "[.] > ttypexpect-wait")
	    (condition-wait ttypexpect-condvar)
	    (ttyexpect-log "[.] < ttypexpect-wait")
	    ))
	)
      (ttyexpect-log "[=] while loop exit")

      )
    (progn
      (ttyexpect-log "[=] catch found: %s" found)
      found)
    )
  )

(defun tty-pexpect-sense-console (idx prompt &optional cnt)
  (ttyexpect-log (format "[.]+> sense-concole %s for '%s' %s times" idx prompt cnt))
  (if (null cnt)
      (setf cnt -1))
  (catch 'ready
    (while (/= cnt 0)
      (ttypexpect_send idx "\n")
      (let ((c (ttypexpect `( ( ,idx ,prompt FOUNDPrompt )  (TIMEOUT) ) 0.5 )))
	(pcase c
	  ('TIMEOUT
	   (progn
	     (ttyexpect-log (format "   +> timeout not detected, continue"))
	     ))
	  ('FOUNDPrompt
	   (progn
	     (ttyexpect-log (format "   +> detected"))
	     (throw 'ready t)))
	  ))
      (decf cnt))
    nil))

(defun tty-pexpect-sense-console-and-execute (idx prompt a &optional cnt)
  (tty-pexpect-sense-console idx prompt cnt)
  (ttypexpect_send idx (concat a "\n"))
  )

(defun tty-pexpect-sense-console-and-execute-list (idx prompt a &optional cnt)
  (dolist (e a)
    (tty-pexpect-sense-console-and-execute idx propmt e cnt)))




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
    (if (string= "/dev/ttyUSB0"  n)
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


(defvar ttylog-buflines (ht-create))
(defvar ttylog-flush-delta 0.5)
(defvar ttylog-flush-timer nil)
(setq ttylog-flush-delta 0.5)

(cl-defstruct bufline
  lastupdate
  linestr
  )

(defun ttylog_bufname_append_or_set ( bufname str &optional set)
  (let ((l (or (ht-get ttylog-buflines bufname ) (make-bufline :lastupdate (float-time) :linestr ""))))
    (if (not set)
	(progn
	  (setf (bufline-lastupdate l) (float-time))
	  (setf (bufline-linestr l) (concat (bufline-linestr l ) str)))
      (progn
	(setf (bufline-linestr l) str)))
    (ht-set! ttylog-buflines bufname l)
    ))

(defun not-null (e) (not (null e)))


;;6573 2031 36e2 8093 3331 2053  resses 16...31 S
;;00025860: 7265 7373 6573 2031 36e2 8093 3331 2053  resses 16...31 S
;;[   15.675849] MAC Addresses 16��–31 Selected                : YES
;; s 16��–31 Selected
;; match lines that have a newline or are ready to flush
(defun ttylog-ready-line (bln)
  (cl-destructuring-bind (n l . rest) bln
    (let* ((c (bufline-linestr l))
	   (f (bufline-lastupdate l))
	   (i (string-match "\\`\\(\\(?:.\\|\n\\)*\n\\)\\([^\n]*\\)\\'" c)))
      (if (not-null i)
	  (progn
	    ;;(message "Detect newline")
	    (list n f (match-string 1 c) (match-string 2 c)))
	(progn
	  ;;(message "Compare c:%f to f:%f" (float-time) (+ f ttylog-flush-delta))
	  (if (<= (float-time) (+ f ttylog-flush-delta))
	      (progn
		;;(message "Detect flush")
		(list  n f c "" ))
	    nil))))))

;; ###########################################################
;; ###########################################################
;; ###########################################################

;;(ttylog_bufname_append_to "a" "hello\nlast" )
;;(mapcar 'ttylog-ready-line (ht-items ttylog-buflines))

(defun ttylog-rec ( bufname str )
  ;;(message (format "%s:%s" bufname str))
  (ttylog_bufname_append_or_set bufname str)
  ;;(message (format "%s" ttylog-buflines))
  (ttylog-flush)

  (if (not-null ttylog-flush-timer)
      (cancel-timer ttylog-flush-timer))
  (setq ttylog-flush-timer (run-at-time 'nil (* 2 ttylog-flush-delta) 'ttylog-flush))
  )

(defun ttylog-flush ( )
  (let* ((rl0 (mapcar 'ttylog-ready-line (ht-items ttylog-buflines)))
	 (rl1 (seq-filter 'not-null rl0))
	 (rl2 (sort rl1 (lambda (a b) (< (elt a 1) (elt b 1))))))
    ;;(message (ttylog_newline (format "hash :'%s'" ttylog-buflines)))
    ;;(message (ttylog_newline (format "found:'%s'" rl2)))
    (mapcar
     (lambda (l)
       (cl-destructuring-bind (n f i0 i1 . rest) l
	 (ttylog-rec-insert n i0 )
	 (ttylog_bufname_append_or_set n i1 't)
	 ))
     rl2)
    ))

(defun ttylog-rec-insert ( bufname str )

  (with-current-buffer bufname

    (f-append-text str 'utf-8 (ttyexpect-datalog-get bufname))

    ;; todo: register list of chunks instead:
    ;;(setq ttylog-curline (concat ttylog-curline str))

    ;;(message "%s: '%s'" pn ttylog-curline)
    (let* (;;(lstr ttylog-curline)
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

      ;; todo: set on expect usage
      (if (string= bufname "/dev/ttyUSB0")
	  (ttypexpect-rec 0 str)
	(ttypexpect-rec 1 str)
	)
      )
    )
  )

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
    (ttyexpect-tickthread-create-test)
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
      (progn
	(ttyexpect-datalog-open "/dev/ttyUSB0")
	(serial-term "/dev/ttyUSB0" 115200))
    (error (get-buffer-create "/dev/ttyUSB0")))
  (condition-case nil
      (progn
	(ttyexpect-datalog-open "/dev/ttyUSB1")
	(serial-term "/dev/ttyUSB1" 115200))
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


;;
;(setq a 10)

;`( ( ,a 'a) )

;;(current-thread)
;;(all-threads)

(provide 'ttylog)
