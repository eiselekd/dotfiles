
(defvar ttypexpect-str '("" ""))
(setq   ttypexpect-str '("" ""))

(defvar ttypexpect-bufname-logname nil)
(setq   ttypexpect-bufname-logname (make-hash-table :test 'equal)) ;;

(defvar ttypexpect-buftype nil)
(setq   ttypexpect-buftype (make-hash-table :test 'equal)) ;;

(defun ttylog_newline (str)
  (replace-regexp-in-string "[\n\r]" "\\\\n" str))

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


;; (setq ttypexpect-bufname-logname (make-hash-table :test 'equal))
;; (puthash "a" 1 ttypexpect-bufname-logname)
;; (gethash "a" ttypexpect-bufname-logname)
;; (ttyexpect-datalog-open "/dev/ttyUSB0")
;;(type-of ttypexpect-bufname-logname)

(defun ttyexpect-log (str)
  (message "%s" str)
  (f-append-text (format "%s\n" str) 'utf-8 "/tmp/tty.log"))

(defun ttyexpect-datalog-open (n)
  (puthash n (concat "/data/ttylog/" (replace-regexp-in-string "/" "_" n) "-"
		     (format-time-string "%Y-%m-%d_%H-%M-%S"))
	   ttypexpect-bufname-logname))

(defun ttyexpect-datalog-get (n)
  (gethash n ttypexpect-bufname-logname))
;; (ttyexpect-datalog-get "/dev/ttyUSB0")




(defun ttypexpect-idx-to-bufname (idx)
  (if (= idx 0)
      "/dev/ttyUSB0"
    "/dev/ttyUSB1"))

(provide 'ttylogbuf)
