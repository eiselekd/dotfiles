;;(setq ttypexpect-mutex   (make-mutex))
;;(setq ttypexpect-condvar (make-condition-variable ttypexpect-mutex))
;;(setq ttyexpect-global-stop t)


;;(ttyexpect-tickthread-create)
;;(ttyexpect-tickthread-create-test)
;;(ttyexpect-tickthread-present)

(defvar ttyexpect-tick 0.2)
(defvar ttyexpect-global-stop nil)
(defvar ttypexpect-mutex nil)
(defvar ttypexpect-condvar nil)
(defvar ttyexpect-default-thread (current-thread))


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


(defun ttyexpect-prepare-mutex ()
  (if (featurep 'threads)
      (progn
	(if (not (string= "mutex" (type-of ttypexpect-mutex)))
	    (setq ttypexpect-mutex   (make-mutex)))
	(if (not (string= "condition-variable" (type-of ttypexpect-condvar)))
	    (setq ttypexpect-condvar (make-condition-variable ttypexpect-mutex)))))
  )


(provide 'ttylogthread)
