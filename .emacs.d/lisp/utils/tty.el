(require 'transient)

(global-set-key (kbd "<f2>") (lambda ()(interactive)
			       (tty-dispatch)))

(defun tty-uart-connect (&rest args)
  (interactive) ;;  (and current-prefix-arg (list "--3way"))
  (message "Connect to tty0 and tty1 uart")
  (serial-term "/dev/ttyUSB0" 115200)
  (serial-term "/dev/ttyUSB1" 115200)
  (setup-ttylog)
  )

(defun tty-uart0-show (&rest args)
  (interactive) ;;  (and current-prefix-arg (list "--3way"))
  )
(defun tty-uart1-show (&rest args)
  (interactive) ;;  (and current-prefix-arg (list "--3way"))
  )

(define-transient-command tty-dispatch ()
  "Invoke a Magit command from a list of available commands."
  ["Transient and dwim commands"
   [("c" "Connect"        tty-uart-connect)]
   [("1" "TTY0"           tty-uart0-show)]
   [("2" "TTY1"           tty-uart1-show)]
   ]
  )

(provide 'utils/tty.el)
