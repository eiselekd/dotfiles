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
  (switch-to-buffer "/dev/ttyUSB0")
  (delete-other-windows)
  )

(defun tty-uart1-show (&rest args)
  (interactive) ;;  (and current-prefix-arg (list "--3way"))
  (switch-to-buffer "/dev/ttyUSB1")
  (delete-other-windows)
  )

(defun i0-commands-show (&rest args)
  (interactive)
  )

(defun g0-commands-show (&rest args)
  (interactive)
  )



(define-transient-command tty-dispatch ()
  "Invoke a Magit command from a list of available commands."
  ["open and execute commands"
   [("c" "Connect"        tty-uart-connect)]
   [("1" "TTY0"           tty-uart0-show)]
   [("2" "TTY1"           tty-uart1-show)]
   [("i" "i0"             i0-commands-show)] ;;i0-command-open
   [("g" "g0"             g0-commands-show)] ;;i0-command-open

  ]
  )




(provide 'utils/tty.el)
