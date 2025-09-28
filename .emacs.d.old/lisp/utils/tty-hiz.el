(require 'ttylog)

(defun tty-pexpect-hiz-cmdsequence (idx a)
  (tty-pexpect-sense-console-and-execute-list idx "HiZ>" a))

(defun p (v)
  (format "%s" v))

(setq v 10)

(defun g (v)
  (vector v v v ))

(p (g v))

(provide 'utils/tty-hiz.el)
