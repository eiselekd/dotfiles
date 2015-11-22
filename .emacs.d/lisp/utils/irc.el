
(defcustom irc-user nil
  "irc default user"
  :type 'string
  )
(put 'irc-user 'safe-local-variable (lambda (x) t))
(defcustom irc-user-fullname nil
  "irc default fullname user"
  :type 'string
  )
(put 'irc-user-fullname 'safe-local-variable (lambda (x) t))

(defun utils/irc-djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick irc-user :full-name irc-user-fullname)
      )))

(provide 'utils/irc.el)
