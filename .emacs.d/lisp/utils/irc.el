
(defun utils/irc-djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      ;;(erc "localhost" 6667 erc-nick erc-user-full-name t)
      ;; (erc "irc.phasenet.co.uk" erc-port erc-nick erc-user-full-name t)
      ;; (erc "irc.slashnet.org" erc-port erc-nick erc-user-full-name t)
      (message "[+] server: %s:%d nick:%s name:%s"  "irc.freenode.net" erc-port erc-nick erc-user-full-name)
      (require 'tls)
      (erc-tls :server "irc.freenode.net" :port erc-port :nick erc-nick :password erc-password :full-name erc-user-full-name )
      (erc :server "irc.freenode.net" :port erc-port :nick erc-nick :password erc-password :full-name erc-user-full-name )
      ;;(setq erc-autojoin-channels-alist '(("freenode.net" "#haskell")
    )))

(add-hook 'erc-mode-hook (lambda () (setq truncate-partial-width-windows nil)))

(provide 'utils/irc.el)
