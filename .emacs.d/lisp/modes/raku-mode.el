;;(require 'use-package)
(require 'nqp-mode)
(require 'raku-mode)

(defun modes/raku-mode-start ()
  (message "[+] start flycheck raku")
  (flycheck-mode)
  (put 'checker-enable 'safe-local-variable (lambda (_) t))
  (require 'flycheck-raku)
  (require 'utils/raku-checker.el)

  (cond
   ((string= "nqp-mode" major-mode)
    (progn
      (message "[+] gud-nqpdb")
      (require 'gud-nqpdb.el)))
   (t
    (progn (message "[+] gud-perl6db %s " major-mode)
	   (require 'gud-perl6db.el))))

   ;;(flycheck-select-checker 'raku)

  )

(defun modes/raku-repl-start ()
  (run-raku)
  )

(add-hook 'raku-mode-hook #'modes/raku-mode-start)

(provide 'modes/raku-mode.el)
