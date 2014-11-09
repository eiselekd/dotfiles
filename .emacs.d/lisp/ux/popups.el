
(defun apps/popup-init ()
  "Initialize popup."
  ;; Install workaround for whitespace-mode bug.
  (after-load 'modes/whitespace
    (defadvice popup-draw (before apps/turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing popup."
      (apps/whitespace-mode-suppress t))

    (defadvice popup-delete (after apps/restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting popup."
      (apps/whitespace-mode-suppress nil))))


(defun apps/popwin-init ()
  "Initialize popwin."
  (with-feature 'popwin
    (popwin-mode t)

    ;; Don't select compilation window when shown
    (push '(compilation-mode :height 20 :dedicated t)
          popwin:special-display-config)

    ;;; (Bindings) ;;;
    (apps/bind-key-global :util :popwin-close 'popwin:close-popup-window)
    (apps/bind-key-global :util :popwin-buffer 'popwin:popup-buffer))
  )


(defun apps/popups-init ()
  "Initialize Emacs popups."
  (setq-default
   ;; Timeout for messages shown in minibuffer.
   minibuffer-message-timeout 5)
  
  (apps/popup-init)
  (apps/popwin-init)
  )

(when
    (and
     (require 'popwin nil 'noerror)
     (require 'popup nil 'noerror))
  (apps/popups-init)
)

(provide 'ux/popups.el)
