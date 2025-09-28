(require 'ttylog)
(require 'ttylogtest)

(defvar ttylog-base-map
  (let ((map (make-keymap))) ;;
    (suppress-keymap map t)
    (define-key map  (kbd "C-i") 'magit-section-toggle)
    (define-key map  [backtab]   'magit-section-cycle-global)
    map)
  "Parent keymap for all keymaps of modes derived from `ttylog-base-mode'.")


(defvar ttylog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ttylog-base-map)
    (define-key map "h" 'ttylog-dispatch)
    (define-key map "g" 'magit-section-toggle)
    (define-key map "G" 'magit-section-cycle-global)
    map)
  "Keymap for `ttylog-mode-map'.")


(defun tty-test-setup ()
  (interactive)
  (if (eq (current-local-map) ttylog-mode-map)
      (message "ttylog-mode-map is active")))

;;;###autoload (autoload 'ttylog-dispatch "ttylog" nil t)
(define-transient-command ttylog-dispatch ()
  "Invoke a Magit command from a list of available commands."
  ["tty commands"
   [("C" "Connect"          tty-uart-connect)
    ("r" "run hiz test"     ttypexpect-hiz-run)
;;    ("t" "test uart input"  tty-test-map)
;;    ("v" "test setup"       tty-test-setup)
    ]
   ]
  ["Applying changes"
   :if-derived magit-mode
   [("a" "Apply"          magit-apply)]]
  ["Essential commands"
   :if-derived ttylog-mode
   ("<tab>" "toggle section at point"  magit-section-toggle)]
  )


(put 'ttylog-mode 'mode-class 'special)

;;fundamental-mode
(define-derived-mode ttylog-mode special-mode "ttylog-mode"
  "Parent major mode from which ttylog major modes inherit.
Magit is documented in info node `(magit)'."
  :group 'ttylog-modes
  (buffer-disable-undo)
  (setq truncate-lines t)
  ;;(setq buffer-read-only nil)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (setq left-margin-width 2 right-margin-width 0)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (hack-dir-local-variables-non-file-buffer)
  )

;;
;(setq a 10)

;`( ( ,a 'a) )

;;(current-thread)
;;(all-threads)

(provide 'ttylogmode)
