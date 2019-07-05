(require 'comint)

(defvar cling-cli-file-path "cling"
  "Path to the program used by `cling'")

(defvar cling-cli-arguments '("-std=c++17")
  "Commandline arguments to pass to `cling'")

(defun run-cling ()
  "Run an inferior instance of `cling-cli' inside Emacs."
  (interactive)
  (let* ((cling-program cling-cli-file-path)
         (buffer (comint-check-proc "Cling")))
    ;; pop to the "*Cling*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'cling-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Cling*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Cling" buffer
             cling-program nil cling-cli-arguments)
      ;;(c-mode)
      )))

(defun cling (&optional flags)
  "Move to the buffer containing Cling, or create one if it does not exist. Defaults to C++11"
  (interactive)
  (let ((flags (or flags "-std=c++11"))) 
    (make-comint "Cling" "cling" nil flags)
    (switch-to-buffer "*Cling*")))

(defun cling-send-string (string &optional process)
  "Send a string terminated with a newline to the inferior-cling buffer. Has the effect of executing a command"
  (let ((process (or process (get-process "Cling"))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun cling-send-region (start end)
  "Sends the region in the current buffer between `start` and `end` to the inferior-cling buffer. Sends the currently selected region when called interactively."
  (interactive "r")
  (cling-send-string (buffer-substring start end)))

(defun cling-send-buffer ()
  "Sends the current buffer to the inferior-cling buffer."
  (interactive)
  (cling-send-region (point-min) (point-max))) ;;do i want to wrap-raw this? 

(defun cling-wrap-raw (string)
  "Wraps `string` in \".rawInput\", which tells Cling to accept function definitions"
  (format ".rawInput\n%s\n.rawInput" string))

(defun cling-wrap-region-and-send (start end)
  "Sends the region between start and end (currently selected when called interactively) to cling in raw input mode "
  (interactive "r")
  (cling-send-string (cling-wrap-raw (buffer-substring start end))))

(defun flatten-function-def ()
  "Flattens a function definition into a single line. This makes it easier to send to the inferior-cling buffer"
  (interactive)
  (replace-regexp "
" "" nil (mark) (point))) ;;;Why did I do this again? 

(defun select-defun ()
  "Selects the defun containing the point. Currently only works when point is on the line where the function's name is declared."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark (point))
  (re-search-forward "{")
  (save-excursion
   (flatten-function-def))
  (backward-char)
  (forward-sexp))

(defun cling-wrap-defun-and-send ()
  "Sends the current defun to cling in raw input mode. Currently only works when point is on the first line of function definition."
  (interactive)
  (save-excursion
    (select-defun)
    (cling-wrap-region-and-send (mark) (point))
    (undo)
    (undo)));;;this is a rather leaky way of doing temporary changes. there should be some way to save buffer contents or something
;;;probably uses with-temp-buffer

(defvar inferior-cling-keymap
  (let ((map (current-global-map)))
    (define-key map (kbd "C-c r") 'cling-send-region)
    (define-key map (kbd "C-c d") 'cling-wrap-defun-and-send)
    map))

(define-minor-mode inferior-cling-mode
  "Toggle inferior-cling-mode. Interactively w/o arguments, this command toggles the mode. A positive prefix argument enables it, and any other prefix argument disables it. 

When inferior-cling-mode is enabled, we rebind keys to facilitate working with cling."
  :keymap inferior-cling-keymap)


(defun utils/run-cling-load (fn)
  (progn
    (cling-send-string (format ".L %s" fn))
    ))

(defun utils/run-cling ()
  (let ((mode major-mode)
	(fn (buffer-file-name)))
    (setq comint-process-echoes 't)
    (run-cling)
    (cond
     ((or (string= mode 'c-mode)
	  (string= mode 'c++-mode)) (utils/run-cling-load fn)))

    )
  )

(provide 'utils/cling.el)
