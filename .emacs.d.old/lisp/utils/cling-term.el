(defun utils/cling-has-clingconfig ()
  (locate-dominating-file (eval default-directory) ".cling.cpp"))

(defun utils/cling-get-dir ()
  (let* ((source-dir (eval default-directory)) ;;(file-name-directory (buffer-file-name)))
	 (cling-config-dir (locate-dominating-file source-dir ".cling.cpp")))
    (message "search for cling.txt in %s: %s" source-dir cling-config-dir )
    (if cling-config-dir cling-config-dir source-dir )))

(defun my-term-send-string (&optional buffer string)
  "Send STRING to a shell process associated with BUFFER.
By default, BUFFER is \"*terminal*\" and STRING is empty."
  (let ((process (get-buffer-process (or buffer "**cling**"))))
    (when (process-live-p process)
      (with-current-buffer (process-buffer process)
        (let ((input (or string "")))
          (term-send-string process input)
          (term-send-input))))))


;; Use this for remote so I can specify command line arguments
(defun cling-term (new-buffer-name cmd &rest switches)
  (let ((default-directory (utils/cling-get-dir)))
    (setq term-ansi-buffer-name new-buffer-name)
    (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
    (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
    (set-buffer term-ansi-buffer-name)
    (term-mode)
    (term-char-mode)
    (pop-to-buffer-same-window term-ansi-buffer-name)
    ;;(switch-to-buffer term-ansi-buffer-name)
    (with-current-buffer term-ansi-buffer-name
      (my-term-send-string term-ansi-buffer-name ".L .cling.cpp")
    )))

(provide 'utils/cling-term.el)
