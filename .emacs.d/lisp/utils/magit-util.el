
(defun utils/magit-internet-up (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       magit-gerrit-remote) ))

(defun utils/magit-commit-all ()
  (progn
    (message "[+] magit commit all" )
    (magit-call-git "commit" "--all" "-m" "all" )
    (magit-call-git "push" "origin")
    (magit-refresh)
    ))

(defun utils/magit-auto-commit-files (list)
  (interactive
    (list (list (buffer-file-name (current-buffer)))))
  "LIST to be auto commit"
  (while list
    (let* ((file (car list))
           (file-buffer (get-file-buffer file)))
      (when file-buffer
        (set-buffer file-buffer)
        (when (magit-anything-modified-p nil file)
          (magit-call-git "add" file)
          (magit-call-git "commit" "-m" (concat file " update"))
          (magit-call-git "push" "origin")
          (magit-refresh)
          (print (concat file "is pushed!!!")))))
    (setq list (cdr list))))

(defun utils/magit-status ()
  (progn
    (let ((doloat 't))

      (require 'magit-gerrit)

      (if (fboundp 'set-magit-gerrit-default)
	  (set-magit-gerrit-default))


      ;;utils/magit-internet-up

    ;;(setq magit-section-visibility-indicator  '("..." . t)) ;; '("…" . t)
      (magit-status))))

(provide 'utils/magit-util.el)
