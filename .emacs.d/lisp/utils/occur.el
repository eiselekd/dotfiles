;;---------------------------------------------
;; http://home.thep.lu.se/~karlf/emacs.html#sec-1-2-6

(eval-when-compile
  (require 'cl-lib))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (require 'swiper)
  (swiper-isearch))

;;  (multi-occur
;;   (get-buffers-matching-mode major-mode)
;;   (car (occur-read-primary-args))))

;; global key for `multi-occur-in-this-mode' - you should change this.
(global-set-key (kbd "M-s") 'multi-occur-in-this-mode)

(provide 'utils/occur.el)
