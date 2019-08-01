(defun utils/flycheck-gtest-getrule ()
  (if (boundp 'buffer-gtest-rule)
      buffer-gtest-rule
    "gtest2"))

;; search for Makefile in parent that contains "gtest" rule
(defun utils/flycheck-gtest-makefile ()
  "Search for linux top `Makefile' "
  (let* ((rule (utils/flycheck-gtest-getrule)))
    (message "[?] Try match Makefile with %s" (format "%s:" rule))
    (cl-labels
      ((find-makefile-file-r (path)
	(let* ((parent (file-name-directory path))
	       (file (concat parent "Makefile")))
	  (cond
	   ((file-exists-p file)
	    (progn
	      (with-temp-buffer
		(insert-file-contents file)
		(if (string-match (format "%s:" rule) (buffer-string))
		    (progn
		      (message "[!]Found Makefile %s" parent)
		      (throw 'found-it parent))
		  (find-makefile-file-r (directory-file-name parent))
		  ))))
	   ((equal path parent) (throw 'found-it nil))
	   (t (find-makefile-file-r (directory-file-name parent)))))))
    (if (buffer-file-name)
        (catch 'found-it
          (find-makefile-file-r (buffer-file-name)))
      nil))))

;; flycheck checker for gtest output:
;; calls "make gtest" and parses for Failure
;; add with :
;; (flycheck-add-next-checker (flycheck-get-checker-for-buffer)  '(warning . utils/gtest-checker-makefile-checker) )
(require 'flycheck)
(flycheck-define-checker utils/gtest-checker-makefile-checker
  "Generic gtest makefile checker"
  :command
  ( "make" "-C" (eval (utils/flycheck-gtest-makefile)) (eval (utils/flycheck-gtest-getrule)) )
  :error-patterns
  (
   ;;Capture errors like: file.cpp:10: Failure
   (error line-start (file-name) ":" line
          ": Failure" "\n"
	  (message (one-or-more (not (any "[")))))
   )
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
	(let* ((fn (flycheck-error-filename err))
	       (rn0 (file-relative-name fn default-directory)) ; flycheck-fix-error-filename converted to absolute, revert
	       (rn1 (expand-file-name rn0 (utils/flycheck-gtest-makefile))) ; make absolute relative to "make -C dir"
	       (ef (file-relative-name rn1 default-directory)) ; relative to source
	       )
	  (setf (flycheck-error-filename err) ef)
	  )))
    errors)
  :modes (c-mode c++-mode)
  )

(add-to-list 'flycheck-checkers 'utils/gtest-checker-makefile-checker)

(provide 'utils/gtest-checker.el)
