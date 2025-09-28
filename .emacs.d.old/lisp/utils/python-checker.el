;; flycheck checker for gtest output:
;; calls "make gtest" and parses for Failure
;; add with :
;; (flycheck-add-next-checker (flycheck-get-checker-for-buffer)  '(warning . utils/python-checker-makefile-checker) )

;; debug : ctrl-c ! ctrl-c
;; and select utils/python-checker-makefile-checker

(require 'flycheck)

(defun utils/python-regex () (rx
	      "======================================================================\n"
	      "FAIL: " (group-n 3 (one-or-more not-newline)) "\n"
	      "----------------------------------------------------------------------\n"
	      "Traceback" (one-or-more not-newline) "\n"
	      (zero-or-more (or "  " "    ") (one-or-more not-newline) "\n" )
	      "  File \"" (group-n 1 (minimal-match (one-or-more not-newline)))  "\", line " (group-n 2 (one-or-more digit))  (one-or-more not-newline) "\n"
	      "    " (one-or-more not-newline) "\n"
	      "AssertionError"
	      ))

(defun utils/python-unittest-parser (output checker buffer)
  "Parse unittest errors "
  ;;(message "[=] buffer '%s'" output)

  (save-match-data
    (let ((string output)
  	  (pos 0)
          errors)
      (while (string-match (utils/python-regex) string pos)
        (push
  	 (flycheck-error-new-at
	   (string-to-number (match-string 2 string))
	  nil
	  'error
	  (match-string 3 string)
	  )

  	 errors)
        (setq pos (match-end 0)))

      (nreverse errors)))
     )

(flycheck-define-checker utils/python-unittest-checker
  "Generic python unittest checker"
  :command ("python3" "-m" "unittest" source-original )

  :enabled (lambda () (utils/python-checker-enabled)) ;; init in  flycheck.el : python-mode-hook

  :error-parser utils/python-unittest-parser

  :error-filter (lambda (errors)
		  (message "[=] %s" errors)
		  errors)

   :modes python-mode
  )

(add-to-list 'flycheck-checkers 'utils/python-unittest-checker)

(provide 'utils/python-checker.el)
