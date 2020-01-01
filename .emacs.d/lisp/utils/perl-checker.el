;; flycheck checker for gtest output:
;; calls "make gtest" and parses for Failure
;; add with :
;; (flycheck-add-next-checker (flycheck-get-checker-for-buffer)  '(warning . utils/perl-checker-makefile-checker) )

;; debug : ctrl-c ! ctrl-c
;; and select utils/perl-checker-makefile-checker

(require 'flycheck)

(flycheck-define-checker utils/perl-checker-makefile-checker
  "Generic perl unittest checker"
  :command
  ( "perl" source )
  :error-patterns
  (
   ;;Capture errors like: #   Failed test at unit.pl line 4.
   (error line-start "#   Failed test at " (file-name) " line " line "." line-end)
   ;; not ok 1 - test
   ;;#   Failed test 'test'
   ;;#   at /home/eiselekd/git/lang/perl/unit.pl line 9.
   (error line-start "#   Failed test '" (message (one-or-more (not (any "'")))) "'\n#   at " (file-name) " line " line "." line-end)
   )
  :enabled (lambda () (utils/perl-checker-enabled)) ;; init in  flycheck.el : perl-mode-hook
  :error-filter
  ;; Add fake message if not present
  (lambda (errors)
    (message "[=] %s" errors)
    (dolist (err errors)
      (unless (flycheck-error-message err)
        (setf (flycheck-error-message err) "Error")))
    errors)
   :modes perl-mode
  )

(add-to-list 'flycheck-checkers 'utils/perl-checker-makefile-checker)

(provide 'utils/perl-checker.el)
