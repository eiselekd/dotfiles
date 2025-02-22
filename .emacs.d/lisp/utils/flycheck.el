;; http://emacswiki.org/emacs/ElispCookbook#toc29
;;(utils/flycheck-search-linux-makefile)

(defun utils/projmake-sethere ()
  "Projmake start."
  ;;; (Bindings) ;;;
  (interactive)
  (let
      ((FLYCHECK_GENERIC_SRC       (read-from-minibuffer "FLYCHECK_GENERIC_SRC: " (or (utils/flycheck-generic-makefile-src-root) default-directory)))
       (FLYCHECK_GENERIC_BUILD     (read-from-minibuffer "FLYCHECK_GENERIC_BUILD: " (or (utils/flycheck-generic-makefile-build-root) default-directory)))
       (FLYCHECK_GENERIC_CMD       (read-from-minibuffer "FLYCHECK_GENERIC_CMD: " (or (utils/flycheck-generic-makefile-cmd) "all")))
       (FLYCHECK_GENERIC_ADDSUFFIX (read-from-minibuffer "FLYCHECK_GENERIC_ADDSUFFIX: " (utils/flycheck-generic-makefile-addsuffix)))
       )
    (progn
      (setenv "FLYCHECK_GENERIC_SRC" FLYCHECK_GENERIC_SRC)
      (setenv "FLYCHECK_GENERIC_BUILD" FLYCHECK_GENERIC_BUILD)
      (if (not (equal FLYCHECK_GENERIC_CMD "")) (setenv "FLYCHECK_GENERIC_CMD" FLYCHECK_GENERIC_CMD))
      (if (not (equal FLYCHECK_GENERIC_ADDSUFFIX "")) (setenv "FLYCHECK_GENERIC_ADDSUFFIX" FLYCHECK_GENERIC_ADDSUFFIX))
      (message (format "[*] FLYCHECK_GENERIC_SRC: %s" (getenv "FLYCHECK_GENERIC_SRC")))
      (message (format "[*] FLYCHECK_GENERIC_BUILD: %s" (getenv "FLYCHECK_GENERIC_BUILD")))
      (message (format "[*] FLYCHECK_GENERIC_CMD: %s" (getenv "FLYCHECK_GENERIC_CMD")))
      (message (format "[*] FLYCHECK_GENERIC_ADDSUFFIX: %s" (getenv "FLYCHECK_GENERIC_ADDSUFFIX")))
      (message (format "[*] Start fycheck mode"))
      (flycheck-mode))))


;;;;; ------------------------------------------------------------------

(defun utils/flycheck-search-linux-makefile ()
  "Search for linux top `Makefile' "
  (cl-labels
      ((find-makefile-file-r (path)
	(let* ((parent (file-name-directory path))
	       (file (concat parent "Makefile")))
	  (cond
	   ((file-exists-p file)
	    (progn
	      (with-temp-buffer
		(insert-file-contents file)
		(if (string-match "VERSION = [0-9]+[[:space:]]*PATCHLEVEL" (buffer-string))
		    (throw 'found-it parent)
		  (find-makefile-file-r (directory-file-name parent))
		  ))))
	   ((equal path parent) (throw 'found-it nil))
	   (t (find-makefile-file-r (directory-file-name parent)))))))
    (if (buffer-file-name)
        (catch 'found-it
          (find-makefile-file-r (buffer-file-name)))
      nil)))

(flycheck-define-checker utils/flycheck-linux-makefile-checker
  "Linux source checker"
  :command
  (
   "make" "C=1" "-C" (eval (utils/flycheck-search-linux-makefile))
   (eval (concat (file-name-sans-extension (file-relative-name buffer-file-name (utils/flycheck-search-linux-makefile))) ".o"))
   )
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
	(let* ((fn (flycheck-error-filename err))
	       (rn0 (file-relative-name fn default-directory)) ; flycheck-fix-error-filename converted to absolute, revert
	       (rn1 (expand-file-name rn0 (utils/flycheck-search-linux-makefile))) ; make absolute relative to "make -C dir"
	       (ef (file-relative-name rn1 default-directory)) ; relative to source
	       )
	  (setf (flycheck-error-filename err) ef)
	  )))
    errors)
  :modes (c-mode c++-mode)
  )

;;;;; ------------------------------------------------------------------

(defcustom flycheck-generic-makefile-cmd nil
  "The command to run in the build root"
  :type 'string
  )
(put 'flycheck-generic-makefile-cmd 'safe-local-variable (lambda (x) t))

(defun utils/flycheck-generic-makefile-cmd ()
  "Return if buffer is inside the src root, eather return local file variable or enviroemtnal variable pointing to root of src"
  (let ((src (or flycheck-generic-makefile-cmd (getenv "FLYCHECK_GENERIC_CMD") "all")))
    src))

(defcustom flycheck-generic-makefile-addsuffix nil
  "Weather ot not"
  :type 'boolean
  )
(put 'flycheck-generic-makefile-addsuffix 'safe-local-variable (lambda (x) t))

(defun utils/flycheck-generic-makefile-addsuffix ()
  "Weather or not to add curdir-srcdir diff to build path"
  (or flycheck-generic-makefile-addsuffix (if (getenv "FLYCHECK_GENERIC_ADDSUFFIX") t) nil ))

(defcustom flycheck-generic-makefile-src-root nil
  "The root of the src directory"
  :type 'string
  )
(put 'flycheck-generic-makefile-src-root 'safe-local-variable (lambda (x) t))

(defun utils/flycheck-generic-makefile-src-root ()
  "Return if buffer is inside the src root, eather return local file variable or enviroemtnal variable pointing to root of src"
  (let ((src (or flycheck-generic-makefile-src-root (getenv "FLYCHECK_GENERIC_SRC")))
	(d (file-name-directory (buffer-file-name)))
	)
    (if (string-equal src (substring d 0 (length src)))
	(progn
	  (message (format "found src root: %s" src))
	  src)
        nil)))

(defcustom flycheck-generic-makefile-build-root nil
  "The root of the build directory"
  :type 'string
  )
(put 'flycheck-generic-makefile-build-root 'safe-local-variable (lambda (x) t))
(defun utils/flycheck-generic-makefile-build-root ()
  "Eather return local file variable or enviroemtnal variable pointing to root of build"
  (let ((src (or flycheck-generic-makefile-build-root (getenv "FLYCHECK_GENERIC_BUILD"))))
    src))

(defun utils/flycheck-get-generic-offset ()
  "Search for top `Makefile' "
  (file-relative-name (file-name-directory (buffer-file-name)) (utils/flycheck-generic-makefile-src-root))
  )

;; determine where to run make from, the final -C dir will be:
;; FLYCHECK_GENERIC_BUILD + (if (FLYCHECK_GENERIC_ADDSUFFIX) ( CURDIR - FLYCHECK_GENERIC_BUILD ))
(defun utils/flycheck-search-generic-makefile ()
  "Search for top `Makefile' "
  (concat (file-name-as-directory (utils/flycheck-generic-makefile-build-root)) (if (utils/flycheck-generic-makefile-addsuffix) (utils/flycheck-get-generic-offset) ""))
  )


(flycheck-define-checker utils/flycheck-generic-makefile-checker
  "Generic makefile checker"
  :command
  (
   "make" "-C" (eval (utils/flycheck-search-generic-makefile)) (eval (utils/flycheck-generic-makefile-cmd))
   ;;"exfat-compile"

;;   (eval (message (format "make -C %s %s\n" (eval (utils/flycheck-search-generic-makefile)) (eval (utils/flycheck-generic-makefile-cmd)))))
;;   (eval (concat (file-name-sans-extension (file-relative-name buffer-file-name (utils/flycheck-search-generic-makefile))) ".o"))
   )
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))

  ;; :error-parser
  ;; (lambda (output checker buffer)
  ;;   (message (format "%s\n" output))
  ;;   ()
  ;;   )

  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
  	(setf (flycheck-error-filename err) (file-name-nondirectory (flycheck-error-filename err)))


	))
    errors)
  :modes (c-mode c++-mode)
  )



  ;; :error-parser
  ;; (lambda (output checker buffer)
  ;;   (message (format "%s\n" output))
  ;;   ()
  ;;   )

;;'(flycheck-c/c++-gcc-executable "/usr/local/bin/gcc-4.9")
;;make-local-variable

(defun utils/flycheck-mode-hook ()
  "Flycheck mode hook."
  (make-variable-buffer-local 'flycheck-linux-makefile)
  (setq flycheck-linux-makefile (utils/flycheck-search-linux-makefile))
  (message "[*] try match custom checker:")
  (let (f (buffer-file-name))
    (if f
	(progn
	  (message (format "[*] buf-dir: %s" (file-name-directory f)))
	  (message (format "[*] FLYCHECK_GENERIC_SRC: %s" (getenv "FLYCHECK_GENERIC_SRC")))
	  (message (format "[*] FLYCHECK_GENERIC_BUILD: %s" (getenv "FLYCHECK_GENERIC_BUILD")))
	  (message (format "[*] FLYCHECK_GENERIC_CMD: %s" (getenv "FLYCHECK_GENERIC_CMD")))
	  (message (format "[*] FLYCHECK_GENERIC_ADDSUFFIX: %s" (getenv "FLYCHECK_GENERIC_ADDSUFFIX")))
	  ;;(if flycheck-linux-makefile
	  ;;    (progn
	  ;;	(message "[*] enable linux makefile checker")
	  ;;	(flycheck-select-checker 'utils/flycheck-linux-makefile-checker)))
	  (if (utils/flycheck-generic-makefile-src-root)
	      (progn
		(message "[*] enable generic makefile")
		(flycheck-select-checker 'utils/flycheck-generic-makefile-checker)
		))
  ;;(flycheck-select-checker 'utils/flycheck-generic-makefile-checker)

	  (utils/flycheck-local-keybind )
	  (setq flycheck-clang-language-standard "c++17")
	  ))))

(defun utils/flymake-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
  (utils/flymake-local-keybind ))

(defun utils/projmake-start ()
  "Projmake start."
  ;;; (Bindings) ;;;
  (interactive)
  (progn
    (when (require 'projmake-mode nil t)
      (progn
	(projmake-mode)
	(projmake-discover/search-load-project)
	(projmake-mode/on)
	(utils/projmake-local-keybind )))))

(defun utils/flycheck-color-mode-line-init ()
  "Initialize flycheck color mode."
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; "Initialize flycheck."
(defun utils/flycheck-init ()
  "Initialize flymake"
  (setq-default
   ;; Wait five seconds before starting checker
   flycheck-idle-change-delay 1)

  ;; Enable flycheck globally.
  ;;(global-flycheck-mode t)

  (after-load 'popwin
    ;; Use popwin for Flycheck error list.
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config))

  (after-load 'popwin
    (push '(flymode-error-list-mode :stick t) popwin:special-display-config))


  ;;(add-hook 'flycheck-mode-hook 'utils/flycheck-mode-hook)
  ;;(add-hook 'flymake-mode-hook 'utils/flymake-mode-hook)
  ;;(message "[*] %s Flycheck hook setup" (timestamp_str))
  )

(add-hook 'after-init-hook 'utils/flycheck-init)

;; #####################
;; perl using Test::More
(defun utils/perl-checker-enabled ()
  (if (boundp 'checker-enable)
      checker-enable
    nil))

(add-hook 'perl-mode-hook
	  (lambda ()
	    (message "[=] perl-mode-hook perl-mode")
	    (flycheck-mode)
	    (put 'checker-enable 'safe-local-variable (lambda (_) t))
	    (add-hook 'hack-local-variables-hook
		      (lambda ()
			(if (utils/perl-checker-enabled)
			    (when (require 'utils/perl-checker.el nil t)
			      )
			  )))))


;; #####################
;; python using unittest
(defun utils/python-checker-enabled ()
  (if (boundp 'checker-enable)
      checker-enable
    nil))

(add-hook 'python-mode-hook
	  (lambda ()
	    (message "[=] python-mode-hook python-mode")
	    (flycheck-mode)
	    (put 'checker-enable 'safe-local-variable (lambda (_) t))
	    (add-hook 'hack-local-variables-hook
		      (lambda ()
			(if (utils/python-checker-enabled)
			    (when (require 'utils/python-checker.el nil t)
			      )
			  )))))


;; #####################
;; c/c++ using gtest
(defun flycheck/test-enable-gtest ()
			   (message "[+] setup gtest, use 'buffer-gtest-rule' local var")
			   (put 'buffer-gtest-rule 'safe-local-variable (lambda (_) t))
			   (make-local-variable 'buffer-gtest-rule)
			   (require 'utils/gtest-checker.el)
			   (add-hook 'hack-local-variables-hook
				     (lambda ()
				       (message "[*] check for gtest rules " )
				       (if (and (boundp 'buffer-gtest-rule) (utils/flycheck-gtest-makefile))
					   (progn
					     (message "[*] enable gtest checker after '%s' with rule %s" (flycheck-get-checker-for-buffer) (utils/flycheck-gtest-getrule))

					     ;;(if
						 ;; (not (and (require 's nil t)
						 ;; 	   (or (s-ends-with? ".lex" (buffer-file-name))
						 ;; 	       (s-ends-with? ".y" (buffer-file-name))
						 ;; 	       (s-ends-with? ".l" (buffer-file-name)))))
						 ;; (flycheck-add-next-checker (flycheck-get-checker-for-buffer) '(warning . utils/gtest-checker-makefile-checker) ))

					     )))))

(add-hook 'c-mode-hook  (lambda ()
			  (message "[=] c-mode-hook c-mode")
			  (flycheck-mode)

			  ;;(flycheck-select-checker 'c/c++-gcc)
			  (if
			      (and (require 's nil t)
				   (or (s-ends-with? ".lex" (buffer-file-name))
				       (s-ends-with? ".y" (buffer-file-name))
				       (s-ends-with? ".l" (buffer-file-name))))
			      (progn
				(message "[=] enable lex/yacc flycheck")
				;;(flycheck-select-checker 'c/c++-clang))
			    	)
			    (progn
			      (message "[=] enable c flycheck")
			      ;;(flycheck-select-checker 'c/c++-clang)
			      )
			    )
			   (setq flycheck-clang-language-standard "c++17")
			   (setq flycheck-gcc-language-standard "c++17")
			   (setq flycheck-clang-force-c++-mode 't)

			   ;;(flycheck/test-enable-gtest)

			   ))

(add-hook 'c++-mode-hook (lambda ()
			  (message "[=] c++-mode-hook c-mode")
			   (flycheck-mode)
			   ;;(flycheck-select-checker 'c/c++-gcc)
			   ;;(flycheck-select-checker 'c/c++-clang)
			   (setq flycheck-clang-language-standard "c++17")
			   (setq flycheck-gcc-language-standard "c++17")

			   (flycheck/test-enable-gtest)


))

;;			   (setq flycheck-command-wrapper-function
;;				 (lambda (command)
;;				   (progn
;;				     (message "[+] run: %s" command)
;;				     command))

(defun flycheck-mode-verbose ()
  (interactive)
  (progn
    (flycheck-compile (flycheck-get-checker-for-buffer))
    ))

(defun flycheck-mode-verbose-select ()
  (interactive)
  (progn
    (call-interactively 'flycheck-compile)
    ))

;; (defadvice flymake-find-buildfile
;;     (around advice-find-makefile-separate-obj-dir
;; 	    activate compile)
  ;; "Look for buildfile in a separate build directory"
  ;; (let* ((source-dir (ad-get-arg 1))
  ;; 	 (bld-dir (ac-build-dir source-dir)))
  ;;   (ad-set-arg 1 bld-dir)
  ;;   ad-do-it))

;; (defun ac-find-configure (source-dir)
;;   (locate-dominating-file source-dir "configure"))

;; (defvar project-build-root nil
;;   "top build directory of the project")

;; (defun ac-build-dir (source-dir)
;;   "find the build directory for the given source directory"
;;   (condition-case nil
;;       (let* ((topdir (ac-find-configure source-dir))
;; 	     (subdir (file-relative-name (file-name-directory source-dir) topdir))
;; 	     (blddir (concat (file-name-as-directory project-build-root) subdir)))
;; 	blddir)
;;         (error source-dir)))




(provide 'utils/flycheck.el)
