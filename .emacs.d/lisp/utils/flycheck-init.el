(add-hook 'after-init-hook (lambda ()
			     (progn
			       (require 'flycheck)
			       (require 'utils/flycheck.el)
			       'utils/flycheck-init)))

(add-hook 'c-mode-hook  (lambda ()
			   (flycheck-mode)
			   ;;(flycheck-select-checker 'c/c++-gcc)
			   (flycheck-select-checker 'c/c++-clang)
			   (setq flycheck-clang-language-standard "c++14")
			   (setq flycheck-gcc-language-standard "c++17")
			   (setq flycheck-clang-force-c++-mode 't)
			   ))

(add-hook 'c++-mode-hook (lambda ()
			   (flycheck-mode)
			   (flycheck-select-checker 'c/c++-gcc)
			   ;;(flycheck-select-checker 'c/c++-clang)
			   (setq flycheck-clang-language-standard "c++14")
			   (setq flycheck-gcc-language-standard "c++17")


			   (message "[+] setup gtest, use 'buffer-gtest-rule' local var")
			   (put 'buffer-gtest-rule 'safe-local-variable (lambda (_) t))
			   (make-local-variable 'buffer-gtest-rule)
			   (require 'utils/gtest-checker.el)
			   (add-hook 'hack-local-variables-hook
				     (lambda ()
				       (if (and (boundp 'buffer-gtest-rule) (utils/flycheck-gtest-makefile))
					   (progn
					     (message "[*] enable gtest checker after '%s' with rule %s" (flycheck-get-checker-for-buffer) (utils/flycheck-gtest-getrule))
					     (flycheck-add-next-checker (flycheck-get-checker-for-buffer) '(warning . utils/gtest-checker-makefile-checker) )
					     ))))
			   ))

(provide 'utils/flycheck-init.el)
