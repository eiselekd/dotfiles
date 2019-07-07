(add-to-list 'auto-mode-alist '("\\.l8\\'" . l8-mode))

(defvar l8-mode-hook nil)

(defvar l8-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for l8 major mode")


(defconst l8-font-lock-keywords-1
  (list
   `( ,(concat "\\<" (regexp-opt '("if" "then" "else" "part"
				   "logic" "layout" "std" "vec" "fun"
				   ) t) "\\>") . font-lock-builtin-face)
   '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for WPDL mode")

(defvar l8-font-lock-keywords l8-font-lock-keywords-1
  "Default highlighting expressions for WPDL mode")

(define-derived-mode l8-mode rust-mode "l8"
  "major mode for editing mymath language code."
  (set (make-local-variable 'font-lock-defaults) '(l8-font-lock-keywords))
  )

(provide 'modes/l8-mode.el)
