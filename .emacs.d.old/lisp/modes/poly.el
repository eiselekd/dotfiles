;; https://www.masteringemacs.org/article/polymode-multiple-major-modes-how-to-use-sql-python-in-one-buffer

(require 'polymode)

(defun pm--c++r-head-matcher (ahead)
  (when (re-search-forward "^[ \t]*/[*]+" nil t ahead)
    (cons (match-beginning 0) (match-end 0))))

(defun pm--c++r-tail-matcher (ahead)
  (when (< ahead 0)
    (error "Backwards tail match not implemented"))
  ;; may be rely on syntactic lookup ?
  (when (re-search-forward "^[ \t]*\\*/")
    (cons (match-beginning 0) (match-end 0))))


(define-innermode c-org-mode-innermode
  :mode 'org-mode
  :head-matcher 'pm--c++r-head-matcher
  :tail-matcher 'pm--c++r-tail-matcher
  :head-mode 'host
  :tail-mode 'host)

(define-hostmode c-org-mode-hostmode :mode 'c-mode)

(define-polymode c-org-mode-mode
    :hostmode 'c-org-mode-hostmode
    :innermodes '(c-org-mode-innermode))


(provide 'modes/poly.el)

