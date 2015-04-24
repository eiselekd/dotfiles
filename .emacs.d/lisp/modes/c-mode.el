
(add-hook 'c++-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'c-mode-hook   (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(provide 'modes/c-mode.el)
