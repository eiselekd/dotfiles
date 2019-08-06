;; google-chrome --remote-debugging-port=9222
;; npm install indium source-map chrome-remote-interface

(defun javascript-mode/detect-react ()
  (save-match-data
    (let ((pos 0)
          (matched nil))
      (if (re-search-forward "import\s+React" nil t )
	  't
	nil
	))))

;;"Hooks for javascript mode."
(defun my-javascript-mode-hook ()
  (interactive)
  (progn
    (setq js-indent-level 4)
    (setq indent-tabs-mode nil)
    (require 'flycheck)
    ;;(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
    (flycheck-mode)
    (message "[+] js2-mode for [%s]" (buffer-file-name))
    (progn
      (let* ((f (buffer-file-name)))
	(if (or
	     (string-match ".*components/.*\.js$" f)
	     (javascript-mode/detect-react)
	     )
	    (progn
	      (message "[+] detect js2-jsx")
	      (require 'rjsx-mode)
	      (rjsx-mode)
	      ))))

    (use-package indium
      :bind ("C-c i" . indium:commands/body)
      :config
      (progn
	(message "[+] indium")
	(require 'hydra)
	(defhydra indium:commands
	  nil
	  "indium"
	  ("l" (indium-launch))
	  ("c" (indium-connect))
	  ("a" (indium-add-breakpoint))
	  ("n" (javascript-mode/indium-prepare-node))
	  ("N" (javascript-mode/indium-prepare-node 't))
	  ("r" (javascript-mode/indium-prepare-react))
	  ("R" (javascript-mode/indium-prepare-react 't))
	  ("W" (javascript-mode/indium-prepare-chrome 't))
	  )
	)
    )))

(setq javascript-mode/indium-config-node "{ \n\
  \"configurations\": [ \n\
    { \n\
      \"name\": \"%s\", \n\
      \"type\": \"node\", \n\
      \"command\": \"node %s\", \n\
      \"inspect-brk\": true \n\
    } \n\
  ] \n\
} " )
(setq javascript-mode/indium-config-chrome "{ \n\
  \"configurations\": [ \n\
    { \n\
      \"name\": \"%s\", \n\
      \"type\": \"chrome\", \n\
      \"host\": \"localhost\",\n\
      \"url\": \"http://localhost/index.html\",\n\
      \"port\": 9222\n\
    } \n\
  ] \n\
} " )


(setq javascript-mode/indium-config-react "{ \n\
  \"configurations\": [ \n\
    { \n\
      \"name\": \"%s\", \n\
      \"type\": \"chrome\", \n\
      \"url\": \"http://localhost:3000\" \n\
    } \n\
  ] \n\
} " )


;;(intern (format "my-%s-function" name))

(defmacro javascript-mode/indium-prepare (var &optional force)
   `(let* ((source-dir (eval default-directory)) ;;(file-name-directory (buffer-file-name)))
	 (indium-config-dir (locate-dominating-file source-dir ".indium.json")))
    (message "Reading indium file: %s : [%s]" source-dir indium-config-dir )
    (if (and (not force) indium-config-dir)
	(progn
	  (let* ((fn (concat indium-config-dir ".indium.json" )))
	    (message "Found indium file %s " fn)
	    (switch-to-buffer fn)
	    (insert-file-contents fn)))
      (progn
	(let* ((indium-config-dir (file-name-directory (buffer-file-name)))
	       (f (concat indium-config-dir ".indium.json"))
	       (bf (expand-file-name (buffer-file-name)))
	       )
	  (message "generating new indium node config in %s for file %s" f ( expand-file-name (buffer-file-name) ) )
	  (with-temp-buffer
	    (insert (format ,var bf bf  ))
	    (write-region nil nil f)))))))

(defun javascript-mode/indium-prepare-node (&optional force)
  (javascript-mode/indium-prepare javascript-mode/indium-config-node force))

(defun javascript-mode/indium-prepare-chrome (&optional force)
  (javascript-mode/indium-prepare javascript-mode/indium-config-chrome force))

(defun javascript-mode/indium-prepare-react (&optional force)
  (javascript-mode/indium-prepare javascript-mode/indium-config-react force))

(autoload 'js2-mode "js2-mode" "js2-mode" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("js" . js2-mode))

(add-hook 'js2-mode-hook 'my-javascript-mode-hook)

;;(use-package js2-jsx-mode
;;  :mode "\\.jsx\\'"
;;  :init (add-hook 'js2-jsx-mode-hook 'my-javascript-mode-hook)
;;  )
(provide 'modes/javascript-mode.el)
