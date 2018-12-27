;; google-chrome --remote-debugging-port=9222
;; npm install indium source-map chrome-remote-interface

;;"Hooks for javascript mode."
(defun my-javascript-mode-hook ()
  (interactive)
  (progn
    ;;(message "[+] my-javascript-mode-hook")
    (setq js-indent-level 4)
    (setq indent-tabs-mode nil)
    (require 'js2-mode)

    (use-package indium
      :bind ("C-c i" . indium:commands/body)
      :config
      (progn
	(require 'hydra)
	(defhydra indium:commands
	  nil
	  "indium"
	  ("l" (indium-launch))
	  ("a" (indium-add-breakpoint))
	  ("n" (javascript-mode/indium-prepare-node))
	  ("N" (javascript-mode/indium-prepare-node 't))
	  )
	))
    ))

(defun javascript-mode/indium-prepare-node (&optional force)

  (let* ((source-dir (eval default-directory)) ;;(file-name-directory (buffer-file-name)))
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
	    (insert (format "{ \n\
  \"configurations\": [ \n\
    { \n\
      \"name\": \"%s\", \n\
      \"type\": \"node\", \n\
      \"command\": \"node %s\", \n\
      \"inspect-brk\": true \n\
    } \n\
  ] \n\
} " bf bf  ))
	    (write-region nil nil f)))))))

(add-hook 'js-mode-hook  'my-javascript-mode-hook)
(provide 'modes/javascript-mode.el)
