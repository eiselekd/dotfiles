;; org mode "* #" break. For or-mode sections in c comments using orgstruct-mode
;;a
;;a
;;* This is a heading
;;b
;;b
;;* #
;;c
;;c

(require 'cl-lib)


(require 'ov)

(defun get-org-buffer ()
  (cdr (assoc 'org-mode  multi-indirect-buffers-alist)))
(defun get-c++-buffer ()
  (cdr (assoc 'c++-mode  multi-indirect-buffers-alist)))

;; exclude the overlays marked with `ismultimode' from overlay list of buffer `buf'
(defun get-real-ov (buf)
  (let ((org-ov))
    (with-current-buffer buf
      (setq org-ov (seq-remove (lambda (x) (ov-val x 'ismultimode )) (ov-all))))
    org-ov))

;; remove all overlays marked with `ismultimode' from buffer
(defun clean-intermediate-overlays (buf)
  (with-current-buffer buf 
      (dolist (e (ov-all))
	(if (ov-val e 'ismultimode )
	    (ov-reset e)))))

(defun sync-buffer-overlays ()
  (interactive)
  (let ((org-buf (get-org-buffer))
	(c++-buf (get-c++-buffer))
	(org-ov)
	(c++-ov)
	)
    (message "org:%s" org-buf)
    (message "c++:%s" c++-buf)

    (clean-intermediate-overlays c++-buf) 
    (clean-intermediate-overlays org-buf) 

    (setq org-ov (get-real-ov org-buf))
    (with-current-buffer c++-buf
      (dolist (e org-ov)
	(let ((beg (ov-beg e))
	      (end (ov-end e)))
	  (ov beg end '(invisible hs ismultimode t)))
	))

    (setq c++-ov (get-real-ov c++-buf))
    (with-current-buffer org-buf
      (dolist (e c++-ov)
	(let ((beg (ov-beg e))
	      (end (ov-end e)))
	  (ov beg end '(invisible outline ismultimode t)))
	))
    )
  )



(defun ov/printranges (all)
  (dolist (e all)
    (let ((beg (ov-beg e))
	  (end (ov-end e)))
      (message "[=] >>>  overlays: %s-%s: %s  inv:'%s'<<<" beg end (ov-prop e) (ov-val e 'invisible))
      ))
  )

(defun show-all-overlays ()
  (interactive)
  (dolist (e multi-indirect-buffers-alist)
    (let ((a))
      (message "[=] --------")
      (message "[=] buffer: '%s' '%s'" (car e) (cdr e))
      (with-current-buffer (cdr e)
	(ov/printranges (ov-all)))
      )
    ))






(defun c++-mode/toggle-org ()
  (interactive)
  (message "[>] c++ to org mode")
  (require 'multi-mode-util nil t)
  (multi-mode-init 'c++-mode)
  (multi-install-chunk-finder "/\\*" "\\*/" 'org-mode)
  (sync-buffer-overlays))
  

;; \"/*\" \"*/\"
  
  ;; (let ((all (mapcar (lambda (x) (list (ov-beg x) (ov-end x) (ov-prop x))) (ov-all))))
  ;;   (setq org-startup-folded nil)
  ;;   (org-mode)
  ;;   (dolist (e all)
  ;;     (let ((ov0))
  ;; 	(setq ov0 (ov (nth 0 e) (nth 1 e) (nth 2 e)))))
    
  ;;   ;;(ov/printranges all)
  ;;   )
  ;; ;;(outline-show-all)
  ;; (global-set-key (kbd "<f9>")  'c++-mode/toggle-c++-mode)
;; )

(defun c++-mode/toggle-c++-mode ()
  (interactive)
  (message "[>] org mode to c++")
  (let ((all (mapcar (lambda (x) (list (ov-beg x) (ov-end x) (ov-prop x))) (ov-all))))
    (c++-mode)
    (dolist (e all)
      (let ((ov0))
	(setq ov0 (ov (nth 0 e) (nth 1 e) (nth 2 e)))))
    
    ;;(ov/printranges all)
    )
  (global-set-key (kbd "<f9>")  'c++-mode/toggle-org)
  )


(defun c-mode/toggle-org ()
  (interactive)
  (setq org-startup-folded nil)
  (org-mode)
  ;;(outline-show-all)
  (global-set-key (kbd "<f9>")  'c-mode/toggle-c-mode)
  )
(defun c-mode/toggle-c-mode ()
  (interactive)
  (c-mode)
  (global-set-key (kbd "<f9>")  'c-mode/toggle-org)
  )



(defun modes/outline-show-children (orig-fun &rest args)
  (let (pre-outline-map-region outline-map-region)
    (let ((res)
	  (ad
	    (lambda (ad-orig-fun &rest ad-args)
	      (message "outline-show-children")
	      (apply ad-orig-fun
		     (lambda ()
		       (if (<= (funcall outline-level) level)
			   (if (looking-at (concat outline-regexp "\s*#" ))
			       (progn
				 (outline-show-heading )
				 (show-entry ))
			     (outline-show-heading))))
		     (cdr ad-args)))))
      (advice-add 'outline-map-region :around ad)
      (setq res (apply orig-fun args))
      (advice-remove 'outline-map-region      ad)
      res
      )))

(defun modes/outline-hide-sublevels (orig-fun &rest args)
  (let (pre-outline-map-region outline-map-region)
    (let ((res)
	  (ad
	    (lambda (ad-orig-fun &rest ad-args)
	      (message "outline-hide-sublevels")
	      (apply ad-orig-fun
		     (lambda ()
		       (if (<= (funcall outline-level) levels)
			   (if (looking-at (concat outline-regexp "\s*#" ))
			       (progn
				 (outline-show-heading )
				 (show-entry ))
			     (outline-show-heading))))
		     (cdr ad-args)))))
      (advice-add 'outline-map-region :around ad)
      (setq res (apply orig-fun args))
      (advice-remove 'outline-map-region      ad)
      res
      )))

(defun modes/org-cycle-internal-local (orig-fun &rest args)
  (cond
   ((not (looking-at (concat outline-regexp "\s*#" )))
    (apply orig-fun args))))

(defun modes/orgstruct-commen ()
  (orgstruct-mode)
  (advice-add 'org-cycle-internal-local :around #'modes/org-cycle-internal-local)
  (advice-add 'outline-show-children    :around #'modes/outline-show-children)
  (advice-add 'outline-hide-sublevels   :around #'modes/outline-hide-sublevels)
  )

(defun c-mode-addfuncs ()
  (ggtags-mode)
  (global-set-key (kbd "M-G")  'magit-log-buffer-file)
  (global-set-key (kbd "M-i")  (lambda () (interactive) (c-mode-add-irony)))

  (require 'prepaint nil t)
  (when (require 'hideshow nil t)
    (progn
      (global-set-key (kbd "M-(")  'hs-toggle-hiding)
      (global-set-key (kbd "M-)")  'hs-toggle-hiding)
      ))
  (global-set-key (kbd "M-SPC")  (lambda () (interactive) (setq write-file-functions (delete 'delete-trailing-whitespace write-file-functions ))))

  )

(defun c-mode-add-irony ()
  (progn
    (when (and (require 'company nil t) (require 'irony nil t) (require 'company-irony nil t) (require 'irony-cdb nil t))
      (progn
	(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    	(message "[+] activate company-irony")
	(call-interactively 'company-mode)
	(call-interactively 'company-irony)
	(global-set-key (kbd "M-I")  (lambda () (interactive) (irony-cdb-menu)))
	))
    ))

(add-hook 'hs-org/minor-mode-hook (lambda ()
				    (progn
				      (message "[+] hs-org/minor-mode hook")
				      (hide-ifdef-mode)

				      ;; in hs-ifdef mode:
				      ;; M-0 : undefine sym over cursor
				      ;; M-1 : define sym over cursor
				      ;; M-2 : hideshow-ifdef evaluation
				      (define-key hs-org/minor-mode-map (kbd "M-1")
					(lambda ()  (interactive)
					  (let* ((default (save-excursion
							    (current-word 'strict)))
						 (var (read-minibuffer "Define what? " default))
						 (val (read-from-minibuffer (format "Set %s to? (default 1): " var)
									    nil nil t nil "1")))
					    (hif-set-var var (or val 1)))))

				      (define-key hs-org/minor-mode-map (kbd "M-0")
					(lambda () (interactive)
					  (progn
					    (setq hide-ifdef-shadow 't)
					    (call-interactively 'hide-ifdef-undef)
					    )))

				      (define-key hs-org/minor-mode-map (kbd "M-2")
					(lambda () (interactive)
					  (progn
					    (setq hide-ifdef-shadow 't)
					    (call-interactively 'hide-ifdefs)
					    )))

				      (define-key hs-org/minor-mode-map (kbd "M-3")
					(lambda () (interactive)
					  (progn
					    (message "[=] hs-ifdef-env: '%s'" hide-ifdef-env)
					    (message "[=] hs-ifdef-env-linenr: '%s'" hide-ifdef-env-linenr)
					    )))


				      )))



(add-hook 'c++-mode-hook
	  (lambda ()
	    (progn (add-to-list 'write-file-functions 'delete-trailing-whitespace)
		   (if (eq system-type 'cygwin) ;; use helm for !=cygwin
		       (ggtags-mode))
		   (global-set-key (kbd "M-(")  'hs-hide-block)
		   (global-set-key (kbd "M-)")  'hs-show-block)
		   (global-set-key (kbd "M-M")
				   (lambda () (interactive)
				     (progn
				       (message "Prepare: helm-man-woman")
				       (when  (require 'helm-man nil t )
				     	 (call-interactively 'helm-man-woman)))))

		   (modes/orgstruct-commen)

		   ;; (hs-org/minor-mode)
		   (global-set-key (kbd "M-h")
				   (lambda () (interactive)
				     (progn
				       (message "Note: hs-org/minor-mode is not compatible with orgstruct-mode")
				       (orgstruct-mode -1)
				       (call-interactively 'hs-org/minor-mode))))
		   (global-set-key (kbd "M-H")  'orgstruct-mode)
		   (global-set-key (kbd "<f9>")  'c++-mode/toggle-org)
		   ;;(global-set-key (kbd "S-<f9>") 'sync-buffer-overlays)

		   (add-hook 'multi-select-mode-hook 'sync-buffer-overlays)
		   
		   (c-mode-addfuncs)
		   )))

(add-hook 'c-mode-hook
	  (lambda ()
	    (progn

	      (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	      (if (eq system-type 'cygwin) ;; use helm for !=cygwin
		  (ggtags-mode))

	      ;;(hs-org/minor-mode)
	      (global-set-key (kbd "M-h")
			      (lambda () (interactive)
				(progn
				  (message "Note: hs-org/minor-mode is not compatible with orgstruct-mode")
				  (orgstruct-mode -1)
				  (call-interactively 'hs-org/minor-mode))))
	      ;;(global-set-key (kbd "M-h")  'hs-org/minor-mode)
	      (global-set-key (kbd "M-H")  'orgstruct-mode)
	      (global-set-key (kbd "M-M")  'helm-man-women)
	      (global-set-key (kbd "<f9>")  'c-mode/toggle-org)

	      (modes/orgstruct-commen)
	      (c-mode-addfuncs)

	      )))



(provide 'modes/c-mode.el)
