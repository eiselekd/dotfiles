;; mode to mimmick notepad++ Analysis Plugin
;;
;; <SearchText bgColor="pink">bt_stack_manager</SearchText>
;; <SearchText bgColor="liteGreen">unexpected event for</SearchText>
;; <SearchText color="yellow" bgColor="red">&gt;&gt;&gt; com.android.bluetooth &lt;&lt;&lt;</SearchText>
;; <SearchText bgColor="red">BluetoothDiagnostics</SearchText>
;; <SearchText color="yellow" bgColor="red">fault addr --------</SearchText>
;; <SearchText color="yellow" bgColor="red">/system/lib64/libbluetooth.so</SearchText>
;; <SearchText bgColor="liteGreen">SUSPEND_TO_RAM</SearchText>
;; <SearchText bgColor="red">BluetoothDeathRecipient::serviceDied</SearchText>
;; <SearchText color="cyan" bgColor="grey">CAR.POWER</SearchText>

;;(with-current-buffer (current-buffer)  (add-text-properties 1 20 `(face (:foreground "#ff0000"))))
;;(font-lock-mode -1)
;;(add-face-text-property (point-min) (point-max) 'italic)
;;(with-current-buffer (current-buffer) (add-text-properties 1 20 '(comment t face highlight)))

(defun utils/occur-multi ()(interactive)
  (call-interactively 'occur)
  )

(defun oa-occur-walk-xml (node)
  (when (listp node)
    (cond
     ((string= "SearchText" (xml-node-name node ))
      (let* ((c (xml-get-attribute node 'color))
	     (bg (xml-get-attribute node 'bgColor)))
	(progn
	  ;;(message " SearchText + %s"  (nth 2 node))
	  (add-to-list 'a (list `( color . ,c)
				`( bgcolor . ,bg)
				`( search . ,(nth 2 node))))
	  )))
     (t
      (mapc 'oa-occur-walk-xml (xml-node-children node))))))


(defun oa-occur-loadxml (fn)
  (message "[+] load %s" fn)
  (progn
    (let* ((a '())
	   (d (xml-parse-file fn)))
      (oa-occur-walk-xml (car d))
      a)))

(defun oa-match-list ( line searchar )
  (let* ((r)
	 (found
	 (catch 'v
	   (dolist (elt searchar r)
	     (let* ((c (alist-get 'color elt))
		    (bg (alist-get 'bgcolor elt))
		    (search (alist-get 'search elt)))
	       (if (string-match-p search line)
		   (progn
		     (message "[+] match: '%s'" line)
		     (throw 'v elt)))
	       ))
	   )))
    found
    ))


(defun oa-occur-engine-line (beg end &optional keep-props)
  (if (and keep-props font-lock-mode)
      (font-lock-ensure beg end))
  (if (and keep-props (not (eq occur-excluded-properties t)))
      (let ((str (buffer-substring beg end)))
	(remove-list-of-text-properties
	 0 (length str) occur-excluded-properties str)
	str)
    (buffer-substring-no-properties beg end)))

(defface color-occur-face
  '((((class color)
      (background dark))
     (:background "SkyBlue" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "Face used for list-matching-lines-face")

(defun oa-test ()
  (save-excursion
    (let*
	((searchar (oa-occur-loadxml "/home/eiselekd/git/dotfiles/.emacs.d/data/bt.xml"))
	 (buffers (list (current-buffer)))
	 )
      (dolist (boo buffers)
	(with-current-buffer (if (overlayp boo) (overlay-buffer boo) boo)
	  (remove-overlays nil nil 'oa-occure t)
	  (forward-line 0)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let*
		((ov)
		 (beg (line-beginning-position))
		 (end (line-end-position))
		 (line (oa-occur-engine-line beg end))
		 (m (oa-match-list line searchar))
		 )

	      (if m
		  (progn
		    (message "+= %s %d %d" line beg end)
		    ;;(font-lock-mode -1)
		    (setq ov (make-overlay beg end))
                    (overlay-put ov 'face `(face (:foreground ,(format "#%02x%02x%02x" 255 16 16) :background ,(format "#%02x%02x%02x" 0 16 16) )))
                    (overlay-put ov 'priority 0)
		    (overlay-put ov 'oa-occure t)

					 )))

	      (forward-line 1))
	    )
	  ))
      ))
;;)



;;(oa-test)

(provide 'utils/occur-util.el)
