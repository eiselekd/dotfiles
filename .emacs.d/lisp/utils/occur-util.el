;; mode to mimmick notepad++ Analysis Plugin
;;
;; <SearchText bgColor="pink">bt_stack_manager</SearchText>
;; <SearchText bgColor="liteGreen">unexpected event for</SearchText>
;; sss<SearchText color="yellow" bgColor="red">&gt;&gt;&gt; com.android.bluetooth &lt;&lt;&lt;</SearchText>
;; <SearchText bgColor="red">BluetoothDiagnostics</SearchText>
;; <SearchText color="yellow" bgColor="red">fault addr --------</SearchText>
;; <SearchText color="yellow" bgColor="red">/system/lib64/libbluetooth.so</SearchText>
;;    <SearchText bgColor="liteGreen">SUSPEND_TO_RAM</SearchText>
;; <SearchText bgColor="red">BluetoothDeathRecipient::serviceDied</SearchText>
;; <SearchText color="cyan" bgColor="grey">CAR.POWER</SearchText>

;;(with-current-buffer (current-buffer)  (add-text-properties 1 20 `(face (:foreground "#ff0000"))))
;;(font-lock-mode -1)
;;(add-face-text-property (point-min) (point-max) 'italic)
;;(with-current-buffer (current-buffer) (add-text-properties 1 20 '(comment t face highlight)))

(defun utils/occur-multi ()(interactive)
  (call-interactively 'occur)
  )

(defun oa-color-scale (n)
  (let ((a (round (* (float 255)  ( / (float n ) (float (nth 0  (color-values  "white"))))))))
    (if (> a 255) 255 a)))

(defun oa-color (n)
  (let ((c (if (color-defined-p n)
	       (color-values  n)
	     (cond
	      ((string= n "red" ) (color-values  "red"))
	      (t (progn
		   (message " cannot find %s " n)
		   (color-values  "red"))
	      )))))
    (setq c (mapcar 'oa-color-scale c))
    (format "#%02x%02x%02x" (nth 0 c) (nth 1 c) (nth 2 c))
    ))

;;(oa-color "red")

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
  (let* ((searchar (oa-occur-loadxml "/home/eiselekd/git/dotfiles/.emacs.d/data/bt.xml"))
	 (buffers (list (current-buffer))))
    (oa-match-buffers buffers searchar )))

(defun oa-match-buffers (buffers searchar &optional occurbuf-name)

  (unless occurbuf-name
    (setq occurbuf-name "*Occur*"))

  (let (occur-buf)

    (setq occur-buf (get-buffer-create occurbuf-name))
    (with-current-buffer occur-buf
      (setq buffer-read-only nil)
      (erase-buffer)
      )

    (save-excursion
      (let* ()
	(dolist (boo buffers)
	  (with-current-buffer boo
	    (remove-overlays nil nil 'oa-occure t)
	    (forward-line 0)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (let*
		  ((ov)
		   (beg (line-beginning-position))
		   (end (line-end-position))
		   (line (oa-occur-engine-line beg end))
		   (marker (make-marker))
		   (m (oa-match-list line searchar)))
		(set-marker marker beg)
		(if m
		    (let* ((c (alist-get 'color m))
			   (bg (alist-get 'bgcolor m))
			   (facelist '()))
		      (if (not (or c bg))
			  (progn
			    (setq bg "SkyBlue")
			    (setq c "Black")))
		      (if c
			  (setq facelist (append facelist `( :foreground  ,(oa-color c)))))
		      (if bg
			  (setq facelist (append facelist `( :background  ,(oa-color bg)))))

		      (message "+= %s facelist: %s" line facelist)
		      ;;(font-lock-mode -1)
		      (setq ov (make-overlay beg end))
                      ;;(overlay-put ov 'face `(face (:foreground ,(format "#%02x%02x%02x" 255 16 16) :background ,(format "#%02x%02x%02x" 0 16 16) )))
		      (overlay-put ov 'face facelist)
                      (overlay-put ov 'priority 0)
		      (overlay-put ov 'oa-occure t)

		      (with-current-buffer occur-buf
			(let* ((oaline
				(propertize line
					    'occur-target marker
					    'follow-link t
					    'help-echo
					    "mouse-2: go to this occurrence"))
			       (oabeg (point)))
			  (insert (concat oaline "\n"))
			  (let* ((oaoverlay (make-overlay oabeg (point))))
			    (overlay-put oaoverlay 'face facelist)
                      	    )))
		      )))
	      (forward-line 1))
	    ))))
    (with-current-buffer occur-buf
      (setq buffer-read-only t)
      (display-buffer occur-buf))

    ))

(defvar oa-occur-mode-map
  (let ((map (make-sparse-keymap)))
    ;; We use this alternative name, so we can use \\[occur-mode-mouse-goto].
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-mode-goto-occurrence)
    (define-key map "e" 'occur-edit-mode)
    (define-key map "\C-m" 'occur-mode-goto-occurrence)
    (define-key map "o" 'occur-mode-goto-occurrence-other-window)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\M-n" 'occur-next)
    (define-key map "\M-p" 'occur-prev)
    (define-key map "r" 'occur-rename-buffer)
    (define-key map "c" 'clone-buffer)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (bindings--define-key map [menu-bar occur] (cons "Occur" occur-menu-map))
    map)
  "Keymap for `oa-occur-mode'.")


(define-derived-mode occur-mode special-mode "Occur"
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (setq-local revert-buffer-function #'occur-revert-function)
  (add-hook 'kill-buffer-hook #'occur--garbage-collect-revert-args nil t)
  (setq next-error-function #'occur-next-error))

;;(if (overlayp boo) (overlay-buffer boo) boo)

;;(oa-test)

(provide 'utils/occur-util.el)
