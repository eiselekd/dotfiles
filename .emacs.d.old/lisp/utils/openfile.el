
(defun xah-open-file-at-cursor ()
  
  "Open the file path under cursor.
   If there is text selection, uses the text selection for path.
   If the path starts with “http://”, open the URL in browser.
   Input path can be {relative, full path, URL}.
   Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
   If path does not have a file extention, automatically try with “.el” for elisp files.
   This command is similar to `find-file-at-point' but without prompting for confirmation.
   URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
    
    (interactive)
    (let ((ξpath (if (use-region-p)
		     (buffer-substring-no-properties (region-beginning) (region-end))
		   (let (p0 p1 p2)
		     (setq p0 (point))
		     ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
		     (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
		     (setq p1 (point))
		     (goto-char p0)
		     (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
		     (setq p2 (point))
		     (goto-char p0)
		     (buffer-substring-no-properties p1 p2)))))
      (if (string-match-p "\\`https?://" ξpath)
	  (browse-url ξpath)
	(progn ; not starting “http://”
	  (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
	      (progn
		(let (
		      (ξfpath (match-string 1 ξpath))
		      (ξline-num (string-to-number (match-string 2 ξpath))))
		  (if (file-exists-p ξfpath)
		      (progn
			(find-file ξfpath)
			(goto-char 1)
			(forward-line (1- ξline-num)))
		    (progn
		      (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
			(find-file ξfpath))))))
	    (progn
	      (if (file-exists-p ξpath)
		  (find-file ξpath)
		(if (file-exists-p (concat ξpath ".el"))
		    (find-file (concat ξpath ".el"))
		  (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
		    (find-file ξpath ))))))))))

(provide 'utils/openfile.el)
