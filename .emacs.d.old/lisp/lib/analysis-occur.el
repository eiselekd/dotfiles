;; Copyright (C) 1985-1987, 1992, 1994, 1996-1997, 2000-2020 Free
;; Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supplies the string and regular-expression replace functions
;; documented in the Emacs user's manual.

;;; Code:

(require 'text-mode)
(eval-when-compile (require 'cl-lib))



(defvar regexp-history nil
  "History list for some commands that read regular expressions.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defvar occur-collect-regexp-history '("\\1")
  "History of regexp for occur's collect operation")

(defcustom read-regexp-defaults-function nil
  "Function that provides default regexp(s) for `read-regexp'.
This function should take no arguments and return one of: nil, a
regexp, or a list of regexps.  Interactively, `read-regexp' uses
the return value of this function for its DEFAULT argument.

As an example, set this variable to `find-tag-default-as-regexp'
to default to the symbol at point.

To provide different default regexps for different commands,
the function that you set this to can check `this-command'."
  :type '(choice
	  (const :tag "No default regexp reading function" nil)
	  (const :tag "Latest regexp history" regexp-history-last)
	  (function-item :tag "Tag at point"
			 find-tag-default)
	  (function-item :tag "Tag at point as regexp"
			 find-tag-default-as-regexp)
	  (function-item :tag "Tag at point as symbol regexp"
			 find-tag-default-as-symbol-regexp)
	  (function :tag "Your choice of function"))
  :group 'matching
  :version "24.4")

(defun read-regexp-suggestions ()
  "Return a list of standard suggestions for `read-regexp'.
By default, the list includes the tag at point, the last isearch regexp,
the last isearch string, and the last replacement regexp.  `read-regexp'
appends the list returned by this function to the end of values available
via \\<minibuffer-local-map>\\[next-history-element]."
  (list
   (find-tag-default-as-regexp)
   (find-tag-default-as-symbol-regexp)
   (car regexp-search-ring)
   (regexp-quote (or (car search-ring) ""))
   (car (symbol-value query-replace-from-history-variable))))

(defun read-regexp (prompt &optional defaults history)
  "Read and return a regular expression as a string.
Prompt with the string PROMPT.  If PROMPT ends in \":\" (followed by
optional whitespace), use it as-is.  Otherwise, add \": \" to the end,
possibly preceded by the default result (see below).

The optional argument DEFAULTS can be either: nil, a string, a list
of strings, or a symbol.  We use DEFAULTS to construct the default
return value in case of empty input.

If DEFAULTS is a string, we use it as-is.

If DEFAULTS is a list of strings, the first element is the
default return value, but all the elements are accessible
using the history command \\<minibuffer-local-map>\\[next-history-element].

If DEFAULTS is a non-nil symbol, then if `read-regexp-defaults-function'
is non-nil, we use that in place of DEFAULTS in the following:
  If DEFAULTS is the symbol `regexp-history-last', we use the first
  element of HISTORY (if specified) or `regexp-history'.
  If DEFAULTS is a function, we call it with no arguments and use
  what it returns, which should be either nil, a string, or a list of strings.

We append the standard values from `read-regexp-suggestions' to DEFAULTS
before using it.

If the first element of DEFAULTS is non-nil (and if PROMPT does not end
in \":\", followed by optional whitespace), we add it to the prompt.

The optional argument HISTORY is a symbol to use for the history list.
If nil, uses `regexp-history'."
  (let* ((defaults
	   (if (and defaults (symbolp defaults))
	       (cond
		((eq (or read-regexp-defaults-function defaults)
		     'regexp-history-last)
		 (car (symbol-value (or history 'regexp-history))))
		((functionp (or read-regexp-defaults-function defaults))
		 (funcall (or read-regexp-defaults-function defaults))))
	     defaults))
	 (default     (if (consp defaults) (car defaults) defaults))
	 (suggestions (if (listp defaults) defaults (list defaults)))
	 (suggestions (append suggestions (read-regexp-suggestions)))
	 (suggestions (delete-dups (delq nil (delete "" suggestions))))
	 ;; Do not automatically add default to the history for empty input.
	 (history-add-new-input nil)
	 (input (read-from-minibuffer
		 (cond ((string-match-p ":[ \t]*\\'" prompt)
			prompt)
		       ((and default (> (length default) 0))
			 (format "%s (default %s): " prompt
				 (query-replace-descr default)))
		       (t
			(format "%s: " prompt)))
		 nil nil nil (or history 'regexp-history) suggestions t)))
    (if (equal input "")
	;; Return the default value when the user enters empty input.
	(prog1 (or default input)
	  (when default
	    (add-to-history (or history 'regexp-history) default)))
      ;; Otherwise, add non-empty input to the history and return input.
      (prog1 input
	(add-to-history (or history 'regexp-history) input)))))




(defvar occur-menu-map
  (let ((map (make-sparse-keymap)))
    (bindings--define-key map [next-error-follow-minor-mode]
      '(menu-item "Auto Occurrence Display"
		  next-error-follow-minor-mode
		  :help "Display another occurrence when moving the cursor"
		  :button (:toggle . (and (boundp 'next-error-follow-minor-mode)
					  next-error-follow-minor-mode))))
    (bindings--define-key map [separator-1] menu-bar-separator)
    (bindings--define-key map [kill-this-buffer]
      '(menu-item "Kill Occur Buffer" kill-this-buffer
		  :help "Kill the current *Occur* buffer"))
    (bindings--define-key map [quit-window]
      '(menu-item "Quit Occur Window" quit-window
		  :help "Quit the current *Occur* buffer.  Bury it, and maybe delete the selected frame"))
    (bindings--define-key map [revert-buffer]
      '(menu-item "Revert Occur Buffer" revert-buffer
		  :help "Replace the text in the *Occur* buffer with the results of rerunning occur"))
    (bindings--define-key map [clone-buffer]
      '(menu-item "Clone Occur Buffer" clone-buffer
		  :help "Create and return a twin copy of the current *Occur* buffer"))
    (bindings--define-key map [occur-rename-buffer]
      '(menu-item "Rename Occur Buffer" occur-rename-buffer
		  :help "Rename the current *Occur* buffer to *Occur: original-buffer-name*."))
    (bindings--define-key map [occur-edit-buffer]
      '(menu-item "Edit Occur Buffer" occur-edit-mode
		  :help "Edit the *Occur* buffer and apply changes to the original buffers."))
    (bindings--define-key map [separator-2] menu-bar-separator)
    (bindings--define-key map [occur-mode-goto-occurrence-other-window]
      '(menu-item "Go To Occurrence Other Window" occur-mode-goto-occurrence-other-window
		  :help "Go to the occurrence the current line describes, in another window"))
    (bindings--define-key map [occur-mode-goto-occurrence]
      '(menu-item "Go To Occurrence" occur-mode-goto-occurrence
		  :help "Go to the occurrence the current line describes"))
    (bindings--define-key map [occur-mode-display-occurrence]
      '(menu-item "Display Occurrence" occur-mode-display-occurrence
		  :help "Display in another window the occurrence the current line describes"))
    (bindings--define-key map [occur-next]
      '(menu-item "Move to Next Match" occur-next
		  :help "Move to the Nth (default 1) next match in an Occur mode buffer"))
    (bindings--define-key map [occur-prev]
      '(menu-item "Move to Previous Match" occur-prev
		  :help "Move to the Nth (default 1) previous match in an Occur mode buffer"))
    map)
  "Menu keymap for `occur-mode'.")

(defvar occur-mode-map
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
  "Keymap for `occur-mode'.")

(defvar-local occur-revert-arguments nil
  "Arguments to pass to `occur-1' to revert an Occur mode buffer.
See `occur-revert-function'.")
(put 'occur-revert-arguments 'permanent-local t)

(defcustom occur-mode-hook '(turn-on-font-lock)
  "Hook run when entering Occur mode."
  :type 'hook
  :group 'matching)

(defcustom occur-hook nil
  "Hook run by Occur when there are any matches."
  :type 'hook
  :group 'matching)

(defcustom occur-mode-find-occurrence-hook nil
  "Hook run by Occur after locating an occurrence.
This will be called with the cursor position at the occurrence.  An application
for this is to reveal context in an outline-mode when the occurrence is hidden."
  :type 'hook
  :group 'matching)

(defun occur--garbage-collect-revert-args ()
  (dolist (boo (nth 2 occur-revert-arguments))
    (when (overlayp boo) (delete-overlay boo)))
  (kill-local-variable 'occur-revert-arguments))

(put 'occur-mode 'mode-class 'special)
(define-derived-mode occur-mode special-mode "Occur"
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (setq-local revert-buffer-function #'occur-revert-function)
  (add-hook 'kill-buffer-hook #'occur--garbage-collect-revert-args nil t)
  (setq next-error-function #'occur-next-error))


;;; Occur Edit mode

(defvar occur-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-cease-edit)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (bindings--define-key map [menu-bar occur] (cons "Occur" occur-menu-map))
    map)
  "Keymap for `occur-edit-mode'.")

(define-derived-mode occur-edit-mode occur-mode "Occur-Edit"
  "Major mode for editing *Occur* buffers.
In this mode, changes to the *Occur* buffer are also applied to
the originating buffer.

To return to ordinary Occur mode, use \\[occur-cease-edit]."
  (setq buffer-read-only nil)
  (add-hook 'after-change-functions #'occur-after-change-function nil t)
  (message (substitute-command-keys
	    "Editing: Type \\[occur-cease-edit] to return to Occur mode.")))

(defun occur-cease-edit ()
  "Switch from Occur Edit mode to Occur mode."
  (interactive)
  (when (derived-mode-p 'occur-edit-mode)
    (occur-mode)
    (message "Switching to Occur mode.")))

(defun occur-after-change-function (beg end length)
  (save-excursion
    (goto-char beg)
    (let* ((line-beg (line-beginning-position))
	   (m (get-text-property line-beg 'occur-target))
	   (buf (marker-buffer m))
	   col)
      (when (and (get-text-property line-beg 'occur-prefix)
		 (not (get-text-property end 'occur-prefix)))
	(when (= length 0)
	  ;; Apply occur-target property to inserted (e.g. yanked) text.
	  (put-text-property beg end 'occur-target m)
	  ;; Did we insert a newline?  Occur Edit mode can't create new
	  ;; Occur entries; just discard everything after the newline.
	  (save-excursion
	    (and (search-forward "\n" end t)
		 (delete-region (1- (point)) end))))
	(let* ((line (- (line-number-at-pos)
			(line-number-at-pos (window-start))))
	       (readonly (with-current-buffer buf buffer-read-only))
	       (win (or (get-buffer-window buf)
			(display-buffer buf
					'(nil (inhibit-same-window . t)
					      (inhibit-switch-frame . t)))))
	       (line-end (line-end-position))
	       (text (save-excursion
		       (goto-char (next-single-property-change
				   line-beg 'occur-prefix nil
				   line-end))
		       (setq col (- (point) line-beg))
		       (buffer-substring-no-properties (point) line-end))))
	  (with-selected-window win
	    (goto-char m)
	    (recenter line)
	    (if readonly
		(message "Buffer `%s' is read only." buf)
	      (delete-region (line-beginning-position) (line-end-position))
	      (insert text))
	    (move-to-column col)))))))


(defun occur-revert-function (_ignore1 _ignore2)
  "Handle `revert-buffer' for Occur mode buffers."
  (apply #'occur-1 (append occur-revert-arguments (list (buffer-name)))))

(defun occur-mode-find-occurrence ()
  (let ((pos (get-text-property (point) 'occur-target)))
    (unless pos
      (error "No occurrence on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this occurrence was killed"))
    pos))

(defalias 'occur-mode-mouse-goto 'occur-mode-goto-occurrence)
(defun occur-mode-goto-occurrence (&optional event)
  "Go to the occurrence specified by EVENT, a mouse click.
If not invoked by a mouse click, go to occurrence on the current line."
  (interactive (list last-nonmenu-event))
  (let ((buffer (when event (current-buffer)))
        (pos
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (occur-mode-find-occurrence)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (occur-mode-find-occurrence))))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    (when buffer (next-error-found buffer (current-buffer)))
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((buffer (current-buffer))
        (pos (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)
    (next-error-found buffer (current-buffer))
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((buffer (current-buffer))
        (pos (occur-mode-find-occurrence))
	window)
    (setq window (display-buffer (marker-buffer pos) t))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos)
      (next-error-found buffer (current-buffer))
      (run-hooks 'occur-mode-find-occurrence-hook))))

(defun occur-find-match (n search message)
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
      (setq r (funcall search (point) 'occur-match))
      (and r
           (get-text-property r 'occur-match)
           (setq r (funcall search r 'occur-match)))
      (if r
          (goto-char r)
        (user-error message))
      (setq n (1- n)))))

(defun occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'next-single-property-change "No more matches"))

(defun occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'previous-single-property-change "No earlier matches"))

(defun occur-next-error (&optional argp reset)
  "Move to the ARGPth (default 1) next match in an Occur mode buffer.
RESET non-nil means rewind to the first match.
This is a compatibility function for \\[next-error] invocations."
  (interactive "p")
  (goto-char (cond (reset (point-min))
		   ((< argp 0) (line-beginning-position))
		   ((> argp 0) (line-end-position))
		   ((point))))
  (occur-find-match
   (abs argp)
   (if (> 0 argp)
       #'previous-single-property-change
     #'next-single-property-change)
   "No more matches")
  ;; In case the *Occur* buffer is visible in a nonselected window.
  (let ((win (get-buffer-window (current-buffer) t)))
    (if win (set-window-point win (point))))
  (occur-mode-goto-occurrence))

(defface match
  '((((class color) (min-colors 88) (background light))
     :background "yellow1")
    (((class color) (min-colors 88) (background dark))
     :background "RoyalBlue3")
    (((class color) (min-colors 8) (background light))
     :background "yellow" :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face used to highlight matches permanently."
  :group 'matching
  :group 'basic-faces
  :version "22.1")

(defcustom list-matching-lines-default-context-lines 0
  "Default number of context lines included around `list-matching-lines' matches.
A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  :type 'integer
  :group 'matching)

(defalias 'list-matching-lines 'occur)

(defcustom list-matching-lines-face 'match
  "Face used by \\[list-matching-lines] to show the text that matches.
If the value is nil, don't highlight the matching portions specially."
  :type 'face
  :group 'matching)

(defcustom list-matching-lines-buffer-name-face 'underline
  "Face used by \\[list-matching-lines] to show the names of buffers.
If the value is nil, don't highlight the buffer names specially."
  :type 'face
  :group 'matching)

(defcustom list-matching-lines-current-line-face 'lazy-highlight
  "Face used by \\[list-matching-lines] to highlight the current line."
  :type 'face
  :group 'matching
  :version "26.1")

(defcustom list-matching-lines-jump-to-current-line nil
  "If non-nil, \\[list-matching-lines] shows the current line highlighted.
Set the point right after such line when there are matches after it."
:type 'boolean
:group 'matching
:version "26.1")

(defcustom list-matching-lines-prefix-face 'shadow
  "Face used by \\[list-matching-lines] to show the prefix column.
If the face doesn't differ from the default face,
don't highlight the prefix with line numbers specially."
  :type 'face
  :group 'matching
  :version "24.4")

(defcustom occur-excluded-properties
  '(read-only invisible intangible field mouse-face help-echo local-map keymap
    yank-handler follow-link)
  "Text properties to discard when copying lines to the *Occur* buffer.
The value should be a list of text properties to discard or t,
which means to discard all text properties."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'matching
  :version "22.1")

(defun occur-read-primary-args ()
  (let* ((perform-collect (consp current-prefix-arg))
         (regexp (read-regexp (if perform-collect
                                  "Collect strings matching regexp"
                                "List lines matching regexp")
                              'regexp-history-last)))
    (list regexp
	  (if perform-collect
	      ;; Perform collect operation
	      (if (zerop (regexp-opt-depth regexp))
		  ;; No subexpression so collect the entire match.
		  "\\&"
		;; Get the regexp for collection pattern.
		(let ((default (car occur-collect-regexp-history)))
		  (read-regexp
		   (format "Regexp to collect (default %s): " default)
		   default 'occur-collect-regexp-history)))
	    ;; Otherwise normal occur takes numerical prefix argument.
	    (when current-prefix-arg
	      (prefix-numeric-value current-prefix-arg))))))

(defun occur-rename-buffer (&optional unique-p interactive-p)
  "Rename the current *Occur* buffer to *Occur: original-buffer-name*.
Here `original-buffer-name' is the buffer name where Occur was originally run.
If UNIQUE-P is non-nil (interactively, the prefix argument), or called
non-interactively with INTERACTIVE-P nil, the renaming will not clobber
the existing buffer(s) of that name, but will use `generate-new-buffer-name'
instead.
You can add this to `occur-hook' if you always want a separate
*Occur* buffer for each buffer where you invoke `occur'."
  (interactive "P\np")
  (with-current-buffer
      (if (eq major-mode 'occur-mode) (current-buffer) (get-buffer "*Occur*"))
    (rename-buffer (concat "*Occur: "
                           (mapconcat #'buffer-name
                                      (car (cddr occur-revert-arguments)) "/")
                           "*")
                   (or unique-p (not interactive-p)))))

;; Region limits when `occur' applies on a region.
(defvar occur--final-pos nil)

(defun occur (regexp &optional nlines region)
  "Show all lines in the current buffer containing a match for REGEXP.
If a match spreads across multiple lines, all those lines are shown.

Each match is extended to include complete lines.  Only non-overlapping
matches are considered.  (Note that extending matches to complete
lines could cause some of the matches to overlap; if so, they will not
be shown as separate matches.)

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

Optional arg REGION, if non-nil, mean restrict search to the
specified region.  Otherwise search the entire buffer.
REGION must be a list of (START . END) positions as returned by
`region-bounds'.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.
If `list-matching-lines-jump-to-current-line' is non-nil, then show
the current line highlighted with `list-matching-lines-current-line-face'
and set point at the first match after such line.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

When NLINES is a string or when the function is called
interactively with prefix argument without a number (`C-u' alone
as prefix) the matching strings are collected into the `*Occur*'
buffer by using NLINES as a replacement regexp.  NLINES may
contain \\& and \\N which convention follows `replace-match'.
For example, providing \"defun\\s +\\(\\S +\\)\" for REGEXP and
\"\\1\" for NLINES collects all the function names in a lisp
program.  When there is no parenthesized subexpressions in REGEXP
the entire match is collected.  In any case the searched buffer
is not modified."
  (interactive
   (nconc (occur-read-primary-args)
          (and (use-region-p) (list (region-bounds)))))
  (let* ((start (and (caar region) (max (caar region) (point-min))))
         (end (and (cdar region) (min (cdar region) (point-max))))
         (in-region (or start end))
         (bufs (if (not in-region) (list (current-buffer))
                 (let ((ol (make-overlay
                            (or start (point-min))
                            (or end (point-max)))))
                   (overlay-put ol 'occur--orig-point (point))
                   (list ol)))))
    (occur-1 regexp nlines bufs)))

(defvar ido-ignore-item-temp-list)

(defun multi-occur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
Optional argument NLINES specifies the number of context lines to show
with each match, see `list-matching-lines-default-context-lines'.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'.  When you invoke this command interactively, you must specify
the buffer names that you want, one by one.
See also `multi-occur-in-matching-buffers'."
  (interactive
   (cons
    (let* ((bufs (list (read-buffer "First buffer to search: "
				    (current-buffer) t)))
	   (buf nil)
	   (ido-ignore-item-temp-list bufs))
      (while (not (string-equal
		   (setq buf (read-buffer
			      (if (eq read-buffer-function #'ido-read-buffer)
				  "Next buffer to search (C-j to end): "
				"Next buffer to search (RET to end): ")
			      nil t))
		   ""))
	(cl-pushnew buf bufs)
	(setq ido-ignore-item-temp-list bufs))
      (nreverse (mapcar #'get-buffer bufs)))
    (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

(defun multi-occur-in-matching-buffers (bufregexp regexp &optional allbufs)
  "Show all lines matching REGEXP in buffers specified by BUFREGEXP.
Normally BUFREGEXP matches against each buffer's visited file name,
but ALLBUFS non-nil (interactively, if you specify a prefix argument),
it matches against the buffer name and includes also buffers that
don't visit files.
See also `multi-occur'."
  (interactive
   (cons
    (let* ((default (car regexp-history))
	   (input
	    (read-regexp
	     (if current-prefix-arg
		 "List lines in buffers whose names match regexp: "
	       "List lines in buffers whose filenames match regexp: "))))
      (if (equal input "")
	  default
	input))
    (occur-read-primary-args)))
  (when bufregexp
    (occur-1 regexp nil
	     (delq nil
		   (mapcar (lambda (buf)
			     (when (if allbufs
				       (string-match bufregexp
						     (buffer-name buf))
				     (and (buffer-file-name buf)
					  (string-match bufregexp
							(buffer-file-name buf))))
			       buf))
			   (buffer-list))))))

(defun occur-regexp-descr (regexp)
  (format " for %s\"%s\""
          (or (get-text-property 0 'isearch-regexp-function-descr regexp)
              "")
          (if (get-text-property 0 'isearch-string regexp)
              (propertize
               (query-replace-descr
                (get-text-property 0 'isearch-string regexp))
               'help-echo regexp)
            (query-replace-descr regexp))))

(defun occur-1 (regexp nlines bufs &optional buf-name)
  ;; BUFS is a list of buffer-or-overlay!
  (unless (and regexp (not (equal regexp "")))
    (error "Occur doesn't work with the empty regexp"))
  (unless buf-name
    (setq buf-name "*Occur*"))
  (let (occur-buf
	(active-bufs
         (delq nil (mapcar (lambda (boo)
			       (when (or (buffer-live-p boo)
                                         (and (overlayp boo)
                                              (overlay-buffer boo)))
                                 boo))
			   bufs))))
    ;; Handle the case where one of the buffers we're searching is the
    ;; output buffer.  Just rename it.
    (when (member buf-name
                  ;; FIXME: Use cl-exists.
                  (mapcar
                   (lambda (boo)
                     (buffer-name (if (overlayp boo) (overlay-buffer boo) boo)))
                   active-bufs))
      (with-current-buffer (get-buffer buf-name)
	(rename-uniquely)))

    ;; Now find or create the output buffer.
    ;; If we just renamed that buffer, we will make a new one here.
    (setq occur-buf (get-buffer-create buf-name))

    (with-current-buffer occur-buf
      (if (stringp nlines)
	  (fundamental-mode) ;; This is for collect operation.
	(occur-mode))
      (let ((inhibit-read-only t)
	    ;; Don't generate undo entries for creation of the initial contents.
	    (buffer-undo-list t)
	    (occur--final-pos nil))
	(erase-buffer)
	(let ((count
	       (if (stringp nlines)
                   ;; Treat nlines as a regexp to collect.
		   (let ((count 0))
		     (dolist (boo active-bufs)
		       (with-current-buffer
                           (if (overlayp boo) (overlay-buffer boo) boo)
			 (save-excursion
			   (goto-char
                            (if (overlayp boo) (overlay-start boo) (point-min)))
                           (let ((end (if (overlayp boo) (overlay-end boo))))
			     (while (re-search-forward regexp end t)
                               ;; Insert the replacement regexp.
			       (let ((str (match-substitute-replacement
                                           nlines)))
			         (if str
				     (with-current-buffer occur-buf
				       (insert str)
				       (setq count (1+ count))
				       (or (zerop (current-column))
					   (insert "\n"))))))))))
                     count)
		 ;; Perform normal occur.
		 (occur-engine
		  regexp active-bufs occur-buf
		  (or nlines list-matching-lines-default-context-lines)
		  (if (and case-fold-search search-upper-case)
		      (isearch-no-upper-case-p regexp t)
		    case-fold-search)
		  list-matching-lines-buffer-name-face
		  (if (face-differs-from-default-p list-matching-lines-prefix-face)
		      list-matching-lines-prefix-face)
		  list-matching-lines-face
		  (not (eq occur-excluded-properties t))))))
	  (let* ((bufcount (length active-bufs))
		 (diff (- (length bufs) bufcount)))
	    (message "Searched %d %s%s; %s %s%s"
		     bufcount
		     (ngettext "buffer" "buffers" bufcount)
		     (if (zerop diff) "" (format " (%d killed)" diff))
		     (if (zerop count) "no" (format "%d" count))
		     (ngettext "match" "matches" count)
		     ;; Don't display regexp if with remaining text
		     ;; it is longer than window-width.
		     (if (> (+ (length (or (get-text-property 0 'isearch-string regexp)
					   regexp))
			       42)
			    (window-width))
			 "" (occur-regexp-descr regexp))))
          (occur--garbage-collect-revert-args)
	  (setq occur-revert-arguments (list regexp nlines bufs))
          (if (= count 0)
              (kill-buffer occur-buf)
            (display-buffer occur-buf)
            (when occur--final-pos
              (set-window-point
               (get-buffer-window occur-buf 'all-frames)
               occur--final-pos))
            (setq next-error-last-buffer occur-buf)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (run-hooks 'occur-hook)))))))

(defun occur-engine (regexp buffers out-buf nlines case-fold
			    title-face prefix-face match-face keep-props)
  ;; BUFFERS is a list of buffer-or-overlay!
  (with-current-buffer out-buf
    (let ((global-lines 0)    ;; total count of matching lines
	  (global-matches 0)  ;; total count of matches
	  (coding nil)
	  (case-fold-search case-fold)
	  (multi-occur-p (cdr buffers)))
      ;; Map over all the buffers
      (dolist (boo buffers)
	(when (if (overlayp boo) (overlay-buffer boo) (buffer-live-p boo))
	  (with-current-buffer (if (overlayp boo) (overlay-buffer boo) boo)
            (let ((inhibit-field-text-motion t)
                  (lines 0)               ; count of matching lines
	          (matches 0)             ; count of matches
		  (headerpt (with-current-buffer out-buf (point)))
		  (orig-line (if (not (overlayp boo))
				 (line-number-at-pos)
			       (line-number-at-pos
				(overlay-get boo 'occur--orig-point)))))
	      (save-excursion
                ;; begin searching in the buffer
		(goto-char (if (overlayp boo) (overlay-start boo) (point-min)))
                (forward-line 0)
	        (let* ((limit (if (overlayp boo) (overlay-end boo) (point-max)))
                       (start-line (line-number-at-pos))
		       (curr-line start-line) ; line count
		       (orig-line-shown-p)
		       (prev-line nil)        ; line number of prev match endpt
		       (prev-after-lines nil) ; context lines of prev match
		       (matchbeg 0)
		       (origpt nil)
		       (begpt nil)
		       (endpt nil)
		       (marker nil)
		       (curstring "")
		       (ret nil)
	               ;; The following binding is for when case-fold-search
	               ;; has a local binding in the original buffer, in which
	               ;; case we cannot bind it globally and let that have
	               ;; effect in every buffer we search.
                       (case-fold-search case-fold))
	          (or coding
		      ;; Set CODING only if the current buffer locally
		      ;; binds buffer-file-coding-system.
		      (not (local-variable-p 'buffer-file-coding-system))
		      (setq coding buffer-file-coding-system))
		  (while (< (point) limit)
		    (setq origpt (point))
		    (when (setq endpt (re-search-forward regexp limit t))
		      (setq lines (1+ lines)) ;; increment matching lines count
		      (setq matchbeg (match-beginning 0))
		      ;; Get beginning of first match line and end of the last.
		      (save-excursion
		        (goto-char matchbeg)
		        (setq begpt (line-beginning-position))
		        (goto-char endpt)
		        (setq endpt (line-end-position)))
		      ;; Sum line numbers up to the first match line.
		      (setq curr-line (+ curr-line (count-lines origpt begpt)))
		      (setq marker (make-marker))
		      (set-marker marker matchbeg)
		      (setq curstring (occur-engine-line begpt endpt keep-props))
		      ;; Highlight the matches
		      (let ((len (length curstring))
			    (start 0))
		        ;; Count empty lines that don't use next loop (Bug#22062).
		        (when (zerop len)
			  (setq matches (1+ matches)))
			(when (and list-matching-lines-jump-to-current-line
				   (not multi-occur-p))
			  (or orig-line (setq orig-line 1))
			  (or nlines (setq nlines (line-number-at-pos (point-max))))
			  (when (= curr-line orig-line)
			    (add-face-text-property
			     0 len list-matching-lines-current-line-face nil curstring)
			    (add-text-properties 0 len '(current-line t) curstring))
			  (when (and (>= orig-line (- curr-line nlines))
				     (<= orig-line (+ curr-line nlines)))
			    ;; Shown either here or will be shown by occur-context-lines
			    (setq orig-line-shown-p t)))
		        (while (and (< start len)
				    (string-match regexp curstring start))
			  (setq matches (1+ matches))
			  (add-text-properties
			   (match-beginning 0) (match-end 0)
			   '(occur-match t) curstring)
			  (when match-face
			    ;; Add `match-face' to faces copied from the buffer.
			    (add-face-text-property
			     (match-beginning 0) (match-end 0)
			     match-face nil curstring))
			  ;; Avoid infloop (Bug#7593).
			  (let ((end (match-end 0)))
			    (setq start (if (= start end) (1+ start) end)))))
		      ;; Generate the string to insert for this match
		      (let* ((match-prefix
			      ;; Using 7 digits aligns tabs properly.
			      (apply #'propertize (format "%7d:" curr-line)
				     (append
				      (when prefix-face
				        `(font-lock-face ,prefix-face))
				      `(occur-prefix t
				                     ;; Allow insertion of text
				                     ;; at the end of the prefix
				                     ;; (for Occur Edit mode).
				                     front-sticky t
						     rear-nonsticky t
						     occur-target ,marker
						     follow-link t
				                     help-echo "mouse-2: go to this occurrence"))))
			     (match-str
			      ;; We don't put `mouse-face' on the newline,
			      ;; because that loses.  And don't put it
			      ;; on context lines to reduce flicker.
			      (propertize curstring
					  'occur-target marker
					  'follow-link t
					  'help-echo
					  "mouse-2: go to this occurrence"))
			     (out-line
			      ;; Add non-numeric prefix to all non-first lines
			      ;; of multi-line matches.
                              (concat
			       (replace-regexp-in-string
			        "\n"
			        (if prefix-face
				    (propertize
				     "\n       :" 'font-lock-face prefix-face)
				  "\n       :")
                                ;; Add mouse face in one section to
                                ;; ensure the prefix and the string
                                ;; get a contiguous highlight.
			        (propertize (concat match-prefix match-str)
                                            'mouse-face 'highlight))
			       ;; Add marker at eol, but no mouse props.
			       (propertize "\n" 'occur-target marker)))
			     (data
			      (if (= nlines 0)
				  ;; The simple display style
				  out-line
			        ;; The complex multi-line display style.
			        (setq ret (occur-context-lines
					   out-line nlines keep-props begpt
					   endpt curr-line prev-line
					   prev-after-lines prefix-face
					   orig-line multi-occur-p))
			        ;; Set first elem of the returned list to `data',
			        ;; and the second elem to `prev-after-lines'.
			        (setq prev-after-lines (nth 1 ret))
			        (nth 0 ret)))
			     (orig-line-str
			      (when (and list-matching-lines-jump-to-current-line
					 (null orig-line-shown-p)
					 (> curr-line orig-line))
				(setq orig-line-shown-p t)
				(save-excursion
				  (goto-char (point-min))
				  (forward-line (1- orig-line))
				  (occur-engine-line (line-beginning-position)
						     (line-end-position) keep-props)))))
		        ;; Actually insert the match display data
		        (with-current-buffer out-buf
			  (when orig-line-str
			    (add-face-text-property
			     0 (length orig-line-str)
			     list-matching-lines-current-line-face nil orig-line-str)
			    (add-text-properties 0 (length orig-line-str)
						 '(current-line t) orig-line-str)
			    (insert (car (occur-engine-add-prefix
					  (list orig-line-str) prefix-face))))
			  (insert data)))
		      (goto-char endpt))
		    (if endpt
		        (progn
			  ;; Sum line numbers between first and last match lines.
			  (setq curr-line (+ curr-line (count-lines begpt endpt)
					     ;; Add 1 for empty last match line
					     ;; since count-lines returns one
					     ;; line less.
					     (if (and (bolp) (eolp)) 1 0)))
			  ;; On to the next match...
			  (forward-line 1))
		      (goto-char (point-max)))
		    (setq prev-line (1- curr-line)))
		  ;; Flush remaining context after-lines.
		  (when prev-after-lines
		    (with-current-buffer out-buf
		      (insert (apply #'concat (occur-engine-add-prefix
					       prev-after-lines prefix-face)))))
		  (when (and list-matching-lines-jump-to-current-line
			     (null orig-line-shown-p))
		    (setq orig-line-shown-p t)
		    (let ((orig-line-str
			   (save-excursion
			     (goto-char (point-min))
			     (forward-line (1- orig-line))
			     (occur-engine-line (line-beginning-position)
						(line-end-position) keep-props))))
		      (add-face-text-property
		       0 (length orig-line-str)
		       list-matching-lines-current-line-face nil orig-line-str)
		      (add-text-properties 0 (length orig-line-str)
					   '(current-line t) orig-line-str)
		      (with-current-buffer out-buf
			(insert (car (occur-engine-add-prefix
			              (list orig-line-str) prefix-face))))))))
	      (when (not (zerop lines)) ;; is the count zero?
	        (setq global-lines (+ global-lines lines)
		      global-matches (+ global-matches matches))
	        (with-current-buffer out-buf
		  (goto-char headerpt)
		  (let ((beg (point))
		        end)
		    (insert (propertize
			     (format "%d %s%s%s in buffer: %s%s\n"
				     matches
				     (ngettext "match" "matches" matches)
				     ;; Don't display the same number of lines
				     ;; and matches in case of 1 match per line.
				     (if (= lines matches)
				         "" (format " in %d %s"
						    lines
						    (ngettext "line" "lines" lines)))
				     ;; Don't display regexp for multi-buffer.
				     (if (> (length buffers) 1)
				         "" (occur-regexp-descr regexp))
				     (buffer-name (if (overlayp boo) (overlay-buffer boo) boo))
				     (if (overlayp boo)
					 (format " within region: %d-%d"
						 (overlay-start boo)
						 (overlay-end boo))
				       ""))
			     'read-only t))
		    (setq end (point))
		    (when title-face
		      (add-face-text-property beg end title-face))
		    (goto-char (if (and list-matching-lines-jump-to-current-line
					(not multi-occur-p))
				   (setq occur--final-pos
					 (and (goto-char (point-max))
					      (or (previous-single-property-change (point) 'current-line)
						  (point-max))))
				 (point-min))))))))))
      ;; Display total match count and regexp for multi-buffer.
      (when (and (not (zerop global-lines)) (> (length buffers) 1))
	(goto-char (point-min))
	(let ((beg (point))
	      end)
	  (insert (format "%d %s%s total%s:\n"
			  global-matches
			  (ngettext "match" "matches" global-matches)
			  ;; Don't display the same number of lines
			  ;; and matches in case of 1 match per line.
			  (if (= global-lines global-matches)
			      "" (format " in %d %s"
					 global-lines
					 (ngettext "line" "lines" global-lines)))
			  (occur-regexp-descr regexp)))
	  (setq end (point))
	  (when title-face
	    (add-face-text-property beg end title-face)))
	(goto-char (point-min)))
      (if coding
	  ;; CODING is buffer-file-coding-system of the first buffer
	  ;; that locally binds it.  Let's use it also for the output
	  ;; buffer.
	  (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      global-matches)))

(defun occur-engine-line (beg end &optional keep-props)
  (if (and keep-props font-lock-mode)
      (font-lock-ensure beg end))
  (if (and keep-props (not (eq occur-excluded-properties t)))
      (let ((str (buffer-substring beg end)))
	(remove-list-of-text-properties
	 0 (length str) occur-excluded-properties str)
	str)
    (buffer-substring-no-properties beg end)))

(defun occur-engine-add-prefix (lines &optional prefix-face)
  (mapcar
   #'(lambda (line)
       (concat (if prefix-face
		   (propertize "       :" 'font-lock-face prefix-face)
		 "       :")
	       line "\n"))
   lines))

(defun occur-accumulate-lines (count &optional keep-props pt)
  (save-excursion
    (when pt
      (goto-char pt))
    (let ((forwardp (> count 0))
	  result beg end moved)
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(and (bobp) (not moved)))))
	(setq count (+ count (if forwardp -1 1)))
	(setq beg (line-beginning-position)
	      end (line-end-position))
	(push (occur-engine-line beg end keep-props) result)
	(setq moved (= 0 (forward-line (if forwardp 1 -1)))))
      (nreverse result))))

;; Generate context display for occur.
;; OUT-LINE is the line where the match is.
;; NLINES and KEEP-PROPS are args to occur-engine.
;; CURR-LINE is line count of the current match,
;; PREV-LINE is line count of the previous match,
;; PREV-AFTER-LINES is a list of after-context lines of the previous match.
;; Generate a list of lines, add prefixes to all but OUT-LINE,
;; then concatenate them all together.
(defun occur-context-lines (out-line nlines keep-props begpt endpt
				     curr-line prev-line prev-after-lines
				     &optional prefix-face
				     orig-line multi-occur-p)
  ;; Find after- and before-context lines of the current match.
  (let ((before-lines
	 (nreverse (cdr (occur-accumulate-lines
			 (- (1+ (abs nlines))) keep-props begpt))))
	(after-lines
	 (cdr (occur-accumulate-lines
	       (1+ nlines) keep-props endpt)))
	separator)

    (when (and list-matching-lines-jump-to-current-line
	       (not multi-occur-p))
      (when (and (>= orig-line (- curr-line nlines))
		 (< orig-line curr-line))
	(let ((curstring (nth (- (length before-lines) (- curr-line orig-line)) before-lines)))
	  (add-face-text-property
	   0 (length curstring)
	   list-matching-lines-current-line-face nil curstring)
	  (add-text-properties 0 (length curstring)
			       '(current-line t) curstring)))
      (when (and (<= orig-line (+ curr-line nlines))
		 (> orig-line curr-line))
	(let ((curstring (nth (- orig-line curr-line 1) after-lines)))
	  (add-face-text-property
	   0 (length curstring)
	   list-matching-lines-current-line-face nil curstring)
	  (add-text-properties 0 (length curstring)
			       '(current-line t) curstring))))

    ;; Combine after-lines of the previous match
    ;; with before-lines of the current match.

    (when prev-after-lines
      ;; Don't overlap prev after-lines with current before-lines.
      (if (>= (+ prev-line (length prev-after-lines))
	      (- curr-line (length before-lines)))
	  (setq prev-after-lines
		(butlast prev-after-lines
			 (- (length prev-after-lines)
			    (- curr-line prev-line (length before-lines) 1))))
	;; Separate non-overlapping context lines with a dashed line.
	(setq separator "-------\n")))

    (when prev-line
      ;; Don't overlap current before-lines with previous match line.
      (if (<= (- curr-line (length before-lines))
	      prev-line)
	  (setq before-lines
		(nthcdr (- (length before-lines)
			   (- curr-line prev-line 1))
			before-lines))
	;; Separate non-overlapping before-context lines.
	(unless (> nlines 0)
	  (setq separator "-------\n"))))

    (list
     ;; Return a list where the first element is the output line.
     (apply #'concat
	    (append
	     (if prev-after-lines
		 (occur-engine-add-prefix prev-after-lines prefix-face))
	     (if separator
		 (list (if prefix-face
			   (propertize separator 'font-lock-face prefix-face)
			 separator)))
	     (occur-engine-add-prefix before-lines prefix-face)
	     (list out-line)))
     ;; And the second element is the list of context after-lines.
     (if (> nlines 0) after-lines))))


(provide 'replace)

;;; replace.el ends here
