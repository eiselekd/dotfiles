;;; cling-cabal.el --- Support for Cabal packages -*- lexical-binding: t -*-

;; Copyright © 2007, 2008  Stefan Monnier
;;             2016 Arthur Fayzrakhmanov

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Todo:

;; - distinguish continued lines from indented lines.
;; - indent-line-function.
;; - outline-minor-mode.

;;; Code:

;; (defun cling-cabal-extract-fields-from-doc ()
;;   (require 'xml)
;;   (let ((section (completing-read
;;                   "Section: "
;;                   '("general-fields" "library" "executable" "buildinfo"))))
;;     (goto-char (point-min))
;;     (search-forward (concat "<sect3 id=\"" section "\">")))
;;   (let* ((xml (xml-parse-region
;;                (progn (search-forward "<variablelist>") (match-beginning 0))
;;                (progn (search-forward "</variablelist>") (point))))
;;          (varlist (cl-remove-if-not 'consp (cl-cddar xml)))
;;          (syms (mapcar (lambda (entry) (cl-caddr (assq 'literal (assq 'term entry))))
;;                        varlist))
;;          (fields (mapcar (lambda (sym) (substring-no-properties sym 0 -1)) syms)))
;;     fields))

(require 'cl-lib)

(defcustom cling-hasktags-path "hasktags"
  "Path to `hasktags' executable."
  :group 'cling
  :type 'string)

(defconst cling-cabal-general-fields
  ;; Extracted with (cling-cabal-extract-fields-from-doc "general-fields")
  '("name" "version" "cabal-version" "license" "license-file" "copyright"
    "author" "maintainer" "stability" "homepage" "package-url" "synopsis"
    "description" "category" "tested-with" "build-depends" "data-files"
    "extra-source-files" "extra-tmp-files"))

(defconst cling-cabal-library-fields
  ;; Extracted with (cling-cabal-extract-fields-from-doc "library")
  '("exposed-modules"))

(defconst cling-cabal-executable-fields
  ;; Extracted with (cling-cabal-extract-fields-from-doc "executable")
  '("executable" "main-is"))

(defconst cling-cabal-buildinfo-fields
  ;; Extracted with (cling-cabal-extract-fields-from-doc "buildinfo")
  '("buildable" "other-modules" "hs-source-dirs" "extensions" "ghc-options"
    "ghc-prof-options" "hugs-options" "nhc-options" "includes"
    "install-includes" "include-dirs" "c-sources" "extra-libraries"
    "extra-lib-dirs" "cc-options" "ld-options" "frameworks"))

(defvar cling-cabal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; The comment syntax can't be described simply in syntax-table.
    ;; We could use font-lock-syntactic-keywords, but is it worth it?
    ;; (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?- "w" st)
    st))

(defvar cling-cabal-font-lock-keywords
  ;; The comment syntax can't be described simply in syntax-table.
  ;; We could use font-lock-syntactic-keywords, but is it worth it?
  '(("^[ \t]*--.*" . font-lock-comment-face)
    ("^ *\\([^ \t:]+\\):" (1 font-lock-keyword-face))
    ("^\\(Library\\)[ \t]*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^\\(Executable\\|Test-Suite\\|Benchmark\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("^\\(Flag\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ("^\\(Source-Repository\\)[ \t]+\\(head\\|this\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ("^ *\\(if\\)[ \t]+.*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^ *\\(}[ \t]*\\)?\\(else\\)[ \t]*\\({\\|$\\)"
     (2 font-lock-keyword-face))
    ("\\<\\(?:True\\|False\\)\\>"
     (0 font-lock-constant-face))))

(defvar cling-cabal-buffers nil
  "List of Cabal buffers.")

(defun cling-cabal-buffers-clean (&optional buffer)
  "Refresh list of known cabal buffers.

Check each buffer in variable `cling-cabal-buffers' and remove
it from list if one of the following conditions are hold:
+ buffer is killed;
+ buffer's mode is not derived from `cling-cabal-mode';
+ buffer is a BUFFER (if given)."
  (let ((bufs ()))
    (dolist (buf cling-cabal-buffers)
      (if (and (buffer-live-p buf)
               (not (eq buf buffer))
               (with-current-buffer buf (derived-mode-p 'cling-cabal-mode)))
          (push buf bufs)))
    (setq cling-cabal-buffers bufs)))

(defun cling-cabal-unregister-buffer ()
  "Exclude current buffer from global list of known cabal buffers."
  (cling-cabal-buffers-clean (current-buffer)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . cling-cabal-mode))

(defvar cling-cabal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'cling-cabal-subsection-arrange-lines)
    (define-key map (kbd "C-M-n") 'cling-cabal-next-section)
    (define-key map (kbd "C-M-p") 'cling-cabal-previous-section)
    (define-key map (kbd "M-n") 'cling-cabal-next-subsection)
    (define-key map (kbd "M-p") 'cling-cabal-previous-subsection)
    (define-key map (kbd "C-<down>") 'cling-cabal-next-subsection)
    (define-key map (kbd "C-<up>") 'cling-cabal-previous-subsection)
    (define-key map (kbd "C-c C-f") 'cling-cabal-find-or-create-source-file)
    (define-key map (kbd "M-g l") 'cling-cabal-goto-library-section)
    (define-key map (kbd "M-g e") 'cling-cabal-goto-executable-section)
    (define-key map (kbd "M-g b") 'cling-cabal-goto-benchmark-section)
    (define-key map (kbd "M-g t") 'cling-cabal-goto-test-suite-section)
    map))

;;;###autoload
(define-derived-mode cling-cabal-mode fundamental-mode "Cling-Cabal"
  "Major mode for Cabal package description files."
  (setq-local font-lock-defaults
              '(cling-cabal-font-lock-keywords t t nil nil))
  (add-to-list 'cling-cabal-buffers (current-buffer))
  (add-hook 'change-major-mode-hook 'cling-cabal-unregister-buffer nil 'local)
  (add-hook 'kill-buffer-hook 'cling-cabal-unregister-buffer nil 'local)
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "\\(^[ \t]*\\)--[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\|\n\\)")
  (setq-local indent-line-function 'cling-cabal-indent-line)
  (setq indent-tabs-mode nil)
  )

(make-obsolete 'cling-cabal-get-setting
               'cling-cabal--get-field
               "March 14, 2016")
(defalias 'cling-cabal-get-setting 'cling-cabal--get-field
  "Try to read value of field with NAME from current buffer.
Obsolete function.  Defined for backward compatibility.  Use
`cling-cabal--get-field' instead.")

(defun cling-cabal--get-field (name)
  "Try to read value of field with NAME from current buffer."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^[ \t]*" (regexp-quote name)
                     ":[ \t]*\\(.*\\(\n[ \t]+[ \t\n].*\\)*\\)")
             nil t)
        (let ((val (match-string 1))
              (start 1))
          (when (match-end 2)             ;Multiple lines.
            ;; The documentation is not very precise about what to do about
            ;; the \n and the indentation: are they part of the value or
            ;; the encoding?  I take the point of view that \n is part of
            ;; the value (so that values can span multiple lines as well),
            ;; and that only the first char in the indentation is part of
            ;; the encoding, the rest is part of the value (otherwise, lines
            ;; in the value cannot start with spaces or tabs).
            (while (string-match "^[ \t]\\(?:\\.$\\)?" val start)
              (setq start (1+ (match-beginning 0)))
              (setq val (replace-match "" t t val))))
          val)))))


(make-obsolete 'cling-cabal-guess-setting
               'cling-cabal-get-field
               "March 14, 2016")
(defalias 'cling-cabal-guess-setting 'cling-cabal-get-field
  "Read the value of field with NAME from project's cabal file.
Obsolete function.  Defined for backward compatibility.  Use
`cling-cabal-get-field' instead.")

;;;###autoload
(defun cling-cabal-get-field (name)
  "Read the value of field with NAME from project's cabal file.
If there is no valid .cabal file to get the setting from (or
there is no corresponding setting with that name in the .cabal
file), then this function returns nil."
  (interactive)
  (when (and name buffer-file-name)
    (let ((cabal-file (cling-cabal-find-file)))
      (when (and cabal-file (file-readable-p cabal-file))
        (with-temp-buffer
          (insert-file-contents cabal-file)
          (cling-cabal--get-field name))))))

;;;###autoload
(defun cling-cabal-get-dir (&optional use-defaults)
  "Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all."
  (let* ((file (cling-cabal-find-file))
         (dir (if file (file-name-directory file) default-directory)))
    (if use-defaults
        dir
        (cling-utils-read-directory-name
         (format "Cabal dir%s: " (if file (format " (guessed from %s)" (file-relative-name file)) ""))
         dir))))

(defun cling-cabal-compute-checksum (dir)
  "Compute MD5 checksum of package description file in DIR.
Return nil if no Cabal description file could be located via
`cling-cabal-find-pkg-desc'."
  (let ((cabal-file (cling-cabal-find-pkg-desc dir)))
    (when cabal-file
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (md5 (buffer-string))))))

(defun cling-cabal-find-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses `cling-cabal-find-pkg-desc' internally."
  (let ((use-dir (or dir default-directory)))
    (while (and use-dir (not (file-directory-p use-dir)))
      (setq use-dir (file-name-directory (directory-file-name use-dir))))
    (when use-dir
      (catch 'found
        (let ((user (nth 2 (file-attributes use-dir)))
              ;; Abbreviate, so as to stop when we cross ~/.
              (root (abbreviate-file-name use-dir)))
          ;; traverse current dir up to root as long as file owner doesn't change
          (while (and root (equal user (nth 2 (file-attributes root))))
            (let ((cabal-file (cling-cabal-find-pkg-desc root)))
              (when cabal-file
                (throw 'found cabal-file)))

            (let ((proot (file-name-directory (directory-file-name root))))
              (if (equal proot root) ;; fix-point reached?
                  (throw 'found nil)
                (setq root proot))))
          nil)))))

(defun cling-cabal-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.cling.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.
  (let* ((cabal-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.cling\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

(defun cling-cabal-find-dir (&optional dir)
  "Like `cling-cabal-find-file' but returns directory instead.
See `cling-cabal-find-file' for meaning of DIR argument."
  (let ((cabal-file (cling-cabal-find-file dir)))
    (when cabal-file
      (file-name-directory cabal-file))))

;;;###autoload
(defun cling-cabal-visit-file (other-window)
  "Locate and visit package description file for file visited by current buffer.
This uses `cling-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'."
  (interactive "P")
  ;; Note: We aren't allowed to rely on cling-session here (which,
  ;; in pathological cases, can have a different .cabal file
  ;; associated with the current buffer)
  (if buffer-file-name
      (let ((cabal-file (cling-cabal-find-file (file-name-directory buffer-file-name))))
        (if cabal-file
            (if other-window
                (find-file-other-window cabal-file)
              (find-file cabal-file))
          (error "Could not locate \".cabal\" file for %S" buffer-file-name)))
    (error "Cannot locate \".cabal\" file for buffers not visiting any file")))

(defvar cling-cabal-commands
  '("install"
    "update"
    "list"
    "info"
    "upgrade"
    "fetch"
    "unpack"
    "check"
    "sdist"
    "upload"
    "report"
    "init"
    "configure"
    "build"
    "copy"
    "haddock"
    "clean"
    "hscolour"
    "register"
    "test"
    "help"
    "run"))

;;;###autoload
(defgroup cling-cabal nil
  "Cling cabal files"
  :group 'cling
)

(defconst cling-cabal-section-header-regexp "^[[:alnum:]]" )
(defconst cling-cabal-subsection-header-regexp "^[ \t]*[[:alnum:]]\\w*:")
(defconst cling-cabal-comment-regexp "^[ \t]*--")
(defconst cling-cabal-empty-regexp "^[ \t]*$")
(defconst cling-cabal-conditional-regexp "^[ \t]*\\(\\if\\|else\\|}\\)")

(defun cling-cabal-classify-line ()
  "Classify the current line into 'section-header 'subsection-header 'section-data 'comment and 'empty '"
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at cling-cabal-subsection-header-regexp ) 'subsection-header)
     ((looking-at cling-cabal-section-header-regexp) 'section-header)
     ((looking-at cling-cabal-comment-regexp) 'comment)
     ((looking-at cling-cabal-empty-regexp ) 'empty)
     ((looking-at cling-cabal-conditional-regexp ) 'conditional)
     (t 'section-data))))

(defun cling-cabal-header-p ()
  "Is the current line a section or subsection header?"
  (cl-case (cling-cabal-classify-line)
    ((section-header subsection-header) t)))

(defun cling-cabal-section-header-p ()
  "Is the current line a section or subsection header?"
  (cl-case (cling-cabal-classify-line)
    ((section-header) t)))


(defun cling-cabal-section-beginning ()
  "Find the beginning of the current section"
  (save-excursion
    (while (not (or (bobp) (cling-cabal-section-header-p)))
      (forward-line -1))
    (point)))

(defun cling-cabal-beginning-of-section ()
  "go to the beginning of the section"
  (interactive)
  (goto-char (cling-cabal-section-beginning))
)

(defun cling-cabal-section-end ()
  "Find the end of the current section"
  (interactive)
  (save-excursion
    (if (re-search-forward "\n\\([ \t]*\n\\)*[[:alnum:]]" nil t)
        (match-beginning 0)
        (point-max))))

(defun cling-cabal-end-of-section ()
  "go to the end of the section"
  (interactive)
  (goto-char (cling-cabal-section-end)))

(defun cling-cabal-next-section ()
  "Go to the next section"
  (interactive)
  (when (cling-cabal-section-header-p) (forward-line))
  (while (not (or (eobp) (cling-cabal-section-header-p)))
    (forward-line)))

(defun cling-cabal-previous-section ()
  "Go to the next section"
  (interactive)
  (when (cling-cabal-section-header-p) (forward-line -1))
  (while (not (or (bobp) (cling-cabal-section-header-p)))
    (forward-line -1)))

(defun cling-cabal-subsection-end ()
  "find the end of the current subsection"
  (save-excursion
    (cling-cabal-beginning-of-subsection)
    (forward-line)
    (while (and (not (eobp))
                (member (cling-cabal-classify-line) '(empty section-data)))
      (forward-line))
    (unless (eobp) (forward-line -1))
    (while (and (equal (cling-cabal-classify-line) 'empty)
                (not (bobp)))
      (forward-line -1))
    (end-of-line)
    (point)))

(defun cling-cabal-end-of-subsection ()
  "go to the end of the current subsection"
  (interactive)
  (goto-char (cling-cabal-subsection-end)))

(defun cling-cabal-section ()
  "Get the name and data of the associated section"
  (save-excursion
    (cling-cabal-beginning-of-section)
    (when (and (cling-cabal-section-header-p)
               (looking-at "^\\(\\w+\\)[ \t]*\\(.*\\)$"))
      (list :name (match-string-no-properties 1)
            :value (match-string-no-properties 2)
            :beginning (match-beginning 0)
            :end (cling-cabal-section-end)))))


(defun cling-cabal-subsection ()
  "Get the name and bounds of of the current subsection"
  (save-excursion
    (cling-cabal-beginning-of-subsection)
    (when (looking-at "\\([ \t]*\\(\\w*\\):\\)[ \t]*")
      (list :name (match-string-no-properties 2)
            :beginning (match-end 0)
            :end (save-match-data (cling-cabal-subsection-end))
            :data-start-column (save-excursion (goto-char (match-end 0))
                                               (current-column)
                                               )))))


(defun cling-cabal-section-name (section)
  (plist-get section :name))

(defun cling-cabal-section-value (section)
  (plist-get section :value))

(defun cling-cabal-section-start (section)
  (plist-get section :beginning))

(defun cling-cabal-section-data-start-column (section)
  (plist-get section :data-start-column))

(defun cling-cabal-enum-targets ()
  "Enumerate .cabal targets."
  (let ((cabal-file (cling-cabal-find-file)))
    (when (and cabal-file (file-readable-p cabal-file))
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (cling-cabal-mode)
        (goto-char (point-min))
        (let ((matches)
              (projectName (cling-cabal--get-field "name")))
          (cling-cabal-next-section)
          (while (not (eobp))
            (if (cling-cabal-source-section-p (cling-cabal-section))
                (let ((val (car (split-string
                                 (cling-cabal-section-value
                                  (cling-cabal-section))))))
                  (if (or (string= val "")
                          (string= val "{")
                          (not val))
                      (push projectName matches)
                    (push val matches))))
            (cling-cabal-next-section))
          (reverse matches))))))

(defmacro cling-cabal-with-subsection (subsection replace &rest funs)
  "Copy subsection data into a temporary buffer, save indentation
and execute FORMS

If REPLACE is non-nil the subsection data is replaced with the
resulting buffer-content"
  (let ((section (make-symbol "section"))
        (beg (make-symbol "beg"))
        (end (make-symbol "end"))
        (start-col (make-symbol "start-col"))
        (section-data (make-symbol "section-data")))
    `(let* ((,section ,subsection)
            (,beg (plist-get ,section :beginning))
            (,end (plist-get ,section :end))
            (,start-col (plist-get ,section :data-start-column))
            (,section-data (buffer-substring ,beg ,end)))
       (save-excursion
         (prog1
             (with-temp-buffer
               (setq indent-tabs-mode nil)
               (indent-to ,start-col)
               (insert ,section-data)
               (goto-char (point-min))
               (prog1
                   (progn (cling-cabal-save-indentation ,@funs))
                 (goto-char (point-min))
                 (when (looking-at (format "[ ]\\{0,%d\\}" (1+ ,start-col)))
                     (replace-match ""))

                 (setq ,section-data (buffer-substring (point-min) (point-max)))))
           ,@(when replace
               `((delete-region ,beg ,end)
                 (goto-char ,beg)
                 (insert ,section-data))))))))

(defmacro cling-cabal-each-line (&rest fun)
  "Execute FORMS on each line"
  `(save-excursion
     (while (< (point) (point-max))
       ,@fun
       (forward-line))))

(defun cling-cabal-chomp-line ()
  "Remove leading and trailing whitespaces from current line"
  (beginning-of-line)
  (when (looking-at "^[ \t]*\\([^ \t]\\|\\(?:[^ \t].*[^ \t]\\)\\)[ \t]*$")
    (replace-match (match-string 1) nil t)
    t))


(defun cling-cabal-min-indentation (&optional beg end)
  "Compute largest common whitespace prefix of each line in between BEG and END"
  (save-excursion
    (goto-char (or beg (point-min)))
    (let ((min-indent nil))
      (while (< (point) (or end (point-max)))
        (let ((indent (current-indentation)))
          (if (and (not (cling-cabal-ignore-line-p))
                   (or (not min-indent)
                       (< indent min-indent)))
              (setq min-indent indent)))
        (forward-line))
      min-indent)))

(defun cling-cabal-ignore-line-p ()
  "Does line only contain whitespaces and comments?"
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\\(?:--.*\\)?$")))

(defun cling-cabal-kill-indentation ()
  "Remove longest common whitespace prefix from each line"
  (goto-char (point-min))
  (let ((indent (cling-cabal-min-indentation)))
    (cling-cabal-each-line (unless (cling-cabal-ignore-line-p)
                               (delete-char indent)) )
    indent))

(defun cling-cabal-add-indentation (indent)
  (goto-char (point-min))
  (cling-cabal-each-line
   (unless (cling-cabal-ignore-line-p)
     (indent-to indent))))


(defmacro cling-cabal-save-indentation (&rest funs)
  "Strip indentation from each line, execute FORMS and reinstate indentation
   so that the indentation of the FIRST LINE matches"
  (let ((old-l1-indent (make-symbol "new-l1-indent"))
        (new-l1-indent (make-symbol "old-l1-indent")))
    `(let ( (,old-l1-indent (save-excursion
                              (goto-char (point-min))
                              (current-indentation))))
       (unwind-protect
           (progn
             (cling-cabal-kill-indentation)
             ,@funs)
         (progn
           (goto-char (point-min))
           (let ((,new-l1-indent (current-indentation)))
             (cling-cabal-add-indentation (- ,old-l1-indent
                                           ,new-l1-indent))))))))

(defun cling-cabal-comma-separatorp (pos)
  "Return non-nil when the char at POS is a comma separator.
Characters that are not a comma, or commas inside a commment or
string, are not comma separators."
  (when (eq (char-after pos) ?,)
    (let ((ss (syntax-ppss pos)))
      (not
       (or
        ;; inside a string
        (nth 3 ss)
        ;; inside a comment
        (nth 4 ss))))))

(defun cling-cabal-strip-list-and-detect-style ()
  "Strip commas from a comma-separated list.
Detect and return the comma style.  The possible options are:

before: a comma at the start of each line (except the first), e.g.
    Foo
  , Bar

after: a comma at the end of each line (except the last), e.g.
    Foo,
    Bar

single: everything on a single line, but comma-separated, e.g.
    Foo, Bar

nil: no commas, e.g.
    Foo Bar

If the styles are mixed, the position of the first comma
determines the style. If there is only one element then `after'
style is assumed."
  (let (comma-style)
    ;; split list items on single line
    (goto-char (point-min))
    (while (re-search-forward
            "\\([^ \t,\n]\\)[ \t]*\\(,\\)[ \t]*\\([^ \t,\n]\\)" nil t)
      (when (cling-cabal-comma-separatorp (match-beginning 2))
        (setq comma-style 'single)
        (replace-match "\\1\n\\3" nil nil)))
    ;; remove commas before
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\),\\([ \t]*\\)" nil t)
      (setq comma-style 'before)
      (replace-match "" nil nil))
    ;; remove trailing commas
    (goto-char (point-min))
    (while (re-search-forward ",[ \t]*$" nil t)
      (unless (eq comma-style 'before)
        (setq comma-style 'after))
      (replace-match "" nil nil))

    ;; if there is just one line then set default as 'after
    (unless comma-style
      (goto-char (point-min))
      (forward-line)
      (when (eobp)
        (setq comma-style 'after)))
    (goto-char (point-min))

    (cling-cabal-each-line (cling-cabal-chomp-line))
    comma-style))

(defun cling-cabal-listify (comma-style)
  "Add commas so that the buffer contains a comma-separated list.
Respect the COMMA-STYLE, see
`cling-cabal-strip-list-and-detect-style' for the possible
styles."
  (cl-case comma-style
    ('before
     (goto-char (point-min))
     (while (cling-cabal-ignore-line-p) (forward-line))
     (indent-to 2)
     (forward-line)
     (cling-cabal-each-line
      (unless (cling-cabal-ignore-line-p)
        (insert ", "))))
    ('after
     (goto-char (point-max))
     (while (equal 0 (forward-line -1))
       (unless (cling-cabal-ignore-line-p)
         (end-of-line)
         (insert ",")
         (beginning-of-line))))
    ('single
     (goto-char (point-min))
     (while (not (eobp))
       (end-of-line)
       (unless (eobp)
         (insert ", ")
         (delete-char 1)
         (just-one-space))))))

(defmacro cling-cabal-with-cs-list (&rest funs)
  "Format the buffer so that each line contains a list element.
Respect the comma style."
  (let ((comma-style (make-symbol "comma-style")))
    `(let ((,comma-style
            (save-excursion
              (cling-cabal-strip-list-and-detect-style))))
       (unwind-protect (progn ,@funs)
         (cling-cabal-listify ,comma-style)))))


(defun cling-cabal-sort-lines-key-fun ()
  (when (looking-at "[ \t]*--[ \t,]*")
    (goto-char (match-end 0)))
  nil)

(defmacro cling-cabal-save-position (&rest forms)
  "Save position as mark, execute FORMs and go back to mark"
  `(prog2
       (cling-cabal-mark)
       (progn ,@forms)
     (cling-cabal-goto-mark)
     (cling-cabal-remove-mark)))

(defun cling-cabal-subsection-arrange-lines ()
  "Sort lines of current subsection"
  (interactive)
  (cling-cabal-save-position
   (cling-cabal-with-subsection
    (cling-cabal-subsection) t
    (cling-cabal-with-cs-list
     (sort-subr nil 'forward-line 'end-of-line
                'cling-cabal-sort-lines-key-fun)
     ))))

(defun cling-cabal-subsection-beginning ()
  "find the beginning of the current subsection"
  (save-excursion
    (while (and (not (bobp))
                (not (cling-cabal-header-p)))
      (forward-line -1))
    (back-to-indentation)
    (point)))

(defun cling-cabal-beginning-of-subsection ()
  "go to the beginning of the current subsection"
  (interactive)
  (goto-char (cling-cabal-subsection-beginning)))

(defun cling-cabal-next-subsection ()
  "go to the next subsection"
  (interactive)
  (if (cling-cabal-header-p) (forward-line))
  (while (and (not (eobp))
              (not (cling-cabal-header-p)))
    (forward-line))
  (cling-cabal-forward-to-line-entry))

(defun cling-cabal-previous-subsection ()
  "go to the previous subsection"
  (interactive)
  (if (cling-cabal-header-p) (forward-line -1))
  (while (and (not (bobp))
              (not (cling-cabal-header-p)))
    (forward-line -1))
  (cling-cabal-forward-to-line-entry)
  )


(defun cling-cabal-find-subsection-by (section pred)
  "Find subsection with name NAME"
  (save-excursion
    (when section (goto-char (cling-cabal-section-start section)))
    (let* ((end (if section (cling-cabal-section-end) (point-max)))
           (found nil))
      (while (and (< (point) end)
                  (not found))
        (let ((subsection (cling-cabal-subsection)))
          (when (and subsection (funcall pred subsection))
            (setq found subsection)))
        (cling-cabal-next-subsection))
      found)))

(defun cling-cabal-find-subsection (section name)
  "Find subsection with name NAME"
  (let ((downcase-name (downcase name)))
    (cling-cabal-find-subsection-by
     section
     `(lambda (subsection)
        (string= (downcase (cling-cabal-section-name subsection))
                 ,downcase-name)))))

(defun cling-cabal-goto-subsection (name)
  (let ((subsection (cling-cabal-find-subsection (cling-cabal-section) name)))
    (when subsection
      (goto-char (cling-cabal-section-start subsection)))))

(defun cling-cabal-goto-exposed-modules ()
  (interactive)
  (cling-cabal-goto-subsection "exposed-modules"))

(defun cling-cabal-subsection-entry-list (section name)
  "Get the data of a subsection as a list"
  (let ((subsection (cling-cabal-find-subsection section name)))
    (when subsection
      (cling-cabal-with-subsection
       subsection nil
       (cling-cabal-with-cs-list
        (delete-matching-lines
         (format "\\(?:%s\\)\\|\\(?:%s\\)"
                 cling-cabal-comment-regexp
                 cling-cabal-empty-regexp)
         (point-min) (point-max))
        (split-string (buffer-substring-no-properties (point-min) (point-max))
                      "\n" t))))))

(defun cling-cabal-remove-mark ()
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(cling-cabal-marker)))


(defun cling-cabal-mark ()
  "Mark the current position with the text property cling-cabal-marker"
  (cling-cabal-remove-mark)
  (put-text-property (line-beginning-position) (line-end-position)
                     'cling-cabal-marker 'marked-line)
  (put-text-property (point) (1+ (point))
                     'cling-cabal-marker 'marked))


(defun cling-cabal-goto-mark ()
  "Go to marked line"
  (let ((marked-pos (text-property-any (point-min) (point-max)
                                       'cling-cabal-marker
                                       'marked))
        (marked-line (text-property-any (point-min) (point-max)
                                       'cling-cabal-marker
                                       'marked-line) )
        )
    (cond (marked-pos (goto-char marked-pos))
          (marked-line (goto-char marked-line)))))

(defmacro cling-cabal-with-subsection-line (replace &rest forms)
  "Mark line, copy subsection data into a temporary buffer, save indentation
and execute FORMS at the marked line.

If REPLACE is non-nil the subsection data is replaced with the
resulting buffer-content.  Unmark line at the end."
  `(progn
     (cling-cabal-mark)
     (unwind-protect
         (cling-cabal-with-subsection (cling-cabal-subsection) ,replace
          (cling-cabal-goto-mark)
          ,@forms)
       (cling-cabal-remove-mark))))


(defun cling-cabal-get-line-content ()
  (cling-cabal-with-subsection-line
   nil
   (cling-cabal-with-cs-list
    (cling-cabal-goto-mark)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))))

(defun cling-cabal-module-to-filename (module)
  (concat (replace-regexp-in-string "[.]" "/" module ) ".hs"))

(defconst cling-cabal-module-sections '("exposed-modules" "other-modules")
  "List of sections that contain module names"
)

(defconst cling-cabal-file-sections
  '("main-is" "c-sources" "data-files" "extra-source-files"
    "extra-doc-files" "extra-tmp-files" )
  "List of subsections that contain filenames"
  )

(defconst cling-cabal-source-bearing-sections
  '("library" "executable" "test-suite" "benchmark"))

(defun cling-cabal-source-section-p (section)
  (not (not (member (downcase (cling-cabal-section-name section))
                    cling-cabal-source-bearing-sections))))

(defun cling-cabal-line-filename ()
  "Expand filename in current line according to the subsection type

Module names in exposed-modules and other-modules are expanded by replacing each dot (.) in the module name with a foward slash (/) and appending \".hs\"

Example: Foo.Bar.Quux ==> Foo/Bar/Quux.hs

Source names from main-is and c-sources sections are left untouched

"
  (let ((entry (cling-cabal-get-line-content))
        (subsection (downcase (cling-cabal-section-name
                               (cling-cabal-subsection)))))
    (cond ((member subsection cling-cabal-module-sections)
           (cling-cabal-module-to-filename entry))
          ((member subsection cling-cabal-file-sections) entry))))

(defun cling-cabal-join-paths (&rest args)
  "Crude hack to replace f-join"
  (mapconcat 'identity args "/")
)

(defun cling-cabal-find-or-create-source-file ()
  "Open the source file this line refers to."
  (interactive)
  (let* ((src-dirs (append (cling-cabal-subsection-entry-list
                            (cling-cabal-section) "hs-source-dirs")
                           '("")))
         (base-dir (file-name-directory (buffer-file-name)))
         (filename (cling-cabal-line-filename)))
    (when filename
      (let ((candidates
             (delq nil (mapcar
                        (lambda (dir)
                          (let ((file (cling-cabal-join-paths base-dir
                                                                dir
                                                                filename)))
                            (when (and (file-readable-p file)
                                       (not (file-directory-p file)))
                              file)))
                        src-dirs))))
        (if (null candidates)
            (unwind-protect
                (progn
                  (cling-mode-toggle-interactive-prompt-state)
                  (let* ((src-dir
                          (cling-cabal-join-paths base-dir
                                                    (or (car src-dirs) "")))
                         (newfile (cling-cabal-join-paths src-dir filename))
                         (do-create-p (y-or-n-p (format "Create file %s ?" newfile))))
                    (when do-create-p
                      (find-file-other-window newfile ))))
              (cling-mode-toggle-interactive-prompt-state t))
          (find-file-other-window (car candidates)))))))


(defun cling-cabal-find-section-type (type &optional wrap)
  (save-excursion
    (cling-cabal-next-section)
    (while
        (not
         (or
          (eobp)
          (string=
           (downcase type)
           (downcase (cling-cabal-section-name (cling-cabal-section))))))
      (cling-cabal-next-section))
    (if (eobp)
      (if wrap (progn
                 (goto-char (point-min))
                 (cling-cabal-find-section-type type nil) )
        nil)
      (point))))

(defun cling-cabal-goto-section-type (type)
  (let ((section (cling-cabal-find-section-type type t)))
    (if section (goto-char section)
      (message "No %s section found" type))))

(defun cling-cabal-goto-library-section ()
  (interactive)
  (cling-cabal-goto-section-type "library"))

(defun cling-cabal-goto-test-suite-section ()
  (interactive)
  (cling-cabal-goto-section-type "test-suite"))

(defun cling-cabal-goto-executable-section ()
  (interactive)
  (cling-cabal-goto-section-type "executable"))

(defun cling-cabal-goto-benchmark-section ()
  (interactive)
  (cling-cabal-goto-section-type "benchmark"))



(defun cling-cabal-line-entry-column ()
  "Column at which the line entry starts"
  (save-excursion
    (cl-case (cling-cabal-classify-line)
      (section-data (beginning-of-line)
                    (when (looking-at "[ ]*\\(?:,[ ]*\\)?")
                      (goto-char (match-end 0))
                      (current-column)))
      (subsection-header
       (cling-cabal-section-data-start-column (cling-cabal-subsection))))))

(defun cling-cabal-forward-to-line-entry ()
  "go forward to the beginning of the line entry (but never move backwards)"
  (let ((col (cling-cabal-line-entry-column)))
    (when (and col (< (current-column) col))
      (beginning-of-line)
      (forward-char col))))

(defun cling-cabal-indent-line ()
  "Indent current line according to subsection"
  (interactive)
  (cl-case (cling-cabal-classify-line)
    (section-data
     (save-excursion
       (let ((indent (cling-cabal-section-data-start-column
                      (cling-cabal-subsection))))
         (indent-line-to indent)
         (beginning-of-line)
         (when (looking-at "[ ]*\\([ ]\\{2\\},[ ]*\\)")
           (replace-match ", " t t nil 1)))))
    (empty
     (indent-relative)))
  (cling-cabal-forward-to-line-entry))

(defun cling-cabal-map-sections (fun)
  "Execute fun over each section, collecting the result"
  (save-excursion
    (goto-char (point-min))
    (let ((results nil))
        (while (not (eobp))
          (let* ((section (cling-cabal-section))
                 (result (and section (funcall fun (cling-cabal-section)))))
            (when section (setq results (cons result results))))
          (cling-cabal-next-section))
        (nreverse results))))

(defun cling-cabal-section-add-build-dependency (dependency &optional sort sec)
  "Add a build dependency to the build-depends section"
  (let* ((section (or sec (cling-cabal-section)))
         (subsection (and section
                          (cling-cabal-find-subsection section "build-depends"))))
    (when subsection
      (cling-cabal-with-subsection
       subsection t
       (cling-cabal-with-cs-list
        (insert dependency)
        (insert "\n")
        (when sort
          (goto-char (point-min))
          (sort-subr nil 'forward-line 'end-of-line
                     'cling-cabal-sort-lines-key-fun)))))))

(defun cling-cabal-add-build-dependency (dependency &optional sort silent)
  "Add the given DEPENDENCY to every section in cabal file.
If SORT argument is given sort dependencies in section after update.
Pass SILENT argument to update all sections without asking user."
  (cling-cabal-map-sections
   (lambda (section)
     (when (cling-cabal-source-section-p section)
       (unwind-protect
           (progn
             (when
                 (or silent
                     (y-or-n-p (format "Add dependency %s to %s section %s?"
                                       dependency
                                       (cling-cabal-section-name section)
                                       (cling-cabal-section-value section))))
               (cling-cabal-section-add-build-dependency dependency
                                                           sort
                                                           section))
             nil)
         (cling-mode-toggle-interactive-prompt-state t))))))

(defun cling-cabal-add-dependency
    (package &optional version no-prompt sort silent)
  "Add PACKAGE to the cabal file.
If VERSION is non-nil it will be appended as a minimum version.
If NO-PROMPT is nil the minimum package version is read from the
minibuffer.  When SORT is non-nil the package entries are sorted
afterwards.  If SILENT is non-nil the user is prompted for each
source-section."
  (interactive
   (list (read-from-minibuffer "Package entry: ") nil t t nil))
  (cling-mode-toggle-interactive-prompt-state)
  (unwind-protect
      (save-window-excursion
        (find-file-other-window (cling-cabal-find-file))
        (let ((entry (if no-prompt package
                       (read-from-minibuffer
                        "Package entry: "
                        (concat package
                                (if version (concat " >= " version) ""))))))
          (cling-cabal-add-build-dependency entry sort silent)
          (when (or silent (y-or-n-p "Save cabal file? "))
            (save-buffer))))
    ;; unwind
    (cling-mode-toggle-interactive-prompt-state t)))


(defun cling-cabal--find-tags-dir ()
  "Return a directory where TAGS file will be generated.
Tries to find cabal file first and if succeeds uses its location.
If cabal file not found uses current file directory.  If current
buffer not visiting a file returns nil."
  (or (cling-cabal-find-dir)
      (when buffer-file-name
        (file-name-directory buffer-file-name))))

(defun cling-cabal--compose-hasktags-command (dir)
  "Prepare command to execute `hasktags` command in DIR folder.
By default following parameters are passed to Hasktags
executable:
-e - generate ETAGS file
-x - generate additional information in CTAGS file.

This function takes into account user's operation system: in case
of Windows it generates simple command, relying on Hasktags
itself to find source files:

hasktags --output=DIR\TAGS -x -e DIR

In other cases it uses `find` command to find all source files
recursively avoiding visiting unnecessary heavy directories like
.git, .svn, _darcs and build directories created by
cabal-install, stack, etc and passes list of found files to Hasktags."
  (if (eq system-type 'windows-nt)
      (format "%s --output=\"%s\\TAGS\" -x -e \"%s\"" cling-hasktags-path dir dir)
    (format "cd %s && %s | %s"
            dir
            (concat "find . "
                    "-type d \\( "
                    "-path ./.git "
                    "-o -path ./.svn "
                    "-o -path ./_darcs "
                    "-o -path ./.stack-work "
                    "-o -path ./dist "
                    "-o -path ./.cabal-sandbox "
                    "\\) -prune "
                    "-o -type f \\( "
                    "-name '*.hs' "
                    "-or -name '*.lhs' "
                    "-or -name '*.hsc' "
                    "\\) -not \\( "
                    "-name '#*' "
                    "-or -name '.*' "
                    "\\) -print0")
            (format "xargs -0 %s -e -x" cling-hasktags-path))))

(provide 'cling-cabal)
;;; cling-cabal.el ends here
