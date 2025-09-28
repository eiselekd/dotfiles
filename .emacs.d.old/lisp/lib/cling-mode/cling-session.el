;;; cling-session.el --- Cling sessions -*- lexical-binding: t -*-

;; Copyright (C) 2011-2012  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

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

;;; Todo:

;;; Code:

(require 'cl-lib)
(require 'cling-cabal)
(require 'cling-customize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

;; Used internally
(defvar-local cling-session nil)

(defvar cling-sessions (list)
  "All Cling sessions in the Emacs session.")

(defun cling-session-tags-filename (session)
  "Get the filename for the TAGS file."
  (concat (cling-session-cabal-dir session) "/TAGS"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding/clearing the session

;;;###autoload
(defun cling-session-maybe ()
  "Maybe get the Cling session, return nil if there isn't one."
  (if (default-boundp 'cling-session)
      cling-session
    (setq cling-session nil)))

(defun cling-session-from-buffer ()
  "Get the session based on the buffer."
  (when (and (buffer-file-name)
             (consp cling-sessions))
    (cl-reduce (lambda (acc a)
                 (let ((dir (cling-session-get a 'cabal-dir)))
                   (if dir
                       (if (string-prefix-p dir
                                            (file-name-directory (buffer-file-name)))
                           (if acc
                               (if (and
                                    (> (length (cling-session-get a 'cabal-dir))
                                       (length (cling-session-get acc 'cabal-dir))))
                                   a
                                 acc)
                             a)
                         acc)
                     acc)))
               cling-sessions
               :initial-value nil)))

(defun cling-session-default-name ()
  "Generate a default project name for the new project prompt."
  (let ((file (cling-cabal-find-file)))
    (or (when file
          (downcase (file-name-sans-extension
                     (file-name-nondirectory file))))
        "cling")))

(defun cling-session-assign (session)
  "Assing current buffer to SESSION.

This could be helpful for temporary or auxiliary buffers such as
presentation mode buffers (e.g. in case when session is killed
with all relevant buffers)."
  (setq-local cling-session session))

(defun cling-session-choose ()
  "Find a session by choosing from a list of the current sessions."
  (when cling-sessions
    (let* ((session-name (funcall cling-completing-read-function
                                  "Choose Cling session: "
                                  (cl-remove-if (lambda (name)
                                                  (and cling-session
                                                       (string= (cling-session-name cling-session)
                                                                name)))
                                                (mapcar 'cling-session-name cling-sessions))))
           (session (cl-find-if (lambda (session)
                                  (string= (cling-session-name session)
                                           session-name))
                                cling-sessions)))
      session)))

(defun cling-session-clear ()
  "Clear the buffer of any Cling session choice."
  (setq-local cling-session nil))

(defun cling-session-lookup (name)
  "Get the session by name."
  (cl-remove-if-not (lambda (s)
                      (string= name (cling-session-name s)))
                    cling-sessions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session modules

(defun cling-session-strip-dir (session file)
  "Strip the load dir from the file path."
  (let ((cur-dir (cling-session-current-dir session)))
    (if (> (length file) (length cur-dir))
        (if (string= (substring file 0 (length cur-dir))
                     cur-dir)
            (replace-regexp-in-string
             "^[/\\]" ""
             (substring file
                        (length cur-dir)))
          file)
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the session

(defun cling-session-current-dir (s)
  "Get the session current directory."
  (let ((dir (cling-session-get s 'current-dir)))
    (or dir
        (error "No current directory."))))

(defun cling-session-name (s)
  "Get the session name."
  (cling-session-get s 'name))

(defun cling-session-target (s)
  "Get the session build target.
If `cling-process-load-or-reload-prompt' is nil, accept `default'."
  (let* ((maybe-target (cling-session-get s 'target))
         (target (if maybe-target maybe-target
                   (let ((new-target
                          (if cling-process-load-or-reload-prompt
                              (read-string "build target (empty for default):")
                            "")))
                     (cling-session-set-target s new-target)))))
    (if (not (string= target "")) target nil)))

(defun cling-session-set-target (s target)
  "Set the session build target."
  (cling-session-set s 'target target))

(defun cling-session-set-interactive-buffer (s v)
  "Set the session interactive buffer."
  (cling-session-set s 'interactive-buffer v))

(defun cling-session-set-process (s v)
  "Set the session process."
  (cling-session-set s 'process v))

;;;###autoload
(defun cling-session-process (s)
  "Get the session process."
  (cling-session-get s 'process))

(defun cling-session-set-cabal-dir (s v)
  "Set the session cabal-dir."
  (let ((true-path (file-truename v)))
    (cling-session-set s 'cabal-dir true-path)
    (cling-session-set-cabal-checksum s true-path)))

(defun cling-session-set-current-dir (s v)
  "Set the session current directory."
  (let ((true-path (file-truename v)))
    (cling-session-set s 'current-dir true-path)))

(defun cling-session-set-cabal-checksum (s cabal-dir)
  "Set the session checksum of .cabal files"
  (cling-session-set s 'cabal-checksum
                       (cling-cabal-compute-checksum cabal-dir)))

(defun cling-session-cabal-dir (s)
  "Get the session cabal-dir."
  (or (cling-session-get s 'cabal-dir)
      (let ((set-dir (cling-cabal-get-dir (not cling-process-load-or-reload-prompt))))
        (if set-dir
            (progn (cling-session-set-cabal-dir s set-dir)
                   set-dir)
            (cling-session-cabal-dir s)))))

(defun cling-session-modify (session key update)
  "Update the value at KEY in SESSION with UPDATE."
  (cling-session-set
   session
   key
   (funcall update
            (cling-session-get session key))))

(defun cling-session-get (session key)
  "Get the SESSION's KEY value.
Returns nil if KEY not set."
  (cdr (assq key session)))

(defun cling-session-set (session key value)
  "Set the SESSION's KEY to VALUE.
Returns newly set VALUE."
  (let ((cell (assq key session)))
    (if cell
        (setcdr cell value) ; modify cell in-place
      (setcdr session (cons (cons key value) (cdr session))) ; new cell
      value)))

(provide 'cling-session)

;;; cling-session.el ends here
