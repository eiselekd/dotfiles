;;; perldb-ui-ex.el --- Extended User Interface for perl debugger

;; Copyright (C) 2011 Free Software Foundation, Inc.
;;
;; Author: Tsujikawa Takaya <ttsujikawa@gmail.com>
;; Maintainer: Tsujikawa Takaya <ttsujikawa@gmail.com>
;; Created: 25 Nov 2011
;; Version: 0.01
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Features:
;; Set breakpints at files not yet loaded.

;; Put this file and perldb-ui.el into your load-path and the following into your ~/.emacs:
;;   (require 'perldb-ui-ex)
;;   (require 'cperl-mode)
;;   (add-hook 'cperl-mode-hook '(lambda () (define-key cperl-mode-map "\C-c\C-b" 'gud-break-file-line)))
;; and M-x perldb-ui

(provide 'perldb-ui-ex)
(require 'perldb-ui)

(defun gud-break-file-line ()
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
          (gud-call (format "z %s:%d"
                            (expand-file-name file) (line-number-at-pos)))
          (gud-call (format "z %s:%d"
                            (file-truename (expand-file-name file)) (line-number-at-pos)))
          (perldb-invalidate-breakpoints))
      (message "not a file buffer"))))

(defadvice perldb-install-methods (after perldb-ui-ex activate)
  (with-temp-buffer
    (let ((conf "~/.perldb")
          found)
      (if (file-exists-p conf)
          (progn
            (insert-file-contents conf)
            (goto-char (point-min))
            (when (re-search-forward "^#### perldb-ui-ex.el version \\([0-9]+\\.[0-9]+\\)" nil t)
              (if (string= (match-string 1) perldb-version)
                  (setq found t)
                (let ((beg (line-beginning-position)) end)
                  (if (re-search-forward "^#### end perldb-ui-ex.el" nil t)
                      (setq end (line-end-position))
                    (setq end (point-max)))
                  (delete-region begin end)))))
        (insert "# -*- perl -*-\n"))
      (unless found
        (message "Install methods...")
        (goto-char (point-max))
        (insert "#### perldb-ui-ex.el version " perldb-version
                "
{
    package DB::emacs;
    sub cmd_z {
        my ($s, $line) = @_;
        my ($file, $lineno) = split /:/, $line;
        $s->set_break_file_line($file, $lineno, '1');
    }
    use File::Spec;
    sub set_break_file_line {
        my ($s, $file, $lineno, $cond) = @_;
        my %file = ($file => 1);
        for(@INC) {
            my $rel = File::Spec->abs2rel($file,$_);
            next if($rel =~/^\\.\\./);
            $file{$rel} = 1;
        }
        for my $file (keys %file) {
            $file =~ s/\s+$//;

            if ( defined $main::{ '_<' . $file } ) { # if loaded
                # We switched, so switch the debugger internals around.
                if ( $file ne $DB::filename ) {
                    *dbline   = $main::{ '_<' . $file };
                    $DB::max      = $#dbline;
                    $DB::filename = $file;
                    $DB::start    = 1;
                    $DB::cmd      = 'l';
                }

                # set breakpoint
                return eval {
                    DB::break_on_filename_line($file, $lineno, $cond);
                    1;
                } or do {
                    local $\\ = '';
                    print $OUT $@ and return;
                };
            }
        }
        for my $file (keys %file) {
            $DB::postponed_file{$file} ||= {};
            $DB::postponed_file{$file}{$lineno} = $cond;
        }
    }
    $DB::alias{'z'} = 's/^z (.*)/DB::emacs->cmd_z(\\'$1\\')/';
}
#### end perldb-ui-ex.el
")
        (write-region (point-min) (point-max) conf)))))

(perldb-define-trigger perldb-invalidate-breakpoints
  perldb-breakpoints-buffer
  "DB::emacs::breakpoints()"
  perldb-info-breakpoints
  (let ((breakpoints perldb-breakpoints)
        bp new-breakpoints file line)
    (goto-char (point-min))
    ;; add new breakpoints
    (while (not (eobp))
      (looking-at "^\s\*\\(.+\\):")
      (setq file (match-string 1))
      (when (string-equal "Postponed breakpoints in files" file)
        (forward-line 1)
        (looking-at "^\s\*\\(.+\\):")
        (setq file (match-string 1)))
      (forward-line 1)
      (while (looking-at "^\\s-+\\([0-9]+\\):")
        (setq line (string-to-number (match-string 1)))
        ;; (message "file: %s line: %d" file line)
        (unless (setq bp (perldb-find-breakpoints file line))
          (setq bp (save-window-excursion (perldb-put-breakpoint file line))))
        (push bp new-breakpoints)
        (forward-line 2)))
    ;; remove not exists breakpoints
    (dolist (bp breakpoints)
      (unless (memq bp new-breakpoints)
        (apply 'perldb-remove-breakpoint (overlay-get bp 'break-position))))
    ;; install to perldb-breakpoints
    (setq perldb-breakpoints new-breakpoints)))

(add-to-list 'perldb-command-handler
             '("^[z] " . perldb-invalidate-breakpoints))
