;;; -*- lexical-binding: t; -*-

;;; org-report.el --- Generate status reports from Org mode files

;; Copyright (C) 2015  Michael Strey

;; Author: Michael Strey <mstrey@strey.biz>
;; Keywords: Org, mode, report, email, period, status

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The code exports reports of sales activities from one or more
;; Org mode source files.  It can generate as well complete reports
;; from a designated subtree as differential reports made over a
;; period of time of the same subtree.

;; Requirements:

;; Files in SOURCES-TARGETS must be Org mode files containing sections
;; named according to HEADING in CLIENTS.  The program expects a
;; property field :EXPORT_DATE: in the respective subtree.

;; Example of usage:

;; With the default values for CLIENTS and SOURCES-TARGETS, the
;; following function call will generate six reports exporting the
;; subtrees for client A from files customers.org and
;; sales_projects.org for three periods: all changes since the last
;; report (DEFAULT), complete subtree export, all changes made to the
;; subtree in June 2015.  Finally it will create an email message
;; buffer with an email addressed to the persons to report to.

;; (org-report-create-report-message
;;  'clientA org-report-clients '(leads sales) org-report-sources-targets
;;  '(default nil '("2015-06-01" "2015-06-31"))
;;  "Dear Mr. Kim,\n\nPlease find attached my weekly reports.\n\n
;; Best regards\nMichael Strey\n\n")
;;

(require 'org)

(defconst gt-tokens '(tests failures disabled errors timestamp time status))

(defun gt-as-property(cons)
  "Convert to org property string"
  (if (consp cons)
    (format "  :%s: %s\n" (car cons) (cdr cons))))

(defun gt-summary-property-list(report tokens)
  "Converts the json properties from the assoc list to string list in org mode property format"
  (remove-if #'null
             (mapcar #'(lambda(x) (gt-as-property (assoc x report))) tokens)))


(defun gt-append-headline(stars report &optional progress)
  "Insert org item. Optionally display percent progress of the subitems"
  (insert stars " " (alist-get 'name report))
  (when progress
    (insert progress))
  (insert "\n  :PROPERTIES:\n")
  (mapc #'insert (gt-summary-property-list report gt-tokens))
  (insert "  :END:\n"))


(defun gt-append-failure-message(msg)
  (insert "*** " (alist-get 'failure msg) "\n\n"))


(defun gt-append-test-case(case)
  (insert  "** " (alist-get 'name case))
  (if (assoc 'failures case)
      (org-todo '(1)) ;;assumed this is TODO
    (org-todo '(2)))  ;;assumed this is DONE
  (insert "\n")
  (mapc #'gt-append-failure-message (alist-get 'failures case)))
      

(defun gt-append-group(group)
  "gtest test suite consists of multiple tests"
  (gt-append-headline "*" group " [%]")
  (mapc 'gt-append-test-case (alist-get 'testsuite group)))

(defun gt-append-test-groups(report)
  (mapc 'gt-append-group report))
      
(defun gt-json-to-org(report buffer)
  (with-current-buffer buffer
    (save-excursion
      (view-mode -1) ;; turn off view mode (from previos calls)
      (erase-buffer)
      (org-mode)
      (insert "#+STARTUP: overview\n\n") ;; everything is folded initially
      (gt-append-test-groups (alist-get 'testsuites report)) ;;append the test groups
      (org-set-startup-visibility)
      (save-buffer)
      (view-mode))))


(provide 'utils/org-report)
