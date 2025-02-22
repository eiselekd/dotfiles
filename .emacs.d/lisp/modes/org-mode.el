(defun org-mode/toggle-visual ()
  (interactive)
 (require 'org-bullets)
  (if (bound-and-true-p org-bullets-mode)
      (progn
	(org-bullets-mode 0)
	(setq org-hide-emphasis-markers nil))
    (progn
      (org-bullets-mode 1)
      (setq org-hide-emphasis-markers 1)
      )))


(defun ck/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "latex") (string= lang "plantuml"))))

(add-hook 'org-mode-hook
          (lambda ()
	    (progn
	      (message "[+] org-version: '%s'" org-version)
	      (require 'ox)
	      (require 'ox-html)

	      (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

	      ;; enable flyspell
	      (flyspell-mode)

	      ;; active Org-babel languages
	      (org-babel-do-load-languages
	       'org-babel-load-languages
	       '(;; other Babel languages
		 (plantuml . t)
		 (dot . t)))

	      (org-mode/toggle-visual)
	      (global-set-key (kbd "M-v") 'org-mode/toggle-visual)

	      (setq org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)
	      (setq org-enforce-todo-dependencies 't)
	      (setq org-columns-default-format "%70ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %5TAGS")
	      (setq org-global-properties (quote (("Effort_ALL" . "1:00 2:00 4:00 1d 2d 4d 8d")
						  ("STYLE_ALL" . "habit"))))

	      (setq org-plantuml-jar-path (concat  *.emacs.d.dir* "lisp/modes/plantuml.jar"  ))
	      (setq org-taskjuggler-default-project-duration 16256)
	      (message "[+] org-plantuml: '%s'" org-plantuml-jar-path)

	      ;; dont show "created" footer:
	      (setq org-html-postamble nil)
	      (setq org-html-validation-link nil)

	      (setq org-taskjuggler-default-resource-def "
shifts home

")

	      (setq org-taskjuggler-default-global-properties
"
shift home \"home\" {
workinghours mon - fri 20:00 - 23:00
workinghours sat - sun 8:00 - 12:00
}

")



	      ;; (setq org-plantuml-jar-path (shell-command-to-string "cygpath --window /home/eiselekd/.emacs.d/lisp/modes/plantuml.jar"))

	      (global-set-key (kbd "<f5>")  'org-narrow-to-subtree)
	      (global-set-key (kbd "S-<f5>")  'widen)
	      (global-set-key (kbd "M-<f3>") 'org-clock-in)
	      (global-set-key (kbd "S-<f3>") 'org-clock-out)
	      (global-set-key (kbd "S-<f9>")  'org-toggle-inline-images)

	      (global-set-key (kbd "<f7>")  'org-beamer-export-to-pdf)
	      (global-set-key (kbd "<f8>")  'org-html-export-to-html)

	      (define-key org-mode-map (kbd "C-c z") #'org-toggle-link-display)

	      (when (require 'which-func)
		(if (fboundp 'which-func-mode)
		    (which-func-mode 1)))

	      (when (require 'ox-reveal nil t)
		(progn
		  (setq org-reveal-root (concat  "file://" *.emacs.d.dir* "lisp/modes/reveal.js"  ))
		  (message "[+] ox-reveal: '%s'" org-reveal-root)
		  (global-set-key (kbd "S-<f7>")  'org-reveal-export-to-html)
		  ))
	      (when (require 'org-tree-slide nil t)
		(progn
		  (message "[+] org-tree-slide")
		  (global-set-key (kbd "M-<f7>")  'org-tree-slide-mode)
		  ))
	      (when (require 'ox-twbs nil t)
		(progn
		  (message "[+] ox-twbs")
		  (global-set-key (kbd "S-<f8>")  'org-twbs-export-to-html)
		  ))
	      (when (require 'ox-taskjuggler nil t)
		(progn
		  (message "[+] ox-taskjuggler")

		  (setq org-taskjuggler-default-reports
			'("textreport report \"Plan\" {
  formats html
  header '== %title =='

  center -8<-
    [#Plan Plan] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name, start, end, effort, chart
  loadunit shortauto
  hideresource 1
}

# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, daily # weekly
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}"))



		  ;;(global-set-key (kbd "<f9>")  'bh/org-sparse-poject-export)



		  ))

	      ))


	  (setq org-src-fontify-natively t)
	  )




(provide 'modes/org-mode.el)
