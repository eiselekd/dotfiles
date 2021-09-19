(require 'magit-core)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-wip)
(require 'magit)
(require 'magit-apply)
(require 'transient)
(require 'format-spec)
(require 'package nil t) ; used in `magit-version'
(require 'with-editor)
(require 'f)

(transient-define-prefix custom-command-menue ()
  "Show available macros."
  :info-manual "(magit)Macros"
  ["Macros"
   [("0" "~/bin/la-1-2-r8.1-push-manifest.sh"
     (lambda () (interactive) (magit-macro-list (format "~/bin/la-1-2-r8.1-push-manifest.sh %s" default-directory))))
    ("!" "switch dirs '!'" custom-dir-menue)
    ]
   ]
  )

(transient-define-prefix custom-dir-menue ()
  "Show available macros."
  :info-manual "(magit)Macros"
  ["Switchdirs"
   [("0" "~/bin/dir0.txt"
     (lambda () (interactive) (dired (replace-regexp-in-string "\n$" "" (f-read-text "~/bin/dir0.txt" 'utf-8)))))
    ("!" "execute original '!'" shell-command)
    ]
   ]
  )


;;;###autoload
(defun magit-macro-list (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'."
  (interactive (list (magit-read-shell-command)))
  (magit--shell-command command))

(provide 'macro-execute)
