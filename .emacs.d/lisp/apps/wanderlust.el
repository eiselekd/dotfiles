
(defconst *user-wanderlust-data-directory*
 (path-join *user-data-directory* "wanderlust")
 "Path to user's Wanderlust data store.")

(defconst *user-wanderlust-cache-directory*
 (path-join *user-cache-directory* "wanderlust")
 "Path to user's Wanderlust cache store.")

(defun apps/wl-folder-mode-hook ()
 "Wanderlust folder mode hook."
 (hl-line-mode t))

(defun apps/wl-summary-mode-hook ()
 "Wanderlust summary mode hook."
 (hl-line-mode t))

(defun apps/wl-draft-mode-hook ()
 "Wanderlust draft mode hook."
 ;;; (Bindings) ;;;
 (apps/bind-key-local :code :try-complete 'bbdb-complete-name)
 (apps/bind-key-local :code :compile 'org-mime-htmlize))

(defun apps/mime-edit-mode-hook ()
 "Wanderlust mime edit mode hook."
 ;; Enable org structured editing.
 (turn-on-orgstruct)
 (turn-on-orgstruct++))

(defun apps/wl-message-buffer-created-hook ()
 "Wanderlust message buffer created hook."
 (setq
  ;; Fold lines that are too long.
  truncate-lines nil))

(defun apps/wl-message-redisplay-hook ()
 "Wanderlust message redisplay hook."
 (when (display-graphic-p)
   (smiley-region (point-min) (point-max))))

(defun apps/bbdbv3-wl-init-hook ()
 "BBDBv3 Wanderlust initiatlization hook."
 (with-feature 'bbdbV3-wl
   (apps/bbdb-init-hook)))

(defun apps/wl-init-hook ()
 "Wanderlust initialization hook."
 (when (el-get-package-is-installed 'bbdbv3-wl)
   (apps/bbdbv3-wl-init-hook))

 (when (el-get-package-is-installed 'org-mode)
   (with-feature 'org-mime
     (setq-default
      org-mime-library 'semi)))

 ;; Set up wanderlust as the default mail user agent.
 (when (boundp 'mail-user-agent)
   (setq mail-user-agent 'wl-user-agent))

 (when (fboundp 'define-mail-user-agent)
   (define-mail-user-agent
     'wl-user-agent
     'wl-user-agent-compose
     'wl-draft-send
     'wl-draft-kill
     'mail-send-hook))

 ;;; (Bindings) ;;;
 ;; Cite original email by default.
 (define-key wl-summary-mode-map (kbd "A") 'wl-summary-reply)
 (define-key wl-summary-mode-map (kbd "a") 'wl-summary-reply-with-citation))


(defun apps/icalendar-import-mime-text (entity &optional situation)
 "Import calendar from ENTITY to org agenda, SITUATION is ignored."
 (save-excursion
   (let ((calendar-temp-file
          (make-temp-file
           (path-join *user-wanderlust-cache-directory* "import.cal"))))
     (mime-write-entity-content entity calendar-temp-file)
     (icalendar-import-file calendar-temp-file diary-file)
     (kill-buffer (find-buffer-visiting calendar-temp-file))
     (delete-file calendar-temp-file))))


(defun apps/mime-display-text/plain-hook ()
  "Plain text display hook."
  (let ((fmt (cdr (assoc "format" (mime-content-type-parameters
                                   (mime-entity-content-type entity))))))
    (when (string= "flowed" fmt)
      (fill-flowed))))


(defun apps/semi-init ()
 "Initialize SEMI."
 (setq-default
  ;; Don't split large mails.
  mime-edit-split-message nil
  ;; MIME type priorities.
  mime-view-type-subtype-score-alist
  '(((text . plain) . 4)
    ((text . enriched) . 3)
    ((text . html) . 2)
    ((text . richtext) . 1)))

 (after-load 'mime-view
   (autoload 'mime-w3m-preview-text/html "mime-w3m")

   ;; Use w3m to view HTML mail.
   (ctree-set-calist-strictly
    'mime-preview-condition
    '((type . text)
      (subtype . html)
      (body . visible)
      (body-presentation-method . mime-w3m-preview-text/html)))

   (set-alist 'mime-view-type-subtype-score-alist '(text . html) 3)

   ;; Support importing calendar invites.
   (ctree-set-calist-strictly
    'mime-acting-condition
    '((mode . "play")
      (type . text)
      (subtype . calendar)
      (method . apps/icalendar-import-mime-text))))

 (add-hook 'mime-edit-mode-hook 'apps/mime-edit-mode-hook))


(defun apps/elmo-init ()
 "Initialize ELMO."
 (setq-default
  ;; Put message database in data directory.
  elmo-msgdb-directory (path-join *user-wanderlust-data-directory* "elmo")
  ;; Folders go in data directory too.
  elmo-localdir-folder-path *user-wanderlust-data-directory*
  elmo-maildir-folder-path *user-wanderlust-data-directory*
  elmo-search-namazu-default-index-path *user-wanderlust-data-directory*
  elmo-archive-folder-path *user-wanderlust-data-directory*
  ;; ELMO's cache go into the user cache directory.
  elmo-cache-directory (path-join *user-wanderlust-cache-directory* "elmo")
  ;; Use modified UTF-8 for IMAP4.
  elmo-imap4-use-modified-utf7 t)

 (unless (executable-find "namazu")
   (message "Namazu not found, mail will not be indexed.")))


(defun apps/mu-cite-init ()
 "Initialize mu-cite."
 (setq-default
  ;; Citation format.
  mu-cite-top-format '("On " date ", " full-name " wrote:\n")
  ;; Use > as prefix.
  mu-cite-prefix-format (quote ("> ")))

 (add-hook 'mail-citation-hook 'mu-cite-original))


(defun apps/wanderlust-set-gmail-user (fullname username)
 "Configure Wanderlust to use \"FULLNAME\" <USERNAME@gmail.com>."
 (let ((email-address (concat username "@gmail.com"))
       (folder-template (format "\"%s@gmail.com\"/clear@imap.gmail.com:993!" username)))
   (unless (file-exists-p wl-folders-file)
     (apps/wanderlust-create-folders-gmail username wl-folders-file))

   ;; Register email address.
   (add-to-list 'wl-user-mail-address-list email-address)

   ;; Set up account template.
   (add-to-list
    'wl-template-alist
    `(,email-address
      (wl-from . ,(concat fullname " <" email-address ">"))
      ("From" . wl-from)
      (wl-insert-message-id . nil)
      (wl-local-domain . "gmail.com")
      (wl-message-id-domain . "smtp.gmail.com")
      ;; SMTP
      (wl-smtp-posting-user . ,username)
      (wl-smtp-authenticate-type . "plain")
      (wl-smtp-connection-type . 'starttls)
      (wl-smtp-posting-port . 587)
      (wl-smtp-posting-server . "smtp.gmail.com")
      ;; Folders
      (wl-default-spec . "%")
      (wl-default-folder . "%inbox")
      (wl-draft-folder . "%[Gmail]/Drafts")
      (wl-spam-folder . "%[Gmail]/Spam")
      (wl-trash-folder . "%[Gmail]/Trash")
      (wl-fcc . "%[Gmail]/Sent")))
   
   ;; Point draft configuration to correct template.
   (add-to-list
    'wl-draft-config-alist
    `((string-match ,email-address  wl-draft-parent-folder)
      (template . ,email-address)))
   
   ;; Set up trash folder for account.
   (add-to-list
    'wl-dispose-folder-alist
    `(,email-address . ,(concat "%[Gmail]/Trash" folder-template)))
   
   ;; Check inbox for new mail.
   (add-to-list 'wl-biff-check-folder-list
                (concat "%inbox" folder-template))))

(defun apps/wanderlust-create-folders-gmail (username folders-file)
 "Write GMail folders for USERNAME@gmail.com to FOLDERS-FILE."
 (let ((server-string (format "\"%s@gmail.com\"/clear@imap.gmail.com:993!" username))
       (folders-template "# -*- conf-unix -*-
+trash  \"Trash\"
+draft  \"Drafts\"

Gmail {
   %inbox:SERVER \"Inbox\"
   Labels {
       %[Gmail]/Important:SERVER \"Important\"
       %[Gmail]/Drafts:SERVER \"Drafts\"
       %[Gmail]/All Mail:SERVER \"All Mail\"
       %[Gmail]/Sent Mail:SERVER \"Sent Mail\"
       %[Gmail]/Starred:SERVER \"Starred\"
       %[Gmail]/Spam:SERVER \"Spam\"
       %[Gmail]/Trash:SERVER \"Trash\"
   }
}
"))
   (with-temp-buffer
     (insert
      (replace-regexp-in-string "SERVER" server-string folders-template t))
     (when (file-writable-p folders-file)
      (write-region (point-min) (point-max) folders-file)))))


(defun apps/wanderlust-set-summary-guides ()
 "Set Wanderlust summary thread guides."
 (if (eq default-terminal-coding-system 'utf-8)
     (setq-default
      wl-thread-indent-level 2
      wl-thread-have-younger-brother-str "├─┳─"
      wl-thread-vertical-str             "┃"
      wl-thread-youngest-child-str       "╰───"
      wl-thread-horizontal-str           "─"
      wl-thread-space-str                " ")
   (setq-default
    wl-thread-indent-level 2
    wl-thread-have-younger-brother-str "+"
    wl-thread-youngest-child-str       "+"
    wl-thread-vertical-str             "|"
    wl-thread-horizontal-str           "-"
    wl-thread-space-str                " ")))


(defun apps/wanderlust-init ()
 "Initialize Wanderlust."
 (el-get-eval-after-load 'semi
   (apps/semi-init))
 (after-load 'elmo
   (apps/elmo-init))

 (setq-default
  ;;; (Basic Configuration) ;;;
  ;; Put configuration into wanderlust data directory.
  wl-init-file (path-join *user-wanderlust-data-directory* "init.el")
  wl-folders-file (path-join *user-wanderlust-data-directory* "folders")
  wl-address-file (path-join *user-wanderlust-data-directory* "addresses")
  ;; Put temporary files in cache directories.
  wl-temporary-file-directory *user-wanderlust-cache-directory*
  ssl-certificate-directory (path-join *user-cache-directory* "certs")
  ;; Mark sent mails as read.
  wl-fcc-force-as-read t
  ;; Check for mail when idle.
  wl-biff-check-interval 180
  wl-biff-use-idle-timer t
  ;; Let SMTP server handle Message-ID.
  wl-insert-message-id nil
  ;; Message window size.
  wl-message-window-size '(1 . 3)
  ;; Quit without asking.
  wl-interactive-exit nil

  ;;; (Folders) ;;;
  ;; Show folders in a pane to the left.
  wl-stay-folder-window nil
  wl-folder-window-width 45
  ;; Asynchronously update folders.
  wl-folder-check-async t

  ;;; (Summary) ;;;
  ;; Set verbose summary.
  wl-summary-width nil
  wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
  ;; Display TO rather than FROM in "Sent" folders.
  wl-summary-showto-folder-regexp ".*Sent.*"
  ;; Divide thread if subject has changed.
  wl-summary-divide-thread-when-subject-changed t

  ;;; (Messages) ;;;
  ;; Field lists.
  wl-message-ignored-field-list '("^.*")
  wl-message-visible-field-list '("^\\(To\\|Cc\\):" "^Subject:"
                                  "^\\(From\\|Reply-To\\):" "^Organization:"
                                  "^X-Attribution:" "^\\(Posted\\|Date\\):"
                                  "^\\(Posted\\|Date\\):"
                                  "^\\(User-Agent\\|X-Mailer\\):")
  wl-message-sort-field-list    '("^From:" "^Organization:" "^X-Attribution:"
                                  "^Subject:" "^Date:" "^To:" "^Cc:")

  ;;; (Drafts) ;;;
  ;; Raise a new frame when creating a draft.
  wl-draft-use-frame t
  ;; Automatically save drafts every two minutes.
  wl-auto-save-drafts-interval 120.0
  ;; Sane forward tag.
  wl-forward-subject-prefix "Fwd: "
  ;; Automatically select the correct template based on folder.
  wl-draft-config-matchone t)

 ;; Support for writing/reading flowed emails.
 (add-hook 'mime-display-text/plain-hook 'apps/mime-display-text/plain-hook)
 (after-load 'wl-mime
             (mime-edit-insert-tag "text" "plain" "; format=flowed"))

 ;; Set up guides in summary mode.
 (apps/wanderlust-set-summary-guides)

 (when (and (display-graphic-p)
            (el-get-package-is-installed 'wl-gravatar))
   (setq-default
    ;; Insert gravatar as email X-Face.
    wl-highlight-x-face-function 'wl-gravatar-insert))

 (with-feature 'fullframe
   (fullframe wl wl-exit nil))

 (setq-default
  ;; Show mail status in mode line.
  global-mode-string (cons '(wl-modeline-biff-status
                             wl-modeline-biff-state-on
                             wl-modeline-biff-state-off) global-mode-string))

 ;; Pick email account template when opening a draft.
 (add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
 ;; Don't apply email account template when sending draft, otherwise switching
 ;; templates won't work.
 (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

 ;; Configuration hooks.
 (add-hook 'wl-init-hook 'apps/wl-init-hook)
 (add-hook 'wl-message-redisplay-hook 'apps/wl-message-redisplay-hook)
 (add-hook 'wl-message-buffer-created-hook
           'apps/wl-message-buffer-created-hook)

 ;; Mode hooks.
 (add-hook 'wl-folder-mode-hook 'apps/wl-folder-mode-hook)
 (add-hook 'wl-summary-mode-hook 'apps/wl-summary-mode-hook)
 (add-hook 'wl-draft-mode-hook 'apps/wl-draft-mode-hook)

 ;;; (Bindings) ;;;
 (apps/bind-key-global :apps :email 'wl))


(defun apps/wl-init ()
 "Initialize wanderlust."
 ;; Create data and cache stores.
 (message (format "[*] wanderlust init"))
  
 (make-directory *user-wanderlust-data-directory* t)
 (set-file-modes *user-wanderlust-data-directory* #o0700)
 (make-directory *user-wanderlust-cache-directory* t)
 (set-file-modes *user-wanderlust-cache-directory* #o0700)

 (require-package '(:name wanderlust :after (apps/wanderlust-init)))
 (require-package '(:name mu-cite :after (apps/mu-cite-init)))
 (require-package '(:name bbdbv3-wl))
 
 (when (display-graphic-p)
   (require-package '(:name wl-gravatar))))

(apps/wl-init)

(provide 'apps/wanderlust.el)
;;; wanderlust.el ends here
