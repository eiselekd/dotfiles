;;; ai-analyze-mode.el --- AI Analysis Mode -*- lexical-binding: t; -*-

(defgroup ai-analyze nil
  "AI Analysis Mode."
  :group 'tools)

(defcustom ai-analyze-skills-dir "~/.emacs.d/ai-skills"
  "Directory containing skill files for AI analysis."
  :type 'directory)

(defvar-local ai-analyze-overlays nil
  "List of overlays in the current buffer.")

(defvar ai-analyze-chat-buffers (make-hash-table :test 'equal)
  "Hash table mapping overlay IDs to chat buffers.")

(defvar ai-analyze-colors '("light blue" "light green" "light pink" "light yellow" "plum1" "paleturquoise1")
  "Colors for regions.")

(defun ai-analyze--get-next-color ()
  (let ((color (pop ai-analyze-colors)))
    (setq ai-analyze-colors (append ai-analyze-colors (list color)))
    color))

(defvar ai-analyze-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") 'ai-analyze-switch-chat)
    (define-key map (kbd "M-S-RET") 'ai-analyze-new-chat)
    (define-key map (kbd "ESC <kp-enter>") 'ai-analyze-new-chat)
    map)
  "Keymap for `ai-analyze-mode'.")

(define-minor-mode ai-analyze-mode
  "Minor mode for AI-assisted code analysis."
  :init-value nil
  :lighter " AI-Analyze"
  :keymap ai-analyze-mode-map
  (if ai-analyze-mode
      (progn
        (read-only-mode 1)
        (message "AI Analyze Mode enabled."))
    (progn
      (read-only-mode -1)
      ;; Optional: clean up overlays
      )))

;; Window Layout Management

(defun ai-analyze-layout-toggle ()
  "Toggle the 3-pane AI analysis layout."
  (interactive)
  (if (and (fboundp 'treemacs-get-local-window)
           (treemacs-get-local-window))
      (progn
        ;; Close layout
        (delete-other-windows)
        (treemacs-quit)
        (ai-analyze-mode -1))
    ;; Open layout
    (delete-other-windows)
    (let ((file-buf (current-buffer))
          (default-directory (or (and (buffer-file-name) (file-name-directory (buffer-file-name)))
                                 default-directory)))
      ;; 1. Open Treemacs on left
      (treemacs-add-and-display-current-project-exclusively)
      ;; treemacs might focus its own window, let's go back to file window
      (select-window (get-buffer-window file-buf))
      ;; 2. Split current (middle) window to create right window
      (let ((right-win (split-window-right)))
        ;; If middle buffer is a file, enable mode
        (when (buffer-file-name file-buf)
          (with-current-buffer file-buf
            (ai-analyze-mode 1)))
        (select-window right-win)
        (let ((help-buf (get-buffer-create "*AI Analyze Help*")))
          (switch-to-buffer help-buf)
          (unless (> (buffer-size) 0)
            (insert "Welcome to AI Analyze Mode.\nSelect a file in Treemacs, go to it, and press M-S-RET on a block to start a chat.")))
        (select-window (get-buffer-window file-buf))))))

(defun ai-analyze-auto-enable ()
  "Auto-enable ai-analyze-mode if layout is active."
  (when (and (fboundp 'treemacs-get-local-window)
             (treemacs-get-local-window)
             (= (length (window-list)) 3))
    (ai-analyze-mode 1)))

(add-hook 'find-file-hook 'ai-analyze-auto-enable)

;; Region and Overlay Management

(defun ai-analyze--get-block-bounds ()
  "Get the bounds of the code block under cursor. Fallback to paragraph."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (save-excursion
      (let ((start (progn (backward-paragraph) (point)))
            (end (progn (forward-paragraph) (point))))
        (cons start end)))))

(defun ai-analyze-new-chat ()
  "Create a new chat region and spawn AI."
  (interactive)
  (unless ai-analyze-mode
    (ai-analyze-mode 1))
  (let* ((bounds (ai-analyze--get-block-bounds))
         (start (car bounds))
         (end (cdr bounds))
         (region-id (md5 (format "%s-%s-%s" (buffer-name) start (current-time-string))))
         (ov (make-overlay start end))
         (color (ai-analyze--get-next-color))
         (code-text (buffer-substring-no-properties start end))
         (file-path (buffer-file-name)))
    ;; In terminal mode, fringes aren't drawn, so we use a line-prefix (a vertical bar)
    ;; which mimics a colored fringe on the left side of the block.
    (let ((prefix (propertize "│ " 'face `(:foreground ,color :weight bold))))
      (overlay-put ov 'line-prefix prefix)
      (overlay-put ov 'wrap-prefix prefix))
    (overlay-put ov 'ai-analyze-id region-id)
    (push ov ai-analyze-overlays)
    
    ;; Make sure skills directory exists
    (unless (file-exists-p (expand-file-name ai-analyze-skills-dir))
      (make-directory (expand-file-name ai-analyze-skills-dir) t))
    
    (let ((skill "Analyze this code block and explain it in context."))
      (ai-analyze--spawn-chat region-id file-path code-text skill)
      (ai-analyze--focus-chat region-id)
      (with-current-buffer (gethash region-id ai-analyze-chat-buffers)
        (condition-case err
            (gptel-send)
          (error (message "GPTel Error: %s" (error-message-string err))))))))

(defun ai-analyze-switch-chat ()
  "Switch to the chat associated with the overlay under cursor."
  (interactive)
  (let ((ovs (overlays-at (point)))
        found-id)
    (dolist (ov ovs)
      (let ((id (overlay-get ov 'ai-analyze-id)))
        (when id
          (setq found-id id))))
    (if found-id
        (ai-analyze--focus-chat found-id)
      (message "No AI region at point."))))

;; Chat Backend (gptel integration)

(defun ai-analyze--spawn-chat (region-id file-path code-text skill)
  "Spawn a gptel chat buffer for the given region."
  (let* ((buf-name (format "*AI-Chat-%s*" (substring region-id 0 6)))
         (buf (get-buffer-create buf-name)))
    (puthash region-id buf ai-analyze-chat-buffers)
    (with-current-buffer buf
      (markdown-mode)
      (gptel-mode 1)
      ;; Insert initial context
      (insert (format "> **Skill:** %s\n> **File:** `%s`\n\n```\n%s\n```\n\n---\n\n"
                      skill
                      (if file-path (file-name-nondirectory file-path) "unknown")
                      code-text))
      (goto-char (point-max)))))

(defun ai-analyze--focus-chat (region-id)
  "Focus the right window on the chat buffer for REGION-ID."
  (let ((buf (gethash region-id ai-analyze-chat-buffers)))
    (if buf
        ;; Find rightmost window
        (let ((win (window-in-direction 'right (selected-window))))
          (if win
              (progn
                (select-window win)
                (switch-to-buffer buf))
            ;; If there is no right window, split again
            (let ((new-win (split-window-right)))
              (select-window new-win)
              (switch-to-buffer buf))))
      (message "Chat buffer not found for this region."))))

(provide 'ai-analyze-mode)
