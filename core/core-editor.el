;;; core-editor.el -*- lexical-binding: t; -*-

(message "core-editor")

(transient-mark-mode 1)     ; Enable transient mark

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Favour spaces over tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

(setq fill-column 80
      word-wrap t)

;; disable file backups
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/auto-save/")))

(defun warmacs-backup-file-name (fpath)
  "If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ➢ for example: “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setq
 make-backup-file-name-function 'warmacs-backup-file-name)
;; end disable file backups ~ and ##autosave

;;;###autoload
(defun warmacs--rename-current-buffer-file (file)
  "Renames current buffer and file it is visiting. Check file
exists before calling this function"
  (let ((new-name (read-file-name "New name: " file)))
    (if (get-buffer new-name)
        (error "A buffer named '%s' already exists!" new-name)
      (f-move file new-name)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
      (message "File '%s' successfully renamed to '%s'"
               file (file-name-nondirectory new-name))))) 

;;;###autoload
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (f-this-file))
        (buffer (current-buffer)))
    (if (not (and filename (f-exists? filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (f-delete filename)
        (when (yes-or-no-p (format "Kill the buffer %s? " buffer))
          (kill-buffer buffer))
        (message "File '%s' successfully removed" filename)))))

;;;###autoload
(defun warmacs--rename-buffer-or-save-new-file ()
  (let ((old-short-name (buffer-name))
        key)
    (while (not (memq key '(?s ?r)))
      (setq key (read-key (propertize
                           (format
                            (concat "Buffer '%s' is not visiting a file: "
                                    "[s]ave to file or [r]ename buffer?")
                            old-short-name)
                           'face 'minibuffer-prompt)))
      (cond ((eq key ?s)            ; save to file
             ;; this allows for saving a new empty (unmodified) buffer
             (unless (buffer-modified-p) (set-buffer-modified-p t))
             (save-buffer))
            ((eq key ?r)            ; rename buffer
             (let ((new-buffer-name (read-string "New buffer name: ")))
               (while (get-buffer new-buffer-name)
                 ;; ask to rename again, if the new buffer name exists
                 (if (yes-or-no-p
                      (format (concat "A buffer named '%s' already exists: "
                                      "Rename again?")
                              new-buffer-name))
                     (setq new-buffer-name (read-string "New buffer name: "))
                   (keyboard-quit)))
               (rename-buffer new-buffer-name)
               (message (concat "Buffer Renamed\n"
                                "From: " old-short-name "\n"
                                "To:   " new-buffer-name))))
            ;; ?\a = C-g, ?\e = Esc and C-[
            ((memq key '(?\a ?\e)) (keyboard-quit))))))

;;;###autoload
(defun warmacs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let ((file (f-this-file)))
    (if (and file (f-exists? file))
        (warmacs--rename-current-buffer-file file)
      (warmacs--rename-buffer-or-save-new-file))))

;; undo-redo system
;; https://codeberg.org/ideasman42/emacs-undo-fu
(use-package undo-fu)

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo)

;; Evil Keybindings
(use-package evil
  :defer 1
  :custom
  (evil-undo-system 'undo-fu)
  :defer 1
  :init
  (setq
   evil-want-integration t
   evil-want-keybinding nil)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :custom
  (evil-goggles-duration 0.05)
  :config
  (evil-goggles-mode 1)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-operator
  :init
  ;; double all the commenting functions so that the inverse operations
  ;; can be called without setting a flag
  (defun warmacs/comment-or-uncomment-lines-inverse (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line t))
      (evilnc-comment-or-uncomment-lines arg)))

  (defun warmacs/comment-or-uncomment-lines (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line nil))
      (evilnc-comment-or-uncomment-lines arg)))

  (defun warmacs/copy-and-comment-lines-inverse (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line t))
      (evilnc-copy-and-comment-lines arg)))

  (defun warmacs/copy-and-comment-lines (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line nil))
      (evilnc-copy-and-comment-lines arg)))

  (defun warmacs/quick-comment-or-uncomment-to-the-line-inverse (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line t))
      (evilnc-comment-or-uncomment-to-the-line arg)))

  (defun warmacs/quick-comment-or-uncomment-to-the-line (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line nil))
      (evilnc-comment-or-uncomment-to-the-line arg)))

  (defun warmacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line t))
      (evilnc-comment-or-uncomment-paragraphs arg)))

  (defun warmacs/comment-or-uncomment-paragraphs (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line nil))
      (evilnc-comment-or-uncomment-paragraphs arg)))

  (warmacs/leader-keys
    ";"  #'evilnc-comment-operator
    "c"  '(:ignore t :which-key "comment")
    "cl" 'warmacs/comment-or-uncomment-lines
    "cL" 'warmacs/comment-or-uncomment-lines-inverse
    "cp" 'warmacs/comment-or-uncomment-paragraphs
    "cP" 'warmacs/comment-or-uncomment-paragraphs-inverse
    "ct" 'warmacs/quick-comment-or-uncomment-to-the-line
    "cT" 'warmacs/quick-comment-or-uncomment-to-the-line-inverse
    "cy" 'warmacs/copy-and-comment-lines
    "cY" 'warmacs/copy-and-comment-lines-inverse)

  :general
  (:keymaps 'evil-normal-state-map
            "gc" #'evilnc-comment-operator
            "gC" '(:ignore t :which-key "yank comment")
            "gCy" 'evilnc-copy-and-comment-operator))

(use-package evil-mc
  :after evil
  :hook
  ((prog-mode markdown-mode org-mode) . evil-mc-mode))

(use-package evil-surround
  :after evil
  :hook
  ((prog-mode markdown-mode org-mode) . evil-surround-mode))

;; Save scratch buffer
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;; hl-todo-mode in individual buffers or use the global variant global-hl-todo-mode
;; highlight todo and similar keywords
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (markdown-mode . hl-todo-mode)
         (org-mode . hl-todo-mode)))

(use-package fira-code-mode
  :if (display-graphic-p)
  :custom
  (fira-code-mode-disabled-ligatures '())  ; ligatures you don't want
  :init
  :hook (prog-mode . fira-code-mode)) ; mode to enable fira-code-mode in

;; enable recent files
(use-package emacs
  :after no-littering
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(provide 'core-editor)
