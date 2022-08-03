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


;; Evil Keybindings

(use-package evil
  :demand t
  :custom
  ;; do not load evil-keybindings.el, we're using evil-collection
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :defer 4
  :init
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :after evil
  :defer 4
  :init
  (progn
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

    (general-def evil-normal-state-map
        "gc" 'evilnc-comment-operator
        "gC" '(:ignore t :which-key "yank comment")
        "gCy" 'evilnc-copy-and-comment-operator)

    (warmacs/leader-keys
      ";"  'evilnc-comment-operator
      "c"  '(:ignore t :which-key "comment")
      "cl" 'warmacs/comment-or-uncomment-lines
      "cL" 'warmacs/comment-or-uncomment-lines-inverse
      "cp" 'warmacs/comment-or-uncomment-paragraphs
      "cP" 'warmacs/comment-or-uncomment-paragraphs-inverse
      "ct" 'warmacs/quick-comment-or-uncomment-to-the-line
      "cT" 'warmacs/quick-comment-or-uncomment-to-the-line-inverse
      "cy" 'warmacs/copy-and-comment-lines
      "cY" 'warmacs/copy-and-comment-lines-inverse)))

(use-package evil-mc
  :after evil
  :init
  (evil-mc-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;; enable recent files
(use-package emacs
  :config
  (recentf-mode 1))

(provide 'core-editor)
