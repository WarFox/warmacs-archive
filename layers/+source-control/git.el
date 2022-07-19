(use-package magit
  :defer 2
  ;; :after git-rebase
  :commands (magit-status magit-get-current-branch magit-completing-read-function)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :init
  (setq
    git-magit-status-fullscreen nil
    magit-bury-buffer-function #'magit-restore-window-configuration
    magit-completing-read-function 'ivy-completing-read
    magit-revision-show-gravatars '("^Author:     " . "^Commit:     ") )
  :config
  (progn
    ;; confirm/abort
    (when warmacs-major-mode-leader-key
      (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
      (let ((mm-key warmacs-major-mode-leader-key))
        (dolist (state '(normal motion))
          (general-def
            :states '(state)
            :keymaps 'with-editor-mode-map
            (concat (kbd mm-key) (kbd mm-key)) 'with-editor-finish
            (concat (kbd mm-key) "a")    'with-editor-cancel
            (concat (kbd mm-key) "c")    'with-editor-finish
            (concat (kbd mm-key) "k")    'with-editor-cancel)
          (general-def
            :states '(state)
            :keymaps 'magit-log-select-mode-map
            (concat (kbd mm-key) (kbd mm-key)) 'magit-log-select-pick
            (concat (kbd mm-key) "a")    'magit-log-select-quit
            (concat (kbd mm-key) "c")    'magit-log-select-pick
            (concat (kbd mm-key) "k")    'magit-log-select-quit))))
    ;; full screen magit-status
    (when git-magit-status-fullscreen
      (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-v1)))
  :general
  (:keymaps 'magit-blame-read-only-mode-map
    :states 'normal
	  (kbd "RET") 'magit-show-commit)

  (:keymaps 'magit-mode-map
	  (kbd "<tab>") 'magit-section-toggle)

  ;; bind function keys
  (:keymaps 'magit-repolist-mode-map
    :major-modes 'magit-repolist-mode
    (kbd "gr") 'magit-list-repositories
    (kbd "RET") 'magit-repolist-status)

  (+general-global-menu! "git" "g"
    "b"  'warmacs/git-blame-transient-state/body
    "c"  'magit-clone
	  "f"  '(:ignore t :which-key "file")
    "fF" 'magit-find-file
    "fl" 'magit-log-buffer-file
    "fd" 'magit-diff
    "fm" 'magit-file-dispatch
    "i"  'magit-init
    "L"  'magit-list-repositories
    "m"  'magit-dispatch
    "s"  'magit-status
    "S"  'magit-stage-file
    "U"  'magit-unstage-file))

(use-package gitignore-templates
  :after (magit)
  :init
  (warmacs/set-major-mode-leader-keys
    :major-modes 'gitignore-mode
    "i" 'gitignore-templates-insert)
  (+general-global-git
    "fi" 'gitignore-templates-new-file))


(use-package forge
  :after magit
  :init
  (progn
    (setq forge-database-file (concat warmacs-cache-dir
                                "forge-database.sqlite")
      forge-add-default-bindings nil)
    (warmacs/set-major-mode-leader-keys
      :major-modes 'forge-topic-mode
      "a" 'forge-edit-topic-assignees
      "c" 'forge-create-post
      "C" 'forge-checkout-pullreq
      "b" 'forge-browse-topic
      "d" 'forge-delete-comment
      "e" 'forge-edit-post
      "m" 'forge-edit-topic-marks
      "M" 'forge-create-mark
      "n" 'forge-edit-topic-note
      "r" 'forge-edit-topic-review-requests
      "s" 'forge-edit-topic-state
      "t" 'forge-edit-topic-title
      "u" 'forge-copy-url-at-point-as-kill)
    (warmacs/set-major-mode-leader-keys
      :major-modes 'forge-post-mode
      warmacs-major-mode-leader-key 'forge-post-submit
      "c" 'forge-post-submit
      "k" 'forge-post-cancel
      "a" 'forge-post-cancel)))


;; highlight changes
(use-package git-gutter-fringe
  :defer 4
  :diminish git-gutter-mode
  :config (global-git-gutter-mode 1))