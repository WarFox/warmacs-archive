;; (use-package magit
;;   :commands (magit-status magit-get-current-branch)
;;   :custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package magit
  :custom
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-completing-read-function 'ivy-completing-read)
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
 :config
    (progn
      ;; seems to be necessary at the time of release
      (require 'git-rebase)
      ;; bind function keys
      ;; (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
      (evilified-state-evilify-map magit-repolist-mode-map
        :mode magit-repolist-mode
        :bindings
        (kbd "gr") 'magit-list-repositories
        (kbd "RET") 'magit-repolist-status)
      ;; confirm/abort
      (when dotspacemacs-major-mode-leader-key
        (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
        (let ((mm-key dotspacemacs-major-mode-leader-key))
          (dolist (state '(normal motion))
            (evil-define-key state with-editor-mode-map
              (concat (kbd mm-key) (kbd mm-key)) 'with-editor-finish
              (concat (kbd mm-key) "a")    'with-editor-cancel
              (concat (kbd mm-key) "c")    'with-editor-finish
              (concat (kbd mm-key) "k")    'with-editor-cancel)
            (evil-define-key state magit-log-select-mode-map
              (concat (kbd mm-key) (kbd mm-key)) 'magit-log-select-pick
              (concat (kbd mm-key) "a")    'magit-log-select-quit
              (concat (kbd mm-key) "c")    'magit-log-select-pick
              (concat (kbd mm-key) "k")    'magit-log-select-quit))))
      ;; whitespace
      (define-key magit-status-mode-map (kbd "C-S-w")
        'spacemacs/magit-toggle-whitespace)
      ;; Add missing which-key prefixes using the new keymap api
      (when (spacemacs//support-evilified-buffer-p)
        (which-key-add-keymap-based-replacements magit-status-mode-map
          "gf"  "jump-to-unpulled"
          "gp"  "jump-to-unpushed"))
      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (setq magit-display-buffer-function
              'magit-display-buffer-fullframe-status-v1))
      (spacemacs|hide-lighter with-editor-mode)
      ;; Workaround for #12747 - org-mode
      (evil-define-key 'normal magit-blame-read-only-mode-map (kbd "RET") 'magit-show-commit)
      ;; Make sure that M-m still switch windows in all magit buffers
      (evil-define-key 'normal magit-section-mode-map (kbd "M-1") 'spacemacs/winum-select-window-1)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-2") 'spacemacs/winum-select-window-2)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-3") 'spacemacs/winum-select-window-3)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-4") 'spacemacs/winum-select-window-4)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-5") 'spacemacs/winum-select-window-5)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-6") 'spacemacs/winum-select-window-6)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-7") 'spacemacs/winum-select-window-7)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-8") 'spacemacs/winum-select-window-8)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-9") 'spacemacs/winum-select-window-9))
    :general
    (+general-global-menu! "git" "g"
        "b"  'spacemacs/git-blame-transient-state/body
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
  (major-mode-leader
    :major-modes 'gitignore-mode
    :keymaps
    "i" 'gitignore-templates-insert)
  (+general-global-git
    "fi" 'gitignore-templates-new-file))


(use-package forge
  :after magit
  :init
  (progn
    (setq forge-database-file (concat spacemacs-cache-directory
                                "forge-database.sqlite")
      forge-add-default-bindings nil)
    (spacemacs/set-leader-keys-for-major-mode 'forge-topic-mode
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
    (spacemacs/set-leader-keys-for-major-mode 'forge-post-mode
      dotspacemacs-major-mode-leader-key 'forge-post-submit
      "c" 'forge-post-submit
      "k" 'forge-post-cancel
      "a" 'forge-post-cancel)))
