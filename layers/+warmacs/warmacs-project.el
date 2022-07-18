(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (warmacs/set-leader-keys
    "p" '(:keymap projectile-command-map :package projectile))
  (setq projectile-switch-project-action #'projectile-dired))

(+general-global-menu! "buffer" "b"
  "d"  'kill-current-buffer
  "o" '((lambda () (interactive) (switch-to-buffer nil))
         :which-key "other-buffer")
  "p"  'previous-buffer
  "r"  'rename-buffer
  "m" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
         :which-key "messages-buffer")
  "n"  'next-buffer
  "s" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
         :which-key "scratch-buffer")
  "TAB" '((lambda () (interactive) (switch-to-buffer nil))
           :which-key "other-buffer"))

(spacemacs/set-leader-keys
        ;; File path
        "fyC" 'spacemacs/projectile-copy-file-path-with-line-column
        "fyD" 'spacemacs/projectile-copy-directory-path
        "fyL" 'spacemacs/projectile-copy-file-path-with-line
        "fyY" 'spacemacs/projectile-copy-file-path
        ;; Project
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "p%" 'projectile-replace-regexp
        "pa" 'projectile-toggle-between-implementation-and-test
        "pb" 'projectile-switch-to-buffer
        "pc" 'projectile-compile-project
        "pd" 'projectile-find-dir
        "pD" 'projectile-dired
        "pe" 'projectile-edit-dir-locals
        "pf" 'projectile-find-file
        "pF" 'projectile-find-file-dwim
        "pg" 'projectile-find-tag
        "pG" 'projectile-regenerate-tags
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "pp" 'projectile-switch-project
        "pr" 'projectile-recentf
        "pR" 'projectile-replace
        "pT" 'projectile-test-project
        "pv" 'projectile-vc)
