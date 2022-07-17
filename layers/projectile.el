(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (+general-global-files
	  ;; File path
    "y" '(:ignore t :which-key "yank")
	  "yC" 'warmacs/projectile-copy-file-path-with-line-column
	  "yD" 'warmacs/projectile-copy-directory-path
	  "yL" 'warmacs/projectile-copy-file-path-with-line
	  "yY" 'warmacs/projectile-copy-file-path)

  (+general-global-menu! "project" "p"
    ;; Project
    "!" 'projectile-run-shell-command-in-root
    "&" 'projectile-run-async-shell-command-in-root
    "%" 'projectile-replace-regexp
    "a" 'projectile-toggle-between-implementation-and-test
    "b" 'projectile-switch-to-buffer
    "c" 'projectile-compile-project
    "d" 'projectile-find-dir
    "D" 'projectile-dired
    "e" 'projectile-edit-dir-locals
    "f" 'projectile-find-file
    "F" 'projectile-find-file-dwim
    "g" 'projectile-find-tag
    "G" 'projectile-regenerate-tags
    "I" 'projectile-invalidate-cache
    "k" 'projectile-kill-buffers
    "p" 'projectile-switch-project
    "o" 'projectile-switch-open-project
    "r" 'projectile-recentf
    "R" 'projectile-replace
    "t" 'treemacs
    "T" 'projectile-test-project
    "v" 'projectile-vc)
  (setq projectile-switch-project-action #'projectile-find-file-dwim))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(put 'dired-find-alternate-file 'disabled nil)
