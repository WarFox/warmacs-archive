;; core-projects.el -*- lexical-binding: t; -*-

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode 1)
  :custom ((projectile-completion-system 'ivy))
  :init
  (warmacs/leader-menu-files
	  ;; File path
    "yC" 'warmacs/projectile-copy-file-path-with-line-column
    "yD" 'warmacs/projectile-copy-directory-path
    "yL" 'warmacs/projectile-copy-file-path-with-line
    "yY" 'warmacs/projectile-copy-file-path)

  (warmacs/leader-menu-project
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
    "l" 'projectile-switch-open-project
    "r" 'projectile-recentf
    "R" 'projectile-replace
    "T" 'projectile-test-project
    "v" 'projectile-vc)
  (setq projectile-switch-project-action #'projectile-find-file-dwim))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1)
  :general
  (warmacs/leader-menu-project
    "p" 'counsel-projectile-switch-project))

(put 'dired-find-alternate-file 'disabled nil)

(use-package persp-projectile)

(provide 'core-projects)
