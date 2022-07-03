(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(+general-global-menu! "+zoom" "z"
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (+general-global-menu! "files" "f"
	    ;; File path
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
          "r" 'projectile-recentf
          "R" 'projectile-replace
	  "t" 'treemacs
          "T" 'projectile-test-project
          "v" 'projectile-vc)
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(put 'dired-find-alternate-file 'disabled nil)
