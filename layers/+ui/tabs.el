;;; +ui/tabs.el -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "⚠")
  (centaur-tabs-cycle-scope 'tabs)
  :hook
  ((prog-mode text-mode) . centaur-tabs-mode)
  ((dired-mode
    dashboard-mode
    term-mode
    calendar-mode
    org-agenda-mode
    helpful-mode) . centaur-tabs-local-mode)
  :config
  (unless (daemonp)
    (setq centaur-tabs-set-bar 'left))

  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  :general
  ("C-c t" '(:ignore t :which-key "tabs"))
  ("C-c t s"  'centaur-tabs-switch-group)
  ("C-c t p"  'centaur-tabs-group-by-projectile-project)
  ("C-c t g"  'centaur-tabs-group-buffer-groups)
  (:keymaps 'evil-normal-state-map
            "g t"      'centaur-tabs-forward
            "g T"      'centaur-tabs-backward
            "g C-t"    'centaur-tabs-move-current-tab-to-right
            "g C-S-t"  'centaur-tabs-move-current-tab-to-left))

(provide 'layer/+ui/tabs)
