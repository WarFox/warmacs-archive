
(use-package centaur-tabs
  :demand t
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "⚠")
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (progn
    (unless (daemonp)
      (setq centaur-tabs-set-bar 'left))

    (centaur-tabs-headline-match)
    (centaur-tabs-group-by-projectile-project)
    (centaur-tabs-mode t))
   :bind
   (:map evil-normal-state-map
         ("g t"     . centaur-tabs-forward)
         ("g T"     . centaur-tabs-backward)
         ("g C-t"   . centaur-tabs-move-current-tab-to-right)
         ("g C-S-t" . centaur-tabs-move-current-tab-to-left))
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups))

