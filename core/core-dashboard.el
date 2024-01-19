;;; core-dashboard.el -*- lexical-binding: t; -*-

(message "core-dashboard")

(use-package dashboard
  :commands (dashboard-refresh-buffer dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Welcome to Warmacs")
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-projects-switch-function #'projectile-switch-project)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (agenda . 5)))
  :hook (after-init . dashboard-setup-startup-hook)
  :init
  (warmacs/leader-menu-buffers
    "h" #'dashboard-refresh-buffer))

(provide 'core-dashboard)
