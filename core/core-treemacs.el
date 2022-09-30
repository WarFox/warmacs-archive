;;; core-treemacs.el -*- lexical-binding: t; -*-

(use-package treemacs
  :after doom-themes
  :custom
  (doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode 0)
  (treemacs-git-mode 'deferred)
  ;; setup treemacs theme
  (doom-themes-treemacs-config))

(use-package treemacs-evil
  :after (treemacs evil)
  :init
  (require 'treemacs-evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-all-the-icons
  :after (treemacs))

(provide 'core-treemacs)
