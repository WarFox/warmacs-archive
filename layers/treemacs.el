;;; treemacs.el -*- lexical-binding: t; -*-

(use-package treemacs
  :after doom-themes
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  (which-key-add-major-mode-key-based-replacements 'treemacs-mode
        "c"         "treemacs-create"
        "o"         "treemacs-visit-node"
        "oa"        "treemacs-visit-node-ace"
        "t"         "treemacs-toggles"
        "y"         "treemacs-copy"
        "C-c C-p"   "treemacs-projects"
        "C-c C-p c" "treemacs-projects-collapse")
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode 0)

  ;; setup treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config))

(use-package treemacs-evil
  :after (treemacs evil))

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

(provide 'layer/treemacs)
