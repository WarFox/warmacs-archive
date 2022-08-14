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

  ;; setup treemacs theme
  (doom-themes-treemacs-config)

  ;; (general-def 'treemacs-mode-map
  ;;   "c"   'treemacs-create
  ;;   "d"   'treemacs-delet-file
  ;;   "o"    (general-key-dispatch 'treemacs-visit-node
  ;;                "a" 'treemacs-visit-node-ace)
  ;;   "t"         'treemacs-toggles
  ;;   "y"         'treemacs-copy
  ;;   "C-c C-p"   (general-key-dispatch 'treemacs-projects
  ;;                 "c" 'treemacs-projects-collapse))
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :init
  (general-def 'evil-treemacs-state-map
    ;; "d"         'treemacs-delet-file
    [return] #'treemacs-RET-action
    [tab]    #'treemacs-TAB-action
    "TAB"    #'treemacs-TAB-action
    ;; REVIEW Fix #1875 to be consistent with C-w {v,s}, but this should really
    ;;        be considered upstream.
    "o v"    #'treemacs-visit-node-horizontal-split
    "o s"    #'treemacs-visit-node-vertical-split))

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
