;; org.el -*- lexical-binding: t; -*-

(warmacs/local-leader-menu org)

(warmacs/leader-menu-applications
  "o" '(:ignore t :which-key "org"))

(use-package org
  :defer 5
  :config
  (add-to-list 'org-export-backends 'beamer)
  (add-to-list 'org-export-backends 'man)
  (add-to-list 'org-export-backends 'md)
  :general
  (warmacs/local-leader-menu-org
   ;; :keymaps 'org-mode-map
   "a" 'org-agenda
   "e" '(:ignore t :which-key "export")
   "ee" 'org-export-dispatch))

(use-package org-contrib
  :after 'org)

(use-package org-indent
  :straight (:type built-in))

(use-package org-tempo
  :straight (:type built-in))

;; org-modules
;; org-export-backends

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/Dropbox/gyan/"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "daily" plain (function org-roam-capture--get-point) ""
      :immediate-finish t
      :file-name "journals/%<%Y-%m-%d>"
      :head "#+title: %<<%Y-%m-%d>>")))
  :config
  (org-roam-db-autosync-enable)
  :general
  (warmacs/local-leader-menu-org
    "r" '(:ignore t :which-key "org-roam" )
    "ri"  #'org-roam-node-insert
    "rf" #'org-roam-node-find
    "rs" #'org-roam-db-sync)

  (warmacs/leader-menu-applications
    "or" '(:ignore t :which-key "org-roam" )
    "ri"  #'org-roam-node-insert
    "orf" #'org-roam-node-find
    "ors" #'org-roam-db-sync))

(provide-layer! +emacs/org)
