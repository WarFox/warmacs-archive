;; org.el -*- lexical-binding: t; -*-

(use-package org
  :defer 5
  :config
  (add-to-list 'org-export-backends 'beamer)
  (add-to-list 'org-export-backends 'man)
  (add-to-list 'org-export-backends 'md)
  :general
  (warmacs/local-leader-keys
    :keymaps 'org-mode-map
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

(provide 'layer/org)
