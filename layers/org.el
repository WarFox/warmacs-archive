;; org.el -*- lexical-binding: t; -*-

(use-package org
  :defer 5
  :config
  (require 'org-tempo)
  :general
  (warmacs/set-local-leader-keys
    :major-modes 'org-mode
    :keymaps 'org-mode-map
    "a" 'org-agenda
    "e" '(:ignore t :which-key "export")
    "ec" 'org-confluence-export-as-confluence
    "ee" 'org-export-dispatch))

(use-package org-contrib
  :after org)


(provide 'layer/org)
