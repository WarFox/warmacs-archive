;; org.el -*- lexical-binding: t; -*-

(use-package org
  :defer 5
  :config
  (require 'org-tempo)
  :general
  (warmacs/local-leader-keys
    :keymaps 'org-mode-map
    "a" 'org-agenda
    "e" '(:ignore t :which-key "export")
    "ee" 'org-export-dispatch))

(use-package org-contrib
  :after 'org
  :general
  (warmacs/local-leader-keys
    "ec" 'org-confluence-export-as-confluence))

(provide 'layer/org)
