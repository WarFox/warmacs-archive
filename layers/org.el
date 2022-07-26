;; org.el -*- lexical-binding: t; -*-

(use-package org
  :defer 5
  :init)

(require 'org-tempo)

(use-package org-contrib
  :after org)

(use-package confluence
    :demand t
    :init
    (defun warmacs/confluence-save-to-confluence-minor-edit ()
    "Store a minor edit to the Confluence server."
    (interactive)
    (let ((confluence-save-page-minor-edits t))
        (cfln-save-page)))

    (defun warmacs/confluence-save-to-confluence-major-edit ()
    "Store a major edit to the Confluence server."
    (interactive)
    (let ((confluence-save-page-minor-edits nil))
        (cfln-save-page)))

    (defun warmacs//confluence-remove-save-hook ()
    "Remove the save to confluence functions from write hook."
    (remove-hook 'write-contents-hooks 'cfln-save-page))

    :config
    (progn
      ;; remove the hook on buffer save that automatically store the buffer
      ;; in confluence, it creates a lot of useless revision in a page history.
      (dolist (mode '(confluence-mode
                      confluence-xml-mode
                      confluence-search-mode))
        (warmacs/set-local-leader-keys
          :major-modes mode
          :keymaps 'org-mode-map
          "s" 'warmacs/confluence-save-to-confluence-minor-edit)
        (warmacs/set-local-leader-keys
          :major-modes mode
          :keymaps 'org-mode-map
          "S" 'warmacs/confluence-save-to-confluence-major-edit)
        ;; (warmacs/set-local-leader-keys
        ;;   :major-modes mode
        ;;   "TAB" 'confluence-toggle-page-content-type)
        )

        (require 'ox-confluence)
      ;; :general
        ;; (warmacs/set-local-leader-keys
        ;;     :major-modes 'org-mode
        ;;     :keymaps 'org-mode-map
        ;;         "e" '(:ignore t :which-key "export")
        ;;         )
        ))


(warmacs/set-local-leader-keys
    :major-modes 'org-mode
    :keymaps 'org-mode-map
    "a" 'org-agenda
    "e" '(:ignore t :which-key "export")
    "ec" 'org-confluence-export-as-confluence
    "ee" 'org-export-dispatch)

(provide 'layer/org)
