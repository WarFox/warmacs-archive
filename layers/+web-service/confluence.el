;; -*- lexical-binding: t; -*-

(use-package confluence
    :after org
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

    (defun warmacs--confluence-remove-save-hook ()
      "Remove the save to confluence functions from write hook."
      (remove-hook 'write-contents-hooks 'cfln-save-page))

    :config
    ;; remove the hook on buffer save that automatically store the buffer
    ;; in confluence, it creates a lot of useless revision in a page history.
    (advice-add 'confluence-base-mode-init
                  :after 'warmacs--confluence-remove-save-hook)
    (warmacs/local-leader-keys
        :major-modes t
        :keymaps '(confluence-mode-map
                    confluence-xml-mode-map
                    confluence-search-mode-map)
        "s" 'warmacs/confluence-save-to-confluence-minor-edit
        "S" 'warmacs/confluence-save-to-confluence-major-edit
        "TAB" 'confluence-toggle-page-content-type)

        (require 'ox-confluence)
        (warmacs/local-leader-keys
          :major-modes t
          :keymaps 'org-mode-map
          "ec" 'org-confluence-export-as-confluence))

(provide 'layer/+web-service/confluence)
