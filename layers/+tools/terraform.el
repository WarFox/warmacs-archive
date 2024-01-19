;; +tools/terraform.el -*- lexical-binding: t; -*-

(defvar terraform-auto-format-on-save nil
    "If non-nil then call `terraform fmt' before saving the terraform buffer.")

(use-package terraform-mode
  :config
  (when terraform-auto-format-on-save
    (add-hook 'terraform-mode-hook
              'terraform-format-on-save-mode)))

(provide-layer! +tools/terraform)
