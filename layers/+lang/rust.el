;; +lang/rust.el -*- lexical-binding: t; -*-

(use-package rust-mode
  :hook
  (rust-mode . lsp))

(provide 'layer/+lang/rust)
