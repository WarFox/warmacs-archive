;; +lang/rust.el -*- lexical-binding: t; -*-

(use-package rust-mode
  :hook
  (rust-mode . lsp)
  :config
  (warmacs/local-leader-menu rust
    "" '(:keymap lsp-command-map :package lsp :which-key "lsp")
    "=" '(:ignore t :which-key "formatting")
    "F" '(:ignore t :which-key "folders")
    "G" '(:ignore t :which-key "peek")
    "T" '(:ignore t :which-key "toggle")
    "a" '(:ignore t :which-key "code actions")
    "h" '(:ignore t :which-key "help")
    "g" '(:ignore t :which-key "goto")
    "r" '(:ignore t :which-key "refactor")
    "w" '(:ignore t :which-key "workspace")))

(provide 'layer/+lang/rust)
