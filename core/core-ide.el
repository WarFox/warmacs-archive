;; -*- lexical-binding: t; -*-

;; tree-sitter

(use-package tree-sitter
  :hook
  (tree-sitter-after-on-hook . #'tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs)

(use-package tree-sitter-indent
  :commands 'tree-sitter-indent-mode)

;; lsp

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix (format "%s l" warmacs-local-leader-key))
  :hook ((python-mode . lsp)
         (scala-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands
  (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :after consult
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols))

(use-package lsp-treemacs
  :after treemacs
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(provide 'core-ide)
