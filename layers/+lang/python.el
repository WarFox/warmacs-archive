;; -*- lexical-binding: t; -*-

;; Package-Requirements: '(lsp)

(use-package blacken)

(use-package pipenv)

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred))

(use-package dap-mode
  :config
  (require 'dap-python))

(use-package python-mode
  :after lsp
  :custom
  (python-indent-offset 4)
  :init
  (tree-sitter-require 'python)
  :hook
  (python-mode . #'tree-sitter-indent-mode)
  :general
  (warmacs/local-leader-keys
    :keymaps 'python-mode-map
    "e" '(:ignore t :which-key "errors")
    "eL" 'lsp-treemacs-error-list))

(use-package poetry
    :commands (poetry-venv-toggle poetry-tracking-mode)
    :general
    (warmacs/local-leader-keys
      :keymaps 'python-mode-map
      "v" '(:ignore t :which-key "venv")
      "vod" 'poetry-venv-deactivate
      "vow" 'poetry-venv-workon
      "vot" 'poetry-venv-toggle))

(provide 'layer/+lang/python)
