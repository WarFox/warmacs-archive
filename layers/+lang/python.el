;; +lang/python.el -*- lexical-binding: t; -*-

;; Package-Requires: '(lsp)

;;; Code:’

;; declare the local leader menu
(warmacs/local-leader-menu python
    "=" '(:ignore t :which-key "format")
    "e" '(:ignore t :which-key "errors")
    "g" '(:ignore t :which-key "goto")
    "r" '(:ignore t :which-key "refactor")
    "v" '(:ignore t :which-key "venv")
    "vo" '(:ignore t :which-key "poetry"))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :general
  (warmacs/local-leader-menu-python
    "==" #'blacken-buffer))

(use-package pipenv)

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred))

(use-package dap-python :ensure nil :straight nil)

(use-package python-mode
  :mode
  (("\\.py\\'" . python-mode))
  :after (lsp lsp-treemacs)
  :custom
  (python-indent-offset 4)
  :hook
  (python-mode . #'tree-sitter-indent-mode)
  :init
  (tree-sitter-require 'python)
  :general
  (warmacs/local-leader-menu-python
    "l" '(:keymap lsp-command-map :which-key "lsp")
    "gb" #'xref-pop-marker-stack
    "gd" #'xref-find-definitions
    "ge" #'lsp-treemacs-errors-list
    "rr" #'lsp-rename) )

(use-package poetry
    :commands (poetry-venv-toggle poetry-tracking-mode)
    :general
    (warmacs/local-leader-menu-python
      "vod" 'poetry-venv-deactivate
      "vow" 'poetry-venv-workon
      "vot" 'poetry-venv-toggle))

(provide 'layer/+lang/python)
