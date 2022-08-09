;; -*- lexical-binding: t; -*-

;; Package-Requirements: '(lsp)

(use-package blacken)

(use-package pipenv)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . lsp-deferred))

(use-package dap-mode
  :config
  (require 'dap-python))

(use-package python-mode)

(use-package poetry
    :defer t
    :commands (poetry-venv-toggle
               poetry-tracking-mode)
    :general
    (warmacs/local-leader-keys
      :keymaps 'python-mode-map
      "vod" 'poetry-venv-deactivate
      "vow" 'poetry-venv-workon
      "vot" 'poetry-venv-toggle))

(provide '+lang/python)
