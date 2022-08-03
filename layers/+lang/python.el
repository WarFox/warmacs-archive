;; -*- lexical-binding: t; -*-

;; Package-Requirements: '(lsp)

(use-package blacken)

(use-package poetry)

(use-package pipenv)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . lsp-deferred))

;; (use-package dap-python)

(provide '+lang/python)
