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
  :after (lsp lsp-treemacs tree-sitter)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  :hook
  ((python-mode . #'lsp)
   (python-mode . #'lsp-enable-which-key-integration)
   (python-mode . #'tree-sitter-mode)
   (python-mode . #'tree-sitter-hl-mode)
   (python-mode . #'tree-sitter-indent-mode))
  :config
  (tree-sitter-require 'python)
  (require 'dap-python)
  ;; Highlight only keywords in Python.
  (add-hook 'python-mode-hook
            (lambda ()
              (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                            (lambda (capture-name)
	                          (string= capture-name "keyword")))))

;; Highlight Python docstrings with a different face.
(add-hook 'python-mode-hook
  (lambda ()
    (add-function :before-until (local 'tree-sitter-hl-face-mapping-function)
      (lambda (capture-name)
	(pcase capture-name
	  ("doc" 'font-lock-comment-face))))))

  :general
  (warmacs/local-leader-menu-python
    "" '(:keymap lsp-command-map :which-key "lsp")
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
