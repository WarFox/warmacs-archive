; +lang/python.el -*- lexical-binding: t; -*-

;; Package-Requires: '(lsp)

;;; Code:’

;; TODO Python mode has issues
;; evil-define-key*: Key sequence , v o d starts with non-prefix key ,

;; declare the local leader menu
(warmacs/local-leader-menu python)

(use-package blacken
  :general
  (warmacs/local-leader-keys
    :keymaps '(python-mode-map)
    "=" '(:ignore t :which-key "format")
    "==" #'blacken-buffer))

(use-package pipenv)

(use-package lsp-pyright)

(use-package dap-python :ensure nil :straight nil)

(use-package python-mode
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  :hook
  (python-mode . lsp-deferred)
  (python-mode . blacken-mode)
  :init
  (tree-sitter-require 'python)
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
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
  ;;                           (lambda (capture-name)
  ;;                             (string= capture-name "keyword")))))

  ;; ;; Highlight Python docstrings with a different face.
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (add-function :before-until (local 'tree-sitter-hl-face-mapping-function)
  ;;                           (lambda (capture-name)
  ;;                             (pcase capture-name
  ;;                               ("doc" 'font-lock-comment-face))))))
  :general
  (warmacs/local-leader-menu-python
    "" '(:keymap lsp-command-map :which-key "lsp")
    "'" #'run-python
    "i" '(:ignore t :which-key "insert")
    "if" #'python-skeleton-if
    "id" #'python-skeleton-def)
    ;; "gb" #'xref-pop-marker-stack))
  (warmacs/local-leader-keys
    :keymaps '(python-mode-map) 
    "" '(:keymap lsp-command-map :package lsp :which-key "lsp")
    "=" '(:ignore t :which-key "format")
    "F" '(:ignore t :which-key "folders")
    "G" '(:ignore t :which-key "peek")
    "T" '(:ignore t :which-key "toggles")
    "a" '(:ignore t :which-key "actions")
    "b" '(:ignore t :which-key "build")
    "g" '(:ignore t :which-key "goto")
    "gb" #'xref-pop-marker-stack
    "h" '(:ignore t :which-key "help")
    "r" '(:ignore t :which-key "refactor")
    "v" '(:ignore t :which-key "venv")
    "w" '(:ignore t :which-key "workspace")))

(use-package poetry
  :general
  (warmacs/local-leader-keys
    :keymaps '(python-mode-map) 
    ;; "v" '(:ignore t :which-key "venv")
    ;; "vo" '(:ignore t :which-key "poetry")
    "bb"  #'poetry-build
    "vod" 'poetry-venv-deactivate
    "vow" 'poetry-venv-workon
    "vot" 'poetry-venv-toggle))

(provide-layer! +lang/python)
