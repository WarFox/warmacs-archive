;; -*- lexical-binding: t; -*-

;; tree-sitter

(use-package tree-sitter
  :hook
  (tree-sitter-after-on . #'tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs)

(use-package tree-sitter-indent
  :commands 'tree-sitter-indent-mode)

;; lsp
;; TODO need a lot of work here to get default which-key menu enabled for
;; all languages with support for lsp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-lens-mode)
   (lsp-mode . lsp-enable-which-key-integration))
  :general
  (:keymaps 'lsp-command-map
  "=" '(:ignore t :which-key "format")
  "=b" #'lsp-format-buffer
  "=o" #'lsp-organize-imports
  "=r" #'lsp-format-region))

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
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode 1))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; smartparens
(use-package smartparens
  :init
  (require 'smartparens-config))

;; Help everywhere for emacs-lisp
(use-package helpful
  :general 
  (warmacs/leader-menu-help
    "h" 'helpful-at-point)
  (:keymaps 'helpful-mode-map
   :states 'normal
   "q" 'quit-window)
  ([remap describe-function] #'helpful-function)
  ([remap describe-symbol] #'helpful-symbol)
  ([remap describe-variable] #'helpful-variable)
  ([remap describe-command] #'helpful-command)
  ([remap describe-key] #'helpful-key))

(provide 'core-ide)
