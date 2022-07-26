;;; help.el -*- lexical-binding: t; -*-

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :general
  (warmacs/set-local-leader-keys
    "h" 'helpful-at-point)
  (warmacs/leader-menu-help
    "h" 'helpful-at-point)
  (:keymaps 'helpful-mode-map
   :states 'normal
   "q" 'quit-window)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(provide 'layer/help)
