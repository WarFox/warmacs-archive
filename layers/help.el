;;; help.el -*- lexical-binding: t; -*-

(use-package helpful
  :general
  (warmacs/local-leader-keys
    "h" 'helpful-at-point)
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

(provide 'layer/help)
