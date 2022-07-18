(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :general
  (warmacs/set-major-mode-leader-keys
    "h" 'helpful-at-point)
  (warmacs/set-leader-keys
    "hh" 'helpful-at-point)
  (:keymaps 'helpful-mode-map
	    :states 'normal
	    (kbd "q") 'quit-window)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
