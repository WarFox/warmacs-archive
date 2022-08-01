;; terminal.el -*- lexical-binding: t; -*-

(use-package vterm
  :general
  (warmacs/leader-menu-project
    "'" 'projectile-run-vterm))

(use-package multi-vterm
  :general
  (warmacs/leader-keys
    "'" 'multi-vterm))

(provide 'layer/terminal)
