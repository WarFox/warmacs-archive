;; -*- lexical-binding: t; -*-

(use-package yaml-mode
  :mode
  (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
   ("Procfile\\'" . yaml-mode))
  :general
  (:keymaps 'yaml-mode-map
   "\C-m" 'newline-and-indent))

(provide 'layer/+lang/yaml)
