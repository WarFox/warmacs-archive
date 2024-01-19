;; +tools/docker.el -*- lexical-binding: t; -*-

(use-package docker)

(use-package tramp-container)

(use-package dockerfile-mode
  :init
  (add-hook 'dockerfile-mode-local-vars-hook #'lsp! 'append)
  :general
  (warmacs/local-leader-menu dockerfile
      "b" '(:ignore t :which-key "build")
      "bb" #'dockerfile-build-buffer
      "bB" #'dockerfile-build-no-cache-buffer))

(provide-layer! +tools/docker)
