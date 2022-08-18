;; +lang/scala.el -*- lexical-binding: t; -*-

(use-package scala-mode
  :hook
  (scala-mode . lsp-deferred))

(provide-layer! +lang/scala)

