;;; core-start.el -*- lexical-binding: t; -*-

(message "core-start")

(require 'core-layers)

;; There's a chance the user will later use package.el or straight in this
;; interactive session. If they do, make sure they're properly initialized
;; when they do.
;; (autoload 'warmacs-initialize-packages "core-packages")
;; (eval-after-load 'package '(require 'core-packages))
;; (eval-after-load 'straight '(warmacs-initialize-packages))

;; Load all things.
(warmacs-initialize-layers)

(provide 'core-start)
