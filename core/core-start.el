;;; core-start.el -*- lexical-binding: t; -*-

(message "core-start")

(require 'core-layers)

;; There's a chance the user will later use package.el or straight in this
;; interactive session. If they do, make sure they're properly initialized
;; when they do.
(autoload 'warmacs-initialize-layers "core-layers")
(eval-after-load 'package '(require 'core-layers))
(eval-after-load 'straight '(warmacs-initialize-layers))

;; Load all things.
(warmacs-initialize-layers)

;; Start deamon server
(require 'server)
(unless (server-running-p)
  (message "starting emacs server")
  (server-start))

(provide 'core-start)
