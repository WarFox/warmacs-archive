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

(setq-default
 warmacs-layer-list '(all-the-icons
                      completion
                      help
                      ;; osx
                      projectile
                      tabs
                      toggles
                      treemacs
                      windows
                      zoom
                      +source-control/git
                      ))

(dolist (item warmacs-layer-list)
 (load (concat (file-name-directory warmacs-layers-dir)
               (symbol-name item))
       nil (not init-file-debug)))


(provide 'core-start)
