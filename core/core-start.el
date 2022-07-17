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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
