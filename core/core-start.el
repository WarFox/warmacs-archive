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

;;; Don't edit below text, emacs uses this for custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cider-shadow-cljs-default-options . "app")
     (org-global-properties
      (header-args:sql . ":dbhost localhost :database uk_data_streams_dev :engine postgresql :exports result"))
     (org-roam-db-location . "~/My Drive/research/docs/org-roam.db")
     (org-roam-directory . "~/My Drive/research/docs"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
