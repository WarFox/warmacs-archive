(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; (use-package doom-themes
;;   :demand t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq
;;     doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;     doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; or for treemacs users
;;   (doom-themes-treemacs-config)
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-colors") ; uses all the icons
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; load default theme
  (load-theme 'doom-one t))


(use-package centaur-tabs
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "⚠")
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (progn
    (unless (daemonp)
      (setq centaur-tabs-set-bar 'left))

    (centaur-tabs-headline-match)
    (centaur-tabs-group-by-projectile-project)
    (centaur-tabs-mode t)

    (which-key-add-keymap-based-replacements evil-normal-state-map  "C-c t" "tab"))
  :bind
  (:map evil-normal-state-map
    ("g t"     . centuar-tabs-forward)
    ("g T"     . centuar-tabs-backward)
    ("g C-t"   . centaur-tabs-move-current-tab-to-right)
    ("g C-S-t" . centaur-tabs-move-current-tab-to-left))
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups))

(message "core-ui")

(provide 'core-ui)
