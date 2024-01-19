;; +lang/sh.el -*- lexical-binding: t; -*-


;; (use-package sh-script ; built-in
;;   :straight (:type built-in)
;;   :mode ("\\.bats\\'" . sh-mode)
;;   :mode ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
;;   :mode ("/bspwmrc\\'" . sh-mode)
;;   :hook
;;   ;; Fontify delimiters by depth
;;   (sh-mode . #'rainbow-delimiters-mode)
;;   :config
;;   ;; autoclose backticks
;;   (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p)))

;; TODO parameter or option for customising the required shell
(use-package fish-mode)

(provide-layer! +lang/sh)

;; +lang/sh.el ends here
