;; auto-completion -*- lexical-binding: t; -*-

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode shell-mode eshell-mode) . corfu-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package cape)

(use-package yasnippet
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(use-package auto-yasnippet
  :after 'yasnippet)

;; https://oremacs.com/2015/01/30/auto-yasnippet/

(use-package tempo
  :straight (:type built-in)
  :hook
  ((org-mode prog-mode markdown-mode) . (lambda () (require 'tempo)))
  :init
  (defun try-tempo-complete-tag (old)
    (unless old
      (tempo-complete-tag)))
    (add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag))

(provide 'layer/+editor/auto-completion)

