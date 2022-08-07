;; auto-completion -*- lexical-binding: t; -*-

(use-package company)

(use-package yasnippet
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(use-package auto-yasnippet
  :after 'yasnippet)

;; https://oremacs.com/2015/01/30/auto-yasnippet/

(defun try-tempo-complete-tag (old)
  (unless old
    (tempo-complete-tag)))

(add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag)

(provide 'layer/auto-completion)
