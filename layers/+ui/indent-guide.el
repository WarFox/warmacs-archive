;;; ui/indent-guides.el -*- lexical-binding: t; -*-

(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-suppress-auto-error t)
  :config

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook 'org-mode-local-vars-hook
    (lambda ()
      (and highlight-indent-guides-mode
           (bound-and-true-p org-indent-mode)
           (highlight-indent-guides-mode -1)))))

(provide 'layer/+ui/indent-guide)
