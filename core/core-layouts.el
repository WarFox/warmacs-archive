;; -*- lexical-binding: t; -*-

;;;###autoload
(defun warmacs--layouts ()
  (interactive)
  (message "layouts"))

(use-package eyebrowse
  :general
  (warmacs/leader-keys
    "l" 'warmacs--layouts))

(provide 'core-layouts)
