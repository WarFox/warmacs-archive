;; +lang/emacs-lisp.el -*- lexical-binding: t; -*-

(use-package elisp-mode
  ;;this is a built in package, so we don't want to try and install it
  :straight (:type built-in)
  :hook
  ;; Allow folding of outlines in comments
  (emacs-lisp-mode . outline-minor-mode)
  :general
  (warmacs/local-leader-keys
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e" '(:ignore t :which-key "eval")
    "eb" #'eval-buffer
    "ed" #'eval-defun
    "ee" #'eval-expression
    "ep" #'pp-eval-last-sexp
    "es" #'eval-last-sexp
    "h" '(:ignore t :which-key "help")
    "hh" #'helpful-at-point
    "i"  '(:ignore t :which-key "insert")
    "il" #'elisp-enable-lexical-binding
    "/"  #'elisp-index-search))

(provide-layer! +lang/emacs-lisp)
