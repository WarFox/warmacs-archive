;;; completion.el -*- lexical-binding: t; -*-

(use-package counsel
  :demand t
  :init
  (progn
    (defun warmacs/counsel-jump-in-buffer ()
      "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in org-mode"
      (interactive)
      (call-interactively
        (cond
          ((eq major-mode 'org-mode) 'counsel-org-goto)
          (t 'counsel-imenu)))))
  :custom
  ;; Enable better auto completion of counsel-find-file
  ;; by recognizing file at point.
  (counsel-find-file-at-point t)
  :config
  (progn
    ;; Temporarily handle older versions of ivy
    ;; https://github.com/abo-abo/swiper/pull/1863/files
    (unless (fboundp 'counsel--elisp-to-pcre)
      (defalias 'counsel--elisp-to-pcre 'counsel-unquote-regex-parens))

    (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
    ;; remaps built-in commands that have a counsel replacement
    (counsel-mode 1)
    ;; Set syntax highlighting for counsel search results

    (ivy-set-display-transformer 'counsel-search 'counsel-git-grep-transformer))

  :general
  (general-define-key
    "C-s" 'counsel-grep-or-swiper)

  (warmacs/leader-menu-files
    "f"  'counsel-find-file
    "el" 'counsel-find-library
    "L"  'counsel-locate)

  (warmacs/leader-menu-help
    "?"   'counsel-descbinds
    "gff" 'counsel-git
    "da" 'counsel-apropos
    "df" 'counsel-describe-function
    "dF" 'counsel-describe-face
    "dv" 'counsel-describe-variable
    "i"  'counsel-info-lookup-symbol
    "m"  'man)

  (warmacs/leader-menu-insert
   "u" 'counsel-unicode-char)

  (warmacs/leader-menu-registers/rings
    "y"  'counsel-yank-pop
    "m"  'counsel-mark-ring)

  (warmacs/set-leader-keys
    ;; search
    "/"   'counsel-rg)

  (warmacs/leader-menu-search
    "j"  'warmacs/counsel-jump-in-buffer
    "gg" 'counsel-git-grep)

  (warmacs/leader-menu-toggles/themes
    "s"  'counsel-load-theme))

(use-package ivy
  :general
  (general-def ivy-minibuffer-map
    "<escape>" 'minibuffer-keyboard-quit))

(use-package swiper)

(provide 'layer/completion)
