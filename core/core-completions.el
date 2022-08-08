;;; completion.el -*- lexical-binding: t; -*-

;; default modern completion framework

(use-package consult
  :custom
  (register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :general
    ([remap apropos]                       #'consult-apropos)
    ([remap bookmark-jump]                 #'consult-bookmark)
    ([remap evil-show-marks]               #'consult-mark)
    ([remap evil-show-registers]           #'consult-register)
    ([remap goto-line]                     #'consult-goto-line)
    ([remap imenu]                         #'consult-imenu)
    ([remap locate]                        #'consult-locate)
    ([remap load-theme]                    #'consult-theme)
    ([remap man]                           #'consult-man)
    ([remap recentf-open-files]            #'consult-recent-file)
    ([remap switch-to-buffer]              #'consult-buffer)
    ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
    ([remap yank-pop]                      #'consult-yank-pop)

  ("C-s" 'consult-line)

  (warmacs/leader-keys
    ;; search
    "/"   'consult-ripgrep)

  (warmacs/leader-menu-search
    "gg" 'consult-git-grep))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package all-the-icons-completion
  :hook
  (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode 1))

(use-package embark)

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :init
  (vertico-mode 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  ;; TODO
  ;; ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-category-defaults nil))

(provide 'core-completions)
