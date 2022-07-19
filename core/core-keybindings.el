;;; core-keybindings.el -*- lexical-binding: t; -*-

(message "core-keybindings")

;; setup keybindings
(use-package which-key
  :init
  (which-key-mode)
  :diminish
  which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; general
(use-package general
  :custom
  (general-use-package-emit-autoloads t)
  :init
  (setq
    warmacs-leader-key "SPC"
    warmacs-major-mode-leader-key ",")
  :config
  (general-evil-setup)

;; Spacemacs-like menu
;; https://gist.github.com/progfolio/1c96a67fcec7584b31507ef664de36cc
;; https://www.reddit.com/r/emacs/comments/des3cl/comment/f2yw45k/?utm_source=share&utm_medium=web2x&context=3

  (general-create-definer warmacs/set-leader-keys
    :keymaps 'override
    :states  '(insert emacs normal hybrid motion visual operator)
    :prefix  warmacs-leader-key
    :non-normal-prefix "S-SPC"
    "" '(:ignore t :whick-key "leader key"))

  (general-create-definer warmacs/set-major-mode-leader-keys
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix warmacs-major-mode-leader-key
    :non-normal-prefix "S-SPC ,"
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping warmacs/set-leader-keys.
  Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
      (general-create-definer ,(intern (concat "+general-global-" name))
        :wrapping warmacs/set-leader-keys
        :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
        :infix ,infix-key
        :wk-full-keys nil
        "" '(:ignore t :which-key ,name))
      (,(intern (concat "+general-global-" name))
        ,@body)))

  (warmacs/set-leader-keys
    "!" 'shell-command
    ":" 'eval-expression
    "SPC" 'counsel-M-x)

  (general-def
    "TAB" 'indent-for-tab-command)

  (+general-global-menu! "buffers" "b")
  (+general-global-menu! "files" "f")
  (+general-global-menu! "help" "h")
  (+general-global-menu! "insert" "i")
  (+general-global-menu! "registers/rings" "r")
  (+general-global-menu! "search" "s")
  (+general-global-menu! "toggles/themes" "T")

  (+general-global-buffers
    "d"  'kill-current-buffer
    "o" '((lambda () (interactive) (switch-to-buffer nil))
          :which-key "other-buffer")
    "b"  'counsel-switch-buffer ;'list-buffers
    "p"  'previous-buffer
    "r"  'rename-buffer
    "m" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n"  'next-buffer
    "s" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
          :which-key "scratch-buffer")
    "TAB" '((lambda () (interactive) (switch-to-buffer nil))
            :which-key "other-buffer"))

;; Make <escape> quit as much as possible
  (general-def minibuffer-local-map
    (kbd "<escape>") 'keyboard-escape-quit)
  (general-def minibuffer-local-ns-map
    (kbd "<escape>") 'keyboard-escape-quit)
  (general-def minibuffer-local-completion-map
    (kbd "<escape>") 'keyboard-escape-quit)
  (general-def minibuffer-local-must-match-map
    (kbd "<escape>") 'keyboard-escape-quit)
  (general-def minibuffer-local-isearch-map
    (kbd "<escape>") 'keyboard-escape-quit))

(provide 'core-keybindings)