;;; core-keybindings.el -*- lexical-binding: t; -*-

(message "core-keybindings")

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

; Use the left alt/option key as meta
; Use the right alt/option key for stock Apple stuff
; e.g Use the right alt/option-option key on Mac for inputing special characters like #
(with-system darwin
  (setq ns-alternate-modifier 'meta)
  (setq ns-right-alternate-modifier 'none))

;; Setup evil and base keybindings for menu

(warmacs/leader-keys
  "!" #'shell-command
  ":" #'eval-expression
  "SPC" #'execute-extended-command)

(defun switch-to-message-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

;; Declare global menus
(warmacs/leader-menu "applications" "a"
  "p" #'list-processes
  "P" #'proced)

(warmacs/leader-menu "buffers" "b"
  "b"  #'consult-buffer
  "d"  'kill-current-buffer
  "e"  'erase-buffer
  "m" '(switch-to-message-buffer
        :which-key "messages-buffer")
  "n"  'next-buffer
  "o" '((lambda () (interactive) (switch-to-buffer nil))
        :which-key "other-buffer")
  "p"  'previous-buffer
  "r"  'rename-buffer
  "s" '(switch-to-scratch-buffer
        :which-key "scratch-buffer")
  "TAB" '(switch-to-other-buffer
          :which-key "other-buffer"))

(warmacs/leader-menu "files" "f"
  "f"  #'find-file
  "el" #'find-library
  "l"  #'locate
  "r"  #'recentf-open-files
  "y" '(:ignore t :which-key "yank"))

(warmacs/leader-menu "help" "h"
  "d" '(:ignore t :which-key "describe")
  "da" 'aprops
  "df" 'describe-function
  "dF" 'describe-face
  "dv" 'describe-variable
  "?"  'describe-bindings
  "i"  'info-lookup-symbol
  "m"  'man
  "g" '(:ignore t :which-key "git"))

(warmacs/leader-menu "git" "g"
  "f"  '(:ignore t :which-key "file"))

(warmacs/leader-menu "insert" "i"
  ;; TODO
  ;;"u" 'counsel-unicode-char)
  )

(warmacs/leader-menu "jump/join/split" "j")

(warmacs/leader-menu "project" "p")

(warmacs/leader-menu "registers/rings" "r"
  "y" #'yank-pop)

(warmacs/leader-menu "search" "s"
  "g"  '(:ignore t :which-key "git"))

(warmacs/leader-menu "toggles/themes" "T"
  "s" #'load-theme)

(warmacs/leader-menu "windows" "w")
(warmacs/leader-menu "text" "x")
(warmacs/leader-menu "zoom" "z")

;; Make <escape> quit as much as possible
(general-def
  :keymaps '(minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
  "<escape>" #'keyboard-escape-quit)

;; Restart / Quit
(use-package restart-emacs
  :commands (restart-emacs)
  :init
  (progn
    (defun warmacs/restart-emacs (&optional args)
    (interactive)
    (restart-emacs args))

    (defun warmacs/kill-emacs (prompt &optional args)
    (interactive)
    (if (not prompt)
        (save-some-buffers nil t))
    (kill-emacs args)))
  :general
  (warmacs/leader-menu "quit" "q"
    "d" '((lambda (&optional args)
            (interactive)
            (warmacs/restart-emacs (cons "--debug-init" args)))
           :which-key "restart-emacs-debug-init")
    "R" 'warmacs/restart-emacs
    "t" '((lambda (&optional args)
             (interactive)
             (warmacs/restart-emacs (cons "--timed-requires" args)))
            :which-key "restart-emacs-timed-requires")
    "T" '((lambda (&optional args)
            (interactive)
            (warmacs/restart-emacs (cons "--adv-timers" args)))
           :which-key "restart-emacs-adv-timers")
    "q" '((lambda (&optional args)
        (interactive)
            (warmacs/kill-emacs t args))
           :which-key "prompt-kill-emacs")
    "Q" 'warmacs/kill-emacs))

(use-package elisp-mode
   ;;this is a built in package, so we don't want to try and install it
   :straight (:type built-in)
   :general
   (warmacs/local-leader-keys
     ;;specify the major modes these should apply to:
     :major-modes '(emacs-lisp-mode lisp-interaction-mode)
     ;;and the keymaps:
     :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
     "e" '(:ignore t :which-key "eval")
     "eb" 'eval-buffer
     "ed" 'eval-defun
     "ee" 'eval-expression
     "ep" 'pp-eval-last-sexp
     "es" 'eval-last-sexp
     "i" 'elisp-index-search))

(general-def
  "TAB" 'indent-for-tab-command
  [remap dabbrev-expand] 'hippie-expand)

(provide 'core-keybindings)
