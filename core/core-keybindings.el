;;; core-keybindings.el -*- lexical-binding: t; -*-

(message "core-keybindings")

;; Setup evil and base keybindings for menu 

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
  "<escape>" 'keyboard-escape-quit)
(general-def minibuffer-local-ns-map
  "<escape>" 'keyboard-escape-quit)
(general-def minibuffer-local-completion-map
  "<escape>" 'keyboard-escape-quit)
(general-def minibuffer-local-must-match-map
  "<escape>" 'keyboard-escape-quit)
(general-def minibuffer-local-isearch-map
  "<escape>" 'keyboard-escape-quit)

;; Restart / Quit
(use-package restart-emacs
  :demand t
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
  (+general-global-menu! "quit" "q"
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
   :ensure nil
   :straight nil
   :general
   (warmacs/set-major-mode-leader-keys
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

(provide 'core-keybindings)
