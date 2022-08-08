;; -*- lexical-binding: t; -*-

(use-package string-inflection
  :init
  (defhydra hydra-string-inflection (:timeout 4)
    "String Inflection"
    ("i" string-inflection-all-cycle "cycle")
    ("q" nil "finished" :exit t))
  :general
    (warmacs/leader-menu-text
      "i" '(:ignore t :which-key "inflection")
      "ic" 'string-inflection-lower-camelcase
      "iC" 'string-inflection-camelcase
      "ii" 'hydra-string-inflection/body
      "i-" 'string-inflection-kebab-case
      "ik" 'string-inflection-kebab-case
      "i_" 'string-inflection-underscore
      "iu" 'string-inflection-underscore
      "iU" 'string-inflection-upcase))

(use-package string-edit
  :general
  (warmacs/leader-keys "xe" 'string-edit-at-point)
  (warmacs/local-leader-keys
    :major-modes 'string-edit-mode
    :keymaps 'string-edit-mode-map
    "," 'string-edit-conclude
    "c" 'string-edit-conclude
    "a" 'string-edit-abort
    "k" 'string-edit-abort))

(use-package avy
  :commands (warmacs/avy-open-url warmacs/avy-goto-url avy-pop-mark avy-with)
  :custom
  (avy-all-windows 'all-frames)
  (avy-background t)
  :config
      (defun warmacs/avy-goto-url ()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy-jump "https?://"))
    (defun warmacs/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (warmacs/avy-goto-url)
        (browse-url-at-point)))
  :general
  (warmacs/leader-menu-jump/join/split
    "b" 'avy-pop-mark
    "j" 'evil-avy-goto-char-timer
    "l" 'evil-avy-goto-line
    "u" 'warmacs/avy-goto-url
    "U" 'warmacs/avy-open-url
    "w" 'evil-avy-goto-word-or-subword-1)

  (warmacs/leader-menu-text
    "o" 'warmacs/avy-open-url))

(use-package evil-easymotion
    :after evil
    :init
    (defun buffer-evil-avy-goto-char-timer ()
      "Call jump to the given chars use avy"
      (interactive)
      (let ((current-prefix-arg t))
        (evil-avy-goto-char-timer)))

    (evilem-default-keybindings "gs")
    (define-key evilem-map "a" (evilem-create #'evil-forward-arg))
    (define-key evilem-map "A" (evilem-create #'evil-backward-arg))
    (define-key evilem-map "o" (evilem-create #'evil-jump-out-args))
    (define-key evilem-map "s" #'evil-avy-goto-char-2)
    (define-key evilem-map "/" #'evil-avy-goto-char-timer)
    (define-key evilem-map (kbd "SPC") #'buffer-evil-avy-goto-char-timer)

    ;; Provide proper prefixes for which key
    (which-key-add-keymap-based-replacements evil-motion-state-map
      "gs"  "evil-easymotion")
    (which-key-add-keymap-based-replacements evilem-map
      "g" "misc"
      "[" "section backward"
      "]" "section forward")

    ;; Use evil-search backend, instead of isearch
    (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                        :bind ((evil-ex-search-highlight-all nil))))

(use-package whitespace-cleanup-mode
  :init
  (setq whitespace-cleanup-mode-only-if-initially-clean nil)
  :config
  (global-whitespace-cleanup-mode 1))

(provide 'layer/editing-plus)
