
(scroll-bar-mode -1)        ; Disable scroll-bar-mode
(set-fringe-mode 10)        ; Give some breathing room
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Warmacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;; Themes

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-colors") ; uses all the icons
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; load default theme
  (load-theme 'doom-one t))

(use-package centaur-tabs
  :demand t
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "⚠")
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (progn
    (unless (daemonp)
      (setq centaur-tabs-set-bar 'left))

    (centaur-tabs-headline-match)
    (centaur-tabs-group-by-projectile-project)
    (centaur-tabs-mode t)

    (which-key-add-keymap-based-replacements evil-normal-state-map  "C-c t" "tab"))
   :bind
   (:map evil-normal-state-map
         ("g t"     . centuar-tabs-forward)
         ("g T"     . centuar-tabs-backward)
         ("g C-t"   . centaur-tabs-move-current-tab-to-right)
         ("g C-S-t" . centaur-tabs-move-current-tab-to-left))
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups))

(message "core-ui")

(provide 'core-ui)
