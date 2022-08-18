;;; core-ui.el -*- lexical-binding: t; -*-

(message "core-ui")

(scroll-bar-mode -1)        ; Disable scroll-bar-mode
(set-fringe-mode 10)        ; Give some breathing room
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(with-system darwin
             ;; frame
             (add-to-list 'default-frame-alist
                          '(ns-transparent-titlebar . t))

             (add-to-list 'default-frame-alist
                          '(ns-appearance . dark)))

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

;; Relative line numbers
(setq display-line-numbers-type 'relative)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;; Themes

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init
  (load-theme 'doom-one t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Load all the icons for pretty UI
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  :hook (after-init . doom-modeline-mode)
  :init
  (column-number-mode 1)
  (size-indication-mode 1))

(use-package hl-line
  ;; Highlights the current line
  :hook (emacs-startup . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
                org-agenda-mode dired-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar warmacs--hl-line-mode nil)

  (add-hook 'hl-line-mode-hook
            (defun warmacs-truly-disable-hl-line-h ()
              (unless hl-line-mode
                (setq-local warmacs--hl-line-mode nil))))

  (dolist (hook '(evil-visual-state-entry-hook activate-mark-hook))
    (add-hook hook
              (defun warmacs-disable-hl-line-h ()
                (when hl-line-mode
                  (hl-line-mode -1)
                  (setq-local warmacs--hl-line-mode t)))))

  (dolist (hook '(evil-visual-state-exit-hook deactivate-mark-hook))
    (add-hook hook
              (defun warmacs-enable-hl-line-maybe-h ()
                (when warmacs--hl-line-mode
                  (hl-line-mode +1))))))

(use-package rainbow-delimiters
  ;; Make parenthesis depth easier to distinguish at a glance
  :custom (rainbow-delimiters-max-face-count 4)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-quoted
  ;; Make quoted symbols easier to distinguish from free variables
  :hook
  (emacs-lisp-mode . highlight-quoted-mode)
  (clojure-mode . highlight-quoted-mode))

(provide 'core-ui)
