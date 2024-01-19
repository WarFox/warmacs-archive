;; +markup/markdown.el -*- lexical-binding: t; -*-

(defcustom markdown--keybinding-maps '(markdown-mode-map gfm-mode-map)
  "Modes using markdown key bindings.")

(defcustom markdown-live-preview-engine 'eww
  "Possibe values are `eww' (built-in browser) or `vmd' (installed with `npm')."
  :type 'string
  :options '(eww vmd))

(use-package markdown-mode
  :mode
  (("\\.mkd\\'" . markdown-mode)
   ("\\.mdk\\'" . markdown-mode)
   ("\\.mdx\\'" . markdown-mode))
  :config
  ;; Make markdown-mode behave a bit more like org w.r.t. code blocks i.e.
  ;; use proper syntax highlighting
  (setq markdown-fontify-code-blocks-natively t)

  ;; Declare prefixes and bind keys
  ;; (prefix '(("mc" . "markdown/command")
  ;;                   ("mh" . "markdown/header")
  ;;                   ("mi" . "markdown/insert")
  ;;                   ("ml" . "markdown/lists")
  ;;                   ("mt" . "markdown/table")
  ;;                   ("mT" . "markdown/toggle")
  ;;                   ("mx" . "markdown/text")))

  :general
  (warmacs/local-leader-keys
    :keymaps markdown--keybinding-maps
    ;; rebind this so terminal users can use it
    "M-RET" 'markdown-insert-list-item
    ;; Movement
    "{"   'markdown-backward-paragraph
    "}"   'markdown-forward-paragraph
    ;; Completion, and Cycling
    "]"   'markdown-complete
    ;; Indentation
    ">"   'markdown-indent-region
    "<"   'markdown-outdent-region
    ;; Buffer-wide commands
    "c"   '(:which-key "command")
    "c]"  'markdown-complete-buffer
    "cc"  'markdown-check-refs
    "ce"  'markdown-export
    "cm"  'markdown-other-window
    "cn"  'markdown-cleanup-list-numbers
    "co"  'markdown-open
    "cp"  'markdown-preview
    "cP"  'markdown-live-preview-mode
    "cv"  'markdown-export-and-preview
    "cw"  'markdown-kill-ring-save
    ;; headings
    "h"   '(:whick-key "headings")
    "hi"  'markdown-insert-header-dwim
    "hI"  'markdown-insert-header-setext-dwim
    "h1"  'markdown-insert-header-atx-1
    "h2"  'markdown-insert-header-atx-2
    "h3"  'markdown-insert-header-atx-3
    "h4"  'markdown-insert-header-atx-4
    "h5"  'markdown-insert-header-atx-5
    "h6"  'markdown-insert-header-atx-6
    "h!"  'markdown-insert-header-setext-1
    "h@"  'markdown-insert-header-setext-2
    ;; Insertion of common elements
    "-"   'markdown-insert-hr
    "if"  'markdown-insert-footnote
    "ii"  'markdown-insert-image
    "ik"  'warmacs/insert-keybinding-markdown
    "il"  'markdown-insert-link
    "iw"  'markdown-insert-wiki-link
    "iu"  'markdown-insert-uri
    "iT"  'markdown-insert-table
    ;; Element removal
    "k"   'markdown-kill-thing-at-point
    ;; List editing
    "li"  'markdown-insert-list-item
    ;; Toggles
    "T" '(:ignore t :whick-key "toggles")
    "Ti"  'markdown-toggle-inline-images
    "Tl"  'markdown-toggle-url-hiding
    "Tm"  'markdown-toggle-markup-hiding
    "Tt"  'markdown-toggle-gfm-checkbox
    "Tw"  'markdown-toggle-wiki-links
    ;; Table
    "ta"  'markdown-table-align
    "tp"  'markdown-table-move-row-up
    "tn"  'markdown-table-move-row-down
    "tf"  'markdown-table-move-column-right
    "tb"  'markdown-table-move-column-left
    "tr"  'markdown-table-insert-row
    "tR"  'markdown-table-delete-row
    "tc"  'markdown-table-insert-column
    "tC"  'markdown-table-delete-column
    "ts"  'markdown-table-sort-lines
    "td"  'markdown-table-convert-region
    "tt"  'markdown-table-transpose
    ;; region manipulation
    "xb"  'markdown-insert-bold
    "xB"  'markdown-insert-gfm-checkbox
    "xc"  'markdown-insert-code
    "xC"  'markdown-insert-gfm-code-block
    "xi"  'markdown-insert-italic
    "xk"  'markdown-insert-kbd
    "xp"  'markdown-insert-pre
    "xq"  'markdown-insert-blockquote
    "xs"  'markdown-insert-strike-through
    "xQ"  'markdown-blockquote-region
    "xP"  'markdown-pre-region
    ;; Following and Jumping
    "N"   'markdown-next-link
    "f"   'markdown-follow-thing-at-point
    "P"   'markdown-previous-link
    "<RET>" 'markdown-do)

  ;; Header navigation in normal state movements
  (general-nmap markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading)
  ;; Promotion, Demotion
  (general-def markdown-mode-map
    "M-<down>" 'markdown-move-down
    "M-<left>" 'markdown-promote
    "M-<right>" 'markdown-demote
    "M-<up>" 'markdown-move-up))

(use-package vmd-mode
  :if (eq 'vmd markdown-live-preview-engine)
  :init
  (warmacs/local-leader-keys
    :keymaps markdown--keybinding-maps
    "cP" #'vmd-mode))

(provide-layer! +markup/markdown)
