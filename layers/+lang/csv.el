;; +lang/csv.el -*- lexical-binding: t; -*-

(use-package csv-mode
  :general
  (warmacs/local-leader-menu csv
      "a"  'csv-align-fields
      "d"  'csv-kill-fields
      "h"  'csv-header-line
      "i"  'csv-toggle-invisibility
      "n"  'csv-forward-field
      "p"  'csv-backward-field
      "r"  'csv-reverse-region
      "sf" 'csv-sort-fields
      "sn" 'csv-sort-numeric-fields
      "so" 'csv-toggle-descending
      "t"  'csv-transpose
      "u"  'csv-unalign-fields
      "yf" 'csv-yank-fields
      "yt" 'csv-yank-as-new-table)

  (warmacs/local-leader-menu tsv
      "" '(:keymap csv-mode-map :package csv :which-key "tsv")
      "y"  '(:ignore t :which-key "yank")
      "s" '(:ignore t :which-key "sort")))

(provide-layer! +lang/csv)
