;;; core-editor.el -*- lexical-binding: t; -*-

(transient-mark-mode 1)     ; Enable transient mark

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Favour spaces over tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

(setq fill-column 80
      word-wrap t)

(provide 'core-editor)
