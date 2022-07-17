(when (spacemacs/system-is-mac)
  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; macOS Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t))

  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.
  (let ((gls (executable-find "gls")))
    (when gls
      (setq insert-directory-program gls))))
