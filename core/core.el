;;; core.el -*- lexical-binding: t; -*-

(message "core")

; single setq for a bunge of flags
(setq

 inhibit-startup-message t ; No startup message

 highlight-nonselected-windows nil

 ;; More performant rapid scrolling over unfontified regions. May cause brief
 ;; spells of inaccurate syntax highlighting right after scrolling, which should
 ;; quickly self-correct.
 fast-but-imprecise-scrolling t

 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject

 ;; Resizing the Emacs frame can be a terribly expensive part of changing the
 ;; font. By inhibiting this, we halve startup times, particularly when we use
 ;; fonts that are larger than the system default (which would resize the frame).
 frame-inhibit-implied-resize t

 ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
 ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
 ;; when it's idle. However, if the idle delay is too long, we run the risk of
 ;; runaway memory usage in busy sessions. If it's too low, then we may as well
 ;; not be using gcmh at all.
 gcmh-idle-delay 'auto  ; default is 15s
 gcmh-auto-idle-delay-factor 10
 gcmh-high-cons-threshold (* 32 1024 1024)  ; 32mb

 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0  ; default is 0.5

 ;; Font compacting can be terribly expensive, especially for rendering icon
 ;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
 ;; hasn't been determined, but do it there anyway, just in case. This increases
 ;; memory usage, however!
 inhibit-compacting-font-caches t

 ;; PGTK builds only: this timeout adds latency to frame operations, like
 ;; `make-frame-invisible', which are frequently called without a guard because
 ;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
 ;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
 ;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
 pgtk-wait-for-event-timeout 0.001

 ;; Increase how much is read from processes in a single chunk (default is 4kb).
 ;; This is further increased elsewhere, where needed (like our LSP layer).
 read-process-output-max (* 64 1024)  ; 64kb

 ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
 ;; receiving input, which should help a little with scrolling performance.
 redisplay-skip-fontification-on-input t)

;; Auto refresh
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)

;;
;;; Directory variables

(defconst warmacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst warmacs-core-dir (concat warmacs-dir "core/")
  "The root directory of warmacs' core files. Must end with a slash.")

(defconst warmacs-layers-dir (concat warmacs-dir "layers/")
  "The root directory for warmacs' layers. Must end with a slash.")

(defconst warmacs-cache-dir (concat warmacs-dir ".cache/")
  "The root directory for warmacs' cache. Must end with a slash.")

;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq
  ;; defer by default
  use-package-always-defer t
  ;; using straight.el so disable use-package-ensure
  use-package-always-ensure nil)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; disable file backups
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/auto-save/")))

(defun warmacs-backup-file-name (fpath)
"If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ➢ for example: “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setq
  make-backup-file-name-function 'warmacs-backup-file-name)
;; end disable file backups ~ and ##autosave

;;
;;; Custom error types

(define-error 'warmacs-error "Error in Warmacs Emacs core")
(define-error 'warmacs-hook-error "Error in a Warmacs startup hook" 'warmacs-error)
(define-error 'warmacs-autoload-error "Error in Warmacs's autoloads file" 'warmacs-error)
(define-error 'warmacs-module-error "Error in a Warmacs module" 'warmacs-error)
(define-error 'warmacs-private-error "Error in private config" 'warmacs-error)
(define-error 'warmacs-package-error "Error with packages" 'warmacs-error)

;;
;;; Bootstrap

;; Ensure Warmacs's core libraries are visible for loading
(add-to-list 'load-path warmacs-core-dir)

;; load core library functions and start!
(require 'core-versions)
(require 'core-lib)

(setq inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

(provide 'core)
