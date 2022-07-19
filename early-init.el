;;; early-init.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Fast startup tricks from Doom emacs
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq
 gc-cons-threshold most-positive-fixnum

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
 load-prefer-newer noninteractive

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
native-comp-deferred-compilation nil

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Package initialization is handled by
;; straight.el, so we must prevent Emacs from doing it early!
package-enable-at-startup nil)


(unless (or (daemonp)
          noninteractive
          init-file-debug)
;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
  (lambda ()
    (setq-default inhibit-redisplay nil
      inhibit-message nil)
    (redisplay)))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-warmacs)
    (advice-remove #'load-file #'load-file@silence)))

;;
;;; Bootstrap

;; Ensure warmacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; ;; Load the core of Warmacs
(load (concat user-emacs-directory "core/core") nil 'nomessage)

(define-advice startup--load-user-init-file (:filter-args (args) init-warmacs)
  "Initialize Warmacs in an interactive session."
  (list (lambda ()
          (message "startup--load-user-init-file")
          (expand-file-name "core-start" warmacs-core-dir))
        nil  ; TODO Replace with safe mode initfile
        (caddr args)))
