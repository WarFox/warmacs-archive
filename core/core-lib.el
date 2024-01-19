;;; core-lib.el -*- lexical-binding: t; -*-

(message "core-lib")

;; Core packages and libs

(use-package s)
(use-package f)
(use-package request)
(use-package bui)
(use-package dash)
(use-package aio)
(use-package tablist)

;; Keep ~/.emacs.d/ clean
(use-package no-littering)
  ;; :config
  ;; (setq auto-save-file-name-transforms
  ;;       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Do things asynchronously
(use-package emacs-async
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package auth-source
  :no-require t
  :config (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))

;; setup keybindings
(use-package which-key
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.1)
  :diminish
  which-key-mode)

;; Hydra for transient menus
(use-package hydra)

;; general
(use-package general
  :custom
  (general-use-package-emit-autoloads t)
  :init
  (setq
   warmacs-leader-key "SPC"
   warmacs-local-leader-key ",")
  (general-evil-setup)
  :config

  ;; Spacemacs-like menu
  ;; https://gist.github.com/progfolio/1c96a67fcec7584b31507ef664de36cc
  ;; https://www.reddit.com/r/emacs/comments/des3cl/comment/f2yw45k/?utm_source=share&utm_medium=web2x&context=3

  (general-create-definer warmacs/leader-keys
    :keymaps 'override
    :states  '(insert emacs normal hybrid motion visual operator)
    :prefix  warmacs-leader-key
    :non-normal-prefix (concat "C-" warmacs-leader-key)
    "" '(:ignore t :whick-key "leader key"))

  (general-create-definer warmacs/local-leader-keys
    :major-modes t
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix warmacs-local-leader-key
    :non-normal-prefix (concat "C-SPC " warmacs-local-leader-key)
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  (defmacro warmacs/leader-menu (name infix-key &rest body)
    "Create a definer named warmacs/leader-NAME-menu wrapping warmacs/leader-keys.
  Create prefix map: warmacs-leader-NAME-menu-map. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "warmacs/leader-menu-" name))
         :wrapping warmacs/leader-keys
         :prefix-map (quote ,(intern (concat "warmacs-leader-menu-" name "-map")))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "warmacs/leader-menu-" name))
        ,@body)))

  (defmacro warmacs/local-leader-menu (mode &rest body)
    "Create a definer named warmacs/local-leader-menu-MODE wrapping warmacs/local-leader-keys
     Create prefix map: warmacs-local.
     Parameter mode must be a symbol not end with -mode"
    (declare (indent 2))
    (let ((local-leader-menu-name (concat "warmacs/local-leader-menu-" (symbol-name mode)))
          (local-keymap (concat (symbol-name mode) "-mode-map")))
      (message ">> Setting up local-leader menu for %s with keymap %s" mode local-keymap)
      `(progn
         (general-create-definer ,(intern local-leader-menu-name)
           :wrapping warmacs/local-leader-keys
           :keymaps (quote ,(intern local-keymap))
           :wk-full-keys nil
           "" '(:ignore t :which-key ,mode))
         (message "created definer %s" ,local-leader-menu-name)
         (,(intern local-leader-menu-name)
          ,@body)))))

;; Core library of functions

(defun warmacs/find-file-in-project (filename)
  "Open a file like find-file. If the file belongs to a project, creates
    a new persp and enables projectile mode for it."
  (interactive (list (read-file-name "Find file: " nil default-directory (confirm-nonexistent-file-or-buffer))))
  (let* ((persp-reset-windows-on-nil-window-conf t)
         (filename-fullpath (file-truename filename))
         (filename-directory (if (file-directory-p filename-fullpath)
                                 (file-name-as-directory filename-fullpath)
                               (file-name-directory filename-fullpath)))
         (projectile-switch-project-action (lambda () (find-file filename-fullpath)))
         (project-root (projectile-root-bottom-up filename-directory)))
    (if project-root
        (progn
          (persp-switch (file-name-nondirectory (directory-file-name project-root)))
          (projectile-switch-project-by-name project-root))
      (message "Requested file does not belong to any project"))))


(defun warmacs--handle-load-error (e target path)
  (let* ((source (file-name-sans-extension target))
         (err (cond ((not (featurep 'core))
                     (cons 'error (file-name-directory path)))
                    ((file-in-directory-p source warmacs-core-dir)
                     (cons 'warmacs-error warmacs-core-dir))
                    ((file-in-directory-p source warmacs-private-dir)
                     (cons 'warmacs-private-error warmacs-private-dir))
                    ((file-in-directory-p source (expand-file-name "cli" warmacs-core-dir))
                     (cons 'warmacs-cli-error (expand-file-name "cli" warmacs-core-dir)))
                    ((cons 'warmacs-module-error warmacs-emacs-dir)))))
    (signal (car err)
            (list (file-relative-name
                   (concat source ".el")
                   (cdr err))
                  e))))

;; Core hook functions

(defun disable-display-line-numbers-mode-h ()
    "Hook function to explicitly disable line number display"
    (display-line-numbers-mode -1))

;; Core Macros

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let* ((path (or path
                   (dir!)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                   `(expand-file-name ,filename ,path)
                 filename)))
    `(condition-case-unless-debug e
         (let (file-name-handler-alist)
           (load ,file ,noerror 'nomessage))
       (warmacs-error (signal (car e) (cdr e)))
       (error (warmacs--handle-load-error e ,file ,path)))))

(defmacro use-layer! (LAYERNAME &rest body)
  "use-package layer name from the layers/ path"
  (declare (indent 2))
  (let ((filename (concat warmacs-dir (format "layers/%s" LAYERNAME)))
        (feature  (intern (format "layer/%s" LAYERNAME))))
    `(progn
        ;; unload if feature is loaded, so that new changes are loaded
        ;; this check is not required on startup
        ;; TODO optimising for only when loading after startup might save time
       (when (featurep (quote ,feature))
        (unload-feature (quote ,feature) t))
       (require (quote ,feature) ,filename)
       (use-package ,feature
         :load-path ,filename
         :straight nil
         :defer nil
         :demand t
         ,@body))))

(defmacro provide-layer! (FEATURE)
  (declare (indent 2))
  (let ((feature (intern (format "layer/%s" FEATURE))))
    `(provide (quote ,feature))))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(provide 'core-lib)
