;;; core-lib.el -*- lexical-binding: t; -*-

(message "core-lib")

;; Core packages

;; Do things asynchronously
(use-package emacs-async
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;; Request package is required by aide
(use-package request)

;; bui is used by lsp
(use-package bui)

;; setup keybindings
(use-package which-key
  :demand t
  :init
  (which-key-mode)
  :diminish
  which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; Hydra for transient menus
(use-package hydra)

;; general
(use-package general
  :demand t
  :custom
  (general-use-package-emit-autoloads t)
  :init
  (setq
    warmacs-leader-key "SPC"
    warmacs-local-leader-key ",")
  :config
  (general-evil-setup)

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
       ,@body))))

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

(defun warmacs--require-layer (layer)
  (let ((filename (format "layers/%s" layer)))
    ;; (message "load feature %s from %s" layer filename)
    (require (intern (concat "layer" "/" layer)) layer)))

(defmacro use-layer! (layername)
   (declare (indent 2))
   `(progn
      ;; (message "use-layer! %s" ,layername)
      (warmacs--require-layer ,layername)))

(provide 'core-lib)
