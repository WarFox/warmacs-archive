;;; core-lib.el -*- lexical-binding: t; -*-

(message "core-lib")

(require 'core-keybindings)

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

(use-package restart-emacs
  :demand t
  :init
  (progn 
    (defun warmacs/restart-emacs (&optional args)
	(interactive)
	(restart-emacs args))

    (defun warmacs/kill-emacs (prompt &optional args)
	(interactive)
	(if (not prompt)
	    (save-some-buffers nil t))
	(kill-emacs args)))
  :general
  (+general-global-menu! "quit" "q"
    "d" '((lambda (&optional args)
            (interactive)
            (warmacs/restart-emacs (cons "--debug-init" args)))
           :which-key "restart-emacs-debug-init")
    "R" 'warmacs/restart-emacs
    "t" '((lambda (&optional args)
             (interactive)
             (warmacs/restart-emacs (cons "--timed-requires" args)))
            :which-key "restart-emacs-timed-requires")
    "T" '((lambda (&optional args)
            (interactive)
            (warmacs/restart-emacs (cons "--adv-timers" args)))
           :which-key "restart-emacs-adv-timers")
    "q" '((lambda (&optional args)
	    (interactive)
            (warmacs/kill-emacs t args))
           :which-key "prompt-kill-emacs")
    "Q" 'warmacs/kill-emacs))


(use-package elisp-mode
   ;;this is a built in package, so we don't want to try and install it
   :ensure nil
   :straight nil
   :general
   (warmacs/set-major-mode-leader-keys
     ;;specify the major modes these should apply to:
     :major-modes '(emacs-lisp-mode lisp-interaction-mode)
     ;;and the keymaps:
     :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
     "e" '(:ignore t :which-key "eval")
     "eb" 'eval-buffer
     "ed" 'eval-defun
     "ee" 'eval-expression
     "ep" 'pp-eval-last-sexp
     "es" 'eval-last-sexp
     "i" 'elisp-index-search))

(provide 'core-lib)
