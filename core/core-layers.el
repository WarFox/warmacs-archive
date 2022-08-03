;;; core-layers.el -*- lexical-binding: t; -*-

(message "core-layers")

(defvar warmacs-init-layers-p nil
  "Non-nil if `warmacs-initialize-layers' has run.")

(defvar warmacs-layers (make-hash-table :test 'equal)
  "A hash table of enabled layers. Set by `warmacs-initialize-layers'.")

(defvar warmacs-layers-dirs
  (list warmacs-layers-dir)
  "A list of layer root directories. Order determines priority.")

(defvar warmacs-layer-init-file "init"
  "The basename of init files for layers.

Init files are loaded early, just after Warmacs core, and before layers' config
files. They are always loaded, even in non-interactive sessions, and before
`warmacs-before-init-layers-hook'. Related to `warmacs-layer-config-file'.")

(defvar warmacs-layer-config-file "config"
  "The basename of config files for layers.

Config files are loaded later, and almost always in interactive sessions. These
run before `warmacs-init-layers-hook'. Relevant to `warmacs-layer-init-file'.")

(defconst warmacs-private-dir
  (if-let (warmacsdir (getenv-internal "WARMACSDIR"))
    (expand-file-name (file-name-as-directory warmacsdir))
    (or (let ((xdgdir
                (expand-file-name "warmacs/"
                  (or (getenv-internal "XDG_CONFIG_HOME")
                    "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
      "~/.warmacs.d/"))
  "Where your private configuration is placed.

Defaults to ~/.config/warmacs, ~/.warmacs.d or the value of the WARMACSDIR envvar;
whichever is found first. Must end in a slash.")

;; (add-to-list )

(defun warmacs-initialize-core-layers ()
  "Load Warmacs's core files for an interactive session. Order matters here"
  (require 'core-keybindings)
  (require 'core-ui)
  (require 'core-editor)
  (require 'core-projects)
  (require 'core-ide))

(defun warmacs-layer-loader (file)
  "Return a closure that loads FILE from a layer.

This closure takes two arguments: a cons cell containing (CATEGORY . LAYER)
symbols, and that layer's plist."
  (declare (pure t) (side-effect-free t))
  (lambda (layer plist)
    (let ((warmacs--current-layer layer)
          (warmacs--current-flags (plist-get plist :flags))
          (inhibit-redisplay t))
      (message "loading layers from file" file)
      (load! file (plist-get plist :path) t))))

(defun warmacs-initialize-layers (&optional force-p no-config-p)
  "Loads the init.el in `warmacs-private-dir' and sets up hooks for a healthy
session of Warmacsing. Will noop if used more than once, unless FORCE-P is
non-nil."
  (when (or force-p (not warmacs-init-layers-p))
    (setq warmacs-init-layers-p t)
    (unless no-config-p
      (message "> Initializing core layers")
      (warmacs-initialize-core-layers))
      (message "> Core layers initialized")
    (when-let (init-p (load! warmacs-layer-init-file warmacs-private-dir t))
      (message "Initializing user config")
      (maphash (warmacs-layer-loader warmacs-layer-init-file) warmacs-layers)
      ;; (warmacs-run-hooks 'warmacs-before-init-layers-hook)
      (unless no-config-p
        (maphash (warmacs-layer-loader warmacs-layer-config-file) warmacs-layers)
        ;; (warmacs-run-hooks 'warmacs-init-layers-hook)
        (load! warmacs-layer-config-file warmacs-private-dir t)
        (when custom-file
          (load custom-file 'noerror (not warmacs-debug-mode)))))))

(provide 'core-layers)
