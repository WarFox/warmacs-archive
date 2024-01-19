;; +lang/scala.el -*- lexical-binding: t; -*-

(warmacs/local-leader-menu scala
    "" '(:keymap lsp-command-map :package lsp :which-key "lsp")
    "=" '(:ignore t :which-key "formatting")
    "F" '(:ignore t :which-key "folders")
    "G" '(:ignore t :which-key "peek")
    "T" '(:ignore t :which-key "toggle")
    "a" '(:ignore t :which-key "code actions")
    "h" '(:ignore t :which-key "help")
    "g" '(:ignore t :which-key "goto")
    "r" '(:ignore t :which-key "refactor")
    "w" '(:ignore t :which-key "workspace"))

(defun scala/newline-and-indent-with-asterisk ()
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(use-package scala-mode
  :hook
  (scala-mode . dap-mode)
  (scala-mode . lsp-deferred)
  :custom
  (scala-indent:align-forms t)
  (scala-indent:align-parameters t)
  (scala-indent:default-run-on-strategy scala-indent:eager-strategy)
  :general
  (general-nmap
    :keymaps 'scala-mode-map
    "RET" #'scala/newline-and-indent-with-asterisk))

(use-package sbt-mode
  :commands (sbt-start sbt-command sbt-hydra)
  :init
  (defun warmacs/scala-sbt-scalafmt-all ()
    "Run `scalafmtAll' via SBT"
    (interactive)
    (sbt-command "scalafmtAll"))

  (defun warmacs/scala-sbt-compile ()
    "Run `compile' via SBT"
    (interactive)
    (sbt-command "compile"))

  (defun warmacs/scala-sbt-test ()
    "Run `test' via SBT"
    (interactive)
    (sbt-command "test"))

  (defun warmacs/scala-sbt-compile-it ()
    "Compile the `it' scope via SBT"
    (interactive)
    (sbt-command "It / compile"))

  (defun warmacs/scala-sbt-compile-test ()
    "Compile the `test' scope via SBT"
    (interactive)
    (sbt-command "Test / compile"))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :general
  (warmacs/local-leader-menu-scala
    "b" '(:ignore t :which-key "build")
    "b." 'sbt-hydra
    "bb" 'sbt-command
    "bc" #'warmacs/scala-sbt-compile
    "bt" #'warmacs/scala-sbt-test
    "bI" #'warmacs/scala-sbt-compile-it
    "bT" #'warmacs/scala-sbt-compile-test
    "br" #'sbt-send-region
    "b=" #'warmacs/scala-sbt-scalafmt-all))

(use-package lsp-metals
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :init
  (setq lsp-metals-treeview-show-when-views-received t))

(provide-layer! +lang/scala)
