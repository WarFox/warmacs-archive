;; +lang/scala.el -*- lexical-binding: t; -*-

(warmacs/local-leader-menu scala
    "b" #'(:ignore t :whick-key "sbt"))

(defun scala/newline-and-indent-with-asterisk ()
  (interactive)
  (newline-and-indent)
  (message "new and inddent")
  (scala-indent:insert-asterisk-on-multiline-comment))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :hook
  ((scala-mode . dap-mode))
  :custom
  (scala-indent:align-forms t)
  (scala-indent:align-parameters t)
  (scala-indent:default-run-on-strategy scala-indent:eager-strategy)
  :general
  (:keymaps 'scala-mode-map
            :states 'normal
            "RET" #'scala/newline-and-indent-with-asterisk))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :init
  (defun spacemacs/scala-sbt-scalafmt-all ()
    "Run `scalafmtAll' via SBT"
    (interactive)
    (sbt-command "scalafmtAll"))

  (defun spacemacs/scala-sbt-compile ()
    "Run `compile' via SBT"
    (interactive)
    (sbt-command "compile"))

  (defun spacemacs/scala-sbt-test ()
    "Run `test' via SBT"
    (interactive)
    (sbt-command "test"))

  (defun spacemacs/scala-sbt-compile-it ()
    "Compile the `it' scope via SBT"
    (interactive)
    (sbt-command "It / compile"))

  (defun spacemacs/scala-sbt-compile-test ()
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
    "b." 'sbt-hydra
    "bb" 'sbt-command
    "bc" #'spacemacs/scala-sbt-compile
    "bt" #'spacemacs/scala-sbt-test
    "bI" #'spacemacs/scala-sbt-compile-it
    "bT" #'spacemacs/scala-sbt-compile-test
    "b=" #'spacemacs/scala-sbt-scalafmt-all))

(use-package lsp-metals
  :after lsp
  :hook
  (scala-mode . #'lsp-deferred)
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :init
  (setq lsp-metals-treeview-show-when-views-received t))

(provide-layer! +lang/scala)
