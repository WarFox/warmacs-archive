;; +lang/java.el -*- lexical-binding: t; -*-

(defcustom enable-gradle-mode nil
  "If non-nill enables gradle-mode")

(use-package lsp-java
  :hook (java-mode . lsp-deferred))

(use-package dap-java :ensure nil :straight nil)

(use-package gradle-mode
  :if enable-gradle-mode
  :ensure enable-gradle-mode
  :mode ("\\.gradle\\" . gradle-mode)
  :general
  (warmacs/local-leader-menu gradle
      "b" '(:ignore t :which-key "build")
      "bb" #'gradle-build
      "t" '(:ignore t :which-key "test")
      "tt" #'gradle-test))

(use-package java-mode
  :straight (:type built-in)
  :general
  (warmacs/local-leader-menu java
      "" '(:keymap lsp-command-map :which-key "lsp")
      "=" '(:ignore t :which-key "format")
      "F" '(:ignore t :which-key "folders")
      "G" '(:ignore t :which-key "peek")
      "T" '(:ignore t :which-key "toggles")
      "a" '(:ignore t :which-key "actions")
      "b" '(:ignore t :which-key "build")
      "g" '(:ignore t :which-key "goto")
      "r" '(:ignore t :which-key "refactor")
      "w" '(:ignore t :which-key "workspace")
      "h" '(:ignore t :which-key "help")
      "bb" #'lsp-java-build-project))

;; Must configure runtimes
;; (lsp-java-configuration-runtimes
;;  '[(:name "JavaSE-1.8" :path "/Library/Java/JavaVirtualMachines/openjdk-8.jdk/Contents/Home/")
;;    (:name "OpenJdk-11" :path "/Library/Java/JavaVirtualMachines/openjdk-11.jdk/Contents/Home/" :default t)

(provide-layer! +lang/java)
