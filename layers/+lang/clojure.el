;; -*- lexical-binding: t; -*-

(defconst clojure-mode-maps
  '(clojure-mode-map
    clojurec-mode-map
    clojurescript-mode-map
    clojurex-mode-map
    cider-repl-mode-map
    cider-clojure-interaction-mode-map)
  "List of all clojure modes")

(use-package cider
  :hook (clojure-mode . cider-mode)
  :custom
  ;; open cider-doc directly and close it with q
  (cider-prompt-for-symbol t)
  :init
  (setq cider-stacktrace-default-filters '(tooling dup)
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t
        cider-repl-history-file (concat warmacs-cache-dir "cider-repl-history"))


  :config
  ;; add support for golden-ratio
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-extra-commands 'cider-popup-buffer-quit-function))
  ;; add support for evil
  (evil-set-initial-state 'cider-stacktrace-mode 'motion)
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (add-hook 'cider--debug-mode-hook 'warmacs/cider-debug-setup)

  (defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump))

  :general

  (warmacs/local-leader-keys
    :keymaps 'cider-repl-history-mode-map
    "s" 'cider-repl-history-save)

  (general-def cider-repl-mode-map
    :states '(normal insert)
    "C-j" 'cider-repl-next-input
    "C-k" 'cider-repl-previous-input
    "RET" 'cider-repl-return)

  (general-def  cider-repl-mode-map
    :states 'insert
    "<C-return>" 'cider-repl-newline-and-indent
    "C-r" 'cider-repl-history)

  (:keymaps 'cider-stacktrace-mode-map
            "C-j" #'cider-stacktrace-next-cause
            "C-k" #'cider-stacktrace-previous-cause
            "TAB" #'cider-stacktrace-cycle-current-cause
            "0"   #'cider-stacktrace-cycle-all-causes
            "1"   #'cider-stacktrace-cycle-cause-1
            "2"   #'cider-stacktrace-cycle-cause-2
            "3"   #'cider-stacktrace-cycle-cause-3
            "4"   #'cider-stacktrace-cycle-cause-4
            "5"   #'cider-stacktrace-cycle-cause-5
            "a"   #'cider-stacktrace-toggle-all
            "c"   #'cider-stacktrace-toggle-clj
            "d"   #'cider-stacktrace-toggle-duplicates
            "J"   #'cider-stacktrace-toggle-java
            "r"   #'cider-stacktrace-toggle-repl
            "T"   #'cider-stacktrace-toggle-tooling)

  (:keymaps 'cider-docview-mode-map
            "q" 'cider-popup-buffer-quit)

  (:keymaps 'cider-inspector-mode-map
            "L" 'cider-inspector-pop
            "n" 'cider-inspector-next-page
            "N" 'cider-inspector-prev-page
            "p" 'cider-inspector-prev-page
            "r" 'cider-inspector-refresh)

  (:keymaps 'cider-test-report-mode-map
            "C-j" 'cider-test-next-result
            "C-k" 'cider-test-previous-result
            "RET" 'cider-test-jump
            "d"   'cider-test-ediff
            "e"   'cider-test-stacktrace
            "q"   'cider-popup-buffer-quit
            "r"   'cider-test-rerun-tests
            "t"   'cider-test-run-test
            "T"   'cider-test-run-ns-tests)

  (:keymaps 'cider-repl-history-mode-map
            "j" 'cider-repl-history-forward
            "k" 'cider-repl-history-previous
            "s" 'cider-repl-history-occur
            "r" 'cider-repl-history-update)

  (warmacs/local-leader-keys
    :keymaps clojure-mode-maps
    ;; shortcuts
    "'"  'sesman-start
    ;; help / documentation
    "h" '(:ignore t :which-key "help")
    "hh" 'cider-doc
    "ha" 'cider-apropos
    "hc" 'cider-cheatsheet
    "hd" 'cider-clojuredocs
    "hj" 'cider-javadoc
    "hn" 'cider-browse-ns
    "hN" 'cider-browse-ns-all
    "hs" 'cider-browse-spec
    "hS" 'cider-browse-spec-all
    ;; evaluate in source code buffer
    "e" '(:ignore t :which-key "evaluate")
    "e;" 'cider-eval-defun-to-comment
    "e$" 'warmacs/cider-eval-sexp-end-of-line
    "e(" 'cider-eval-list-at-point
    "eb" 'cider-eval-buffer
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "ei" 'cider-interrupt
    "el" 'warmacs/cider-eval-sexp-end-of-line
    "em" 'cider-macroexpand-1
    "eM" 'cider-macroexpand-all
    "ena" 'cider-ns-reload-all
    "enn" 'cider-eval-ns-form
    "enr" 'cider-ns-refresh
    "enl" 'cider-ns-reload  ;; SPC u for cider-ns-reload-all
    "ep;" 'cider-pprint-eval-defun-to-comment
    "ep:" 'cider-pprint-eval-last-sexp-to-comment
    "epf" 'cider-pprint-eval-defun-at-point
    "epe" 'cider-pprint-eval-last-sexp
    "er" 'cider-eval-region
    "eu" 'cider-undef
    "ev" 'cider-eval-sexp-at-point
    "eV" 'cider-eval-sexp-up-to-point
    "ew" 'cider-eval-last-sexp-and-replace
    ;; format code style
    "="  '(:ignore t :which-key "format")
    "=r" 'cider-format-region
    "==" 'cider-format-buffer
    "=eb" 'cider-format-edn-buffer
    "=ee" 'cider-format-edn-last-sexp
    "=er" 'cider-format-edn-region
    "=f" 'cider-format-defun
    ;; goto
    "g"  '(:ignore t :which-key "goto")
    "gS" 'cider-browse-spec-all
    "gb" 'cider-pop-back
    "gc" 'cider-classpath
    "ge" 'cider-jump-to-compilation-error
    "gg" 'warmacs/clj-find-var
    "gn" 'cider-find-ns
    "gr" 'cider-find-resource
    "gs" 'cider-browse-spec
    ;; manage cider connections / sesman
    "m"  '(:ignore t :which-key "manager")
    "mb" 'sesman-browser
    "mi" 'sesman-info
    "mg" 'sesman-goto
    "mlb" 'sesman-link-with-buffer
    "mld" 'sesman-link-with-directory
    "mlu" 'sesman-unlink
    "mqq" 'sesman-quit
    "mqr" 'sesman-restart
    "mlp" 'sesman-link-with-project
    "mSj" 'cider-connect-sibling-clj
    "mSs" 'cider-connect-sibling-cljs
    "ms" 'sesman-start
    ;; send code - warmacs convention
    ;; "sa" (if (eq m 'cider-repl-mode)
    ;;          'cider-switch-to-last-clojure-buffer
    ;;        'cider-switch-to-repl-buffer)
    "s"  '(:ignore t :which-key "session")
    "sb" 'cider-load-buffer
    "sB" 'warmacs/cider-send-buffer-in-repl-and-focus
    "scj" 'cider-connect-clj
    "scm" 'cider-connect-clj&cljs
    "scs" 'cider-connect-cljs
    "se" 'warmacs/cider-send-last-sexp-to-repl
    "sE" 'warmacs/cider-send-last-sexp-to-repl-focus
    "sf" 'warmacs/cider-send-function-to-repl
    "sF" 'warmacs/cider-send-function-to-repl-focus
    "si" 'sesman-start
    "sjj" 'cider-jack-in-clj
    "sjm" 'cider-jack-in-clj&cljs
    "sjs" 'cider-jack-in-cljs
    "sl" 'warmacs/cider-find-and-clear-repl-buffer
    "sL" 'cider-find-and-clear-repl-output
    "sn" 'warmacs/cider-send-ns-form-to-repl
    "sN" 'warmacs/cider-send-ns-form-to-repl-focus
    "so" 'cider-repl-switch-to-other
    "sqq" 'cider-quit
    "sqr" 'cider-restart
    "sqn" 'cider-ns-reload
    "sqN" 'cider-ns-reload-all
    "sr" 'warmacs/cider-send-region-to-repl
    "sR" 'warmacs/cider-send-region-to-repl-focus
    "su" 'cider-repl-require-repl-utils
    ;; toggle options
    "Te" 'cider-enlighten-mode
    "Tf" 'warmacs/cider-toggle-repl-font-locking
    "Tp" 'warmacs/cider-toggle-repl-pretty-printing
    "Tt" 'cider-auto-test-mode
    ;; cider-tests
    "ta" 'warmacs/cider-test-run-all-tests
    "tb" 'cider-test-show-report
    "tl" 'warmacs/cider-test-run-loaded-tests
    "tn" 'warmacs/cider-test-run-ns-tests
    "tp" 'warmacs/cider-test-run-project-tests
    "tr" 'warmacs/cider-test-rerun-failed-tests
    "tt" 'warmacs/cider-test-run-focused-test
    ;; cider-debug and inspect
    "db" 'cider-debug-defun-at-point
    "de" 'warmacs/cider-display-error-buffer
    "dve" 'cider-inspect-last-sexp
    "dvf" 'cider-inspect-defun-at-point
    "dvi" 'cider-inspect
    "dvl" 'cider-inspect-last-result
    "dvv" 'cider-inspect-expr
    ;; profile
    "p+" 'cider-profile-samples
    "pc" 'cider-profile-clear
    "pn" 'cider-profile-ns-toggle
    "ps" 'cider-profile-var-summary
    "pS" 'cider-profile-summary
    "pt" 'cider-profile-toggle
    "pv" 'cider-profile-var-profiled-p)

  ;; cider-repl-mode only
  (warmacs/local-leader-keys
    :keymaps 'cider-repl-mode-map
    "," 'cider-repl-handle-shortcut)

  (warmacs/local-leader-keys
    :keymaps 'cider-clojure-interaction-mode-map
    "epl" 'cider-eval-print-last-sexp))

;; (use-package clj-refactor
;;   :defer t
;;   :init
;;   (add-hook 'clojure-mode-hook 'clj-refactor-mode)
;;   :config
;;   (progn
;;     (cljr-add-keybindings-with-prefix "C-c C-f")
;;     ;; Usually we do not set keybindings in :config, however this must be done
;;     ;; here because it reads the variable `cljr--all-helpers'. Since
;;     ;; `clj-refactor-mode' is added to the hook, this should trigger when a
;;     ;; clojure buffer is opened anyway, so there's no "keybinding delay".
;;     (warmacs|forall-clojure-modes m
;;                                   (dolist (r cljr--all-helpers)
;;                                     (let* ((binding (car r))
;;                                            (func (cadr r)))
;;                                       (unless (string-prefix-p "hydra" (symbol-name func))
;;                                         (warmacs/leader-keys-for-major-mode m
;;                                                                                 (concat "r" binding) func)))))))

;; (use-package clojure-mode
;;   :defer t
;;   :init
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
;;     ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
;;     (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
;;     (add-hook 'clojure-mode-hook #'warmacs//clojure-setup-backend)
;;     ;; Define all the prefixes here, although most of them apply only to bindings in clj-refactor
;;     (let ((clj-refactor--key-binding-prefixes
;;            '(("mra" . "add")
;;              ("mrc" . "cycle/clean/convert")
;;              ("mrd" . "destructure")
;;              ("mre" . "extract/expand")
;;              ("mrf" . "find/function")
;;              ("mrh" . "hotload")
;;              ("mri" . "introduce/inline")
;;              ("mrm" . "move")
;;              ("mrp" . "project/promote")
;;              ("mrr" . "remove/rename/replace")
;;              ("mrs" . "show/sort/stop")
;;              ("mrt" . "thread")
;;              ("mru" . "unwind/update")))
;;           (clj-refactor--key-binding-non-lsp-prefixes
;;            '(("mr" . "refactor"))))
;;       (warmacs|forall-clojure-modes m
;;                                     (mapc (lambda (x) (warmacs/declare-prefix-for-mode
;;                                                        m (car x) (cdr x)))
;;                                           clj-refactor--key-binding-prefixes)
;;                                     (unless (eq clojure-backend 'lsp)
;;                                       (mapc (lambda (x) (warmacs/declare-prefix-for-mode
;;                                                          m (car x) (cdr x)))
;;                                             clj-refactor--key-binding-non-lsp-prefixes))
;;                                     (warmacs/leader-keys-for-major-mode m
;;                                                                             "=l" 'clojure-align
;;                                                                             "ran" 'clojure-insert-ns-form
;;                                                                             "raN" 'clojure-insert-ns-form-at-point
;;                                                                             "rci" 'clojure-cycle-if
;;                                                                             "rcp" 'clojure-cycle-privacy
;;                                                                             "rc#" 'clojure-convert-collection-to-set
;;                                                                             "rc'" 'clojure-convert-collection-to-quoted-list
;;                                                                             "rc(" 'clojure-convert-collection-to-list
;;                                                                             "rc[" 'clojure-convert-collection-to-vector
;;                                                                             "rc{" 'clojure-convert-collection-to-map
;;                                                                             "rc:" 'clojure-toggle-keyword-string
;;                                                                             "rsn" 'clojure-sort-ns
;;                                                                             "rtf" 'clojure-thread-first-all
;;                                                                             "rth" 'clojure-thread
;;                                                                             "rtl" 'clojure-thread-last-all
;;                                                                             "rua" 'clojure-unwind-all
;;                                                                             "ruw" 'clojure-unwind)
;;                                     (unless clojure-enable-clj-refactor
;;                                       (warmacs/leader-keys-for-major-mode m
;;                                                                               "r?" 'warmacs/clj-describe-missing-refactorings)))))
;;   :config
;;   (when clojure-enable-fancify-symbols
;;     (warmacs|forall-clojure-modes m
;;                                   (clojure/fancify-symbols m))))

;; (use-package clojure-snippets
;;   :defer t)

;; (use-package sayid
;;   :defer t
;;   :init
;;   (setq sayid--key-binding-prefixes
;;           '(("mdt" . "trace")))

;;   (warmacs/local-leader-keys
;;     :major-modes t
;;     :keymaps '(clojure-mode-map)
;;                                     ;;These keybindings mostly preserved from the default sayid bindings
;;                                     "d!" 'sayid-load-enable-clear
;;                                     "dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
;;                                     "dc" 'sayid-clear-log
;;                                     "df" 'sayid-query-form-at-point
;;                                     "dh" 'sayid-show-help
;;                                     "ds" 'sayid-show-traced
;;                                     "dS" 'sayid-show-traced-ns
;;                                     "dtb" 'sayid-trace-ns-in-file
;;                                     "dtd" 'sayid-trace-fn-disable
;;                                     "dtD" 'sayid-trace-disable-all
;;                                     "dte" 'sayid-trace-fn-enable
;;                                     "dtE" 'sayid-trace-enable-all
;;                                     "dtK" 'sayid-kill-all-traces
;;                                     "dtn" 'sayid-inner-trace-fn
;;                                     "dto" 'sayid-outer-trace-fn
;;                                     "dtp" 'sayid-trace-ns-by-pattern
;;                                     "dtr" 'sayid-remove-trace-fn
;;                                     "dty" 'sayid-trace-all-ns-in-dir
;;                                     "dV" 'sayid-set-view
;;                                     "dw" 'sayid-get-workspace
;;                                     "dx" 'sayid-reset-workspace)
;;   :config
;;   (progn
;;     ;; If sayid-version is null the .elc file
;;     ;; is corrupted. Then force a reinstall and
;;     ;; reload the feature.
;;     (when (null sayid-version)
;;       (package-reinstall 'sayid)
;;       (unload-feature 'sayid)
;;       (require 'sayid)
;;       (setq cider-jack-in-lein-plugins (delete `("com.billpiel/sayid" nil) cider-jack-in-lein-plugins)))

;;     ;; Make it evil
;;     (evilified-state-evilify-map sayid-mode-map
;;                                  :mode sayid-mode
;;                                  :bindings
;;                                  (kbd "H") 'sayid-buf-show-help
;;                                  (kbd "n") 'sayid-buffer-nav-to-next
;;                                  (kbd "N") 'sayid-buffer-nav-to-prev
;;                                  (kbd "C-s v") 'sayid-toggle-view
;;                                  (kbd "C-s V") 'sayid-set-view
;;                                  (kbd "L") 'sayid-buf-back
;;                                  (kbd "e") 'sayid-gen-instance-expr) ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil
;;     (evilified-state-evilify-map sayid-pprint-mode-map
;;                                  :mode sayid-pprint-mode
;;                                  :bindings
;;                                  (kbd "h") 'sayid-pprint-buf-show-help
;;                                  (kbd "n") 'sayid-pprint-buf-next
;;                                  (kbd "N") 'sayid-pprint-buf-prev
;;                                  (kbd "l") 'sayid-pprint-buf-exit)
;;     (evilified-state-evilify-map sayid-traced-mode-map
;;                                  :mode sayid-traced-mode
;;                                  :bindings
;;                                  (kbd "l") 'sayid-show-traced
;;                                  (kbd "h") 'sayid-traced-buf-show-help)))

;; (use-package flycheck-clojure
;;   :if (configuration-layer/package-usedp 'flycheck)
;;   :config (progn
;;             (flycheck-clojure-setup)
;;             (with-eval-after-load 'cider
;;               (flycheck-clojure-inject-jack-in-dependencies))))

(provide 'layer/+lang/clojure)
