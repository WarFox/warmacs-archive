
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(+general-global-menu! "zoom" "z"
  "s" '(hydra-text-scale/body :which-key "scale text"))
