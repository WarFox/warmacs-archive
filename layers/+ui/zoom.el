;;; +ui/zoom.el -*- lexical-binding: t; -*-

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" text-scale-adjust "adjust")
  ("q" nil "finished" :exit t))

(warmacs/leader-menu-zoom
  "s" '(hydra-text-scale/body :which-key "scale text"))

(provide 'layer/+ui/zoom)
