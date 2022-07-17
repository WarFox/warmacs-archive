(use-package evil
  :defer 2
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :init
  (progn
    (message "loaded evil")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :init
  (progn
    ;; double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (defun warmacs/comment-or-uncomment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun warmacs/comment-or-uncomment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun warmacs/copy-and-comment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-copy-and-comment-lines arg)))

    (defun warmacs/copy-and-comment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-copy-and-comment-lines arg)))

    (defun warmacs/quick-comment-or-uncomment-to-the-line-inverse
      (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun warmacs/quick-comment-or-uncomment-to-the-line (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun warmacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (defun warmacs/comment-or-uncomment-paragraphs (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (general-def evil-normal-state-map
	    "gc" 'evilnc-comment-operator
      "gy" 'warmacs/copy-and-comment-lines)

    (global-definer
      ";"  'evilnc-comment-operator
	    "c"  '(:ignore t :which-key "comment")
      "cl" 'warmacs/comment-or-uncomment-lines
      "cL" 'warmacs/comment-or-uncomment-lines-inverse
      "cp" 'warmacs/comment-or-uncomment-paragraphs
      "cP" 'warmacs/comment-or-uncomment-paragraphs-inverse
      "ct" 'warmacs/quick-comment-or-uncomment-to-the-line
      "cT" 'warmacs/quick-comment-or-uncomment-to-the-line-inverse
      "cy" 'warmacs/copy-and-comment-lines
      "cY" 'warmacs/copy-and-comment-lines-inverse)))

(use-package evil-mc
  :init
  (evil-mc-mode 1))
