;; from https://gist.github.com/3402786
(defun warmacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(defun warmacs/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(+general-global-menu! "windows" "w"
  "TAB"  'warmacs/alternate-window
  "1"  'warmacs/window-split-single-column
  "2"  'warmacs/window-split-double-columns
  "3"  'warmacs/window-split-triple-columns
  "4"  'warmacs/window-split-grid
  "b"  'warmacs/switch-to-minibuffer-window
  "d"  'warmacs/delete-window
  "t"  'warmacs/toggle-current-window-dedication
  "f"  'follow-mode
  "F"  'make-frame
  "H"  'evil-window-move-far-left
  "<S-left>"  'evil-window-move-far-left
  "h"  'evil-window-left
  "<left>"  'evil-window-left
  "J"  'evil-window-move-very-bottom
  "<S-down>"  'evil-window-move-very-bottom
  "j"  'evil-window-down
  "<down>"  'evil-window-down
  "K"  'evil-window-move-very-top
  "<S-up>"  'evil-window-move-very-top
  "k"  'evil-window-up
  "<up>"  'evil-window-up
  "L"  'evil-window-move-far-right
  "<S-right>"  'evil-window-move-far-right
  "l"  'evil-window-right
  "<right>"  'evil-window-right
  "m"  'warmacs/toggle-maximize-buffer
  ;; "wcc"  'warmacs/toggle-centered-buffer
  ;; "wcC"  'warmacs/toggle-distraction-free
  ;; "wc."  'warmacs/centered-buffer-transient-state
  "o"  'other-frame
  "r"  'warmacs/rotate-windows-forward
  "R"  'warmacs/rotate-windows-backward
  "s"  'split-window-below
  "S"  'split-window-below-and-focus
  "-"  'split-window-below
  "U"  'winner-redo
  "u"  'winner-undo
  "v"  'split-window-right
  "V"  'split-window-right-and-focus
  "w"  'other-window
  "x"  'kill-buffer-and-window
  "/"  'split-window-right
  "="  'balance-windows-area
  "+"  'warmacs/window-layout-toggle
  "_"  'warmacs/maximize-horizontally
  "|"  'warmacs/maximize-vertically)
 
