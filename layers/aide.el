;; -*- lexical-binding: t; -*-

(use-package aide
  :straight (aide
             :type git
             :host github
             :repo "junjizhi/aide.el"
             :depth full
             :fork (:host github
                          :repo "WarFox/aide.el"))
  :custom
  (aide-temperature 0.3)
  (aide-top-p 1)
  (aide-completions-model "text-davinci-002")
  :general
  (warmacs/leader-menu "open-ai" "o"
    "o" 'aide-openai-tldr-region
    "i" 'aide-openai-complete-buffer-insert
    "r" 'aide-openai-complete-region
    "R" 'aide-openai-complete-region-insert))


(provide 'layer/aide)
;;; aide.el ends here
