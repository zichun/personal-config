(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :init
  (add-hook 'completion-preview-mode 'copilot-mode)
  )
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

(provide 'init-language-copilot)
