;; GitHub Copilot integration using modern vc package syntax
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (completion-preview-mode . copilot-mode)
  :bind (:map copilot-completion-map
         ("<tab>" . copilot-accept-completion)))

(provide 'init-language-copilot)
