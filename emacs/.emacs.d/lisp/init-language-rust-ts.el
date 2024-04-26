;; Configure eglot with rust-analyzer
(use-package eglot
  :ensure t
  :defer t
  :hook ((rust-ts-mode . eglot-ensure))
;         (rust-ts-mode . eglot-inlay-hints-mode))
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  (:check (:command "clippy")))))
  :custom
  ((eglot-sync-connect 0)))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :bind
  ("C-c C-c a" . eglot-code-actions)
  ("C-c C-c r" . eglot-rename)
  ("C-c C-c q" . eglot-restart)
  ("C-c C-c f" . eglot-format-buffer))

(provide 'init-language-rust-ts)
