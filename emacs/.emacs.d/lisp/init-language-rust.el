(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
;  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'my/rustic-mode-hook))

(defun my/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
                                        ;  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (rustic-lsp-server 'rust-analyzer)
  (lsp-rust-analyzer-server-display-inlay-hints 't)
  (lsp-eldoc-render-all t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-idle-delay 0.6)
  (lsp-enable-file-watchers 'nil)
  ;; (lsp-prefer-capf t)
  ;; (lsp-completion-provider :capf)
  ;; (lsp-completion-enable t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)

  :bind (
         ("C-M-." . lsp-ui-peek-find-definitions)
         ("C-?" . eldoc-doc-buffer)
         )
  )

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))
(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode :ensure)

(require 'lsp-ui)
(require 'rust-mode)

(setq lsp-rust-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-display-inlay-hints 't)
(setq lsp-ui-doc-enable 'nil)
(setq lsp-ui-sideline-code-actions-prefix "¤ ")
(setq lsp-ui-sideline-show-hover 't)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(add-hook 'rust-mode-hook
          (lambda ()
            (lsp)))

(setq lsp-prefer-capf t)
(setq lsp-completion-provider :capf)
(setq lsp-completion-enable t)

(provide 'init-language-rust)
