(require 'lsp-ui)
(require 'rust-mode)

(setq lsp-rust-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-display-inlay-hints 't)
(setq lsp-ui-doc-enable 'nil)
(setq lsp-ui-sideline-code-actions-prefix " ")
(setq lsp-ui-sideline-show-hover 't)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(add-hook 'rust-mode-hook
          (lambda ()
            (lsp)))

(provide 'init-language-rust)
