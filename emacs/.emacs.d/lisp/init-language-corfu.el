;; Modern completion-at-point using Corfu
;; This replaces company-mode for in-buffer completion

(use-package corfu
  :hook ((lsp-completion-mode . kb/corfu-setup-lsp)
         (eglot-managed-mode . kb/corfu-setup-lsp))
  :bind (:map corfu-map
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous)
         ("<escape>" . corfu-quit)
         ("<return>" . corfu-insert)
         ("M-d" . corfu-show-documentation)
         ("C-g" . corfu-quit)
         ("M-l" . corfu-show-location))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (corfu-echo-documentation nil)
  (lsp-completion-provider :none)
  :init
  (global-corfu-mode)
  (setq corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-mode)
  :config
  ;; Enable Corfu in minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; Setup for LSP/eglot compatibility
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-language-corfu)
