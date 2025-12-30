(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . flycheck-mode)
         (tsx-ts-mode . flycheck-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package js
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :hook (js-ts-mode . eglot-ensure))

(use-package json-ts-mode
  :mode "\\.json\\'"
  :hook (json-ts-mode . flycheck-mode))

;; Ensure Eglot uses Flycheck
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (bound-and-true-p flycheck-mode)
                (flycheck-mode 1)))))

(provide 'init-language-web)
