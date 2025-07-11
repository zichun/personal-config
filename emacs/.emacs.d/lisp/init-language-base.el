;; Base language configuration and programming modes

;; PowerShell mode
(use-package powershell
  :defer t
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)))

;; Tree-sitter integration (modern syntax parsing)
(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist '(rust toml c++))
  (global-treesit-auto-mode))

;; Enhanced language-specific configurations
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)
                ("\\.js$" . js3-mode)
                ("\\.log$" . log4j-mode))
              auto-mode-alist))

;; Font lock and syntax highlighting optimization
(global-font-lock-mode t)
(setq font-lock-maximum-decoration 2
      font-lock-maximum-size nil
      font-lock-support-mode 'jit-lock-mode
      jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;; C/C++ configuration
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r")))
(setq c-basic-offset 4)

;; Rainbow delimiters for programming modes
(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)

;; Programming mode hooks
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 0)
              (flycheck-mode 0))))

;; Ripgrep integration for counsel-grep
(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never '%s' .")

;; Use Corfu instead of Company for completion
(require 'init-language-corfu)

;; Language-specific configurations
(require 'init-language-rust-ts)
(require 'init-language-web)
(require 'init-language-cpp)
(require 'init-language-copilot)

;; Kusto language support
(require 'kusto-ts-mode)
(require 'init-language-kusto)

;; Flyover integration for better error display
(use-package flyover
  :defer t
  :hook (flycheck-mode . flyover-mode)
  :custom
  (flyover-levels '(error warning))
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)
  (flyover-text-tint-percent 50))

(provide 'init-language-base)
