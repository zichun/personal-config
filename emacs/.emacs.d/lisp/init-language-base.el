;;; init-language-base.el -*- lexical-binding: t; -*-

(use-package powershell
  :defer t
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)))

;; Tree-sitter automatic grammar installation and mode remapping
(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; In-buffer completion using Corfu
(require 'init-language-corfu)

;; Language-specific configurations
(require 'init-language-rust-ts)
(require 'init-language-web)
(require 'init-language-cpp)
;; (require 'init-language-copilot)

(use-package flyover
  :defer t
  :hook (flycheck-mode . flyover-mode)
  :custom
  (flyover-levels '(error warning))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)
  (flyover-text-tint-percent 50)

  ;; Display settings
  (flyover-display-mode 'always)
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-show-at-eol t)
  (flyover-hide-when-cursor-is-on-same-line t)
  (flyover-virtual-line-icon "─►")
  ;; Performance
  (flyover-debounce-interval 0.1))

(provide 'init-language-base)
