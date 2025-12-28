(use-package powershell
  :defer t
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)))

(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never '%s' .")

;;
;; cc-mode c++-mode customize font locking
;; (global-font-lock-mode t)
;; (setq font-lock-maximum-decoration 2
;; 	  font-lock-maximum-size nil)
;; (setq font-lock-support-mode 'jit-lock-mode)
;; (setq jit-lock-stealth-time 16
;;       jit-lock-defer-contextually t
;;       jit-lock-stealth-nice 0.5)
;; (setq-default font-lock-multiline t)

;; (use-package treesit-auto
;;   :demand t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist '(rust toml c++))
;;   (global-treesit-auto-mode))

(use-package treesit-auto
  :defer t
  :hook (find-file . treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist '(rust toml c++)))

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
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-show-at-eol t)
  (flyover-hide-when-cursor-is-on-same-line t)
  (flyover-virtual-line-icon "─►")
;  (flyover-line-position-offset 1)

  ;; Performance
  (flyover-debounce-interval 0.1))

(provide 'init-language-base)
