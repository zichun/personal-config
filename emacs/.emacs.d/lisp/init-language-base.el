(use-package powershell
  :config
  (autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t))

;; Enable gg-modes for c-mode

(setq ggtags-completing-read-function
      (lambda (&rest args)
        (apply #'ido-completing-read
               (car args)
               (all-completions "" ggtags-completion-table)
               (cddr args))))

;; (add-hook 'c-mode-hook 'counsel-gtags-mode)
;; (add-hook 'c++-mode-hook 'counsel-gtags-mode)

(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never '%s' .")

;; (with-eval-after-load 'counsel-gtags
;;   (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-find-definition)
;;   (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-find-reference)
;;   (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
;;   (define-key counsel-gtags-mode-map (kbd "M-n") 'counsel-gtags-go-forward)
;;   (define-key counsel-gtags-mode-map (kbd "M-p") 'counsel-gtags-go-backward))

;; Enable rainbow-delimiters for c-mode

(add-hook 'c-mode-common-hook
          'rainbow-delimiters-mode)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r")))

(setq c-basic-offset 4)

;;
;; cc-mode c++-mode customize font locking
(global-font-lock-mode t)
(setq font-lock-maximum-decoration 2
	  font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

(setq auto-mode-alist
      (append '(
                ("\\.cs$" . csharp-mode)
                ("\\.js$" . js3-mode)
                ("\\.ps1$" . powershell-mode)
                ("\\.psm1$" . powershell-mode)
                ("\\.log$" . log4j-mode)
                ) auto-mode-alist ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 0)
              (flycheck-mode 0))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist '(rust toml c++))
  (global-treesit-auto-mode))

;; (use-package company
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-minimum-prefix-length 1)
;;   :custom
;;   (company-idle-delay 0.3) ;; how long to wait until popup
;;   ;; (company-begin-commands nil) ;; uncomment to disable popup
;;   :bind
;;   (:map company-active-map
;; 	    ("C-n". company-select-next)
;; 	    ("C-p". company-select-previous)
;; 	    ("M-<". company-select-first)
;; 	    ("M->". company-select-last)))

;; use corfu isntead of company
(require 'init-language-corfu)

                                        ;(require 'ra-emacs-lsp)
;(require 'init-language-rust)
(require 'init-language-rust-ts)
(require 'init-language-web)
(require 'init-language-cpp)

(provide 'init-language-base)
