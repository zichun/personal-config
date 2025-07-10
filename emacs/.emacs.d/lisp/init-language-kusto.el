;; Kusto LSP configuration for Eglot

(require 'eglot)

;; Define the LSP server command
(defcustom kusto-lsp-server-command '("C:/repos/sandbox/kusto-lsp/target/debug/kusto-lsp.exe")
  "Command to run the Kusto LSP server."
  :type '(repeat string)
  :group 'kusto)

;; Add Kusto LSP server to Eglot's server programs
(add-to-list 'eglot-server-programs
             `(kusto-ts-mode . ,kusto-lsp-server-command))

;; Optional: Automatically start Eglot when opening Kusto files
(add-hook 'kusto-ts-mode-hook #'eglot-ensure)

;; Optional: Configure Eglot settings for Kusto
(defun kusto-eglot-config ()
  "Configure Eglot settings for Kusto mode."
  (setq-local eglot-autoshutdown t)  ; Shutdown LSP server when last buffer is closed
  (setq-local eglot-sync-connect-timeout 10)  ; Connection timeout in seconds
  )

(add-hook 'kusto-ts-mode-hook #'kusto-eglot-config)

(cl-defmethod eglot-initialization-options ((server eglot-lsp-server) &context (major-mode kusto-ts-mode))
  "Provide initialization options for Kusto LSP server."
  '(:completion (:triggerCharacters ["|" "." "("])
    :hover t))

;; If you're using Corfu for completion (recommended with Eglot)
(when (featurep 'corfu)
  (add-hook 'kusto-ts-mode-hook
            (lambda ()
              (setq-local corfu-auto t)  ; Enable automatic completion
              (setq-local corfu-auto-delay 0.2)  ; Delay before showing completions
              (setq-local corfu-auto-prefix 2))))  ; Minimum prefix length

;; Advice to fix null params for Kusto LSP only
(defun kusto-lsp-fix-shutdown-params (orig-fun connection method params &rest args)
  "Fix null params in shutdown request for Kusto LSP servers."
  (if (and (object-of-class-p connection 'eglot-kusto-lsp)
           (eq method :shutdown)
           (null params))
      ;; For Kusto LSP shutdown, don't pass params at all
      (apply orig-fun connection method args)
    ;; Normal call for everything else
    (apply orig-fun connection method params args)))

;(advice-add 'jsonrpc-request :around #'kusto-lsp-fix-shutdown-params)

;; Provide feature
(provide 'init-language-kusto)
