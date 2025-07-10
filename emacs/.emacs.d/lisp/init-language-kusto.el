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

(defvar kusto-lsp--current-server nil)

;; Advice to track current server
(defun kusto-lsp--track-server (orig-fun server &rest args)
  "Track the current server for context."
  (let ((kusto-lsp--current-server server))
    (apply orig-fun server args)))

(advice-add #'jsonrpc-request :around #'kusto-lsp--track-server)

;; Advice to fix JSON encoding for shutdown requests
(defun kusto-lsp-fix-shutdown-json (orig-fun object)
  "Remove null params from shutdown requests for Kusto LSP."
  (if (and kusto-lsp--current-server
           (listp object)
           (equal (plist-get object :method) "shutdown")
           (plist-member object :params)
           (null (plist-get object :params)))
      ;; Remove :params from the plist
      (let ((fixed-object (copy-sequence object)))
        (cl-remf fixed-object :params)
        (funcall orig-fun fixed-object))
    (funcall orig-fun object)))

(advice-add #'jsonrpc--json-encode :around #'kusto-lsp-fix-shutdown-json)

;; Provide feature
(provide 'init-language-kusto)
