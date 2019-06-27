(require 'lsp-mode)
;; (require 's)
;; (require 'f)


(setq lsp-trace 't)
(setq lsp-print-io 't)


(defvar lsp-cpp-exe "c:\\tools\\personal-configs\\emacs\\.emacs.d\\server\\Microsoft.VSCode.CPP.Extension.exe")


(defvar lsp-cpp-cache-dir (expand-file-name ".lsp-cpp" user-emacs-directory)
 "Path to directory where server will write cache files. Must not nil.")

(defun lsp-cpp--extra-init-params ()
 "Return form describing parameters for language server."
 )

(defvar lsp-cpp--sess-id 0)
(defvar lsp-cpp--major-modes '(c++-mode))

(lsp-register-client
(make-lsp-client
 :new-connection (lsp-stdio-connection "Microsoft.VSCode.CPP.Extension.exe")
 :major-modes '(c++-mode)
 :server-id 'cpp-ls
 :priority 0
 ))

(provide 'lsp-cpp)

