(require 'lsp-mode)

(setq lsp-trace 't)
(setq lsp-print-io 't)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection "c:\\tools\\cpptools\\Microsoft.VSCode.CPP.Extension.exe")
  :major-modes '(c++-mode)
  :server-id 'cpp-ls
  :priority 0
  ))

(provide 'lsp-cpp)
