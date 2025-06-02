(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
            :rev :newest
            :branch "main")
  :after eglot
  :config	(eglot-booster-mode))

;; Configure eglot with rust-analyzer
(use-package eglot
  :ensure t
  :defer t
  :hook ((rust-ts-mode . eglot-ensure))
;         (rust-ts-mode . eglot-inlay-hints-mode))
  :config
  ;; Properly configure rust-analyzer with logging and improved settings
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions (
                    :check (:command "clippy")
                    ;; Explicitly disable expensive initial operations
                    :checkOnSave (:enable nil)
                    :cargo (:buildScripts (:enable nil)
                            :features ""
                            :loadOutDirsFromCheck nil
                            :noDefaultFeatures t
                            :targetDir "target/rust-analyzer" ;; Custom target directory
                            :target nil)
                    ;; Limit analysis scope
                    :procMacro (:enable nil)
                    :diagnostics (:disabled ["unresolved-import"
                                            "unresolved-proc-macro"])
                    :files (:excludeDirs ["target" "tests" "examples"])
                    :lruCapacity 64))))
  :custom
  ((eglot-sync-connect nil)         ;; Don't wait for server
   (eglot-events-buffer-size 0)     ;; Disable events buffer
   (eglot-autoshutdown t)          ;; Shutdown when not needed
   (eglot-connect-timeout 30)))     ;; Timeout for connection attempts

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :bind
  ("C-c C-c a" . eglot-code-actions)
  ("C-c C-c r" . eglot-rename)
  ("C-c C-c q" . eglot-restart)
  ("C-c C-c f" . eglot-format-buffer))

(provide 'init-language-rust-ts)
