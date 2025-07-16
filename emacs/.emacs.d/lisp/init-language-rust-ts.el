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
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . flycheck-mode)
         )
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

(use-package flycheck-rust
  :defer t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(require 'cl-lib)
(defun treesit--find-argument-parent ()
  "Recursively find the nearest parent whose children include a comma."
  (let ((node (treesit-node-at (point)))
        found-parent siblings)
    (while (and node (not found-parent))
      (setq siblings (treesit-node-children node))
      (when (cl-find-if (lambda (sib)
                          (equal (treesit-node-type sib) ","))
                        siblings)
        (setq found-parent node))
      (setq node (treesit-node-parent node)))
    found-parent))

(defun treesit--get-surronding-commas (siblings point)
  "Returns indices of commas in a list of siblings to point"
  (let* ((index 0)
         (past_point 'nil)
         prev_index
         next_index)
    (while (and (not next_index) siblings)
      (let ((sib (car siblings)))

        (if (not past_point)
            (if (>= (treesit-node-start sib) point)
                (setq past_point 't)))
        (if (equal (treesit-node-type sib) ",")
            (if (and past_point (not next_index))
                (setq next_index index)
              (if (not past_point)
                  (setq prev_index index))))
        (setq index (1+ index))
        (setq siblings (cdr siblings))

        ))
    (cons prev_index next_index)
  ))

(defun treesit--find-argument-parent ()
  "Recursively find the nearest parent whose children include a comma."
  (let ((node (treesit-node-at (point)))
        found-parent siblings)
    (while (and node (not found-parent))
      (setq siblings (treesit-node-children node))
      (when (cl-find-if (lambda (sib)
                          (equal (treesit-node-type sib) ","))
                        siblings)
        (setq found-parent node))
      (setq node (treesit-node-parent node)))
    found-parent))

(defun treesit-move-to-argument-end ()
  (interactive)
  (let ((point (point))
        (found-parent (treesit--find-argument-parent)))
    (when found-parent
      (let* ((siblings (treesit-node-children found-parent))
             (comma-index (cl-position-if (lambda (sib)
                                            (and (> (treesit-node-start sib) point)
                                                 (equal (treesit-node-type sib) ",")))
                                          siblings)))
        (if comma-index
            (goto-char (treesit-node-start (car (cl-subseq siblings comma-index))))
          (let ((last-arg
                 (cl-find-if (lambda (sib)
                               (not (member (treesit-node-type sib)
                                            '("," "(" ")"))))
                             (reverse siblings))))
            (when last-arg
              (goto-char (treesit-node-end last-arg)))
        ))))))

(defun treesit-move-to-next-argument ()
  "Move point to the start of the next argument in a function/macro call using tree-sitter.
Recursively finds the nearest parent whose children include a comma."
  (interactive)
  (let ((point (point))
        (found-parent (treesit--find-argument-parent)))
    (when found-parent
      (let* ((siblings (treesit-node-children found-parent))
             (comma-index (cl-position-if (lambda (sib)
                                            (and (> (treesit-node-start sib) point)
                                                 (equal (treesit-node-type sib) ",")))
                                          siblings)))
        (when comma-index
          ;; Find the next non-punctuation node after the comma
          (let ((next-arg
                 (cl-find-if (lambda (sib)
                               (not (member (treesit-node-type sib)
                                            '("," "(" ")"))))
                             (cl-subseq siblings (1+ comma-index)))))
            (when next-arg
              (goto-char (treesit-node-start next-arg)))))))))

(defun treesit-move-to-prev-argument ()
  "Move point to the start of the previous argument in a function/macro call using tree-sitter."
  (interactive)
  (let* ((point (point))
         (found-parent (treesit--find-argument-parent)))
    (when found-parent
      (let* ((siblings (treesit-node-children found-parent))
             (commas (treesit--get-surronding-commas siblings point))
             (prev_comma (car commas)))
        (print commas)
        (if prev_comma
            (let ((ind (treesit-node-start (car (cl-subseq siblings (1+ prev_comma))))))
              (if (< ind point)
                  (goto-char ind)
                (when (> prev_comma 0)
                  (goto-char (treesit-node-end (car (cl-subseq siblings (- prev_comma 1))))))))
          (let ((first-arg
                 (cl-find-if (lambda (sib)
                               (not (member (treesit-node-type sib)
                                            '("," "(" ")"))))
                             siblings)))
            (when first-arg
              (goto-char (treesit-node-start first-arg)))))))))


;; (defun treesit-move-to-prev-argument ()
;;   "Move point to the start of the previous argument in a function/macro call using tree-sitter."
;;   (interactive)
;;   (let* ((point (point))
;;          (found-parent (treesit--find-argument-parent)))
;;     (when found-parent
;;       (let* ((siblings (treesit-node-children found-parent))
;;              ;; Define punctuation types to ignore
;;              (punctuation '("," "(" ")" "&"))
;;              ;; Get all arguments (non-punctuation)
;;              (args (cl-remove-if (lambda (sib)
;;                                    (member (treesit-node-type sib) punctuation))
;;                                  siblings))
;;              ;; Find all arguments whose start is strictly less than point
;;              (prev-args (cl-remove-if-not (lambda (sib)
;;                                             (< (treesit-node-start sib) point))
;;                                           args)))
;;         (when prev-args
;;           (goto-char (treesit-node-start (car (last prev-args)))))))))

;; Keybindings for Rust tree-sitter major mode.
(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "M-e") #'treesit-move-to-argument-end)
  (define-key rust-ts-mode-map (kbd "C-M-n") #'treesit-move-to-next-argument)
  (define-key rust-ts-mode-map (kbd "C-M-p") #'treesit-move-to-prev-argument))

(provide 'init-language-rust-ts)
