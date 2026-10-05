;;; init-language-rust-ts.el -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil  ; Built-in to Emacs 29+
  :defer t
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . flymake-mode))
  :config
  ;; Optimization: Fast communication with rust-analyzer
  (setq eglot-events-buffer-size 0
        eglot-connect-timeout 30
        json-serialize-default-json-false :json-false)

  (add-to-list 'eglot-server-programs
               `((rust-ts-mode rustic-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  (
                   ;; Disable Check On Save to prevent blocking during rapid edits
                   :checkOnSave :json-false

                   ;; Limit cargo operations
                   :cargo (:buildScripts (:enable :json-false)
                           :features "all")

                   ;; Disable Proc Macros for max responsiveness
                   :procMacro (:enable :json-false)

                   ;; Exclude heavy directories
                   :files (:excludeDirs ["target" "tests" "examples" "node_modules"])

                   ;; Disable expensive diagnostics
                   :diagnostics (:disabled ["unresolved-import" "unresolved-proc-macro" "inactive-code"])
                   )))))

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
         (past_point nil)
         prev_index
         next_index)
    (while (and (not next_index) siblings)
      (let ((sib (car siblings)))
        (if (not past_point)
            (if (>= (treesit-node-start sib) point)
                (setq past_point t)))
        (if (equal (treesit-node-type sib) ",")
            (if (and past_point (not next_index))
                (setq next_index index)
              (if (not past_point)
                  (setq prev_index index))))
        (setq index (1+ index))
        (setq siblings (cdr siblings))))
    (cons prev_index next_index)))

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
              (goto-char (treesit-node-end last-arg)))))))))

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

;; Keybindings for Rust tree-sitter major mode
(with-eval-after-load 'rust-ts-mode
  (keymap-set rust-ts-mode-map "M-e" #'treesit-move-to-argument-end)
  (keymap-set rust-ts-mode-map "C-M-n" #'treesit-move-to-next-argument)
  (keymap-set rust-ts-mode-map "C-M-p" #'treesit-move-to-prev-argument))

(provide 'init-language-rust-ts)
