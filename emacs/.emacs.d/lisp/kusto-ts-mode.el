;; ;;; kusto-ts-mode.el --- Basic major mode for Kusto using tree-sitter

;; ;(when (treesit-language-available-p 'kusto)

;; (define-derived-mode kusto-ts-mode prog-mode "Kusto"
;;   "Major mode for editing Kusto scripts using tree-sitter."
;;   :syntax-table nil
;;   :abbrev-table nil
;;   (setq-local comment-start "//")
;;   (setq-local comment-end "")

;;   ;; Enable tree-sitter for this mode
;;   (treesit-parser-create 'kusto)
;;   (setq-local treesit-font-lock-feature-list
;;               '((comment definition)
;;                 (string)
;;                 (keyword type builtin)
;;                 (function variable constant)))

;;   (treesit-major-mode-setup))

;; (add-to-list 'auto-mode-alist '("\\.csl\\'" . kusto-ts-mode))

;; (add-to-list 'treesit-language-source-alist
;;              '(kusto "https://github.com/Willem-J-an/tree-sitter-kusto"
;;                      nil
;;                      nil
;;                      "~/.emacs.d/treesit-queries"))

(defvar kusto-ts-mode-font-lock-rules
  (treesit-font-lock-rules
   ;; Comments
   :language 'kusto
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; Strings
   :language 'kusto
   :feature 'string
   '((string) @font-lock-string-face)

   ;; Numbers and constants
   :language 'kusto
   :feature 'constant
   '((number) @font-lock-number-face
     (bool) @font-lock-constant-face
     (null) @font-lock-constant-face
     (join_types) @font-lock-constant-face)

   ;; Keywords
   :language 'kusto
   :feature 'keyword
   '((let_keyword) @font-lock-keyword-face
     (sort_keyword) @font-lock-keyword-face
     (binary_operator) @font-lock-keyword-face
     (compound_keywords) @font-lock-keyword-face
     (operator) @font-lock-keyword-face
     (range_operator) @font-lock-keyword-face
     (join_operator) @font-lock-keyword-face
     (sub_operator) @font-lock-keyword-face
     (to_operator) @font-lock-keyword-face
     (mv_apply_operator) @font-lock-keyword-face)

   ;; Function calls
   :language 'kusto
   :feature 'function
   '((function_call
      (identifier) @font-lock-function-name-face)
     (type_cast_function) @font-lock-function-name-face
     (to_scalar_function) @font-lock-function-name-face
     (between_function) @font-lock-function-name-face
     (datatable_function) @font-lock-function-name-face)

   ;; Types
   :language 'kusto
   :feature 'type
   '((type) @font-lock-type-face)

   ;; Variables and identifiers
   :language 'kusto
   :feature 'variable
   '((let_statement
      (identifier) @font-lock-variable-name-face)
     (typed_parameter
      (identifier) @font-lock-variable-name-face)
     (function_arguments (identifier) @font-lock-variable-name-face)
     (operation (identifier) @font-lock-variable-name-face)
     (compound_expression (identifier) @font-lock-variable-name-face)
     (binary_expression (identifier) @font-lock-variable-name-face)
     (assignment (identifier) @font-lock-variable-name-face)
     (property_identifier (identifier) @font-lock-property-name-face)
     (property_index (identifier) @font-lock-property-name-face)
     (sort_by (identifier) @font-lock-variable-name-face)
     (range_operation (identifier) @font-lock-variable-name-face)
     ;; Catch-all for other identifiers
     (source) @font-lock-variable-name-face)

   ;; Delimiters and brackets
   :language 'kusto
   :feature 'delimiter
   '((pipe) @font-lock-delimiter-face
     ["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face
     [","] @font-lock-delimiter-face)))

(define-derived-mode kusto-ts-mode prog-mode "Kusto"
  "Major mode for editing Kusto scripts using tree-sitter."
  :syntax-table nil
  :abbrev-table nil
  (setq-local comment-start "//")
  (setq-local comment-end "")

  ;; Enable tree-sitter for this mode
  (treesit-parser-create 'kusto)
  
  ;; Set the font-lock rules
  (setq-local treesit-font-lock-settings kusto-ts-mode-font-lock-rules)
  
  ;; Define which features to enable
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (keyword constant type)
                (function variable)
                (delimiter)))

  (treesit-major-mode-setup))

(add-to-list 'auto-mode-alist '("\\.csl\\'" . kusto-ts-mode))

;; (add-to-list 'treesit-extra-load-path "~/.emacs.d/treesit-queries")

(provide 'kusto-ts-mode)
