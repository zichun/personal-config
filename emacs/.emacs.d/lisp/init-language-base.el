;;; init-language-base.el -*- lexical-binding: t; -*-

(use-package powershell
  :defer t
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)))

;; Tree-sitter automatic grammar installation and mode remapping
(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (defvar zc/treesit-auto--cached-remap 'uninitialized)
  (defvar zc/treesit-auto--cached-langs nil)

  (defun zc/treesit-auto--installed-langs ()
    "Return list of tree-sitter language symbols with grammars on disk."
    (let ((dirs (delq nil (delete-dups
                           (append (list (expand-file-name "tree-sitter" user-emacs-directory))
                                   (bound-and-true-p treesit-extra-load-path)))))
          (langs nil))
      (dolist (dir dirs langs)
        (when (file-directory-p dir)
          (dolist (file (directory-files dir nil "\\`libtree-sitter-.*\\.\\(dll\\|so\\|dylib\\)\\'"))
            (when (string-match "\\`libtree-sitter-\\(.+\\)\\.\\(?:dll\\|so\\|dylib\\)\\'" file)
              (push (intern (match-string 1 file)) langs)))))))

  (defun zc/treesit-auto--fast-build-remap (&rest _)
    "Cached replacement for `treesit-auto--build-major-mode-remap-alist'.
Avoids probing 57 missing DLLs on disk on every `set-auto-mode' call."
    (when (or (eq zc/treesit-auto--cached-remap 'uninitialized)
              (not (equal zc/treesit-auto--cached-langs treesit-auto-langs)))
      (let* ((installed (zc/treesit-auto--installed-langs))
             (remap-alist nil))
        (dolist (recipe (treesit-auto--selected-recipes))
          (let ((lang (treesit-auto-recipe-lang recipe))
                (ts-mode (treesit-auto-recipe-ts-mode recipe)))
            (when (and (or (null installed) (memq lang installed))
                       (treesit-auto--ready-p ts-mode))
              (dolist (remap (ensure-list (treesit-auto-recipe-remap recipe)))
                (push (cons remap ts-mode) remap-alist)))))
        (setq zc/treesit-auto--cached-langs (copy-sequence treesit-auto-langs)
              zc/treesit-auto--cached-remap remap-alist)))
    (append (default-value 'major-mode-remap-alist) zc/treesit-auto--cached-remap))

  (advice-add #'treesit-auto--build-major-mode-remap-alist :override #'zc/treesit-auto--fast-build-remap)
  (advice-add #'treesit-install-language-grammar :after
              (lambda (&rest _)
                (setq zc/treesit-auto--cached-remap 'uninitialized)))

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; In-buffer completion using Corfu
(require 'init-language-corfu)

;; Language-specific configurations
(require 'init-language-rust-ts)
(require 'init-language-web)
(require 'init-language-cpp)
;; (require 'init-language-copilot)

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
  (flyover-display-mode 'always)
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-show-at-eol t)
  (flyover-hide-when-cursor-is-on-same-line t)
  (flyover-virtual-line-icon "─►")
  ;; Performance
  (flyover-debounce-interval 0.1))

(provide 'init-language-base)
