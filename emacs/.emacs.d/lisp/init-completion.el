;; Modern completion framework using Vertico, Consult, Marginalia, and Corfu
;; This replaces the older ivy/counsel setup

(use-package marginalia
  :defer t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :hook (after-init . marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :demand t
  :bind (:map vertico-map
         ("<tab>" . vertico-insert)
         ("<escape>" . minibuffer-keyboard-quit)
         ("?" . minibuffer-completion-help)
         ("C-M-n" . vertico-next-group)
         ("C-M-p" . vertico-previous-group)
         ("<backspace>" . vertico-directory-delete-char)
         ("C-w" . vertico-directory-delete-word)
         ("C-<backspace>" . vertico-directory-delete-word)
         ("RET" . vertico-directory-enter))
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  :init
  (vertico-mode)
  :config
  ;; Prefix the current candidate with "» "
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c f" . consult-ripgrep)
         ("C-x C-i" . consult-imenu)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

(use-package consult-ls-git
  :bind (("C-c g" . consult-ls-git)))

;; Use hotfuzz for fuzzy completion
(use-package hotfuzz
  :custom
  (completion-styles '(hotfuzz))
  (completion-ignore-case t))

(provide 'init-completion)