(remove-hook 'c++-mode-hook 'flycheck-mode)
(provide 'init-language-cpp)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r")))

(setq c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 0)
              (flycheck-mode 0))))

(use-package c-ts-mode
  :defer t
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  :init
  ;; Remap the standard C/C++ modes
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))
