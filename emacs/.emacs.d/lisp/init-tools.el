;; git-messenger
(use-package git-messenger
  :ensure t
  :defer t
  :commands git-messenger:popup-message
  :bind (("C-x v p" . git-messenger:popup-message))
  :config
  (setq git-messenger:show-detail t))

;; prog-fill
(use-package prog-fill
  :ensure t
  :defer t
  :commands prog-fill
  :bind (("M-q" . prog-fill))
  :config
  (setq prog-fill-floating-open-paren-p 'nil)
  (setq prog-fill-floating-close-paren-p 'nil))

;; Set-up origami
(use-package origami
  :defer t
  :ensure t)

;; git-timemachine
(use-package git-timemachine
  :defer t
  :ensure t)

;; expand-region

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ;; I almost never use this, so it has an awkward binding
         ("C--" . er/contract-region)))

(use-package multiple-cursors
  :defer t
  :bind
  ("C-S-c C-S-c"   . mc/edit-lines)
  ("C->"   . mc/mark-next-like-this)
  ("C-<"   . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

;; shell-pop

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
  (setq shell-pop-window-position "bottom")
  (setq shell-pop-full-span t)
  (setq shell-pop-window-size 30)
  (setq shell-file-name "powershell")
  (setq explicit-pwsh.exe-args '("-noprofile")))


(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))

(provide 'init-tools)
