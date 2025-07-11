;; Development tools and utilities

;; Git integration
(use-package git-messenger
  :defer t
  :bind ("C-x v p" . git-messenger:popup-message)
  :custom
  (git-messenger:show-detail t))

;; Programming text formatting
(use-package prog-fill
  :defer t
  :bind ("M-q" . prog-fill)
  :custom
  (prog-fill-floating-open-paren-p nil)
  (prog-fill-floating-close-paren-p nil))

;; Code folding
(use-package origami
  :defer t)

;; Git time machine
(use-package git-timemachine
  :defer t)

;; Region expansion
(use-package expand-region
  :defer t
  :after org
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Multiple cursors
(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Interactive edit mode
(use-package iedit
  :bind ("C-;" . iedit-mode)
  :config
  (set-face-background 'iedit-occurrence "Magenta"))

;; Shell popup terminal
(use-package shell-pop
  :bind ("C-t" . shell-pop)
  :custom
  (shell-pop-window-position "bottom")
  (shell-pop-full-span t)
  (shell-pop-window-size 30)
  :config
  (when window-system
    (if (string-equal system-type "windows-nt")
        (progn
          (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
          (setq shell-file-name "powershell")
          (setq explicit-pwsh.exe-args '("-noprofile")))
      ;; Unix systems use default shell
      )))

;; Hide trailing whitespace in shell mode
(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))

;; Visual bookmarks
(use-package bm
  :demand t
  :bind (("<f1>" . bm-next)
         ("S-<f1>" . bm-previous)
         ("C-<f1>" . bm-toggle))
  :custom
  (bm-restore-repository-on-load t)
  (bm-repository-file "~/.emacs.d/bm-repository")
  (bm-buffer-persistence t)
  :config
  ;; Loading and saving bookmarks
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

(provide 'init-tools)
