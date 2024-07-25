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
  :after (org)
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

(if window-system
  (use-package shell-pop
    :bind (("C-t" . shell-pop))
    :config
    (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
    (setq shell-pop-window-position "bottom")
    (setq shell-pop-full-span t)
    (setq shell-pop-window-size 30)
    (setq shell-file-name "powershell")
    (setq explicit-pwsh.exe-args '("-noprofile")))
  (use-package shell-pop
    :bind (("C-t" . shell-pop))
    :config
    (setq shell-pop-window-position "bottom")
    (setq shell-pop-full-span t)
    (setq shell-pop-window-size 30)))

(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))

;; visual bookmark - https://github.com/joodland/bm

(use-package bm
  :ensure t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)


  :config
  ;; Allow cross-buffer 'next'
;  (setq bm-cycle-all-buffers f)

  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


  :bind (("<f1>" . bm-next)
         ("S-<f1>" . bm-previous)
         ("C-<f1>" . bm-toggle)))

(provide 'init-tools)
