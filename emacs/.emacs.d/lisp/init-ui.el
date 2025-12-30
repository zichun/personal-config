(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace 1)

(column-number-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

(display-time)

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(setq inhibit-startup-message t)
(setq electric-pair-mode nil) ; disable auto matching of braces
(setq visible-bell t)

(use-package golden-ratio
  :defer t
  :hook (window-configuration-change . golden-ratio-mode))

(use-package anzu
  :defer t
  :hook (after-init . global-anzu-mode))

;; Line Numbers (display-line-number-mode)
(set-fill-column 119)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow Delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Theme
;;

(set-face-attribute 'default nil :height 125)
(when window-system
  (if (string-equal system-type "windows-nt")
      (set-face-attribute 'default nil :font "Cascadia Code")
    (set-face-attribute 'default nil :font "Hack"))
  (setq-default line-spacing 3))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  ;; Use Nerd Icons
  (doom-modeline-icon (display-graphic-p))

  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-persp-name t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-github-interval (* 30 60))
  (doom-modeline-env-version t)
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-elixir t)
  (doom-modeline-env-enable-rust t)
  (doom-modeline-env-python-executable "python")
  (doom-modeline-env-ruby-executable "ruby")
  (doom-modeline-env-perl-executable "perl")
  (doom-modeline-env-go-executable "go")
  (doom-modeline-env-elixir-executable "iex")
  (doom-modeline-env-rust-executable "rustc")
  (doom-modeline-mu4e t)
  (doom-modeline-irc t)
  (doom-modeline-irc-stylize 'identity))

;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'adwaita-dark t)
;;   (doom-themes-org-config))

(use-package doom-themes
  :defer t
  :hook (after-init . (lambda ()
                        (setq doom-themes-enable-bold t
                              doom-themes-enable-italic t)
                        (load-theme 'adwaita-dark t)
                        (doom-themes-org-config))))

;;
;; The uniquify library makes it so that when you visit two files with the same name in different directories,
;; the buffer names have the directory name appended to them instead of the silly hello<2> names you get by default.
;;
;(use-package uniquify
;  :ensure nil  ; Built-in package
;  :custom
;  (uniquify-buffer-name-style 'forward))

;;
;; The saveplace library saves the location of the point when you kill a buffer
;; and returns to it next time you visit the associated file.
;;
(use-package saveplace
  :ensure nil  ; Built-in package
  :defer t
  :hook (after-init . save-place-mode))

(use-package which-key
  :defer t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3))

;; Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs (if treemacs-python-executable 3 0))
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-directory-name-transformer #'identity)
  (treemacs-display-in-side-window t)
  (treemacs-eldoc-display t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-extension-regex treemacs-last-period-regex-value)
  (treemacs-file-follow-delay 0.2)
  (treemacs-file-name-transformer #'identity)
  (treemacs-follow-after-init t)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-missing-project-action 'ask)
  (treemacs-move-forward-on-expand nil)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-position 'left)
  (treemacs-read-string-input 'from-child-frame)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-user-mode-line-format nil)
  (treemacs-user-header-line-format nil)
  (treemacs-width 35)
  (treemacs-workspace-switch-cleanup nil)
  :config
  (dolist (face '(treemacs-root-face
                  treemacs-git-unmodified-face
                  treemacs-git-modified-face
                  treemacs-git-renamed-face
                  treemacs-git-ignored-face
                  treemacs-git-untracked-face
                  treemacs-git-added-face
                  treemacs-git-conflict-face
                  treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-tags-face))
    (set-face-attribute face nil :family "Consolas" :height 110))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

(remove-hook 'find-file-hooks 'vc-refresh-state)

(provide 'init-ui)
