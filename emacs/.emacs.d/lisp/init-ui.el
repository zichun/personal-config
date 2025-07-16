;; Basic UI Configuration
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)

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

;; Line Numbers (display-line-number-mode)
(set-fill-column 119)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Anzu - Show search match count
(use-package anzu
  :defer t
  :hook (after-init . global-anzu-mode))

;; Font and Theme Configuration
(set-face-attribute 'default nil :height 125)
(when window-system
  (if (string-equal system-type "windows-nt")
      (set-face-attribute 'default nil :font "Cascadia Code")
    (set-face-attribute 'default nil :font "Hack"))
  (setq-default line-spacing 3))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
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

(use-package doom-themes
  :defer t
  :hook (after-init . (lambda ()
                        (setq doom-themes-enable-bold t
                              doom-themes-enable-italic t)
                        (load-theme 'adwaita-dark t)
                        (doom-themes-org-config))))

;; Utility packages for better Emacs experience
(use-package uniquify
  :ensure nil  ; Built-in package
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package saveplace
  :ensure nil  ; Built-in package
  :defer t
  :hook (after-init . save-place-mode))

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3))

;; Remove commented out ivy/counsel configuration as we're using vertico/consult

;; Treemacs file explorer
(use-package treemacs
  :defer t
  :commands (treemacs treemacs-select-window treemacs-bookmark treemacs-find-file treemacs-find-tag)
  :custom
  (treemacs-width 35)
  (treemacs-position 'left)
  (treemacs-display-in-side-window t)
  (treemacs-show-hidden-files t)
  (treemacs-follow-after-init t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

;; Disable VC refresh for better performance
(remove-hook 'find-file-hooks 'vc-refresh-state)

(provide 'init-ui)
