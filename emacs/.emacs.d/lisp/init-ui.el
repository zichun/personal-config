(use-package golden-ratio)

(setq-default indent-tabs-mode nil)

(column-number-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

(display-time)
(tool-bar-mode 0)
(menu-bar-mode 0)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(golden-ratio-mode 1)
(setq-default show-trailing-whitespace 1)
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq electric-pair-mode nil) ; disable auto matching of braces
(setq visible-bell t)

;;
;; Line Numbers (display-line-number-mode)
;;

(set-fill-column 119)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;
;; Rainbow Delimiter
;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Theme
;;

;(load-theme 'monokai t)
(set-face-attribute 'default nil :height 125)
(when window-system
  (if (string-equal system-type "windows-nt")
      (set-face-attribute 'default nil :font "Cascadia Code")
    (set-face-attribute 'default nil :font "Hack"))
  (setq-default line-spacing 3))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)

  :config
  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)
  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 3)
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are expereicing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  ;; Whether display icons or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)
  ;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)
  ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)
  ;; Whether display minor modes or not. Non-nil to display in mode-line.
  (setq doom-modeline-minor-modes nil)
  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)
  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)
  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)
  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name t)
  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)
  ;; Whether display github notifications or not. Requires `ghub` package.
  (setq doom-modeline-github nil)
  ;; The interval of checking github.
  (setq doom-modeline-github-interval (* 30 60))
  ;; Whether display environment version or not
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)
  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python")
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")
  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-mu4e t)
  ;; Whether display irc notifications or not. Requires `circe' package.
  (setq doom-modeline-irc t)
  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'adwaita-dark t)
  (doom-themes-org-config))

;; (use-package solaire-mode
;;   ;; Ensure solaire-mode is running in all solaire-mode buffers
;;   :hook (change-major-mode . turn-on-solaire-mode)
;;   ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;;   ;; itself off every time Emacs reverts the file
;;   :hook (after-revert . turn-on-solaire-mode)
;;   ;; To enable solaire-mode unconditionally for certain modes:
;;   :hook (ediff-prepare-buffer . solaire-mode)
;;   ;; Highlight the minibuffer when it is activated:
;;   :hook (minibuffer-setup . solaire-mode-in-minibuffer)
;;   :config
;;   ;; The bright and dark background colors are automatically swapped the first
;;   ;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
;;   ;; `solaire-default-face` faces are swapped. This is done because the colors
;;   ;; are usually the wrong way around. If you don't want this, you can disable it:
;;   (setq solaire-mode-auto-swap-bg nil)
;;   (solaire-global-mode +1))

(global-anzu-mode +1)

;;
;; The uniquify library makes it so that when you visit two files with the same name in different directories,
;; the buffer names have the directory name appended to them instead of the silly hello<2> names you get by default.
;;
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;;
;; The saveplace library saves the location of the point when you kill a buffer
;; and returns to it next time you visit the associated file.
;;
(use-package saveplace
  :config
  (save-place-mode 1))

(use-package which-key
  :ensure
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-f" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :init
;;   (ivy-mode 1)
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-wrap t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq enable-recursive-minibuffers t)

;;  (setq ivy-dynamic-exhibit-delay-ms 250)
;;  (setq ivy-use-virtual-buffers t)
;;  (setq ivy-count-format "(%d/%d) ")
;;  (setq ivy-re-builders-alist
;;        '((swiper . ivy--regex-plus)
;;          (t      . ivy--regex-fuzzy))))

;; (use-package ivy-posframe
;; ;  :disabled
;;   :after ivy
;;   :diminish
;;   :config
;;   (set-face-attribute 'ivy-posframe nil :background "gray11")
;;   (set-face-attribute 'ivy-posframe-border nil :background "gray11")
;;   (setq ivy-posframe-display-functions-alist
;;         '((swiper          . ivy-display-function-fallback)
;;           (complete-symbol . ivy-posframe-display-at-point)
;;           (t . ivy-posframe-display-at-frame-top-center))
;;         ivy-posframe-height-alist '((t . 20)))
;;   (setq ivy-posframe-border-width 10)
;;   (if (string-equal system-type "windows-nt")
;;       (setq ivy-posframe-parameters '((internal-border-width . 10) (font . "Cascadia Code")))
;;     (setq ivy-posframe-parameters '((internal-border-width . 10) (font . "Hack"))))
;;   (setq ivy-posframe-width 700)
;;   (ivy-posframe-mode 1))

;; (use-package ivy-rich
;;   :preface
;;   (defun ivy-rich-switch-buffer-icon (candidate)
;;     (with-current-buffer
;;         (get-buffer candidate)
;;       (let ((icon (all-the-icons-icon-for-mode major-mode)))
;;         (if (symbolp icon)
;;             (all-the-icons-icon-for-mode 'fundamental-mode)
;;           icon))))
;;   :init
;;   (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
;;         '(ivy-switch-buffer
;;           (:columns
;;            ((ivy-rich-switch-buffer-icon (:width 2))
;;             (ivy-rich-candidate (:width 40))
;;             (ivy-rich-switch-buffer-project (:width 15 :face success))
;;             (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
;;            :predicate
;;            (lambda (cand) (get-buffer cand)))

;;           ivy-switch-buffer-other-window
;;           (:columns
;;            ((ivy-rich-switch-buffer-icon (:width 2))
;;             (ivy-rich-candidate (:width 40))
;;             (ivy-rich-switch-buffer-project (:width 15 :face success))
;;             (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
;;            :predicate
;;            (lambda (cand) (get-buffer cand)))

;;           counsel-M-x
;;           (:columns
;;            ((counsel-M-x-transformer (:width 40))  ; thr original transformer
;;             (ivy-rich-counsel-function-docstring (:width 100 :face font-lock-doc-face))))

;;           counsel-describe-function
;;           (:columns
;;            ((counsel-describe-function-transformer (:width 40))
;;             (ivy-rich-counsel-function-docstring (:width 100 :face font-lock-doc-face))))

;;           counsel-describe-variable
;;           (:columns
;;            ((counsel-describe-variable-transformer (:width 40))
;;             (ivy-rich-counsel-variable-docstring (:width 100 :face font-lock-doc-face))))

;;           package-install
;;           (:columns
;;            ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
;;             (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable

;;           counsel-recentf
;;           (:columns
;;            ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
;;             (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file
;;   :config
;;   (ivy-rich-mode +1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

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
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;;; Git-gutter

;; (use-package git-gutter
;;   :ensure t
;;   :hook (prog-mode . git-gutter-mode)
;;   :config
;;   (setq git-gutter:update-interval 0.02))

;; (use-package git-gutter-fringe
;;   :ensure t
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(remove-hook 'find-file-hooks 'vc-refresh-state)

(provide 'init-ui)
