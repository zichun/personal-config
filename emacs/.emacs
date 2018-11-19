;;
;; MELPA
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;; Some Global Settings
;;

(setq-default indent-tabs-mode nil)

(column-number-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

(display-time)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq-default show-trailing-whitespace 1)
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq electric-pair-mode nil) ;; disable auto matching of braces
(setq visible-bell t)

;; Line Numbers (linum)

(set-fill-column 119)
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)

;; Load functions
(load-file "~/.emacs.d/functions.el")
(load-file "~/.emacs.d/packages.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/.emacs.d/spolsky-theme.el")
(load-file "~/.emacs.d/org-outlook.el")

;; Theme
(load-theme 'monokai t)
(set-face-attribute 'default nil :height 135)
(when window-system
  (set-face-attribute 'default nil :font "Consolas")
  (setq-default line-spacing 3))

;;
;; Keybindings
;;

(global-set-key [C-backspace] 'backward-delete-word)
(global-set-key (kbd "C-<f5>") 'linum-mode)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-l" 'copy-current-line-position-to-clipboard)
(global-set-key (kbd "C-x C-e") 'eval-and-replace)
(global-set-key '[f9] 'c-beginning-of-defun)
(global-set-key '[f10] 'c-end-of-defun)
(global-set-key '[f11] 'copy-region-as-kill)
(global-set-key '[f12] 'my-copy-c-function)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-M-[") 'origami-close-node)
(global-set-key (kbd "C-M-]") 'origami-open-node)

;; movements
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; Expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; Multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Highlight-symbols
(global-set-key [(control f1)] 'hl-highlight-mode)
(global-set-key [(control f2)] 'hl-highlight-thingatpt-local)
(global-set-key [f2] 'hl-find-next-thing)
(global-set-key [(shift f2)] 'hl-find-prev-thing)
;(global-set-key [(meta f2)] 'highlight-symbol-query-replace)

;; Highlight2Clipboard
(global-set-key [(meta f8)]
                'copy-region-as-richtext-to-clipboard)

;; git-messenger
(require 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;; prog-fill
(require 'prog-fill)
(add-hook
 'prog-mode-hook
 (lambda () (local-set-key (kbd "M-q") #'prog-fill)))

;; Ido-mode with flx and configuration
;; (require 'ido-vertical-mode)
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (ido-vertical-mode 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;;
;; Ivy instead of Ido
;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
;; Ivy-based replacement for standard commands
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
;; Other commands
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x C-i") 'counsel-imenu)

;; (setq ido-use-faces t)
;; (set-face-attribute 'ido-vertical-first-match-face nil
;;                     :background nil
;;                     :foreground "orange"
;;                     :underline nil
;;                     :box t)
;; (set-face-attribute 'ido-vertical-only-match-face nil
;;                     :background nil
;;                     :foreground nil
;;                     :underline nil)
;; (set-face-attribute 'ido-vertical-match-face nil
;;                     :foreground nil
;;                     :underline nil)

;; shell-pop

;; (custom-set-variables
;;  '(shell-pop-universal-key "C-t")
;;  '(shell-pop-window-size 30)
;;  '(shell-pop-full-span t)
;;  '(shell-pop-window-position "bottom"))

(setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
(setq explicit-shell-file-name "C:/windows/system32/WindowsPowerShell/v1.0/powershell.exe")
(setq shell-pop-window-position "bottom")
(setq shell-pop-full-span t)
(setq shell-pop-window-size 30)
(setq shell-file-name "powershell")(setq explicit-powershell.exe-args '("-Command" "-" )) ;

(global-set-key (kbd "C-t") 'shell-pop)

(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))

;; Extensions and language specific stuff

;(add-hook 'after-init-hook 'global-company-mode)
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)

(setq auto-mode-alist
      (append '(
                ("\\.cs$" . csharp-mode)
                ("\\.js$" . js3-mode)
                ("\\.ps1$" . powershell-mode)
                ("\\.psm1$" . powershell-mode)
                ("\\.log$" . log4j-mode)
                ) auto-mode-alist ))

;; Enable gg-modes for c-mode

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(setq ggtags-completing-read-function
      (lambda (&rest args)
        (apply #'ido-completing-read
               (car args)
               (all-completions "" ggtags-completion-table)
               (cddr args))))

;; (add-hook 'c-mode-hook 'counsel-gtags-mode)
;; (add-hook 'c++-mode-hook 'counsel-gtags-mode)

;; (with-eval-after-load 'counsel-gtags
;;   (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-find-definition)
;;   (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-find-reference)
;;   (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
;;   (define-key counsel-gtags-mode-map (kbd "M-n") 'counsel-gtags-go-forward)
;;   (define-key counsel-gtags-mode-map (kbd "M-p") 'counsel-gtags-go-backward))


;; Set-up origami
(require 'origami)
(global-origami-mode)

;; Enable rainbow-delimiters for c-mode

(add-hook 'c-mode-common-hook
          'rainbow-delimiters-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "fa11f855b5f606f84e50106a7360c72aac88fee5f6fb8084aa4329009b61c5a2" "c3d385d9214f5b613e85fcaa2f746e52d272dbcceaaeea480ad3244a815882e7" "49de25b465bc3c2498bcd4c1575fa0090bd56fc79cdb49b919b49eaea17ee1dd" "ac88f5baa9b100652e0cc1d5891e66283d43a2b263cef048c3bff102983c29df" "86f4407f65d848ccdbbbf7384de75ba320d26ccecd719d50239f2c36bec18628" "a99e7c91236b2aba4cd374080c73f390c55173c5a1b4ac662eeb3172b60a9814" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.xml\\'" flymake-xml-init nil nil)
     ("\\.html?\\'" flymake-xml-init nil nil)
     ("\\.cs\\'" csharp-flymake-init csharp-flymake-cleanup nil)
     ("\\.p[ml]\\'" flymake-perl-init nil nil)
     ("\\.php[345]?\\'" flymake-php-init nil nil)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup nil)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup nil)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup nil)
     ("\\.tex\\'" flymake-simple-tex-init nil nil)
     ("\\.idl\\'" flymake-simple-make-init nil nil))))
 '(hl-highlight-mode t)
 '(icomplete-mode t)
 '(js3-auto-indent-p t)
 '(js3-bounce-indent-p t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-indent-dots t)
 '(js3-indent-level 4)
 '(js3-indent-on-enter-key t)
 '(js3-visitor-offset t t)
 '(linum-format " %7i ")
 '(org-agenda-files (quote ("~/OneDrive/org/journal.org")))
 '(package-selected-packages
   (quote
    (js2-mode zen-mode counsel-gtags counsel swiper ivy ripgrep prog-fill org-mime shell-pop git-messenger itail git-gutter origami zzz-to-char irony flycheck-clangcheck flycheck tide hl-anything less-css-mode web-mode typescript-mode gratuitous-dark-theme magit yaml-mode subatomic256-theme smex restclient rainbow-delimiters projectile powershell omnisharp multiple-cursors monokai-theme molokai-theme material-theme marmalade markdown-mode log4j-mode ido-vertical-mode highlight2clipboard groovy-mode gist ggtags fsharp-mode flx-ido flatland-theme expand-region csharp-mode company badwolf-theme autopair))))

(load-file "~/.emacs.d/csharp.el")

;; azure etags
;(setq-default tags-file-name "e:/rd/rd_fabric_n_core/developer/kohzi/azure-etags.dat")
;(global-set-key "\M-/" 'tags-apropos)

                                        ;
;; magit configuration
(define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
  "Mode for showing staged and unstaged changes."
  :group 'magit-status)
(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-untracked-files)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))
(defun magit-staging ()
  (interactive)
  (magit-mode-setup #'magit-staging-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when window-system
  (server-start))

(setq visible-bell t)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r")))

;;
;; cc-mode c++-mode customize font locking
(global-font-lock-mode t)
(setq font-lock-maximum-decoration 2
	  font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;;
;; Set-up Typescript
;;
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
;(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


;; org-mode

;; Enable inline highlighting for codeblocks
(setq org-src-fontify-natively t)
;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)
;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

(add-to-list 'org-emphasis-alist
             '("*" (:foreground "Red" :height 145)
               ))
