;;; init.el -*- lexical-binding: t; -*-

;; Emacs 31 Native Compilation & Process I/O Optimizations
(setq native-comp-jit-compilation t
      native-comp-async-report-warnings-errors 'silent)

;; Increase read buffer for faster LSP (Eglot) & external process throughput
(setq read-process-output-max (* 2 1024 1024)) ;; 2MB
(setq process-adaptive-read-buffering nil)

;; Use async processes where possible for Eglot
(when (fboundp 'eglot--async-request)
  (setq eglot-send-changes-idle-time 0.5))

(set-default-coding-systems 'utf-8)

;; Smooth pixel scrolling (built-in Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when window-system
  (server-start))

(desktop-save-mode 1)

;; Load modular configs
(require 'init-packages)
(require 'init-ui)
(require 'init-completion)
(require 'init-utils)
(require 'init-tools)
(require 'init-org)
(require 'notebook)
;; (require 'init-magit)

(require 'init-language-base)
(require 'init-keybindings)

;; Backup & Auto-save configuration
(defvar --backup-directory "~/MyEmacsBackups/")
(unless (file-exists-p --backup-directory)
  (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq auto-save-file-name-transforms `((".*" ,--backup-directory t)))
(setq make-backup-files t               ; backup of a file the first time it is saved
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep
      kept-new-versions 3               ; newest versions to keep
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; idle seconds before auto-save
      auto-save-interval 200)           ; keystrokes between auto-saves

;; Recentf remote handling
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-exclude '("Z:\\'"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ee0785c299c1d228ed30cf278aab82cf1fa05a2dc122e425044e758203f097d2"
     "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1"
     default))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
              "main")))
 '(warning-suppress-log-types '((straight package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
