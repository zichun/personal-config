(defvar native-comp-deferred-compilation-deny-list nil)

;; Package initialization is handled by init-packages.el (using straight.el)
;; (package-initialize) ;; Removed to prevent double-loading warnings with straight.el

(setq gc-cons-threshold (* 50 1000 1000))
;; Reduce background work
(setq read-process-output-max (* 1024 1024)) ;; 1MB
;; Make sure process filters don't get stuck
(setq process-adaptive-read-buffering nil)
;; Use async processes where possible
(when (fboundp 'eglot--async-request)
  (setq eglot-send-changes-idle-time 0.5))
(setq comp-async-report-warnings-errors nil)
(set-default-coding-systems 'utf-8)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when window-system
  (server-start))

(require 'init-packages)
(require 'init-ui)
(require 'init-completion)
(require 'init-utils)
(require 'init-tools)
(require 'init-org)
(require 'notebook)
                                        ;(require 'init-magit)

(require 'init-language-base)

(require 'init-keybindings)

(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
(defvar --backup-directory "~/MyEmacsBackups/")
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" "~/MyEmacsBackups/" t)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 3               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

                                        ; do not check if remote files are readable
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-exclude (quote ("Z:\\'")))

                                        ;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
              "main"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

(setq native-comp-deferred-compilation t)
