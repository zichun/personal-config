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
  (add-hook 'after-init-hook #'pixel-scroll-precision-mode))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when window-system
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'server)
              (when (eq system-type 'windows-nt)
                (setq server-use-tcp t))
              (condition-case err
                  (unless (server-running-p)
                    (server-start))
                (error
                 ;; Neomacs on Windows currently fails `make-network-process :server t'
                 ;; with "Unsupported network option :reuseaddr"; suppress that noise so
                 ;; startup stays clean and auto-recovers once Neomacs fixes it upstream.
                 (unless (and (eq system-type 'windows-nt)
                              (featurep 'neomacs)
                              (equal (error-message-string err)
                                     "Unsupported network option :reuseaddr"))
                   (message "Emacs server failed to start: %s" (error-message-string err))))))))

;; Work around Neomacs Windows `group-name' throwing "Invalid GID specification" instead of returning nil
(when (and (eq system-type 'windows-nt)
           (fboundp 'group-name)
           (condition-case err
               (progn (group-name 0) nil)
             (error (equal (error-message-string err) "Invalid GID specification"))))
  (defun zc/neomacs-safe-group-name (orig gid)
    "Return nil when `group-name' raises \"Invalid GID specification\" on Windows."
    (condition-case err
        (funcall orig gid)
      (error
       (if (equal (error-message-string err) "Invalid GID specification")
           nil
         (signal (car err) (cdr err))))))
  (advice-add 'group-name :around #'zc/neomacs-safe-group-name))

;; Only probe Git VC backend (avoids RCS/CVS/SVN/Hg/Bzr checks on every file)
(setq vc-handled-backends '(Git))

;; Restore top active buffers immediately; load remaining desktop buffers when idle
(setq desktop-restore-eager 3
      desktop-lazy-verbose nil
      desktop-lazy-idle-delay 1)

(defvar zc/desktop-deferred-minor-modes
  '(flycheck-mode flyover-mode diff-hl-mode anzu-mode corfu-mode)
  "Minor modes managed by post-startup idle timers rather than eager desktop restore.")

(defvar zc/desktop--reading-p nil)
(defvar zc/desktop--eager-remaining nil)

(with-eval-after-load 'desktop
  ;; Avoid saving or eagerly loading minor modes managed by post-startup idle timers
  (dolist (mode zc/desktop-deferred-minor-modes)
    (add-to-list 'desktop-minor-mode-table `(,mode nil))
    (add-to-list 'desktop-minor-mode-handlers `(,mode . ignore)))
  (advice-add 'desktop-load-file :before-until
              (lambda (fn) (memq fn zc/desktop-deferred-minor-modes)))

  ;; Enforce `desktop-restore-eager' even when loading an older .emacs.desktop
  (advice-add 'desktop-read :around
              (lambda (orig &rest args)
                (let ((zc/desktop--reading-p t)
                      (zc/desktop--eager-remaining desktop-restore-eager))
                  (apply orig args))))
  (advice-add 'desktop-create-buffer :around
              (lambda (orig &rest args)
                (if (and zc/desktop--reading-p
                         (integerp zc/desktop--eager-remaining))
                    (if (> zc/desktop--eager-remaining 0)
                        (progn
                          (setq zc/desktop--eager-remaining (1- zc/desktop--eager-remaining))
                          (apply orig args))
                      (apply #'desktop-append-buffer-args args))
                  (apply orig args)))))

(desktop-save-mode 1)

(defun zc/frameset--offset-pos (pos delta)
  "Offset positional frame parameter POS by DELTA pixels."
  (pcase pos
    (`(+ ,n) (list '+ (+ n delta)))
    (`(- ,n) (list '- (- n delta)))
    ((pred numberp) (+ pos delta))
    (_ delta)))

(with-eval-after-load 'frameset
  ;; `frameset--restore-frame' adds `dx'/`dy' directly to `(frame-parameter frame 'left)'
  ;; and `'top' when `tty-to-GUI' is non-nil, which signals `number-or-marker-p' when
  ;; Windows stores negative offsets as `(+ -8)'.
  (advice-add 'frameset--restore-frame :around
              (lambda (orig parameters window-state filters force-onscreen &optional dx dy)
                (if (and (frameset-switch-to-gui-p parameters)
                         (numberp dx) (numberp dy))
                    (let ((frame (funcall orig parameters window-state filters force-onscreen nil nil)))
                      (when (and (frame-live-p frame) (or (/= dx 0) (/= dy 0)))
                        (modify-frame-parameters
                         frame
                         `((left . ,(zc/frameset--offset-pos (frame-parameter frame 'left) dx))
                           (top  . ,(zc/frameset--offset-pos (frame-parameter frame 'top) dy)))))
                      frame)
                  (funcall orig parameters window-state filters force-onscreen dx dy)))))

;; Load modular configs
(require 'init-packages)
(require 'init-ui)
(require 'init-completion)
(require 'init-utils)
(require 'init-tools)
(require 'init-org)
(autoload 'notebook-mode "notebook" "Minor mode for graphical tag as rounded box." t)
(autoload 'global-notebook-mode "notebook" "Global notebook mode." t)
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
