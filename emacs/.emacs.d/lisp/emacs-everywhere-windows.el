;;; emacs-everywhere-windows.el --- AHK-driven emacs-everywhere on Windows -*- lexical-binding: t; -*-
;;
;; Pair with emacs-everywhere.ahk. AHK owns focus/copy/paste; Emacs keeps
;; everything else from upstream (org/markdown handling, frame naming,
;; C-c C-c / C-c C-k, spell-check, pandoc conversion).
;;
;; Requires: (server-start) or `emacs --daemon', and emacs-everywhere from MELPA.

(with-eval-after-load 'emacs-everywhere
  (when (eq system-type 'windows-nt)

    ;; 1. Upstream bug (HEAD 09a6a64): `emacs-everywhere--system-compositor'
    ;;    returns `windows', but the default config is keyed on
    ;;    `desktop-window-manager', so nothing matches on Windows. Add a
    ;;    `windows' entry. All commands nil: AHK refocuses and pastes.
    (defun my/ee-app-info-from-ahk ()
      "Read window info from the .app sidecar written by emacs-everywhere.ahk."
      (let ((meta (concat (or buffer-file-name "") ".app")))
        (if (not (file-exists-p meta))
            (make-emacs-everywhere-app :id "0" :class "unknown" :title "" :geometry nil)
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents meta))
            (pcase-let ((`(,id ,class ,title ,geom)
                         (split-string (buffer-string) "\n")))
              (make-emacs-everywhere-app
               :id id :class class :title (or title "")
               :geometry (split-string (or geom ""))))))))

    (add-to-list 'emacs-everywhere-system-configs
                 '(windows :paste-command nil :copy-command nil :focus-command nil
                           :info-function my/ee-app-info-from-ahk)
                 t)
    (setq emacs-everywhere--system-configured nil)
    (emacs-everywhere--ensure-system-configured)

    ;; 2. Selection: AHK already did Ctrl+C, so the clipboard holds just the
    ;;    selection. Replace upstream's PowerShell focus-flip-and-SendKeys dance.
    (advice-add 'emacs-everywhere-insert-selection--windows :override
                (lambda ()
                  (when-let* ((s (gui-get-selection 'CLIPBOARD 'STRING)))
                    (unless (string-empty-p s) (insert s)))))

    ;; 3. Tell AHK "paste this": write the file on a non-aborted finish only.
    ;;    Final hooks don't run on abort, so no file => AHK doesn't paste.
    (add-hook 'emacs-everywhere-final-hooks
              (lambda ()
                (let ((coding-system-for-write 'utf-8-unix))
                  (write-region nil nil buffer-file-name nil 'silent)))
              90)

    ;; Keep the temp files out of recentf.
    (with-eval-after-load 'recentf
      (dolist (p emacs-everywhere-file-patterns)
        (add-to-list 'recentf-exclude p)))))
