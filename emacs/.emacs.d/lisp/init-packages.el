(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
; (add-to-list 'package-archives
;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar zichun/packages '(; misc / tools
                          use-package
                          auto-complete
                          autopair
                          gist
                          htmlize
;                          marmalade
                          restclient
                          multiple-cursors
                          expand-region
                          highlight-symbol
                          smex
                          highlight2clipboard
                          hl-anything
                          git-messenger
                          git-gutter
                          ivy
                          swiper
                          counsel
                          counsel-gtags
                          prog-fill
                          shell-pop
                          golden-ratio
                          flycheck
                          anzu
                          deadgrep

                          ; modes
                          company
                          log4j-mode
                          ggtags
                          projectile
                          rainbow-delimiters
                          flx-ido
                          ido-vertical-mode
                          powershell
                          magit

                          ; language modes
                          lsp-mode
                          lsp-ui
                          csharp-mode
                          fsharp-mode
                          markdown-mode
                          yaml-mode
                          js3-mode
                          web-mode
                          tide
                          rust-mode
                          flycheck-rust

                          ; UI
                          spaceline
                          doom-modeline
                          doom-themes
                          solaire-mode
                          ivy-posframe
                          ivy-rich
                          org-bullets

                          ; themes
                          molokai-theme
                          monokai-theme
                          subatomic256-theme
                          flatland-theme
                          badwolf-theme
                          )
  "Default packages")

(require 'cl-lib)

(defun zichun/packages-installed-p ()
  (cl-loop for pkg in zichun/packages
        when (not (package-installed-p pkg)) do (cl-return nil)
        finally (return t)))

(unless (zichun/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg zichun/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;
;; Bootstrap Straight.el
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'init-packages)
