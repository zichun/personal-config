(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
                          spaceline
                          anzu

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
                          origami

                          ; language modes
                          csharp-mode
                          fsharp-mode
                          markdown-mode
                          yaml-mode
                          js3-mode
                          web-mode
                          tide
                          rust-mode
                          flycheck-rust

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

(provide 'init-packages)
