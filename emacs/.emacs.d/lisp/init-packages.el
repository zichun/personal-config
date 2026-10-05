;;; init-packages.el -*- lexical-binding: t; -*-

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install Org via straight.el EARLY to ensure we use the latest version
(straight-use-package 'org)

;; In Emacs 29+, use-package is built-in. Configure use-package to use straight by default:
(require 'use-package)
(setq straight-use-package-by-default t)

;; Essential packages that need to be loaded early
(use-package general
  :demand t)

;; In Emacs 30+, which-key is built-in
(if (fboundp 'which-key-mode)
    (progn
      (setq which-key-idle-delay 0.3)
      (which-key-mode 1))
  (use-package which-key
    :demand t
    :init (which-key-mode 1)
    :config (setq which-key-idle-delay 0.3)))

;; Built-in EditorConfig (Emacs 30+)
(when (fboundp 'editorconfig-mode)
  (editorconfig-mode 1))

(use-package nerd-icons
  :defer t)

(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode))

;; Completion & Selection (Vertico/Consult are in init-completion)

;; Utility packages
(use-package gist
  :defer t)

(use-package htmlize
  :defer t)

(use-package restclient
  :defer t)

(use-package highlight-symbol
  :defer t)

(use-package highlight2clipboard
  :defer t)

(use-package hl-anything
  :defer t)

(use-package deadgrep
  :defer t)

(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package flycheck-eglot
  :defer t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; Language modes
(use-package markdown-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package yasnippet
  :defer t)

;; UI packages (Themes & Modeline)
(use-package doom-modeline
  :defer t)

(use-package solaire-mode
  :defer t)

(use-package svg-tag-mode
  :defer t)

(use-package olivetti
  :defer t)

;; Theme packages
(use-package molokai-theme :defer t)
(use-package monokai-theme :defer t)
(use-package subatomic256-theme :defer t)
(use-package flatland-theme :defer t)
(use-package badwolf-theme :defer t)
(use-package vscode-dark-plus-theme :defer t)
(use-package adwaita-dark-theme :defer t)

;; Org mode presentation packages
(use-package org-present :defer t)
(use-package org-tree-slide :defer t)
(use-package org-appear :defer t)

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(provide 'init-packages)
