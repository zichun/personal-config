(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; Essential packages that need to be loaded early
(use-package general
  :demand t)

(use-package which-key
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Additional utility packages commonly used
(use-package gist
  :defer t)

(use-package htmlize
  :defer t)

(use-package restclient
  :defer t)

(use-package highlight-symbol
  :defer t)

(use-package smex
  :defer t)

(use-package highlight2clipboard
  :defer t)

(use-package hl-anything
  :defer t)

(use-package git-gutter
  :defer t)

(use-package git-gutter-fringe
  :defer t)

(use-package deadgrep
  :defer t)

(use-package flycheck
  :defer t)

(use-package projectile
  :defer t)

(use-package flx-ido
  :defer t)

(use-package ido-vertical-mode
  :defer t)

(use-package magit
  :defer t)

;; Language modes
(use-package rustic
  :defer t)

(use-package fsharp-mode
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package js3-mode
  :defer t)

(use-package web-mode
  :defer t)

(use-package tide
  :defer t)

(use-package yasnippet
  :defer t)

(use-package flycheck-rust
  :defer t)

;; UI packages
(use-package spaceline
  :defer t)

(use-package solaire-mode
  :defer t)

(use-package svg-tag-mode
  :defer t)

(use-package olivetti
  :defer t)

;; Theme packages
(use-package molokai-theme
  :defer t)

(use-package monokai-theme
  :defer t)

(use-package subatomic256-theme
  :defer t)

(use-package flatland-theme
  :defer t)

(use-package badwolf-theme
  :defer t)

(use-package vscode-dark-plus-theme
  :defer t)

(use-package adwaita-dark-theme
  :defer t)

;; Org mode packages
(use-package org-superstar
  :defer t)

(use-package org-present
  :defer t)

(use-package ob-rust
  :defer t)

(use-package org-tree-slide
  :defer t)

(provide 'init-packages)
