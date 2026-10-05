;;; init-keybindings.el -*- lexical-binding: t; -*-

;; Global Keybindings (Emacs 29+ keymap-global-set API)
(keymap-global-set "C-<backspace>" #'backward-delete-word)
(keymap-global-set "C-<f5>" #'display-line-numbers-mode)
(keymap-global-set "M-g" #'goto-line)
(keymap-global-set "M-l" #'copy-current-line-position-to-clipboard)
(keymap-global-set "C-x C-e" #'eval-and-replace)
(keymap-global-set "<f9>" #'c-beginning-of-defun)
(keymap-global-set "<f10>" #'c-end-of-defun)
(keymap-global-set "<f11>" #'copy-region-as-kill)
(keymap-global-set "<f12>" #'my-copy-c-function)
(keymap-global-set "C-x g" #'magit-status)

;; Fast line & char movements
(keymap-global-set "C-S-n" (lambda () (interactive) (ignore-errors (forward-line 5))))
(keymap-global-set "C-S-p" (lambda () (interactive) (ignore-errors (forward-line -5))))
(keymap-global-set "C-S-f" (lambda () (interactive) (ignore-errors (forward-char 5))))
(keymap-global-set "C-S-b" (lambda () (interactive) (ignore-errors (backward-char 5))))

(defun my/go-to-next-paren ()
  "Jump to the next closing parenthesis or string quote.
If on a starting parenthesis/quote, jump to the matching closing one.
If inside a string, jump to the closing quote.
Otherwise, go up a list level."
  (interactive)
  (push-mark)
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) ;; Inside a string
      (goto-char (nth 8 ppss)) ;; Go to the start of the string
      (forward-sexp)) ;; Jump to the end
     ((looking-at "\\s(\\|\\s\"") ;; On a list starter or quote
      (forward-sexp))
     (t
      (up-list)))))

(defun my/go-to-prev-paren ()
  "Jump to the previous opening parenthesis or string quote.
If immediately after a closing parenthesis/quote, jump to its matching opening one.
If inside a string, jump to the opening quote.
Otherwise, go backward up a list level."
  (interactive)
  (push-mark)
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) ;; Inside a string
      (goto-char (nth 8 ppss))) ;; Go to the start of the string
     ((looking-back "\\s)\\|\\s\"" 1) ;; After a list closer or quote
      (backward-sexp))
     (t
      (backward-up-list)))))

(keymap-global-set "M-n" #'my/go-to-next-paren)
(keymap-global-set "M-p" #'my/go-to-prev-paren)

;; Highlight-symbols
(keymap-global-set "C-<f2>" #'hl-highlight-thingatpt-local)
(keymap-global-set "<f2>" #'hl-find-next-thing)
(keymap-global-set "S-<f2>" #'hl-find-prev-thing)

;; Highlight2Clipboard
(keymap-global-set "M-<f8>" #'copy-region-as-richtext-to-clipboard)

;; Append line to scratch
(keymap-global-set "M-]" #'append-line-to-scratch)

(provide 'init-keybindings)
