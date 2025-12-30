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

;; movements
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

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

(global-set-key (kbd "M-n") 'my/go-to-next-paren)
(global-set-key (kbd "M-p") 'my/go-to-prev-paren)

;; Highlight-symbols
;(global-set-key [(control f1)] 'hl-highlight-mode)
(global-set-key [(control f2)] 'hl-highlight-thingatpt-local)
(global-set-key [f2] 'hl-find-next-thing)
(global-set-key [(shift f2)] 'hl-find-prev-thing)
;(global-set-key [(meta f2)] 'highlight-symbol-query-replace)

;; Highlight2Clipboard
(global-set-key [(meta f8)]
                'copy-region-as-richtext-to-clipboard)

;; append-line-to-scratch
(global-set-key (kbd "M-]") 'append-line-to-scratch)

;; Ivy-based replacement for standard commands
; (global-set-key (kbd "C-s") 'swiper)
; (global-set-key (kbd "C-r") 'swiper)
;(global-set-key (kbd "M-x") 'counsel-M-x)
; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; Ivy-based interface to shell and system tools
; (global-set-key (kbd "C-c g") 'counsel-git)
;; Other commands
;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;(global-set-key (kbd "C-x C-i") 'counsel-imenu)
;(global-set-key (kbd "C-c f") #'deadgrep)

;; Swiper
;(global-set-key (kbd "C-s") 'swiper)
;(global-set-key (kbd "C-r") 'swiper-backward)
;; (define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line-or-history)
;; (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history)

(provide 'init-keybindings)
