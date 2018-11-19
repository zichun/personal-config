(c-add-style "myC#Style"
             '("C#"
               (c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-guess-make-offsets-alist
                . (
                   (substatement-open . 0)
                   (arglist-close . c-lineup-arglist-close-under-paren)
                   (topmost-intro-cont . +)
                   (defun-open . 0)
                   (defun-block-intro . +)
                   (inline-open . +)
                   (inline-close . 0)
                   (statement-open . 0)
                   (statement-block-intro . 0)
                   (statement-cont . +)
                   (brace-list-intro . +)
                   (topmost-intro . 0)
                   (topmost-intro-cont . +)
                   (block-open . 0)
                   (block-close . 0)
                   (access-label . +)
                   (arglist-intro . +)
                   (arglist-cont . 0)
                   (arglist-close . c-lineup-arglist-close-under-paren)
                   ))
               ))

(setq-default c-basic-offset 4)

(defun my-csharp-mode-hook ()
  (turn-on-font-lock)
  (c-set-style "myC#Style")
  )

; (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
; (add-hook 'csharp-mode-hook 'omnisharp-mode)
