;;; init-utils.el -*- lexical-binding: t; -*-

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word, with argument ARG, do that arg number of times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while (< i numWindows)
           (let* ((w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

;; Keep region when undoing in region using modern advice
(defun kb/undo-tree-undo-keep-region (orig-fun &rest args)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        (apply orig-fun args)
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    (apply orig-fun args)))

(with-eval-after-load 'undo-tree
  (advice-add 'undo-tree-undo :around #'kb/undo-tree-undo-keep-region))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(with-eval-after-load 'multiple-cursors-core
  (advice-add 'mc/mark-next-like-this :after (lambda (&rest _) (mc/cycle-forward))))

(defun my-copy-c-function ()
  (interactive)
  (save-excursion (let ((beg (progn (c-beginning-of-defun) (point)))
                        (end (progn (c-end-of-defun) (point))))
                    (copy-region-as-kill beg end))))

(defun uuidgen-braces ()
  (interactive)
  (save-excursion (insert "{")
                  (shell-command "uuidgen.exe" t)
                  (exchange-point-and-mark)
                  (backward-char 1)
                  (delete-char 1)
                  (insert "}")))

(defun uuidgen-nobraces ()
  (interactive)
  (save-excursion (shell-command "uuidgen.exe" t)
                  (exchange-point-and-mark)
                  (backward-char 1)
                  (delete-char 1)))

(defun uuidgen-struct ()
  (interactive)
  (save-excursion (save-excursion (shell-command "uuidgen.exe -s" t)
                                  (exchange-point-and-mark)
                                  (backward-char 2)
                                  (delete-char 2))
                  (delete-char 16)
                  (forward-char 2)
                  (delete-char 47)
                  (forward-char 11)
                  (delete-char 4)
                  (forward-char 8)
                  (delete-char 4)
                  (forward-char 8)
                  (delete-char 4)
                  (move-end-of-line nil)
                  (delete-char 2)))

(defun uuidgen-messageid ()
  (interactive)
  (save-excursion (save-excursion (uuidgen-struct))
                  (delete-char 1)
                  (insert "MessageId(")
                  (forward-char 29)
                  (delete-char 1)
                  (forward-char 46)
                  (delete-char 1)
                  (forward-char 1)
                  (delete-char 1)
                  (insert ")")))

(defun uuidgen-contractid ()
  (interactive)
  (save-excursion (save-excursion (uuidgen-struct))
                  (delete-char 1)
                  (insert "ContractId(")
                  (forward-char 29)
                  (delete-char 1)
                  (forward-char 46)
                  (delete-char 1)
                  (forward-char 1)
                  (delete-char 1)
                  (insert ")")))

(defun uuidgen-parens ()
  (interactive)
  (save-excursion (save-excursion (uuidgen-struct))
                  (delete-char 1)
                  (insert "(")
                  (delete-char 1)
                  (forward-char 28)
                  (delete-char 1)
                  (forward-char 46)
                  (delete-char 3)
                  (insert ")")))

(defun uuidgen-struct-long ()
  (interactive)
  (save-excursion (shell-command "uuidgen.exe -s" t)
                  (exchange-point-and-mark)
                  (backward-char 1)
                  (delete-char 1)))

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>::<line-number>'"
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message "%s copied to clipboard" path-with-line-number)))

(defun copy-region-as-richtext-to-clipboard (beg end)
  "Copy Region as rich text to clipboard"
  (interactive "r")
  (highlight2clipboard-copy-region-to-clipboard beg end)
  (pop-to-mark-command)
  (message "Copied to clipboard"))

(defun append-line-to-scratch ()
  "Append current line in file to scratch"
  (interactive)
  (save-excursion
    (let ((oldbuf (current-buffer)))
      (set-mark-command (beginning-of-line))
      (end-of-line)
      (copy-region-as-kill (region-beginning) (region-end))
      (set-buffer (get-buffer-create "*scratch*"))
      (barf-if-buffer-read-only)
      (goto-char (point-max))
      (yank)
      (insert ?\n))))

;; Enable Hide Show minor mode globally
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Custom HideShow Function
(defun toggle-fold ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (hs-toggle-hiding)))

(keymap-global-set "C-<tab>" #'toggle-fold)
(keymap-global-set "C-." #'hs-show-all)

(provide 'init-utils)
