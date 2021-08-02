(require 'org)
(require 'org-tempo)

(defun zc/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . zc/org-mode-setup)
  :config
  (setq org-directory "~/OneDrive/org")
  (setq org-default-notes-file (concat org-directory "/journal.org"))
  (setq org-default-journal-file (concat org-directory "/journal.org"))
  (setq org-default-todo-file (concat org-directory "/todo.org"))
  (setq org-default-personal-file (concat org-directory "/personal.org"))
  (setq org-catch-invisible-edits 'show-and-error)

  (setq org-agenda-files (quote (org-default-journal-file)))

  (setq org-capture-templates
        '(    ;; ... other templates
          ("t" "Todo"
           entry
           (file+headline org-default-todo-file "Tasks") "* TODO %?\n %i\n %a")

          ("j" "Journal Entry"
           entry
           (file+datetree org-default-journal-file)
           "* %?"
           :empty-lines 1)

          ("p" "Personal Notes"
           entry
           (file+datetree org-default-personal-file)
           "* %?"
           :empty-lines 1)
          ))

  ;; Enable inline highlighting for codeblocks
  (setq org-src-fontify-natively t)
  ;; set maximum indentation for description lists
  (setq org-list-description-max-indent 5)
  ;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil)
  ;; Hide emphasis char
  (setq org-hide-emphasis-markers t)

  (add-to-list 'org-emphasis-alist
               '("*" (:inherit font-lock-warning-face :height 1.8 :weight bold)))

  (add-to-list 'org-emphasis-alist
               '("/" (:inherit font-lock-type-face :slant italic :height 145)))

  (add-to-list 'org-emphasis-alist
               '("_" (:inherit font-lock-function-name-face :height 145 :underline t)))

  (add-to-list 'org-emphasis-alist
               '("~" (:inherit font-lock-string-face))))

(with-eval-after-load 'org
  (bind-key "\C-c l" 'org-store-link)
  (bind-key "\C-c a" 'org-agenda)
  (bind-key "\C-c c" 'org-capture)
  (bind-key "\C-c b" 'org-iswitchb)
  (bind-key "\C-c j" 'org-open-journal))

(defun org-open-journal()
  (interactive)
  (find-file org-default-journal-file))

;;(defun org-outlook-open (path) (w32-shell-execute "open" "C:/Program Files (x86)/Microsoft Office/root/Office16/OUTLOOK.exe" (concat "outlook:" path)))
;;(org-add-link-type "outlook" 'org-outlook-open)

(defun org-insert-clipboard-image ()
  "Insert clipboard image into org"
  (interactive)
  (call-process-shell-command "powershell.exe Get-OrgImageLink")
  (yank))

;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(set-face-attribute 'org-document-title nil :font "Consolas" :weight 'bold :height 1.3)
(custom-theme-set-faces
 'user
 `(org-level-4 ((t :inherit outline-5 :height 1.3)))
 `(org-level-4 ((t :inherit outline-4 :height 1.5)))
 `(org-level-3 ((t :inherit outline-3 :font "Cambria" :height 1.3)))
 `(org-level-2 ((t :inherit outline-2 :font "Cambria" :height 1.25)))
 `(org-level-1 ((t :inherit outline-1 :font "Cambria" :height 1.25)))
 `(org-block-begin-line ((nil :font "Consolas" :height 0.8 :slant italic))))

;; Make sure org-indent face is available
(require 'org-indent)

;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:kusto
  '((:results . "table") (:exports . "results"))
  "Default arguments for evaluatiing a kusto source block.")

(defun org-babel-execute:kusto (body params)
  (let* ((temp-file (org-babel-temp-file "org-"))
         (cmd (concat "powershell -noprofile -noninteractive "
                      "C:/repos/scripts/invoke-kusto.ps1"
                      " -QueryFile " temp-file)))
    (with-temp-file temp-file (insert body))
    (message "%s" cmd)
    (org-babel-eval cmd "")
    (message "temp-file: %s" temp-file)
    (with-temp-buffer
      (insert-file-contents temp-file)
      (buffer-string))
  ))

(defvar org-babel-default-header-args:mermaid
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluatiing a mermaid source block.")

(defun org-babel-execute:mermaid (body params)
  (let* ((out-file
          (or (expand-file-name (concat "~/OneDrive/org/"
                                        (cdr (assoc :file params))))
              (error "mermaid requires a \":file\" header argument")))
	 (theme (cdr (assoc :theme params)))
	 (width (cdr (assoc :width params)))
	 (height (cdr (assoc :height params)))
	 (background-color (cdr (assoc :background-color params)))
     (temp-file (org-babel-temp-file "mermaid-"))
     (cmd (concat "pwsh"
                  " -NonInteractive -NoProfile C:/Users/kzc16/AppData/Roaming/npm/mmdc.ps1 "
                  " -i " temp-file
                  " -o " out-file
                  (when theme
                    (concat " -t " theme))
                  (when background-color
                    (concat " -b " background-color))
                  (when width
                    (concat " -w " width))
                  (when height
                    (concat " -H " height)))))
    (with-temp-file temp-file (insert body))
    (message "%s" cmd)
    (message "%s" out-file)
    (org-babel-eval cmd "")
    nil))

;; ob-powershell

(add-to-list 'org-babel-tangle-lang-exts '("Powershell" . "ps"))

(defvar org-babel-default-header-args:powershell '((:lang . "powershell"))
  "A list of default header args for Powershell code blocks.")

(defvar org-babel-command:powershell "pwsh"
  "The path to the Powershell interpreter executable.")

;; -- Babel Functions --

(defun org-babel-expand-body:powershell (body params)
  "Expands the body of a Powershell code block."
  ;; Currently we don't do any expansion for tangled blocks. Just return
  ;; body unmodified as specified by the user.
  body)

(defun org-babel-execute:powershell (body params)
  "Executes a Powershell code block."
  ;; Round up the stuff we need
  (let* ((parsed-params (ob-powershell--parse-params params))
         (expanded-body (org-babel-expand-body:powershell body params))
         (result-type (nth 0 parsed-params))
         (vars (nth 1 parsed-params))
         (temp-file (make-temp-file "ob-powershell-")))
    (message (format "===vars %s" vars))
    ;; Build script in temporary file
    (with-temp-file temp-file
      (let ((vars-string
             (mapconcat (lambda (var) (format "$%s = %s;\n" (car var) (cdr var))) vars " ")))
        (insert (format "%s\n\n%s"
                        vars-string
                        expanded-body)))
      )
    ;; Run script with Racket interpreter, delete temp file, and return output
    (with-temp-buffer
      (prog2
          (call-process org-babel-command:powershell nil (current-buffer) nil temp-file)
          (buffer-string)
        (delete-file temp-file)))))

(defun org-babel-prep-session:powershell (session params)
  (error "Powershell does not currently support sessions."))

;; -- Parameter Parsing --

(defun ob-powershell--parse-params (params)
  "Processes and parses parameters for an Org Babel code block. The results are
returned as a list."
  (let ((processed-params (org-babel-process-params params))
        (result-type nil)
        (vars nil))
    (dolist (processed-param processed-params)
      (let ((key (car processed-param)) (value (cdr processed-param)))
        (cond
         ((equal key :result-type) (setq result-type value))
         ((equal key :var) (push value vars)))))
    (list result-type vars)))

(use-package ob-rust
  :if (executable-find "rustc")
  :init (add-to-list 'org-babel-load-languages '(rust . t)))

;; (org-babel-do-load-languages
;;       'org-babel-load-languages
;;       '((mermaid . t)))

;;
;; org-present
;;

(defun zc/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun zc/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (zc/org-present-prepare-slide))

(defun zc/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun zc/org-present-prev ()
  (interactive)
  (org-present-prev)
  (zc/org-present-prepare-slide))

(defun zc/org-present-next ()
  (interactive)
  (org-present-next)
  (zc/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-n" . zc/org-present-next)
         ("C-c C-p" . zc/org-present-prev))
  :hook ((org-present-mode . zc/org-present-hook)
         (org-present-mode-quit . zc/org-present-quit-hook)))

(defun zc/org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

(defun zc/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message "Presentation started."
        org-tree-slide-deactivate-message "Presentation ended."
        org-tree-slide-header t))

(setq org-clock-sound "~/personal-config/emacs/bell.wav")

(provide 'init-org)
