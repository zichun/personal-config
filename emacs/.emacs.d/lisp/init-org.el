(require 'org)

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
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cj" 'org-open-journal)

(defun org-open-journal()
  (interactive)
  (find-file org-default-journal-file))

(defun org-outlook-open (path) (w32-shell-execute "open" "C:/Program Files (x86)/Microsoft Office/root/Office16/OUTLOOK.exe" (concat "outlook:" path)))
(org-add-link-type "outlook" 'org-outlook-open)

(defun org-insert-clipboard-image ()
  "Insert clipboard image into org"
  (interactive)
  (call-process-shell-command "powershell.exe Get-OrgImageLink")
  (yank))

;; Enable inline highlighting for codeblocks
(setq org-src-fontify-natively t)
;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)
;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)
;; Hide emphasis char
(setq org-hide-emphasis-markers t)

(add-to-list 'org-emphasis-alist
             '("*" (:inherit font-lock-warning-face :height 1.8 :weight bold :underline f)))

(add-to-list 'org-emphasis-alist
             '("/" (:inherit font-lock-type-face :slant italic :height 145)))

(add-to-list 'org-emphasis-alist
             '("_" (:inherit font-lock-function-name-face :height 145 :underline t)))

(add-to-list 'org-emphasis-alist
             '("~" (:inherit font-lock-string-face)))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-theme-set-faces
 'user
 `(org-level-4 ((t :inherit outline-5 :height 1.3)))
 `(org-level-4 ((t :inherit outline-4 :height 1.5)))
 `(org-level-3 ((t :inherit outline-3 :font "Cambria" :height 1.3)))
 `(org-level-2 ((t :inherit outline-2 :font "Cambria" :height 1.25)))
 `(org-level-1 ((t :inherit outline-1 :font "Cambria" :height 1.25)))
 `(org-block-begin-line ((nil :font "Consolas" :height 0.8 :slant italic)))
 `(org-block ((t :background "#000"))))


(require 'ob)
(require 'ob-eval)

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

;; (org-babel-do-load-languages
;;       'org-babel-load-languages
;;       '((mermaid . t)))

(provide 'init-org)
