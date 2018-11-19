(setq org-directory "~/OneDrive/org")
(setq org-default-notes-file (concat org-directory "/journal.org"))
(setq org-default-journal-file (concat org-directory "/journal.org"))
(setq org-default-todo-file (concat org-directory "/todo.org"))
(setq org-default-personal-file (concat org-directory "/personal.org"))

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
