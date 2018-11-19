(require 'org)

(defun org-outlook-open (path) (w32-shell-execute "open" "C:/Program Files (x86)/Microsoft Office/root/Office16/OUTLOOK.exe" (concat "outlook:" path)))
(org-add-link-type "outlook" 'org-outlook-open)

(defun org-insert-clipboard-image ()
  "Insert clipboard image into org"
  (interactive)
  (call-process-shell-command "powershell.exe Get-OrgImageLink")
  (yank))

(provide 'org-outlook)

;;; org-outlook.el ends here
