;;; hotsauce-mode.el --- Org src block margin rails -*- lexical-binding: t -*-

(require 'org)
(require 'color)

(defgroup hotsauce nil
  "Colored margin rails for Org source blocks."
  :group 'org)

;;;; Customization

(defcustom hotsauce-margin-width 3
  "Total left margin width (in columns) used by hotsauce-mode."
  :type 'integer)

(defcustom hotsauce-language-face-alist
  '(("rust"       . font-lock-keyword-face)
    ("python"     . font-lock-function-name-face)
    ("c"          . font-lock-type-face)
    ("cpp"        . font-lock-type-face)
    ("c++"        . font-lock-type-face)
    ("powershell" . font-lock-builtin-face))
  "Mapping of Org src block languages to faces.
The face foreground is used to derive the rail color."
  :type '(alist :key-type string :value-type face))

(defcustom hotsauce-secondary-bar-color-fn
  (lambda ()
    (color-darken-name
     (or (face-background 'default nil t) "#000000")
     5))
  "Function returning the color for the secondary (inner) rail."
  :type 'function)

;;;; Internals

(defvar-local hotsauce--overlays nil)

(defun hotsauce--ensure-left-margin ()
  (setq left-margin-width hotsauce-margin-width)
  (set-window-buffer nil (current-buffer)))

(defun hotsauce--face-foreground (face)
  (or (face-foreground face nil t)
      (face-foreground 'default nil t)))

(defun hotsauce--lang-color (lang)
  (let ((face (cdr (assoc-string
                    lang
                    hotsauce-language-face-alist
                    t))))
    (if face
        (color-darken-name (hotsauce--face-foreground face) 10)
      (funcall hotsauce-secondary-bar-color-fn))))

(defun hotsauce--left-bar (color cols)
   (propertize " "
              'display
              `(left-margin
                ,(propertize
                  (make-string cols ?\s)
                  'display `(space :width ,cols)
                  'face `(:background ,color)))))

(defun hotsauce--clear ()
  (when hotsauce--overlays
    (mapc #'delete-overlay hotsauce--overlays)
    (setq hotsauce--overlays nil)))

(defun hotsauce--apply ()
  (hotsauce--clear)
  (hotsauce--ensure-left-margin)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*#\\+begin_src[ \t]+\\([^ \t\n]+\\)"
            nil t)
      (let* ((lang (match-string 1))
             (ov (make-overlay
                  (line-beginning-position)
                  (line-end-position))))
        (overlay-put ov 'before-string
                     (concat
                      ;; primary (language-colored) bar
                      (hotsauce--left-bar
                       (hotsauce--lang-color lang) hotsauce-margin-width)
                      ;; secondary neutral bar
                      (hotsauce--left-bar
                       (funcall hotsauce-secondary-bar-color-fn) 1)))
        (overlay-put ov 'priority 100)
        (push ov hotsauce--overlays)))))

;;;; Minor mode definition

;;;###autoload
(define-minor-mode hotsauce-mode
  "Add language-aware colored margin rails to Org src blocks."
  :lighter " 🌶"
  (if hotsauce-mode
      (hotsauce--apply)
    (hotsauce--clear)))

;;;; Refresh on theme change

(defun hotsauce--refresh (&rest _)
  (when hotsauce-mode
    (hotsauce--apply)))

(advice-add 'load-theme :after #'hotsauce--refresh)

(provide 'hotsauce-mode)
