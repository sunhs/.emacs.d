;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; face ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface hs--modeline-unimportant-face
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements.")

(defface hs--modeline-modified-face
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line.")

(defface hs--modeline-line-number-face
  '((t (:inherit (font-lock-variable-name-face) :inverse-video t)))
  "The face for line number on the mode-line of an active window.")

(defface hs--modeline-buffer-name-face
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line.")

(defface hs--modeline-project-name-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for buffer name on the mode-line of an active window.")

(defface hs--modeline-status-normal
  '((t (:inherit (shadow))))
  "Face used for neutral or inactive status indicators in the mode-line.")

(defface hs--modeline-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line.")

(defface hs--modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface hs--modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface hs--modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line.")

(defface hs--modeline-major-mode
  '((t (:inherit (bold))))
  "Face used for major mode indicator in the mode-line.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hs--trim-left (string)
  (if (string-match "\\`[ \t\n\r]+" string)
    (replace-match "" t t string)
    string))

(defun hs--trim-right (string)
  (if (string-match "[ \t\n\r]+\\'" string)
    (replace-match "" t t string)
    string))

(defun hs--trim (string) (hs--trim-left (hs--trim-right string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; segments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hs--modeline-file-modified ()
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
    (if (buffer-modified-p)
      (propertize "*" 'face 'hs--modeline-modified-face)
      (if (buffer-file-name)
        (propertize "-" 'face 'hs--modeline-unimportant-face)
        ""))
    ""))

(defun hs--modeline-cur-line ()
  (propertize
    (concat
      " %l/"
      (number-to-string (count-lines (point-min) (point-max)))
      " ")
    'face 'hs--modeline-line-number-face))

(defun hs--modeline-buffer-name ()
  (propertize "%b" 'face 'hs--modeline-buffer-name-face))

(defun hs--modeline-project ()
  (if (derived-mode-p 'eshell-mode)
    ""
    (let* ((proj (project-current)))
      (if proj
        (propertize
          (format "[%s]" (project-name proj))
          'face
          'hs--modeline-project-name-face)
        ""))))

(defun hs--modeline-vc-mode ()
  (when (and vc-mode buffer-file-name)
    (let*
      (
        (backend (vc-backend buffer-file-name))
        (state (vc-state buffer-file-name backend))
        face
        state-indicator)
      (cond
        ((memq state '(edited added))
          (setq face 'hs--modeline-status-info)
          (setq state-indicator "✚"))
        ((eq state 'needs-merge)
          (setq face 'hs--modeline-status-warning)
          (setq state-indicator "⟷"))
        ((eq state 'needs-update)
          (setq face 'hs--modeline-status-warning)
          (setq state-indicator "↑"))
        ((memq state '(removed conflict unregistered))
          (setq face 'hs--modeline-status-error)
          (setq state-indicator "✖"))
        (t
          (setq face 'hs--modeline-status-normal)
          (setq state-indicator "✔")))
      (concat
        (propertize state-indicator 'face face) " "
        (propertize
          (substring
            vc-mode
            (+
              (if (eq backend 'Hg)
                2
                3)
              2))
          'face face 'mouse-face face)))))

(defvar-local hs--modeline-flycheck-text nil)
(defun hs--modeline-update-flycheck-text (&optional status)
  (setq hs--modeline-flycheck-text
    (pcase status
      ('finished
        (if flycheck-current-errors
          (let-alist
            (flycheck-count-errors flycheck-current-errors)
            (let ((sum (+ (or .error 0) (or .warning 0))))
              (propertize
                (concat "⚑ Issues:" (number-to-string sum))
                'face
                (if .error
                  'hs--modeline-status-error
                  'hs--modeline-status-warning))))
          (propertize "✔ Good" 'face 'hs--modeline-status-success)))
      ('running
        (propertize "Δ Checking.." 'face 'hs--modeline-status-info))
      ('errored
        (propertize "✖ Error" 'face 'hs--modeline-status-error))
      ('interrupted
        (propertize "⏸ Paused" 'face 'hs--modeline-status-normal))
      ('no-checker ""))))

(defun hs--modeline-process ()
  (hs--trim (format-mode-line mode-line-process)))

(defun hs--modeline-major-mode ()
  (format-mode-line mode-name 'hs--modeline-major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; useless, just for reference
;;
;; (defmacro hs--modeline-blank (rhs)
;;   `
;;   (quote
;;     (
;;       (:eval
;;         (propertize
;;           " " 'display
;;           `
;;           (
;;             (space
;;               :align-to
;;               (-
;;                 (+ right right-fringe right-margin)
;;                 ,(string-width (format-mode-line ,rhs))))))))))

(defun hs--modeline-format-string (left right)
  (let ((reserve (length right)))
    (concat
      left " "
      (propertize
        " "
        'display
        `((space :align-to (- right ,reserve))))
      right)))

(defun hs--modeline-short ()
  (hs--modeline-format-string
    (concat
      " "
      (hs--modeline-file-modified)
      " "
      (hs--modeline-cur-line)
      "  "
      (hs--modeline-buffer-name)
      (hs--modeline-project)
      "  "
      (hs--modeline-vc-mode))
    (concat (hs--modeline-major-mode) " ")))

(defun hs--modeline-long ()
  (hs--modeline-format-string
    (concat
      " "
      (hs--modeline-file-modified)
      " "
      (hs--modeline-cur-line)
      "  "
      (hs--modeline-buffer-name)
      (hs--modeline-project)
      "  "
      (hs--modeline-vc-mode))
    (concat
      (hs--modeline-process)
      hs--modeline-flycheck-text
      "  "
      (hs--modeline-major-mode)
      " ")))

(setq-default mode-line-format
  '
  (
    (:eval
      (if (>= (window-total-width) 80)
        (hs--modeline-long)
        (hs--modeline-short)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'flycheck-status-changed-functions
  #'hs--modeline-update-flycheck-text)
(add-hook 'flycheck-mode-hook #'hs--modeline-update-flycheck-text)

(provide 'init-modeline)
