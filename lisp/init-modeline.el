(defface hs--modeline-project-name-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for buffer name on the mode-line of an active window.")

(defface hs--modeline-line-number-face
  '((t (:inherit (mode-line-inactive) :inverse-video t)))
  "The face for line number on the mode-line of an active window.")

(setq hs--modeline-leftmost
  '
  (" %& "
    (:propertize
      (:eval
        (concat
          " %l/"
          (number-to-string (count-lines (point-min) (point-max)))
          " "))
      face hs--modeline-line-number-face)
    " " mode-line-buffer-identification
    (:propertize
      (:eval
        (if (derived-mode-p 'eshell-mode)
          ""
          (let ((pname (projectile-project-name)))
            (if pname
              (format "[%s]" pname)
              ""))))
      face hs--modeline-project-name-face)))

(setq hs--modeline-vc-mode '((:eval vc-mode)))

(defmacro hs--modeline-blank (rightmost)
  `
  (quote
    (
      (:eval
        (propertize
          " " 'display
          `
          (
            (space
              :align-to
              (-
                (+ right right-fringe right-margin)
                ,(string-width (format-mode-line ,rightmost))))))))))

(setq hs--modeline-cur-line '())

(setq hs--modeline-rightmost-short
  `((:eval mode-name) " " ,hs--modeline-cur-line))

(setq hs--modeline-rightmost-long
  `((:eval mode-line-modes) " " ,hs--modeline-cur-line))

(defun hs--modeline-long ()
  `
  (,@hs--modeline-leftmost
    ,@hs--modeline-vc-mode
    "  "
    ,@
    (hs--modeline-blank hs--modeline-rightmost-long)
    ,@hs--modeline-rightmost-long))

(defun hs--modeline-short ()
  `
  (,@hs--modeline-leftmost
    "  "
    ,@
    (hs--modeline-blank hs--modeline-rightmost-short)
    ,@hs--modeline-rightmost-short))

(setq-default mode-line-format
  '
  (
    (:eval
      (if (>= (window-total-width) 80)
        (hs--modeline-long)
        (hs--modeline-short)))))

(provide 'init-modeline)
