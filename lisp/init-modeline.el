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
        (if (derived-mode-p 'eshell-mode)
          ""
          (let ((pname (projectile-project-name)))
            (if pname
              (format "[%s]" pname)
              ""))))
      face hs--modeline-project-name-face)
    mode-line-buffer-identification))

(setq hs--modeline-vc-mode '((:eval vc-mode)))

(setq hs--modeline-blank
  '
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
              ,
              (string-width
                (format-mode-line hs--modeline-rightmost)))))))))

(setq hs--modeline-rightmost
  '
  ((:eval mode-name)
    " "
    (:propertize
      (:eval
        (concat
          " %l/"
          (number-to-string (count-lines (point-min) (point-max)))
          " "))
      face hs--modeline-line-number-face)))

(defun hs--modeline-long ()
  (let* ()
    `
    (,@hs--modeline-leftmost
      ,@hs--modeline-vc-mode
      ,@hs--modeline-blank
      ,@hs--modeline-rightmost)))

(defun hs--modeline-short ()
  (let* ()
    `
    (,@hs--modeline-leftmost
      ,@hs--modeline-blank
      ,@hs--modeline-rightmost)))

(setq-default mode-line-format
  '
  (
    (:eval
      (if (>= (window-total-width) 80)
        (hs--modeline-long)
        (hs--modeline-short)))))

(provide 'init-modeline)
