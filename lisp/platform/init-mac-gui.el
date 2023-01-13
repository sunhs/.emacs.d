;; special settings
;; (setq mac-option-modifier (quote (:ordinary meta :function meta :mouse meta)))
(setq
  ns-pop-up-frames
  nil ;; don't open file in a new frame
  mac-command-modifier 'control) ;; map command to control

;; display chinese fonts normally in GUI
(set-frame-font "JetBrains Mono 16")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
    (frame-parameter nil 'font)
    charset
    (font-spec :family "Microsoft Yahei" :size 14)))

(let
  (
    mon-width
    (max-width 0)
    largest-mon-work-area-attr
    wa-x
    wa-y
    wa-width
    wa-height
    (ratio 0.9)
    frame-x
    frame-y
    frame-width
    frame-height)
  (dolist (mon-attrs (display-monitor-attributes-list))
    (setq mon-width (nth 3 (car mon-attrs)))
    (when (> mon-width max-width)
      (setq max-width mon-width)
      (setq largest-mon-work-area-attr (cadr mon-attrs))))
  (setq
    wa-x
    (nth 1 largest-mon-work-area-attr)
    wa-y (nth 2 largest-mon-work-area-attr)
    wa-width (nth 3 largest-mon-work-area-attr)
    wa-height (nth 4 largest-mon-work-area-attr)
    frame-width (floor (* wa-width ratio))
    frame-height (floor (* wa-height ratio))
    frame-x (floor (+ (/ (- wa-width frame-width) 2.0) wa-x))
    frame-y (floor (+ (/ (- wa-height frame-height) 2.0) wa-y)))
  (set-frame-position (selected-frame) frame-x frame-y)
  (set-frame-width (selected-frame) frame-width nil t)
  (set-frame-height (selected-frame) frame-height nil t))

(use-package blamer
  :custom (blamer-type 'both))

(use-package company-quickhelp
  :commands company-quickhelp-mode
  :hook (company-mode . company-quickhelp-mode)
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 0.2))
