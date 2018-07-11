(require 'utils)

;; specific settings on gnu/linux
;; (set-default-font "CamingoCode 12")
(set-default-font (font-spec :family "Ubuntu Mono"
			     :size 16))
(hyesun/call-or-add-to-frame-hook
 (lambda ()
   (when (display-graphic-p)
     (dolist (charset '(kana han symbol cjk-misc bopomofo))
       (set-fontset-font (frame-parameter nil 'font)
			 charset
			 (font-spec :family "Microsoft Yahei" :size 14))))))

;; --------------------------------------------------------------------------------------------------------------
;; dired-sidebar
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-face '(:family "Mono" :size 12))
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (define-key dired-mode-map (kbd "SPC") spc-leader-map))

;; --------------------------------------------------------------------------------------------------------------
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
;; (add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
;; (load-theme 'solarized t)
(require 'spacemacs-dark-theme)
