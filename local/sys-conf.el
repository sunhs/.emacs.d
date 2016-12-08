

;; specific settings on windows
(when (eq system-type 'windows-nt)
  ;; display chinese fonts normally in GUI
  (set-default-font "Verdana 12")
  (call-or-add-to-frame-hook
   (lambda ()
     (when (display-graphic-p)
       (dolist (charset '(kana han symbol cjk-misc bopomofo))
		 (set-fontset-font (frame-parameter nil 'font)
						   charset
						   (font-spec :family "Microsoft Yahei" :size 14)))))))
