(require 'hyeshell)

;; By default, `add-hook' places the hook at the front. And there's no absolute way to place it at the end.
;; So we add our hook first. After requiring `aweshell' it adds other hooks to modify the kbd, which come first.
;; In this way our hook is able to overwrite theirs.
;; (add-hook 'eshell-mode-hook
;;   (lambda () (define-key eshell-mode-map (kbd "C-l") hs-leader-map)))

(define-key hs-leader-map (kbd "ss") #'eshell)
(define-key hs-leader-map (kbd "st") #'hyeshell/toggle)
(define-key hs-leader-map (kbd "sc") #'hyeshell/clear-buffer)
(define-key hs-leader-map (kbd "sb") #'hyeshell/switch-buffer)
(define-key hs-leader-map (kbd "sd") #'hyeshell/toggle-dedicated)
(add-hook 'eshell-mode-hook
  #'
  (lambda ()
    (define-key eshell-mode-map (kbd "C-r")
      #'
      (lambda ()
        (interactive)
        (consult-history (hyeshell/get-shell-history))))
    (define-key eshell-cmpl-mode-map (kbd "TAB")
      #'(lambda () (interactive) (company-complete)))))

(use-package eshell-prompt-extras
  :config
  (with-eval-after-load "esh-opt"
    (setq
      eshell-highlight-prompt
      nil
      eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-up
  :config
  (defalias 'eshell/up 'eshell-up)
  (defalias 'eshell/up-peek 'eshell-up-peek))

;; (use-package eshell-did-you-mean
;;   :init
;;   (add-hook 'eshell-mode-hook
;;     (lambda ()
;;       (run-with-idle-timer
;;         1 nil
;;         #'
;;         (lambda ()
;;           (require 'eshell-did-you-mean)
;;           (eshell-did-you-mean-setup))))))

(provide 'hyeshell-config)
