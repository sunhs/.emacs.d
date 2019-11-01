;; tab indent
(setq python-indent-offset 4)

;; (use-package anaconda-mode
;;   :defer t

;;   :init
;;   (add-hook 'python-mode-hook 'anaconda-mode)

;;   :config
;;   (use-package company-anaconda :defer t)
;;   (anaconda-eldoc-mode t)

;;   (define-key spc-leader-map (kbd "gd") 'anaconda-mode-find-definitions)
;;   (define-key spc-leader-map (kbd "ga") 'anaconda-mode-find-assignments)
;;   (define-key spc-leader-map (kbd "gr") 'anaconda-mode-find-references)
;;   (define-key spc-leader-map (kbd "gb") 'anaconda-mode-go-back)
;;   (define-key spc-leader-map (kbd "sd") 'anaconda-mode-show-doc)
 
;;   (add-hook 'anaconda-mode-hook
;;             (lambda ()
;;               (add-to-list
;;                (make-local-variable 'company-backends)
;;                '(company-anaconda :with company-yasnippet)))))

(use-package sphinx-doc
  :commands sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode)
  :config
  (define-key spc-leader-map (kbd "id") 'sphinx-doc))
