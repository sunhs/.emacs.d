;; tab indent
(setq python-indent-offset 4)

;; (use-package anaconda-mode
;;   :defer t

;;   :init
;;   (add-hook 'python-mode-hook 'anaconda-mode)

;;   :config
;;   (use-package company-anaconda :defer t)
;;   (anaconda-eldoc-mode t)

;;   (define-key hs-leader-map (kbd "gd") 'anaconda-mode-find-definitions)
;;   (define-key hs-leader-map (kbd "ga") 'anaconda-mode-find-assignments)
;;   (define-key hs-leader-map (kbd "gr") 'anaconda-mode-find-references)
;;   (define-key hs-leader-map (kbd "gb") 'anaconda-mode-go-back)
;;   (define-key hs-leader-map (kbd "sd") 'anaconda-mode-show-doc)
 
;;   (add-hook 'anaconda-mode-hook
;;             (lambda ()
;;               (add-to-list
;;                (make-local-variable 'company-backends)
;;                '(company-anaconda :with company-yasnippet)))))

(use-package sphinx-doc
  :commands sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode)
  :config
  (define-key hs-leader-map (kbd "id") 'sphinx-doc))

(setq python-shell-interpreter (substitute-in-file-name "$HOME/conda/bin/python"))

(use-package lsp-mode
  :config
  (setq lsp-pyls-plugins-rope-completion-enabled nil ;; Already enabled jedi.
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-pyflakes-enabled nil ;; Use flake8.
        lsp-pyls-plugins-mccabe-enabled t
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-autopep8-enabled nil ;; Use black.
        lsp-pyls-plugins-yapf-enabled nil ;; Use black.
        lsp-pyls-plugins-flake8-ignore "E111,E114,E121,E127,E402,E731"
        lsp-pyls-plugins-flake8-max-line-length 80)
  )

(use-package lsp-pyright
  )

(provide 'lang-python)
