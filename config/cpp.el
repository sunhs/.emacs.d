(use-package irony
  :defer t
  
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  
  :config
  ;; Use compilation database first, clang_complete as fallback.
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
												  irony-cdb-clang-complete))
  
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  
  (use-package company-irony :defer t)
  (use-package company-irony-c-headers :defer t)
  
  (add-hook 'irony-mode-hook
			(lambda ()
			  (add-to-list
			   (make-local-variable 'company-backends)
			   '(company-irony company-irony-c-headers company-capf :with company-yasnippet))))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))


(use-package flycheck-irony
  :defer t
  
  :init
  (add-hook 'c++-mode-hook 'flycheck-irony-setup)
  (add-hook 'c-mode-hook 'flycheck-irony-setup)
 
  :config
  (use-package flycheck :defer t)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save)))


(use-package irony-eldoc
  :defer t

  :init
  (add-hook 'c++-mode-hook 'irony-eldoc)
  (add-hook 'c-mode-hook 'irony-eldoc))
