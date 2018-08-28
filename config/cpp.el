(setq-default c-basic-offset 4
              tab-width 4)

(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (innamespace . -)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +))))
(setq c-default-style "microsoft")

(defun hyesun/c-cpp-bracket-newline ()
  (interactive)
  (if (= (char-before) 123)
      (progn
        (newline-and-indent)
        (newline-and-indent)
        (previous-line)
        (c-indent-line-or-region))
    (newline-and-indent)))

(defun hyesun/c-cpp-mode-hook ()
  (c-set-offset 'arglist-intro '+)
  (define-key c++-mode-map (kbd "RET") 'hyesun/c-cpp-bracket-newline)
  (define-key c-mode-map (kbd "RET") 'hyesun/c-cpp-bracket-newline))

(add-hook 'c++-mode-hook 'hyesun/c-cpp-mode-hook)
(add-hook 'c-mode-hook 'hyesun/c-cpp-mode-hook)

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
               '(company-irony company-irony-c-headers :with company-yasnippet))))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))


(use-package flycheck-irony
  :defer t
  
  :init
  (add-hook 'c++-mode-hook 'flycheck-irony-setup)
  (add-hook 'c-mode-hook 'flycheck-irony-setup)
  
  :config
  (use-package flycheck :defer t)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save)))


(use-package irony-eldoc
  :defer t

  :init
  (add-hook 'c++-mode-hook 'irony-eldoc)
  (add-hook 'c-mode-hook 'irony-eldoc))
