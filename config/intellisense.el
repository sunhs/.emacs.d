;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(use-package lsp-mode
  :commands lsp
  :hook ((c++-mode
          c-mode
          python-mode) . lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")
        lsp-prefer-flymake nil
        lsp-auto-configure nil
        ;; lsp-eldoc-render-all t
        lsp-signature-render-all t
        lsp-enable-xref nil)
  (lsp--set-configuration `(:pyls (:configurationSource ("flake8"))))
  (setq lsp-pyls-plugins-pyflakes-enabled nil)
  (setq lsp-pyls-plugins-pylint-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled nil)
  (define-key hs-leader-map (kbd "gd") 'lsp-ui-peek-find-definitions)

  (require 'lsp-clients)
  (push '(company-lsp :with company-yasnippet) company-backends))

  ;; (defun force-company-lsp-with-yasnippet ()
    ;; (let ((backend '(company-lsp :with company-yasnippet)))
    ;;   (message "fuck1")
    ;;   (message company-backends)
    ;;   (if (equal (car company-backends)
    ;;              backend)
    ;;       nil
    ;;     (setq-local company-backends
    ;;                 (add-to-list 'company-backends backend))))
    ;; (message "fuck2")
    ;; (message company-backends))

  ;; (add-hook 'python-mode-hook 'force-company-lsp-with-yasnippet))


(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (define-key hs-leader-map (kbd "ds") 'lsp-ui-doc-show)
  (define-key hs-leader-map (kbd "dh") 'lsp-ui-doc-hide))


(use-package company-lsp
  :requires company
  :commands company-lsp
  :hook (lsp-mode . company-lsp)
  :config
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates 'auto))


(use-package flycheck
  :commands flycheck-mode
  :hook ((python-mode
          c++-mode
          c-mode) . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save)
        flycheck-checker-error-threshold 4096))
