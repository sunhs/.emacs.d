;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(use-package lsp-mode
  :commands lsp
  :hook ((python-mode
          c++-mode
          c-mode) . lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")
        lsp-prefer-flymake nil
        lsp-enable-snippet t
        lsp-auto-configure t
        lsp-eldoc-render-all t
        lsp-signature-render-all t
        lsp-enable-xref t)
  (define-key spc-leader-map (kbd "gd") 'lsp-ui-peek-find-definitions))


(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable nil
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))


(use-package company-lsp
  :requires company
  :commands company-lsp
  :hook (lsp-mode . company-lsp)
  :config
  (push 'company-lsp company-backends)
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))


(use-package flycheck
  :config
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save)))
