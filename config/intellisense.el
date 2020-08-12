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
        lsp-eldoc-render-all nil  ;; Render doc along with signature in echo area.
        lsp-signature-render-all t  ;; Render full doc.
        lsp-signature-render-documentation nil
        lsp-enable-xref t)
  (setq lsp-pyls-plugins-rope-completion-enabled nil ;; Already enabled jedi.
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-pyflakes-enabled nil ;; Use flake8.
        lsp-pyls-plugins-mccabe-enabled t
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-autopep8-enabled nil ;; Use black.
        lsp-pyls-plugins-yapf-enabled nil ;; Use black.
        lsp-pyls-plugins-flake8-ignore "E111,E114,E121,E127,E402,E731"
        lsp-pyls-plugins-flake8-max-line-length 80)
  (define-key hs-leader-map (kbd "gd") 'lsp-ui-peek-find-definitions)

  (require 'lsp-clients)
  ;; (require 'lsp-python-ms)
  (require 'lsp-pyls)
  (push '(company-files company-lsp :with company-yasnippet) company-backends)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (lsp-flycheck-enable))))

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
  (setq ;;flycheck-check-syntax-automatically '(idle-change)
        flycheck-checker-error-threshold 4096))
