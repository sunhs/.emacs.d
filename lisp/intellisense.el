;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(use-package lsp-mode
  :commands lsp
  :hook ((c++-mode
          c-mode
          python-mode
          go-mode) . lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=16" "--background-index" "--log=error" "--compile-commands-dir=build")
        lsp-prefer-flymake nil
        lsp-auto-configure t
        lsp-signature-render-documentation t ;; 应该是补全时的 doc
        lsp-signature-doc-lines 30 ;; Defaults to 20
        lsp-eldoc-render-all nil ;; Defaults to nul; 在底部渲染 doc
        lsp-file-watch-threshold nil)
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

  ;; (require 'lsp-clients)
  ;; (require 'lsp-python-ms)
  ;; (require 'lsp-pyls)
  (require 'lsp-pyright)
  (push '(company-files company-capf :with company-yasnippet) company-backends)
  ;; (add-hook 'lsp-mode-hook
  ;;           (lambda ()
  ;;             (lsp-flycheck-enable)))
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (define-key hs-leader-map (kbd "ds") 'lsp-ui-doc-show)
  (define-key hs-leader-map (kbd "dh") 'lsp-ui-doc-hide))


(use-package flycheck
  :commands flycheck-mode
  :hook ((python-mode
          c++-mode
          c-mode) . flycheck-mode)
  :config
  (setq ;;flycheck-check-syntax-automatically '(idle-change)
        flycheck-checker-error-threshold 4096
        flycheck-display-errors-delay 0.1
        display-buffer-alist
        '(("\\*Flycheck errors\\*"
	       (display-buffer-reuse-window
	        display-buffer-in-side-window)
	       (reusable-frames . visible)
	       (side            . bottom)
	       (window-height   . 0.33)))))

(provide 'intellisense)