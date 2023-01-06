;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(use-package lsp-mode
  :commands lsp
  :hook ((c++-mode
          c++-ts-mode
          c-mode
          c-ts-mode
          python-mode
          python-ts-mode
          go-mode
          cmake-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil
        lsp-auto-configure t
        lsp-signature-render-documentation t ;; 应该是补全时的 doc
        lsp-signature-doc-lines 30 ;; Defaults to 20
        lsp-eldoc-render-all nil ;; Defaults to nul; 在底部渲染 doc
        lsp-file-watch-threshold nil)
  (define-key hs-leader-map (kbd "gd") 'lsp-ui-peek-find-definitions)

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
          c-mode
          go-mode
          cmake-mode) . flycheck-mode)
  :config
  ;; (setq flycheck-check-syntax-automatically '(idle-change))
  (setq flycheck-checker-error-threshold 4096
        flycheck-display-errors-delay 0.1
        display-buffer-alist
        '(("\\*Flycheck errors\\*"
	       (display-buffer-reuse-window
	        display-buffer-in-side-window)
	       (reusable-frames . visible)
	       (side            . bottom)
	       (window-height   . 0.33)))))

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< treesit >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(require 'treesit-parser-manager)
(setq treesit-parser-manager-grammars
      '(
        ("https://github.com/tree-sitter/tree-sitter-bash" ("tree-sitter-bash"))
        ("https://github.com/tree-sitter/tree-sitter-cpp" ("tree-sitter-cpp"))
        ("https://github.com/theHamsta/tree-sitter-cuda" ("tree-sitter-cuda"))
        ("https://github.com/Wilfred/tree-sitter-elisp" ("tree-sitter-elisp"))
        ("https://github.com/tree-sitter/tree-sitter-json" ("tree-sitter-json"))
        ("https://github.com/alemuller/tree-sitter-make" ("tree-sitter-make"))
        ("https://github.com/ikatyang/tree-sitter-markdown" ("tree-sitter-markdown"))
        ("https://github.com/tree-sitter/tree-sitter-python" ("tree-sitter-python"))
        ("https://github.com/ikatyang/tree-sitter-toml" ("tree-sitter-toml"))
        ("https://github.com/ikatyang/tree-sitter-yaml" ("tree-sitter-yaml"))

        ;; in development
        ("https://github.com/camdencheek/tree-sitter-dockerfile" ("tree-sitter-dockerfile"))
        ("https://github.com/milisims/tree-sitter-org" ("tree-sitter-org"))
        ("https://github.com/mitchellh/tree-sitter-proto" ("tree-sitter-proto"))
        ))
(add-to-list 'treesit-extra-load-path treesit-parser-manager-target-directory)
(add-hook 'emacs-startup-hook 'treesit-parser-manager-install-grammars)

(when (treesit-available-p)
  (setq major-mode-remap-alist
        '(
          ;; (sh-mode . bash-ts-mode)
          ;; (c++-mode . c++-ts-mode)
          ;; (js-json-mode . json-ts-mode)
          ;; (python-mode . python-ts-mode)
          ;; (conf-toml-mode . toml-ts-mode)
          )
        )
  )

(provide 'intellisense)
