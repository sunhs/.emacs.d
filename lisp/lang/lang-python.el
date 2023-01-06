;; tab indent
(setq python-indent-offset 4)

(use-package sphinx-doc
  :commands sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode)
  :config (define-key hs-leader-map (kbd "id") 'sphinx-doc))

(setq python-shell-interpreter
  (substitute-in-file-name "$HOME/conda/bin/python"))

(use-package lsp-mode
  :config
  (setq
    lsp-pyls-plugins-rope-completion-enabled
    nil ;; Already enabled jedi.
    lsp-pyls-plugins-flake8-enabled
    t
    lsp-pyls-plugins-pyflakes-enabled
    nil ;; Use flake8.
    lsp-pyls-plugins-mccabe-enabled
    t
    lsp-pyls-plugins-pycodestyle-enabled
    nil
    lsp-pyls-plugins-autopep8-enabled
    nil ;; Use black.
    lsp-pyls-plugins-yapf-enabled
    nil ;; Use black.
    lsp-pyls-plugins-flake8-ignore
    "E111,E114,E121,E127,E402,E731"
    lsp-pyls-plugins-flake8-max-line-length
    80))

(use-package lsp-pyright
  :init (setq lsp-pyright-multi-root nil)
  :config
  (setq lsp-pyright-venv-path
    (substitute-in-file-name "$HOME/conda/envs")))

(provide 'lang-python)
