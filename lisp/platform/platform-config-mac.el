(require 'utils)

(scroll-bar-mode -1)

;; to prompt root privileges for non-writable files
;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; --------------------------------------------------------------------------------------------------------------
;; org-bullets
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; --------------------------------------------------------------------------------------------------------------
;; latex
(with-eval-after-load 'tex
  (setq
    my-latex-phase
    "pdflatex -synctex=1 -interaction=nonstopmode -file-line-error %s.tex"
    my-bibtex-phase
    "bibtex %s.aux"
    my-pdf-phase
    "pdflatex -synctex=1 -interaction=nonstopmode -file-line-error -output-format=pdf %s.tex"
    my-rm-list
    (list
      "*.aux"
      "*.bbl"
      "*.blg"
      "*.idx"
      "*.ind"
      "*.lof"
      "*.lot"
      "*.out"
      "*.toc"
      "*.acn"
      "*.acr"
      "*.alg"
      "*.glg"
      "*.glo"
      "*.gls"
      "*.ist"
      "*.fls"
      "*.log"
      "*.fdb_latexmk"
      "*.synctex.gz"
      "*.dvi"
      "*.spl")
    my-rm-phase
    (concat "rm " (string-join my-rm-list "; rm ") "; rm -rf auto"))

  (setq my-tex-build-chain
    (list
      (list
        "My Build Chain"
        (string-join
          (list
            my-latex-phase
            my-bibtex-phase
            my-latex-phase
            my-pdf-phase
            my-rm-phase)
          " ; ")
        TeX-run-command nil '(latex-mode))))

  (setq TeX-command-list
    (append my-tex-build-chain TeX-command-list)))

(use-package exec-path-from-shell
  ;; :if (featurep 'cocoa)
  :config (exec-path-from-shell-initialize))

(hs/call-or-add-to-frame-hook
  #'
  (lambda ()
    (when (display-graphic-p)
      (load "init-mac-gui"))))

(lsp-register-client
  (make-lsp-client
    :new-connection
    (lsp-tramp-connection 'lsp-clients--clangd-command)
    ;; (lsp-tramp-connection "clangd")
    :remote? t
    :activation-fn (lsp-activate-on "c" "cpp" "objective-c" "cuda")
    :priority -1
    :server-id 'clangd-remote
    :download-server-fn
    (lambda (_client callback error-callback _update?)
      (lsp-package-ensure 'clangd callback error-callback))))

(lsp-register-client
  (make-lsp-client
    :new-connection
    (lsp-tramp-connection
      (cons "pyright-langserver" lsp-pyright-langserver-command-args))
    :remote? t
    :major-modes '(python-mode python-ts-mode)
    :server-id 'pyright-remote
    :multi-root lsp-pyright-multi-root
    :priority 3
    :initialized-fn
    (lambda (workspace)
      (with-lsp-workspace
        workspace
        ;; we send empty settings initially, LSP server will ask for the
        ;; configuration of each workspace folder later separately
        (lsp--set-configuration (make-hash-table :test 'equal))))
    :download-server-fn
    (lambda (_client callback error-callback _update?)
      (lsp-package-ensure 'pyright callback error-callback))
    :notification-handlers
    (lsp-ht
      ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
      ("pyright/reportProgress"
        'lsp-pyright--report-progress-callback)
      ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))

(provide 'platform-config-mac)
