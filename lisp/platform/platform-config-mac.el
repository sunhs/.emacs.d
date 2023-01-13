(require 'utils)

(scroll-bar-mode -1)

;; special settings
;; (setq mac-option-modifier (quote (:ordinary meta :function meta :mouse meta)))
(setq
  ns-pop-up-frames
  nil ;; don't open file in a new frame
  mac-command-modifier 'control) ;; map command to control

;; display chinese fonts normally in GUI
;; (set-default-font "Monaco 12")
(set-frame-font "Monaco 15")
(hs/call-or-add-to-frame-hook
  (lambda ()
    (when (display-graphic-p)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font
          (frame-parameter nil 'font)
          charset
          (font-spec :family "Microsoft Yahei" :size 14))))))

(let
  (
    mon-width
    (max-width 0)
    largest-mon-work-area-attr
    wa-x
    wa-y
    wa-width
    wa-height
    (ratio 0.9)
    frame-x
    frame-y
    frame-width
    frame-height)
  (dolist (mon-attrs (display-monitor-attributes-list))
    (setq mon-width (nth 3 (car mon-attrs)))
    (when (> mon-width max-width)
      (setq max-width mon-width)
      (setq largest-mon-work-area-attr (cadr mon-attrs))))
  (setq
    wa-x
    (nth 1 largest-mon-work-area-attr)
    wa-y (nth 2 largest-mon-work-area-attr)
    wa-width (nth 3 largest-mon-work-area-attr)
    wa-height (nth 4 largest-mon-work-area-attr)
    frame-width (floor (* wa-width ratio))
    frame-height (floor (* wa-height ratio))
    frame-x (floor (+ (/ (- wa-width frame-width) 2.0) wa-x))
    frame-y (floor (+ (/ (- wa-height frame-height) 2.0) wa-y)))
  (set-frame-position (selected-frame) frame-x frame-y)
  (set-frame-width (selected-frame) frame-width nil t)
  (set-frame-height (selected-frame) frame-height nil t))

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

(provide 'platform-config-mac)
