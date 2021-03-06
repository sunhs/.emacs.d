(require 'utils)

(scroll-bar-mode -1)

;; the path for emacs shell
(setenv "PATH"
        (concat (substitute-in-file-name "$HOME/go/bin:$HOME/conda/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/texlive/2017/bin/x86_64-darwin:")
                (getenv "PATH")))

;; the path to find programs
(setq exec-path
      (append (split-string (getenv "PATH") ":") exec-path))

;; special settings for emacs mac port
;; (setq mac-option-modifier (quote (:ordinary meta :function meta :mouse meta)))
(setq mac-pass-command-to-system nil
      mac-system-move-file-to-trash-use-finder t
      ns-pop-up-frames nil ;; don't open file in a new frame
      mac-command-modifier 'control) ;; map command to control

;; display chinese fonts normally in GUI
;; (set-default-font "Monaco 12")
(hs/call-or-add-to-frame-hook
 (lambda ()
   (when (display-graphic-p)
     (dolist (charset '(kana han symbol cjk-misc bopomofo))
       (set-fontset-font (frame-parameter nil 'font)
                         charset
                         (font-spec :family "Microsoft Yahei" :size 14))))))

;; also setting the height may conflict with some gui features
;; guess it's the spaceline
(when (display-graphic-p)
  (set-frame-position (selected-frame) 80 40)
  (set-frame-width (selected-frame) 160))

;; to prompt root privileges for non-writable files
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; --------------------------------------------------------------------------------------------------------------
;; org-bullets
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; --------------------------------------------------------------------------------------------------------------
;; latex
(with-eval-after-load 'tex
  (setq my-latex-phase "pdflatex -synctex=1 -interaction=nonstopmode -file-line-error %s.tex"
        my-bibtex-phase "bibtex %s.aux"
        my-pdf-phase "pdflatex -synctex=1 -interaction=nonstopmode -file-line-error -output-format=pdf %s.tex"
        my-rm-list (list "*.aux"
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
        my-rm-phase (concat "rm " (string-join my-rm-list "; rm ") "; rm -rf auto"))
  
  (setq my-tex-build-chain (list
                            (list "My Build Chain"
                                  (string-join (list my-latex-phase my-bibtex-phase my-latex-phase my-pdf-phase my-rm-phase) " ; ")
                                  TeX-run-command
                                  nil
                                  '(latex-mode))))
  
  (setq TeX-command-list (append my-tex-build-chain TeX-command-list)))

;; --------------------------------------------------------------------------------------------------------------
;; dired-sidebar
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  ;; (setq dired-sidebar-subtree-line-prefix ".")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-face '(:family "Monaco" :size 12))
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (define-key dired-mode-map (kbd "SPC") hs-leader-map))

;; --------------------------------------------------------------------------------------------------------------
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
;; (add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
;; (load-theme 'solarized t)
;; (require 'spacemacs-dark-theme)
(load-theme 'doom-city-lights t)

;; --------------------------------------------------------------------------------------------------------------
;; language specific
(add-to-list 'load-path (concat config-dir "/lang"))
(load "lang-cpp")
(load "lang-python")
(load "org-config")
