(deftheme zsh "The zsh color theme for Emacs 24")

(let ((class '((class color) (min-colors 89)))
      ;; zsh palette
      (zsh-white          "#ffffff")
      (zsh-red            "#ff0000")
      (zsh-shallowred     "#fd5252")
      (zsh-yellow         "#fcfc4d")
      (zsh-brown          "#b1501c")
      (zsh-lime           "#5cfc9f")
      (zsh-darkwine       "#1e0010")
      (zsh-wine           "#960050")
      (zsh-aqua           "#5aefff")
      (zsh-greyblue       "#80a2a5")
      (zsh-blue           "#66d9ef")
      (zsh-deepblue       "#0808d2")
      (zsh-purple         "#ee5ada")
      (zsh-palevioletred  "#e557d2")
      (zsh-shallowgrey    "#3b3b3b")
      (zsh-grey           "#5c5c5c")
      (zsh-dark           "#000000"))
  (custom-theme-set-faces
   'zsh

   ;; base
   `(default ((t (:background ,zsh-dark :foreground ,zsh-white))))
   `(cursor ((t (:background ,zsh-white :foreground ,zsh-dark))))
   `(fringe ((t (:foreground ,zsh-white :background ,zsh-dark))))
   `(region ((t (:background ,zsh-blue :foreground ,zsh-dark))
             (t :inverse-video t)))
   `(warning ((t (:foreground ,zsh-red :weight bold))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zsh-blue))))
   `(font-lock-comment-face ((t (:foreground ,zsh-greyblue :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zsh-greyblue))))
   `(font-lock-constant-face ((t (:foreground ,zsh-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,zsh-palevioletred))))
   `(font-lock-function-name-face ((t (:foreground ,zsh-white))))
   `(font-lock-keyword-face ((t (:foreground ,zsh-yellow))))
   `(font-lock-negation-char-face ((t (:foreground ,zsh-wine))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,zsh-palevioletred))))
   `(font-lock-type-face ((t (:foreground ,zsh-lime))))
   `(font-lock-variable-name-face ((t (:foreground ,zsh-shallowred))))
   `(font-lock-warning-face ((t (:foreground ,zsh-red :weight bold))))

   ;; mode line
   `(mode-line ((t (:foreground ,zsh-white
                                :background ,zsh-dark
                                :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground ,zsh-grey
                                         :background ,zsh-dark
                                         :box nil))))

   ;; search
   `(isearch ((t (:foreground ,zsh-dark :background ,zsh-yellow :weight bold))))
   `(isearch-fail ((t (:foreground ,zsh-wine :background ,zsh-darkwine))))

   ;; linum-mode
   `(linum ((t (:foreground ,zsh-yellow :background ,zsh-dark))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,zsh-shallowgrey)) (t :weight bold)))
   `(hl-line ((,class (:background ,zsh-shallowgrey)) (t :weight bold)))

   ;; TODO
   ;; ido-mode
   ;; flycheck
   ;; show-paren
   ;; rainbow-delimiters
   ;; highlight-symbols
   ))

(defcustom zsh-theme-kit nil
  "Non-nil means load zsh-theme-kit UI component"
  :type 'boolean
  :group 'zsh-theme)

(defcustom zsh-theme-kit-file
  (concat (file-name-directory
           (or (buffer-file-name) load-file-name))
          "zsh-theme-kit.el")
  "zsh-theme-kit-file"
  :type 'string
  :group 'zsh-theme)

(if (and zsh-theme-kit
         (file-exists-p zsh-theme-kit-file))
    (load-file zsh-theme-kit-file))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zsh)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zsh-theme.el ends here
