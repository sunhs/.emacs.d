(setq-default c-basic-offset 4
              tab-width 4)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (defun hs/c-cpp-bracket-newline ()
;;   (interactive)
;;   (if (= (char-before) 123)
;;       (progn
;;         (newline-and-indent)
;;         (newline-and-indent)
;;         (previous-line)
;;         (c-indent-line-or-region))
;;     (newline-and-indent)))

;; (defun hs/c-cpp-ret-kbd ()
;;   (define-key c++-mode-map (kbd "RET") 'hs/c-cpp-bracket-newline)
;;   (define-key c-mode-map (kbd "RET") 'hs/c-cpp-bracket-newline))

;; (setq-default clang-format-style "{BasedOnStyle: llvm, IndentWidth: 4}")
;; (add-hook 'c++-mode-hook 'hs/c-cpp-ret-kbd)
;; (add-hook 'c-mode-hook 'hs/c-cpp-ret-kbd)

(setq cc-hook-map
      '((c++-mode-hook c++-mode-map)
        (c-mode-hook c-mode-map))
      )

(dolist (hook-map cc-hook-map)
  (lexical-let ((hook (car hook-map))
                (mmap (car (cdr hook-map))))
    (add-hook hook
              #'(lambda ()
                  (define-key (symbol-value mmap) (kbd "M-o") 'hs-cmd/find-other-file-in-project)
                  (define-key (symbol-value mmap) (kbd "M-e") nil)
                  (c-set-offset 'innamespace 0)
                  )
              )
    )
  )

(use-package dap-mode
  :config
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  )

(use-package lsp-mode
  :config
  (setq lsp-clients-clangd-args '("-j=16" "--background-index" "--log=error" "--compile-commands-dir=build" "--clang-tidy"))
  )

(use-package clang-format+
  :config
  (add-hook 'before-save-hook
            #'(lambda ()
                (dolist (mode '(c++-mode cc-mode c-mode))
                  (if (eq major-mode mode)
                      (clang-format-buffer))))
            )
  )

(use-package consult
  :config
  ;; overwrite this function if consult exists
  (defun hs-cmd/find-other-file-in-project ()
    (interactive)
    (let ((results (hs/find-other-file-in-project)))
      (cond ((not results))
            ((length= results 1)
             (find-file (car results)))
            (t
             (consult--read results :prompt "Open file:" :category 'file))
            )
      )
    )
  )

(provide 'lang-cpp)
