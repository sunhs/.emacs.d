(setq-default c-basic-offset 4
              tab-width 4)

;; don't indent in namespace
(c-set-offset 'innamespace 0)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq ff-search-directories
      '("." "../src" "../include" "../../src" "../../include"))

(defun hs/c-cpp-bracket-newline ()
  (interactive)
  (if (= (char-before) 123)
      (progn
        (newline-and-indent)
        (newline-and-indent)
        (previous-line)
        (c-indent-line-or-region))
    (newline-and-indent)))

(defun hs/c-cpp-ret-kbd ()
  (define-key c++-mode-map (kbd "RET") 'hs/c-cpp-bracket-newline)
  (define-key c-mode-map (kbd "RET") 'hs/c-cpp-bracket-newline))

;; (setq-default clang-format-style "{BasedOnStyle: llvm, IndentWidth: 4}")
;; (add-hook 'c++-mode-hook 'hs/c-cpp-ret-kbd)
;; (add-hook 'c-mode-hook 'hs/c-cpp-ret-kbd)

(defun hs/find-other-file-kbd ()
  (define-key c++-mode-map (kbd "M-o") 'hs-cmd/find-other-file-in-project)
  (define-key c-mode-map (kbd "M-o") 'hs-cmd/find-other-file-in-project))
(add-hook 'c++-mode-hook 'hs/find-other-file-kbd)
(add-hook 'c-mode-hook 'hs/find-other-file-kbd)

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
