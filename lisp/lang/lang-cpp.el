(setq-default c-basic-offset 4
              tab-width 4)

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

(use-package dap-mode
  :config
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  )

(use-package lsp-mode
  :config
  (setq lsp-clients-clangd-args '("-j=16" "--background-index" "--log=error" "--compile-commands-dir=build" "--clang-tidy"))
  )

(use-package clang-format+)

(provide 'lang-cpp)
