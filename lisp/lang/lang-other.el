(use-package cmake-mode)

(use-package protobuf-mode)
(use-package clang-format+
  :config
  (add-hook 'protobuf-mode-hook
            #'(lambda ()
                (setq clang-format-style "{ IndentWidth: 4, BasedOnStyle: google, AlignConsecutiveAssignments: true }")
                )
            )
  )

(provide 'lang-other)
