;; --------------------------------------------------------------------------------------------------------------
;; org-mode
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
(setq org-log-done 'note)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; (eval-after-load "org"
;;   '(progn
;;      (define-key org-mode-map (kbd "C-c l") 'org-store-link)
;;      (define-key org-mode-map (kbd "C-c i") 'org-iswitchb)
;;      (define-key org-mode-map (kbd "C-j") 'backward-char)))

;; export options
(setq org-export-with-toc nil)

;; org-capture
(defconst org-file-dir (substitute-in-file-name "$HOME/org"))
(defconst capture-todo-file (concat org-file-dir "/TODO.org"))
(defconst capture-memo-file (concat org-file-dir "/MEMO.org"))
(defconst capture-note-file (concat org-file-dir "/READING_NOTE.org"))
;; (defconst capture-birthday-file (concat org-file-dir "/org/BIRTHDAY.org"))
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline capture-todo-file "TASK") "** TODO %?\n   %i\n")
        ("m" "MEMO" entry (file+headline capture-memo-file "MEMO") "** %?\n   %i\n")
        ("n" "NOTE" entry (file+headline capture-note-file "NOTE") "** %?\n   %i\n   %a")
        ;; ("b" "BIRTHDAY" entry (file+headline capture-birthday-file "BIRTHDAY") "")
        ))

;; agenda
(setq
 org-agenda-files (list capture-todo-file capture-memo-file capture-note-file)
 org-agenda-include-diary t)

;; iCalendar
;; (setq org-icalendar-include-todo t
;;       org-icalendar-combined-agenda-file (concat org-file-dir "/org/ICSFILE.ics"))
