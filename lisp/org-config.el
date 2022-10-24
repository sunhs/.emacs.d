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
;; (defconst capture-memo-file (concat org-file-dir "/MEMO.org"))
;; (defconst capture-note-file (concat org-file-dir "/READING_NOTE.org"))
;; (defconst capture-birthday-file (concat org-file-dir "/org/BIRTHDAY.org"))
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline capture-todo-file "TASK") "** TODO %?\n   %i\n")
        ;; ("m" "MEMO" entry (file+headline capture-memo-file "MEMO") "** %?\n   %i\n")
        ;; ("n" "NOTE" entry (file+headline capture-note-file "NOTE") "** %?\n   %i\n   %a")
        ;; ("b" "BIRTHDAY" entry (file+headline capture-birthday-file "BIRTHDAY") "")
        ))

;; agenda
(setq
 org-agenda-files `(,capture-todo-file)
 org-agenda-include-diary nil)

;; iCalendar
;; (setq org-icalendar-include-todo t
;;       org-icalendar-combined-agenda-file (concat org-file-dir "/org/ICSFILE.ics"))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; ==========================================================================================
;; Copied from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html.
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         (
          (alltodo "" ((org-agenda-overriding-header "todo")
                       (org-agenda-todo-list-sublevels t)))
          (agenda "" ((org-agenda-start-day "-7d")
                      (org-agenda-span 14)
                      (org-agenda-show-log t)
                      (org-agenda-overriding-header "agenda")))
          )
         ;; ((tags "PRIORITY=\"A\""
         ;;        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
         ;;         (org-agenda-overriding-header "High-priority unfinished tasks:")))
         ;; (agenda "" ((org-agenda-ndays 1)))
         ;; (alltodo ""
         ;;          ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
         ;;                                          (air-org-skip-subtree-if-priority ?A)
         ;;                                          (org-agenda-skip-if nil '(scheduled deadline))))
         ;;           (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ;; ((org-agenda-compact-blocks t))
         )))

(provide 'org-config)
