(require 'dash)


(defvar killboard--history-list '())
(defvar killboard--max-num 64)


(defun killboard-paste ()
  (interactive)
  (let*
    (
      (prompt " Killboard History: ")
      (content
        (cond
          ((fboundp 'consult--read)
            (consult--read
              killboard--history-list
              :prompt prompt
              :sort nil))
          (t
            (completing-read prompt killboard--history-list)))))
    (kill-new content)
    (if (and (boundp 'evil-state) (not (eq evil-state 'emacs)))
      (evil-paste-after 1)
      (yank))))

(defun killboard--save-content-to-history (content)
  (let* ((content-idx (-elem-index content killboard--history-list)))
    (if content-idx
      (setq killboard--history-list
        (-remove-at content-idx killboard--history-list))
      nil)
    (setq killboard--history-list
      (-insert-at 0 content killboard--history-list)))
  (if (> (length killboard--history-list) killboard--max-num)
    (setq killboard--history-list
      (-drop-last 1 killboard--history-list))
    nil))


(defun killboard--save-current-kill (&rest args)
  (killboard--save-content-to-history (current-kill 0)))


(advice-add 'evil-yank :after #'killboard--save-current-kill)
(advice-add 'kill-region :after #'killboard--save-current-kill)
(advice-add 'kill-ring-save :after #'killboard--save-current-kill)
(advice-add 'kill-word :after #'killboard--save-current-kill)
(advice-add 'backward-kill-word :after #'killboard--save-current-kill)


(define-key hs-leader-map (kbd "kb") 'killboard-paste)

(provide 'killboard)
