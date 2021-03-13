(require 'dash)
(require 'evil)
(require 'ivy)


(setq killboard--history-list '())
(setq killboard--max-num 64)


(defun killboard-view-history ()
  (interactive)
  (ivy-read " Killboard History: "
            killboard--history-list
            :action (lambda (content)
                      (kill-new content)
                      (if (and (boundp 'evil-state)
                               (not (eq evil-state 'emacs)))
                          (evil-paste-after 1)
                        (yank)))))


(defun killboard--save-content-to-history (content)
  (let* ((content-idx (-elem-index content killboard--history-list)))
    (if content-idx
        (setq killboard--history-list (-remove-at content-idx killboard--history-list))
      nil)
    (setq killboard--history-list (-insert-at 0 content killboard--history-list)))
  (if (> (length killboard--history-list) killboard--max-num)
      (setq killboard--history-list (-drop-last 1 killboard--history-list))
    nil))


(defun killboard--save-current-kill (&rest args)
  (killboard--save-content-to-history (current-kill 0)))


(advice-add 'evil-yank :after #'killboard--save-current-kill)
(advice-add 'kill-region :after #'killboard--save-current-kill)
(advice-add 'kill-ring-save :after #'killboard--save-current-kill)
(advice-add 'kill-word :after #'killboard--save-current-kill)
(advice-add 'backward-kill-word :after #'killboard--save-current-kill)


(define-key hs-leader-map (kbd "y") 'killboard-view-history)
