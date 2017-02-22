(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-right)
(global-set-key (kbd "C-c C-x m") 'set-mark-command)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c /") 'uncomment-region)
(global-set-key (kbd "C-c C-f") 'load-file)
(global-set-key (kbd "C-c C-x s") 'replace-string)
(global-set-key (kbd "C-c C-x <up>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-x <down>") 'shrink-window-horizontally)
;; (define-key (current-global-map) [remap newline] 'newline-and-indent)

(defun show-file-path ()
  "show the path of file in the current buffer"
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c C-x f") 'show-file-path)

(defun show-buffer-name ()
  "show the buffer name"
  (interactive)
  (message (buffer-name)))
(global-set-key (kbd "C-c C-x b") 'show-buffer-name)

(defun move-beginning-of-first-word ()
  (interactive)
  (move-beginning-of-line 1)
  (while (or
		  (= (char-after) 9)
		  (= (char-after) 32))
	(forward-char)))
(global-set-key (kbd "C-c C-x w") 'move-beginning-of-first-word)

(defun select-stripped-line ()
  (interactive)
  (move-beginning-of-first-word)
  (set-mark-command nil)
  (move-end-of-line 1))
(global-set-key (kbd "C-c C-x l") 'select-stripped-line)

(defun delete-stripped-line ()
  (interactive)
  (select-stripped-line)
  (backward-delete-char-untabify 1))
(global-set-key (kbd "C-c C-x d") 'delete-stripped-line)

(defun select-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1))

(defun delete-line ()
  (interactive)
  (select-line)
  (backward-delete-char-untabify 1)
  (backward-delete-char-untabify 1))
(global-set-key (kbd "C-c C-x x") 'delete-line)

(defun comment-line ()
  (interactive)
  (select-line)
  (comment-region (mark) (point)))
(global-set-key (kbd "C-c C-x ;") 'comment-line)

(defun uncomment-line ()
  (interactive)
  (select-line)
  (uncomment-region (mark) (point)))
(global-set-key (kbd "C-c C-x /") 'uncomment-line)