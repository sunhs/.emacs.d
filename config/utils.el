(require 'cl) ;; to ensure that lexical-let works

(defun hyesun/call-or-add-to-frame-hook (fun)
  "`fun: A function receiving an optional parameter `frame.
   The purpose of `fun is to decide whether the frame is graphic and
   thus turn on graphic features.
   But in daemon mode, this is decided after the client frame is made.
   Thus we call `fun immediately in normal mode while in daemon mode
   add it to make frame hook.
   For client frames to work normally, `fun should explicitly
   turn on or off graphic features."
  (if (daemonp)
	  (lexical-let ((fun fun))
		(add-hook 'after-make-frame-functions
				  '(lambda (frame)
					 (select-frame frame)
					 (funcall fun))))
	(funcall fun)))


(defun hyesun/sort-package-list ()
  (if (boundp 'package-selected-packages)
	  (setq package-selected-packages
			(sort package-selected-packages
				  '(lambda (a b)
					 (string< (symbol-name a) (symbol-name b)))))
	nil))


(defun hyesun/show-file-path ()
  "Show the path of file in the current buffer."
  (interactive)
  (message (buffer-file-name)))


(defun hyesun/show-buffer-name ()
  "Show the buffer name."
  (interactive)
  (message (buffer-name)))


(defun hyesun//valid-line-beginning-pos (&optional line)
  (unless line
	(setq line (line-number-at-pos)))
  (let ((valid-point (line-beginning-position (- 1
												 (- (line-number-at-pos) line)))))
	(while (or (= (char-after valid-point) 9)
			   (= (char-after valid-point) 32))
	  (incf valid-point))
	(if (or (= (char-after valid-point) 10)
			(= (char-after valid-point) 13))
		nil
	  valid-point)))


(defun hyesun/move-beginning-of-first-word ()
  "Move to the first non-space and non-tab position of the line.
Behaviors:
1) Current line contains effective characters:
   Move to beginning of first word.
2) Current line contains only spaces:
   Do nothing.
3) Current line is empty:
   Do nothing."
  (interactive)
  (let ((valid-point (hyesun//valid-line-beginning-pos)))
	(if valid-point
	  (goto-char valid-point))))


(defun hyesun/smart-beginning-of-line ()
  "Jump to beginning of first word or beginning of line.
Behaviors:
1) Current line contains effective characters:
   i.   Point is in between words, jump to beginning of first word.
   ii.  Point is on beginning of first word, jump to beginning of line.
   iii. Point is on leading spaces, jump to beginning of line.
2) Current line contains only spaces:
   Jump to beginning of line.
3) Current line is empty:
   Jump to beginning of line."
  (interactive)
  (let ((valid-point (hyesun//valid-line-beginning-pos)))
	(if (and valid-point
			 (> (point) valid-point))
		(goto-char valid-point)
	  (beginning-of-line))))


(defun hyesun/select-stripped-line ()
  "After this, the line two sides of which are non empty are selected.
Behaviors:
1) Current line contains effective characters:
   Select those effective characters.
2) Current line contains only spaces:
   Do nothing.
3) Current line is empty:
   Do nothing."
  (interactive)
  (let ((valid-point (hyesun//valid-line-beginning-pos)))
	(if valid-point
		(progn (goto-char valid-point)
			   (push-mark)
			   (activate-mark)
			   (end-of-line)))))


(defun hyesun/select-line ()
  (interactive)
  (beginning-of-line)
  (push-mark)
  (activate-mark)
  (move-end-of-line 1))


(defun hyesun/kill-stripped-line ()
  (interactive)
  (let ((valid-point (hyesun//valid-line-beginning-pos)))
	(if valid-point
		(kill-region valid-point (line-end-position)))))


(defun hyesun/kill-whole-line ()
  (interactive)
  (hyesun/kill-stripped-line)
  (while (not (= (point) (line-beginning-position)))
	(backward-delete-char 1))
  (backward-delete-char 1))


(defun hyesun/backward-kill-line ()
  "Kill from point till start of the line.
Behaviors:
1) Current line contains effective characters:
   i.  Point in between words. Kill from point till start of effective char.
   ii. Point on first char or on leading spaces. Kill leading spaces.
2) Current line contains only spaces:
   Kill whole line.
3) Current line is empty:
   Kill whole line."
  (interactive)
  (let ((valid-point (hyesun//valid-line-beginning-pos))
		(del-space-p nil))
	(if valid-point
		(if (> (point) valid-point)
			(kill-region valid-point (point))
		  (progn (setq del-space-p t)
				 (goto-char valid-point)))
	  (progn (setq del-space-p t)
			 (goto-char (line-end-position))))
	(if del-space-p
		(progn (while (not (= (point) (line-beginning-position)))
				 (backward-delete-char 1))
			   (backward-delete-char 1)))))


(defun hyesun/comment-line ()
  (interactive)
  (hyesun/select-line)
  (comment-region (mark) (point)))


(defun hyesun/uncomment-line ()
  (interactive)
  (hyesun/select-line)
  (uncomment-region (mark) (point)))

(provide 'utils)
