(require 'cl) ;; to ensure that lexical-let works
(defun call-or-add-to-frame-hook (fun)
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


(defun sort-package-list ()
  (if (boundp 'package-selected-packages)
	  (setq package-selected-packages
			(sort package-selected-packages
				  '(lambda (a b)
					 (string< (symbol-name a) (symbol-name b)))))
	nil))


(defun show-file-path ()
  "show the path of file in the current buffer"
  (interactive)
  (message (buffer-file-name)))


(defun show-buffer-name ()
  "show the buffer name"
  (interactive)
  (message (buffer-name)))


(defun move-beginning-of-first-word ()
  (interactive)
  (move-beginning-of-line 1)
  (while (or
		  (= (char-after) 9)
		  (= (char-after) 32))
	(forward-char)))


(defun select-stripped-line ()
  (interactive)
  (move-beginning-of-first-word)
  (set-mark-command nil)
  (move-end-of-line 1))


(defun delete-stripped-line ()
  (interactive)
  (select-stripped-line)
  (backward-delete-char-untabify 1))


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


(defun comment-line ()
  (interactive)
  (select-line)
  (comment-region (mark) (point)))


(defun uncomment-line ()
  (interactive)
  (select-line)
  (uncomment-region (mark) (point)))
