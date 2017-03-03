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
