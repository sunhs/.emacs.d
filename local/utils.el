(defun sort-package-list ()
  (setq package-activated-list
		(sort package-activated-list
			  (lambda (a b)
				(string< (symbol-name a) (symbol-name b))))))

(defun save-package-list-to-file (file)
  (with-temp-buffer
	(sort-package-list)
	(insert (prin1-to-string package-activated-list))
	(when (file-writable-p file)
	  (write-region (point-min)
					(point-max)
					file))))

(defun read-package-list-from-file (file)
  (when (file-readable-p file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (read (buffer-string)))))
