(defun save-package-list-to-file (file)
  (with-temp-buffer
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
