(defun sort-package-list (package-list)
  "Sort the list according to symbols' name.
   Note that it has side effects on the list.
   Thus making a copy of the list is recommended."
  (sort package-list
		(lambda (a b)
		  (string< (symbol-name a) (symbol-name b)))))

(defun save-package-list-to-file (file)
  (with-temp-buffer
	(setq package-list (copy-list package-activated-list))
	(setq package-list (sort-package-list package-list))
	(setq package-list (remove-duplicates package-list))
	(insert (prin1-to-string package-list))
	(when (file-writable-p file)
	  (write-region (point-min)
					(point-max)
					file))))

(defun read-package-list-from-file (file)
  (when (file-readable-p file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (read (buffer-string)))))
