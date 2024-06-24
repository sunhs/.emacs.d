(require 'project)

(defvar hs--project-markers
  '
  ((raw raw (".project" ".projectile"))
   (conf conf-cpp ("CMakeFiles.txt" "Makefile" "xmake.lua"))
   (conf conf-go ("go.mod"))
   (conf conf-rust ("Cargo.toml"))
   (conf conf-js ("package.json"))
   (conf conf-py ("setup.lua" "pyproject.toml"))
   (doc readme ("README.org" "README.md"))))

(defun hs--project-find-function (dir)
  (catch 'ret
    (dolist (type-be-markers hs--project-markers)
      (let
          (
           (type (nth 0 type-be-markers))
           (be (nth 1 type-be-markers))
           (markers (nth 2 type-be-markers)))
        (dolist (marker markers)
          (when-let ((root (locate-dominating-file dir marker)))
            (throw 'ret (list type be (file-truename root)))))))))

(setq project-find-functions
      '(project-try-vc hs--project-find-function))

(cl-defmethod project-root (project) (file-truename (nth 2 project)))

(setq project-switch-commands 'project-find-file)

;; patch to hide *BUFFER*, eshell and magit buffers
(defun project--read-project-buffer ()
  (let*
      (
       (pr (project-current t))
       (current-buffer (current-buffer))
       (other-buffer (other-buffer current-buffer))
       (other-name (buffer-name other-buffer))
       (buffers (project-buffers pr))
       (predicate
        (lambda (buffer)
          ;; BUFFER is an entry (BUF-NAME . BUF-OBJ) of Vbuffer_alist.
          (and
           (memq (cdr buffer) buffers)
           (not
            (project--buffer-check
             (cdr buffer)
             project-ignore-buffer-conditions))
           (cl-notany
            #'(lambda (regex) (string-match regex (car buffer)))
            '("^\\*" "^magit" "^eshell.*:"))))))
    (read-buffer
     "Switch to buffer: "
     (when (funcall predicate (cons other-name other-buffer))
       other-name)
     nil predicate)))

;; patch to hide "... (choose a dir)"
(defun project-prompt-project-dir ()
  (project--ensure-read-project-list)
  (let*
      (
       (choices
        ;; XXX: Just using this for the category (for the substring
        ;; completion style).
        (project--file-completion-table project--list))
       (pr-dir ""))
    (while
        (equal pr-dir "")
      ;; If the user simply pressed RET, do this again until they don't.
      (setq pr-dir
            (completing-read "Select project: " choices nil t)))
    pr-dir))

(add-hook 'find-file-hook
          #'
          (lambda ()
            (let ((project (project-current)))
              (when project
                (let ((expected-project-root (file-truename (nth 2 project))))
                  (setcar (cdr (cdr project)) expected-project-root)
                  (project-remember-project project))))))

(provide 'init-project)
