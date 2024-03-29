;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(require 'lines)

(defun hs/call-or-add-to-frame-hook (fun)
  "`fun: A function receiving an optional parameter `frame.
   The purpose of `fun is to decide whether the frame is graphic and
   thus turn on graphic features.
   But in daemon mode, this is decided after the client frame is made.
   Thus we call `fun immediately in normal mode while in daemon mode
   add it to make frame hook.
   For client frames to work normally, `fun should explicitly
   turn on or off graphic features."
  (if (daemonp)
    (let ((fun fun))
      (add-hook 'after-make-frame-functions
        (lambda (frame) (select-frame frame) (funcall fun))))
    (funcall fun)))


(defun hs/sort-package-list ()
  (if (boundp 'package-selected-packages)
    (sort
      package-selected-packages
      (lambda (a b) (string< (symbol-name a) (symbol-name b))))
    nil))


(defmacro hs/define-key-when-set (set-sym key-map key-seq func)
  (if (and (boundp set-sym) (symbol-value set-sym))
    `(define-key ,key-map ,key-seq ,func)
    nil))


(defun hs/show-file-path ()
  "Show the path of file in the current buffer."
  (interactive)
  (message (buffer-file-name)))


(defun hs/show-buffer-name ()
  "Show the buffer name."
  (interactive)
  (message (buffer-name)))


(defun hs/kill-user-buffers ()
  "Kill all buffers created by user."
  (interactive)
  (dolist (b (buffer-list))
    (cond
      ;; skip eshell buffers
      ((not (null (string-match "^eshell:.*" (buffer-name b))))
        nil)
      ;; buffer names start with alphanumeric
      ((= ?w (char-syntax (aref (buffer-name b) 0)))
        (kill-buffer b))))
  (message "Done."))

(defun hs/jump-up-half ()
  (interactive)
  (if (> (line-number-at-pos) 1)
    (goto-line (/ (line-number-at-pos) 2))))

(defun hs/jump-down-half ()
  (interactive)
  (let
    (
      (distance-to-bottom
        (- (line-number-at-pos (point-max)) (line-number-at-pos))))
    (if (> distance-to-bottom 1)
      (goto-line (+ (line-number-at-pos) (/ distance-to-bottom 2))))))

(defun hs/term ()
  (interactive)
  (term "/bin/zsh"))

(defun hs/pad-string (content len pad &optional right-pad)
  "pad `content' until the string size is `len'
  `pad' should be a char, and will be repeated and equally placed both on the left and right side of `'content'
  if `right-pad' is also a char, use `pad' on the left and `right-pad' on the right"

  (let
    (
      (pad-len (/ (- (- len (length content)) 2) 2))
      (cur-len 0)
      (result ""))
    (while (< cur-len pad-len)
      (setq result (concat result pad))
      (incf cur-len))
    (setq result (concat result " " content " "))
    (incf cur-len (+ 2 (length content)))
    (if (not right-pad)
      (setq right-pad pad))
    (while (< cur-len len)
      (setq result (concat result right-pad))
      (incf cur-len))
    result))

(defun hs/insert-package-header ()
  (interactive)
  (let ((content (read-from-minibuffer "content: ")))
    (insert ";; ")
    (insert (hs/pad-string content 110 "-"))))

(defun hs/insert-collection-header ()
  (interactive)
  (let ((content (read-from-minibuffer "content: ")))
    (insert ";; ")
    (insert (hs/pad-string content 110 "<" ">"))))

(defun hs/insert-comment-header ()
  (interactive)
  (let ((content (read-from-minibuffer "content: ")))
    (insert (hs/pad-string content 110 ";" ";"))))

(defun hs/find-other-file-in-project (&optional fpath)
  (require 'find-file)
  (let*
    (
      (cur-project (project-current))
      project-files
      (fname
        (if fpath
          (file-name-nondirectory fpath)
          (file-name-nondirectory (buffer-file-name))))
      (alist
        (if (symbolp ff-other-file-alist)
          (symbol-value ff-other-file-alist)
          ff-other-file-alist))
      (rule (car alist))
      (pos (ff-string-match (car rule) fname))
      (stub "")
      (candidate-fnames nil)
      (results nil)
      (nomatch nil))

    (if (null (project-current))
      (user-error "Not in a project."))

    ;; find a rule that matches the buffer filename
    (while
      (and
        rule
        (if (and pos (>= pos 0))
          nil
          (not pos)))
      (setq alist (cdr alist))
      (setq rule (car alist))
      (setq pos (ff-string-match (car rule) fname)))

    (if (not rule)
      (setq nomatch t)

      (setq
        suffixes
        (car (cdr rule))
        action (car (cdr rule)))

      ;; action is a function
      (if (and (atom action) (fboundp action))
        (setq candidate-fnames (funcall action (ff-buffer-file-name)))

        ;; get the stub (filename without extension)
        (setq format (concat "\\(.+\\)" (car rule)))
        (string-match format fname)
        (setq stub (match-string 1 fname))
        ;; concat with new suffixes
        (setq candidate-fnames
          (mapcar (lambda (suffix) (concat stub suffix)) suffixes)))

      (setq project-files (project-files cur-project))

      ;; find new filenames
      (dolist (candfname candidate-fnames)
        ;; prevent such cases: submodule.cc -> fuck_submodule.cc.o
        (setq regex (concat "^" candfname "$"))
        (dolist (pfile project-files)
          (if (string-match regex (file-name-nondirectory pfile))
            (setq results (nconc results (list pfile)))))))

    (cond
      ((or nomatch (not results))
        (message "no match with `fname' %s and `rule' %s" fname rule)
        nil)
      (t
        results))))

(defun hs-cmd/find-other-file-in-project ()
  (interactive)
  (let ((results (hs/find-other-file-in-project)))
    (cond
      ((not results))
      ((length= results 1)
        (find-file (car results)))
      (t
        (find-file (completing-read "Open file:" results))))))

(provide 'utils)
