(defun hs//valid-line-beginning-pos (&optional line)
  (unless line
    (setq line (line-number-at-pos)))
  (let
    (
      (valid-point
        (line-beginning-position
          (- 1 (- (line-number-at-pos) line)))))
    (while
      (or
        (= (char-after valid-point) 9)
        (= (char-after valid-point) 32))
      (incf valid-point))
    (if
      (or
        (= (char-after valid-point) 10)
        (= (char-after valid-point) 13))
      nil
      valid-point)))


(defun hs/move-beginning-of-first-word ()
  "Move to the first non-space and non-tab position of the line.
Behaviors:
1) Current line contains effective characters:
   Move to beginning of first word.
2) Current line contains only spaces:
   Do nothing.
3) Current line is empty:
   Do nothing."
  (interactive)
  (let ((valid-point (hs//valid-line-beginning-pos)))
    (if valid-point
      (goto-char valid-point))))


(defun hs/smart-beginning-of-line ()
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
  (let ((valid-point (hs//valid-line-beginning-pos)))
    (if (and valid-point (> (point) valid-point))
      (goto-char valid-point)
      (beginning-of-line))))


(defun hs/select-stripped-line ()
  "After this, the line two sides of which are non empty are selected.
Behaviors:
1) Current line contains effective characters:
   Select those effective characters.
2) Current line contains only spaces:
   Do nothing.
3) Current line is empty:
   Do nothing."
  (interactive)
  (let ((valid-point (hs//valid-line-beginning-pos)))
    (if valid-point
      (progn
        (goto-char valid-point)
        (push-mark)
        (activate-mark)
        (end-of-line)))))


(defun hs/select-line ()
  (interactive)
  (beginning-of-line)
  (push-mark)
  (activate-mark)
  (move-end-of-line 1))


(defun hs/kill-stripped-line ()
  (interactive)
  (let ((valid-point (hs//valid-line-beginning-pos)))
    (if valid-point
      (kill-region valid-point (line-end-position)))))


(defun hs/kill-whole-line ()
  (interactive)
  (hs/kill-stripped-line)
  (while (not (= (point) (line-beginning-position)))
    (backward-delete-char 1))
  (backward-delete-char 1))


(defun hs/backward-kill-line ()
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
  (let
    (
      (valid-point (hs//valid-line-beginning-pos))
      (del-space-p nil))
    (if valid-point
      (if (> (point) valid-point)
        (kill-region valid-point (point))
        (progn
          (setq del-space-p t)
          (goto-char valid-point)))
      (progn
        (setq del-space-p t)
        (goto-char (line-end-position))))
    (if del-space-p
      (progn
        (while (not (= (point) (line-beginning-position)))
          (backward-delete-char 1))
        (backward-delete-char 1)
        (insert " ")))))

(defun hs/comment-line ()
  (interactive)
  (hs/select-line)
  (comment-region (mark) (point)))

(defun hs/uncomment-line ()
  (interactive)
  (hs/select-line)
  (uncomment-region (mark) (point)))

(defun hs-cmd/recenter-top ()
  (interactive)
  ;; (let ((old-margin scroll-margin))
  ;;   (setq scroll-margin 0)
  ;;   (recenter-top-bottom 0)
  ;;   (setq scroll-margin old-margin)
  ;;   )
  (let
    (
      (scroll-num
        (if (> scroll-margin 0)
          (- scroll-margin 1)
          0)))
    (next-line scroll-num)
    (recenter-top-bottom 0)))

(defun hs--select-region-lines (start end)
  "The result `mark' < `point'"
  (let*
    (
      (line-diff
        (abs
          (-
            (line-number-at-pos (mark)) (line-number-at-pos (point)))))
      (bol-scan
        (if (>= (point) (mark))
          ;; +1 because `line-beginning-position' scan through N-1
          (+ 1 (- line-diff))
          nil))
      (eol-scan
        (if (>= (point) (mark))
          nil
          ;; +1 because `line-end-position' scan through N-1
          (+ 1 line-diff)))
      (bol-start (line-beginning-position bol-scan))
      (eol-end (line-end-position eol-scan)))
    (set-mark bol-start)
    (goto-char eol-end)))

(defun hs--move-region-lines (start end n)
  (if (use-region-p)
    (hs--select-region-lines start end)
    (hs/select-line))
  (cond
    (
      (and
        (> n 0)
        (=
          (line-number-at-pos end)
          (line-number-at-pos (buffer-end 1))))
      (message "Already at the end of buffer."))
    (
      (and
        (< n 0)
        (=
          (line-number-at-pos start)
          (line-number-at-pos (buffer-end -1))))
      (message "Already at the beginning of buffer."))
    (t
      (let
        (
          (region-text (delete-and-extract-region (mark) (point)))
          (tmpn (- (abs n) 1)))
        ;; delete '\r'
        (cond
          ((and (> n 0) (= (char-after) 10))
            (delete-char 1)
            (next-line tmpn)
            (end-of-line)
            (newline))
          ((and (< n 0) (= (char-before) 10))
            (backward-delete-char 1)
            (previous-line tmpn)
            (beginning-of-line)
            (newline)
            (previous-line)))

        (let ((curpoint (point)))
          (insert region-text)
          (setq deactivate-mark nil)
          (set-mark curpoint))))))

(defun hs--get-region-and-prefix ()
  ;; copied from https://github.com/emacsfodder/move-text
  (list
    (when (use-region-p)
      (region-beginning)) ;; otherwise nil
    (when (use-region-p)
      (region-end))
    (prefix-numeric-value current-prefix-arg)))

(defun hs/move-region-lines-up (start end n)
  (interactive (hs--get-region-and-prefix))
  (hs--move-region-lines
    start end
    (if (null n)
      1
      (- n))))

(defun hs/move-region-lines-down (start end n)
  (interactive (hs--get-region-and-prefix))
  (hs--move-region-lines
    start end
    (if (null n)
      1
      n)))

(provide 'lines)
