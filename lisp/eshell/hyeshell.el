;;; hyeshell.el --- Hyesun eshell

;; Modified from:
;; http://www.emacswiki.org/emacs/download/aweshell.el
;; by Andy Stewart <lazycat.manatee@gmail.com>

(require 'eshell)
(require 'projectile)
(require 'consult)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup hyeshell nil "Hyesun eshell." :group 'hyeshell)

(defcustom hyeshell-valid-command-color "#98C379"
  "The color of valid command by `hyeshell--validate-command-set-color'."
  :type 'string
  :group 'hyeshell)

(defcustom hyeshell-invalid-command-color "#FF0000"
  "The color of invalid command by `hyeshell--validate-command-set-color'."
  :type 'string
  :group 'hyeshell)

(defcustom hyeshell-dedicated-window-height 14
  "The height of `hyeshell' dedicated window."
  :type 'integer
  :group 'hyeshell)

(defcustom hyeshell-auto-complete-p t
  "Whether to auto complete."
  :type 'boolean
  :group 'hyeshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; internal variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar hyeshell/match-dir-funs nil)
(defvar hyeshell--buffer-list nil "Existing eshell buffers")
(defvar hyeshell--dedicated-window nil "The dedicated esehll window.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; heuristically find eshell buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hyeshell--find-nearest-eshell-buffer (&optional match-dir-funs)
  "Find the nearest eshell buffer.
`Nearest' is determined by the current buffer's directory and the eshell buffer's directory.
It's examined as follow:
  1) Apply each function in `match-dir-funs' to all eshell buffer directories
     until the first match is found;
  2) Exactly same directory;
  3) Same `projectile-project-root';

Arg `match-dir-funs' is a list of functions with two arguments,
the first of which represents the current buffer's directory, i.e., `default-directory',
and the second of which represents the directory to match against.
The return value should be `t' meaning a match, otherwise `nil'."

  (cl-block
    block
    (let* ((curdir default-directory))
      (if match-dir-funs
        (dolist (match-dir-fun match-dir-funs)
          (dolist (buf hyeshell--buffer-list)
            (with-current-buffer buf
              (if
                (funcall
                  match-dir-fun
                  (file-truename curdir)
                  (file-truename default-directory))
                (cl-return-from block buf))))))

      (dolist (buf hyeshell--buffer-list)
        (with-current-buffer buf
          (if
            (string=
              (file-truename curdir)
              (file-truename default-directory))
            (cl-return-from block buf))))

      (dolist (buf hyeshell--buffer-list)
        (with-current-buffer buf
          (if
            (string=
              (file-truename (projectile-project-root curdir))
              (file-truename
                (projectile-project-root default-directory)))
            (cl-return-from block buf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hyeshell/toggle ()
  (interactive)
  (cond
    ;; toggle off
    ((equal major-mode 'eshell-mode)
      (while (equal major-mode 'eshell-mode)
        (switch-to-prev-buffer)))
    ;; toggle on
    (t
      (let ((buf (hyeshell--find-nearest-eshell-buffer)))
        (if (not buf)
          (eshell)
          (switch-to-buffer buf))))))

(defun hyeshell/clear-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defvar hyeshell--consult-source-buffer
  `
  (:name
    "eshell buffer"
    :category buffer
    :face consult-buffer
    :state ,#'consult--buffer-state
    :default t
    :items ,(lambda () (mapcar #'buffer-name hyeshell--buffer-list))))

(defun hyeshell--marginalia-annotate-eshell-buffer (candidate)
  "Additional to `marginalia-annotate-buffer', if the buffer is an eshell buffer, display eshell command."
  (let
    (
      (esh-buf-alist
        (mapcar
          #'(lambda (buffer) `(,(buffer-name buffer) . ,buffer))
          hyeshell--buffer-list)))
    (if (not (alist-get candidate esh-buf-alist nil nil #'equal))
      ;; not eshell buffer, use `marginalia-annotate-buffer'
      (marginalia-annotate-buffer candidate)
      ;; eshell buffer, display eshell command
      (let*
        (
          (candidate-buffer
            (alist-get candidate esh-buf-alist nil nil #'equal))
          (annotation
            (with-current-buffer candidate-buffer
              (format
                "  <%s> %s" (eshell-get-history 0)
                (if eshell-current-command
                  "(Running)"
                  "")))))
        (marginalia--fields (annotation))))))

(add-to-list 'marginalia-annotator-registry
  '(buffer hyeshell--marginalia-annotate-eshell-buffer builtin none))

(defun hyeshell/switch-buffer ()
  (interactive)
  (cond
    (
      (or
        (not hyeshell--buffer-list)
        (length= hyeshell--buffer-list 0))
      (hyeshell/toggle)
      (message "No buffer yet, create a new one."))
    ((length= hyeshell--buffer-list 1)
      (message "Only one buffer.")
      (switch-to-buffer (nth 0 hyeshell--buffer-list)))
    (t
      (consult--multi '(hyeshell--consult-source-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dedicated window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hyeshell--dedicated-window-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun hyeshell--update-dedicated-window ()
  (let ((buf (hyeshell--find-nearest-eshell-buffer)))
    (if (not buf)
      (progn
        (setq buf (eshell))
        (previous-buffer)))
    (if
      (or
        (not hyeshell--dedicated-window)
        (not (window-live-p hyeshell--dedicated-window)))
      (setq hyeshell--dedicated-window
        (display-buffer
          buf
          `
          (display-buffer-in-side-window
            (side . bottom)
            (window-height . ,hyeshell-dedicated-window-height))))
      (set-window-dedicated-p hyeshell--dedicated-window t))))

(defun hyeshell/toggle-dedicated ()
  (interactive)
  (if
    (and
      hyeshell--dedicated-window
      (window-live-p hyeshell--dedicated-window)
      (eq (selected-window) hyeshell--dedicated-window))
    ;; toggle off
    (delete-window hyeshell--dedicated-window)
    ;; toggle on
    (hyeshell--update-dedicated-window)
    (select-window hyeshell--dedicated-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hyeshell--remove-killed-buffers ()
  (setq hyeshell--buffer-list
    (cl-remove-if
      #'(lambda (buf) (not (buffer-live-p buf)))
      hyeshell--buffer-list)))

(defun hyeshell--sync-buffers ()
  "Rename eshell buffer, and add it to `hyeshell-buffer-list', if not exist."
  (when (equal major-mode 'eshell-mode)
    (let*
      (
        (possible-proj-name (projectile-project-name))
        (proj-name
          (if
            (or
              (not possible-proj-name)
              (string= possible-proj-name "-"))
            "/"
            possible-proj-name))
        (dir-name (file-truename default-directory))
        (buf-name (format "eshell: [%s](%s)" proj-name dir-name)))
      (rename-buffer buf-name t))
    (let
      (
        (foundp
          (catch 'foundp
            (dolist (buf hyeshell--buffer-list)
              (if (eq buf (current-buffer))
                (throw 'foundp t))))))
      (if (not foundp)
        (setq hyeshell--buffer-list
          (nconc hyeshell--buffer-list (list (current-buffer)))))))
  (hyeshell--remove-killed-buffers))

(add-hook 'eshell-directory-change-hook #'hyeshell--sync-buffers)
(add-hook 'eshell-mode-hook #'hyeshell--sync-buffers)

(defun hyeshell-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'eshell-mode)
    (let ((killed-buffer (current-buffer)))
      (setq hyeshell--buffer-list
        (delq killed-buffer hyeshell--buffer-list))))
  (hyeshell--remove-killed-buffers))

(add-hook 'kill-buffer-hook #'hyeshell-kill-buffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First completion candidate is best match that pick from shell history.
;; Rest is completion arguments from shell completion.
(when hyeshell-auto-complete-p
  (defun hyeshell--company-prefix ()
    "Get current eshell input.)

If this function return non-nil prefix, hyeshell will popup completion menu in hyeshell buffer.
This function only return prefix when current point at eshell prompt line, avoid insert unnecessary indent char, such as ghci prompt. (See issue #49)."
    (when
      (save-excursion
        (beginning-of-line)
        (looking-at-p eshell-prompt-regexp))
      (string-trim-left
        (buffer-substring-no-properties
          (save-excursion (eshell-bol))
          (line-end-position)))))

  (defun hyeshell--company-candidates (prefix)
    (let*
      (
        ;; copied from https://github.com/Henry/dot-emacs/blob/master/my-lisp/company-pcomplete.el
        (pcomplete-stub)
        (pcomplete-show-list t)
        pcomplete-seen
        pcomplete-norm-func
        pcomplete-args
        pcomplete-last
        pcomplete-index
        (pcomplete-autolist pcomplete-autolist)
        (pcomplete-suffix-list pcomplete-suffix-list)
        (pcmpl-candidates (pcomplete-completions))
        (pcmpl-prefix (buffer-substring (pcomplete-begin) (point)))
        ;; Collect all possible completions for the current stub
        (pcmpl-cnds (all-completions pcomplete-stub pcmpl-candidates))
        (pcmpl-bnds
          (completion-boundaries
            pcomplete-stub
            pcmpl-candidates
            nil
            ""))
        (skip (- (length pcomplete-stub) (car pcmpl-bnds)))
        ;; Replace the stub at the beginning of each candidate by the prefix
        (res-candidates-pcomplete
          (mapcar
            #'(lambda (cand) (concat prefix (substring cand skip)))
            pcmpl-cnds))
        (res-candidates-history
          (cl-remove-if-not
            #'(lambda (candidate) (string-prefix-p prefix candidate))
            (hyeshell/get-shell-history)))
        (res-candidates
          (cl-remove-duplicates
            (append
              (mapcar
                (lambda (cand) (propertize cand 'anno "history"))
                res-candidates-history)
              (mapcar
                (lambda (cand) (propertize cand 'anno "pcomplete"))
                res-candidates-pcomplete))
            :test #'string=)))
      res-candidates))

  (defun hyeshell--company-backend
    (command &optional arg &rest ignored)
    "`company-mode' backend."
    (interactive (list 'interactive))
    (cl-case
      command
      (interactive (company-begin-backend 'hyeshell--company-backend))
      (prefix
        (and
          (derived-mode-p 'eshell-mode) (hyeshell--company-prefix)))
      (candidates
        (let ((ret (hyeshell--company-candidates arg)))
          ret))
      (annotation (get-text-property 0 'anno arg))
      (sorted nil)))

  (add-hook 'eshell-mode-hook
    (lambda ()
      (company-mode 1)
      (setq-local company-idle-delay nil)
      (setq-local company-backends '(hyeshell--company-backend)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; enhance eshell commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validate command before post to eshell.
(defun hyeshell--validate-command-set-color ()
  (save-excursion
    (let
      (
        end
        (line-end-position))
      (forward-line 0)
      (re-search-forward
        (format "%s\\([^ \t\r\n\v\f]*\\)" eshell-prompt-regexp)
        end
        t))
    (let
      (
        (beg (match-beginning 1))
        (end (match-end 1))
        (command (match-string 1)))
      (when command
        (put-text-property
          beg end 'face
          `
          (:foreground
            ,
            (if
              (or
                ;; Command exists?
                (executable-find command)
                ;; Or command is an alias?
                (seq-contains (eshell-alias-completions "") command)
                ;; Or it is ../. ?
                (or
                  (equal command "..")
                  (equal command ".")
                  (equal command "exit"))
                ;; Or it is a file in current dir?
                (member
                  (file-name-base command)
                  (directory-files default-directory))
                ;; Or it is a elisp function
                (functionp (intern command)))
              hyeshell-valid-command-color
              hyeshell-invalid-command-color)))
        (put-text-property beg end 'rear-nonsticky t)))))

(add-hook 'eshell-mode-hook
  (lambda ()
    (add-hook 'post-command-hook
      #'hyeshell--validate-command-set-color
      t
      t)))

(defun hyeshell--emacs (&rest args)
  "Open a file in Emacs with ARGS, Some habits die hard."
  (if (null args)
    ;; If I just ran "emacs", I probably expect to be launching
    ;; Emacs, which is rather silly since I'm already in Emacs.
    ;; ;; So just pretend to do what I ask.
    ;; (bury-buffer)
    nil
    (let ((file-paths (mapcar #'expand-file-name args)))
      ;; if in dedicated window, switch to another window
      (if
        (and
          hyeshell--dedicated-window
          (window-live-p hyeshell--dedicated-window)
          (eq (selected-window) hyeshell--dedicated-window))
        (other-window 1))
      (mapc #'find-file file-paths))))

(defalias 'eshell/e 'hyeshell--emacs)

(defun hyeshell--unpack (file &rest args)
  "Unpack FILE with ARGS."
  (let
    (
      (command
        (some
          (lambda (x)
            (if (string-match-p (car x) file)
              (cadr x)))
          '
          ((".*\.tar.bz2" "tar xjf")
            (".*\.tar.gz" "tar xzf")
            (".*\.bz2" "bunzip2")
            (".*\.rar" "unrar x")
            (".*\.gz" "gunzip")
            (".*\.tar" "tar xf")
            (".*\.tbz2" "tar xjf")
            (".*\.tgz" "tar xzf")
            (".*\.zip" "unzip")
            (".*\.Z" "uncompress")
            (".*" "echo 'Could not unpack the file:'")))))
    (let
      (
        (unpack-command
          (concat
            command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))))

(defalias 'eshell/unpack 'hyeshell--unpack)

;; Make cat with syntax highlight.
(defun hyeshell--cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let
    (
      (existing-buffer (get-file-buffer filename))
      (buffer (find-file-noselect filename)))
    (eshell-print
      (with-current-buffer buffer
        (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
          (with-no-warnings (font-lock-fontify-buffer)))
        (let ((contents (buffer-string)))
          (remove-text-properties
            0
            (length contents)
            '(read-only nil)
            contents)
          contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(advice-add 'eshell/cat
  :override #'hyeshell--cat-with-syntax-highlight)

(defun hyeshell/fj (&rest args)
  (let
    (
      (str-args
        (mapcar
          #'
          (lambda (arg)
            (if (stringp arg)
              arg
              (number-to-string arg)))
          args)))

    (cond
      ((length= str-args 0)
        (eshell/cd))

      (
        (and
          (length= str-args 1)
          (string-match "^-.*" (nth 0 str-args)))
        (let* ((str-arg (nth 0 str-args)))
          (if
            (or
              (string= str-arg "-")
              (< (string-to-number str-arg) 0))
            ;; `cd -' or `cd -1', `cd -2', etc
            (eshell/cd str-arg)
            ;; maybe illegal use
            (let
              (
                (output
                  (string-trim
                    (eshell-command-result
                      (string-join `("pyfj_cli.py" ,str-arg) " ")))))
              (eshell/echo output)))))

      (t
        (let*
          (
            (cmd (string-join `("pyfj_cli.py jump" ,@str-args) " "))
            (code)
            (output (eshell-command-result cmd 'code))
            (dir
              (file-truename
                (if (and output (= code 0))
                  (string-trim output)
                  nil))))
          (if (not dir)
            (eshell/echo "no match")
            (eshell/cd dir)))))))

(defalias 'eshell/fj #'hyeshell/fj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shell history ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hyeshell--reload-shell-history ()
  (with-temp-message
    "Reloading shell histories.."
    (cond
      ((string-equal shell-file-name "/bin/bash")
        (shell-command "history -r"))
      ((string-equal shell-file-name "/bin/zsh")
        (shell-command "fc -W; fc -R")))))

(defun hyeshell--parse-bash-history ()
  (if (file-exists-p "~/.bash_history")
    (let
      (
        collection
        bash_history)
      (hyeshell--reload-shell-history)
      (setq collection
        (nreverse
          (split-string
            (with-temp-buffer
              (insert-file-contents (file-truename "~/.bash_history"))
              (buffer-string))
            "\n" t)))
      (when
        (and
          collection
          (> (length collection) 0)
          (setq bash_history collection))
        bash_history))
    nil))

(defun hyeshell--parse-zsh-history ()
  (if (file-exists-p "~/.zsh_history")
    (let
      (
        collection
        zsh_history)
      (hyeshell--reload-shell-history)
      (setq collection
        (nreverse
          (split-string
            (with-temp-buffer
              (insert-file-contents (file-truename "~/.zsh_history"))
              (replace-regexp-in-string
                "^:[^;]*;" "" (buffer-string)))
            "\n" t)))
      (when
        (and
          collection
          (> (length collection) 0)
          (setq zsh_history collection))
        zsh_history))
    nil))

(defun hyeshell/get-shell-history ()
  (delete-dups
    (mapcar
      (lambda (str) (string-trim (substring-no-properties str)))
      (append
        (ring-elements eshell-history-ring)
        (hyeshell--parse-bash-history)
        (hyeshell--parse-zsh-history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; temp. don't know if usefull ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add completions for git command.
(when (executable-find "git")
  (defun pcmpl-git-commands ()
    "Return the most common git commands by parsing the git output."
    (with-temp-buffer
      (call-process-shell-command
        "git"
        nil
        (current-buffer)
        nil
        "help"
        "--all")
      (goto-char 0)
      (search-forward "\n\n")
      (let (commands)
        (while
          (re-search-forward
            "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
            nil
            t)
          (push (match-string 1) commands)
          (when (match-string 2)
            (push (match-string 2) commands)))
        (sort commands #'string<))))

  (defconst pcmpl-git-commands
    (pcmpl-git-commands)
    "List of `git' commands.")

  (defvar pcmpl-git-ref-list-cmd
    "git for-each-ref refs/ --format='%(refname)'"
    "The `git' command to run to get a list of refs.")

  (defun pcmpl-git-get-refs (type)
    "Return a list of `git' refs filtered by TYPE."
    (with-temp-buffer
      (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
      (goto-char (point-min))
      (let (refs)
        (while
          (re-search-forward
            (concat "^refs/" type "/\\(.+\\)$")
            nil
            t)
          (push (match-string 1) refs))
        (nreverse refs))))

  (defun pcmpl-git-remotes ()
    "Return a list of remote repositories."
    (split-string (shell-command-to-string "git remote")))

  (defun pcomplete/git ()
    "Completion for `git'."
    ;; Completion for the command argument.
    (pcomplete-here* pcmpl-git-commands)
    (cond
      ((pcomplete-match "help" 1)
        (pcomplete-here* pcmpl-git-commands))
      ((pcomplete-match (regexp-opt '("pull" "push")) 1)
        (pcomplete-here (pcmpl-git-remotes)))
      ;; provide branch completion for the command `checkout'.
      ((pcomplete-match "checkout" 1)
        (pcomplete-here*
          (append
            (pcmpl-git-get-refs "heads")
            (pcmpl-git-get-refs "tags"))))
      (t
        (while (pcomplete-here (pcomplete-entries)))))))

(defface hyeshell-alert-buffer-face
  '((t (:foreground "#ff2d55" :bold t)))
  "Alert buffer face."
  :group 'hyeshell)

(defface hyeshell-alert-command-face
  '((t (:foreground "#ff9500" :bold t)))
  "Alert command face."
  :group 'hyeshell)

(defun hyeshell--process-alert (process status)
  "Send `alert' with severity based on STATUS when PROCESS finished."
  (let*
    (
      (cmd (process-command process))
      (buffer (process-buffer process))
      (msg
        (replace-regexp-in-string
          "\n" " "
          (string-trim
            (format "%s: %s" (mapconcat 'identity cmd " ") status))))
      (buffer-visible
        (member buffer (mapcar #'window-buffer (window-list)))))
    (unless buffer-visible
      (message
        "%s %s"
        (propertize
          (format
            "[Hyeshell Alert] %s"
            (string-remove-prefix "Hyeshell: " (buffer-name buffer)))
          'face 'hyeshell-alert-buffer-face)
        (propertize msg 'face 'hyeshell-alert-command-face)))))

(add-hook 'eshell-kill-hook #'hyeshell--process-alert)

(add-hook 'eshell-mode-hook
  (lambda ()
    (face-remap-add-relative
      'hl-line
      :background (face-background 'default))))

(provide 'hyeshell)
