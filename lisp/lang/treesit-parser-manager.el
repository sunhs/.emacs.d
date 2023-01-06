;; Modified from:
;; URL: https://codeberg.org/ckruse/treesit-parser-manager
;; Author: Christian Kruse <christian@kruse.cool>
;; Modified according to:
;; https://github.com/casouri/tree-sitter-module/blob/master/build.sh

(defcustom treesit-parser-manager-working-directory
  (expand-file-name "tree-sit-repos" user-emacs-directory)
  "Directory where tree-sit-parser-manager will clone repositories."
  :type 'directory
  :group 'treesit-parser-manager)

(defcustom treesit-parser-manager-clone-command "git clone --depth 1"
  "Command used to clone repositories."
  :type 'string
  :group 'treesit-parser-manager)

(defcustom treesit-parser-manager-update-command "git pull"
  "Command used to update repositories."
  :type 'string
  :group 'treesit-parser-manager)

(defcustom treesit-parser-manager-target-directory
  (expand-file-name "tree-sit-parsers" user-emacs-directory)
  "Directory where tree-sit-parser-manager will install parsers."
  :type 'directory
  :group 'treesit-parser-manager)

(defcustom treesit-parser-manager-grammars '()
  "List of grammars to install."
  :type
  '
  (repeat
    (list
      :tag
      "Grammar"
      (string :tag "Repository")
      (repeat :tag "Grammars" (string :tag "Grammar"))))
  :group 'treesit-parser-manager)

(defcustom treesit-parser-manager-c-compiler "cc"
  "C Compiler to build the grammars"
  :type 'string
  :group 'treesit-parser-manager)

(defcustom treesit-parser-manager-c++-compiler "c++"
  "C++ Compiler to build the grammars"
  :type 'string
  :group 'treesit-parser-manager)

(defun treesit-parser-manager--clone (repo)
  "Clone REPO."
  (make-directory treesit-parser-manager-working-directory 'parents)
  (let ((default-directory treesit-parser-manager-working-directory))
    (shell-command
      (format "%s %s" treesit-parser-manager-clone-command repo))))

(defun treesit-parser-manager--update (repo)
  "Update REPO."
  (let
    (
      (default-directory
        (expand-file-name
          repo
          treesit-parser-manager-working-directory)))
    (shell-command treesit-parser-manager-update-command)))

;;;###autoload
(defun treesit-parser-manager-install-grammars ()
  "Install tree-sitter grammars."
  (interactive)
  (dolist (gramar treesit-parser-manager-grammars)
    (let
      (
        (url (car gramar))
        (targets (nth 1 gramar)))
      (dolist (target targets)
        (let
          (
            (target-dir
              (expand-file-name
                target
                treesit-parser-manager-working-directory)))
          (unless (file-exists-p target-dir)
            (message "Installing %s (%s)" url target)
            (treesit-parser-manager--clone url)
            (treesitter-grammar-manager--compile-grammar
              treesit-parser-manager-target-directory
              target-dir)))))))

;;;###autoload
(defun treesit-parser-manager-update-grammars ()
  "Update tree-sitter grammars."
  (interactive)
  (dolist (gramar treesit-parser-manager-grammars)
    (let
      (
        (url (car gramar))
        (targets (nth 1 gramar)))
      (dolist (target targets)
        (let
          (
            (target-dir
              (expand-file-name
                target
                treesit-parser-manager-working-directory)))
          (when (file-exists-p target-dir)
            (message "Updating %s (%s)" url target)
            (treesit-parser-manager--update target-dir)
            (treesitter-grammar-manager--compile-grammar
              treesit-parser-manager-target-directory
              target-dir)))))))

;;;###autoload
(defun treesit-parser-manager-install-or-update-grammars ()
  "Compile or update tree-sitter grammars."
  (interactive)
  (dolist (gramar treesit-parser-manager-grammars)
    (let
      (
        (url (car gramar))
        (targets (nth 1 gramar)))
      (dolist (target targets)
        (let
          (
            (target-dir
              (expand-file-name
                target
                treesit-parser-manager-working-directory)))
          (if (file-exists-p target-dir)
            (progn
              (message "Updating %s (%s)" url target)
              (treesit-parser-manager--update target-dir))
            (progn
              (message "Installing %s (%s)" url target)
              (treesit-parser-manager--clone url)))

          (treesitter-grammar-manager--compile-grammar
            treesit-parser-manager-target-directory
            target-dir))))))

(defun treesitter-grammar-manager--compile-grammar
  (destination &optional path)
  "Compile grammar at PATH, and place the resulting shared library in DESTINATION."
  (make-directory destination 'parents)

  (let*
    (
      (default-directory
        (expand-file-name "src/" (or path default-directory)))
      (parser-name
        (thread-last
          (expand-file-name "grammar.json" default-directory)
          (json-read-file)
          (alist-get 'name)))
      (has-scanner
        (if (file-exists-p "scanner.cc")
          t
          nil))
      (compile-parser-command-args '("-fPIC" "-c" "-I." "parser.c"))
      (compile-scanner-command-args
        '("-fPIC" "-c" "-I." "scanner.cc"))
      (link-compiler
        (if has-scanner
          treesit-parser-manager-c++-compiler
          treesit-parser-manager-c-compiler))
      (objects
        (if has-scanner
          '("parser.o" "scanner.o")
          '("parser.o")))
      (output-path
        (expand-file-name
          (format
            "libtree-sitter-%s%s"
            parser-name
            module-file-suffix)
          destination))
      (link-command-args
        `("-fPIC" "-shared" ,@objects "-o" ,output-path)))
    (message "Compiling grammar at %s" path)

    (with-temp-buffer
      ;; compile parser
      (unless
        (zerop
          (apply #'call-process
            treesit-parser-manager-c-compiler
            nil
            t
            nil
            compile-parser-command-args))
        (user-error
          "%s\n\nFailed to compile parser with args:\n%s"
          (buffer-string)
          (string-join compile-parser-command-args " ")))
      (message "Done compiling parser for %s" parser-name)

      ;; compile scanner, if any
      (when has-scanner
        (unless
          (zerop
            (apply #'call-process
              treesit-parser-manager-c++-compiler
              nil
              t
              nil
              compile-scanner-command-args))
          (user-error
            "%s\n\nFailed to compile scanner with args:\n%s"
            (buffer-string)
            (string-join compile-scanner-command-args " ")))
        (message "Done compiling scanner for %s" parser-name))

      ;; link
      (unless
        (zerop
          (apply #'call-process
            link-compiler
            nil
            t
            nil
            link-command-args))
        (user-error
          "%s\n\nFailed to link with args:\n%s"
          (buffer-string)
          (string-join link-command-args " "))))
    (message "Done linking for %s" parser-name)))

;;;###autoload
(defun treesit-parser-manager-remove-grammar (&optional repo)
  "Remove REPO and TARGETS."
  (interactive)

  (let*
    (
      (repo
        (or
          repo
          (completing-read
            "Repository: "
            treesit-parser-manager-grammars)))
      (targets (nth 1 (assoc repo treesit-parser-manager-grammars))))
    (dolist (target targets)
      (let*
        (
          (target-dir
            (expand-file-name
              target
              treesit-parser-manager-working-directory))
          (default-directory
            (expand-file-name
              "src/"
              (or target-dir default-directory)))
          (parser-name
            (thread-last
              (expand-file-name "grammar.json" default-directory)
              (json-read-file)
              (alist-get 'name)))
          (lib-file
            (expand-file-name
              (format
                "libtree-sitter-%s%s"
                parser-name
                module-file-suffix)
              treesit-parser-manager-target-directory)))
        (when (file-exists-p target-dir)
          (delete-directory target-dir 'recursive)
          (message "Removed %s" target-dir))

        (when (file-exists-p lib-file)
          (delete-file lib-file)
          (message "Removed %s" lib-file))))

    (setq treesit-parser-manager-grammars
      (delete (cons repo targets)
        treesit-parser-manager-grammars))))

(provide 'treesit-parser-manager)
;;; treesit-parser-manager.el ends here
