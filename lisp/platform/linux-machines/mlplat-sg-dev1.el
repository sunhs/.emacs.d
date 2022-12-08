(setq-default clang-format-executable "clang-format-8")

(defun unfollow-home-dir-symlink (dir)
  (replace-regexp-in-string "/data1/zhuhaisheng" "~" dir))

(advice-add #'file-truename :filter-return #'unfollow-home-dir-symlink)
