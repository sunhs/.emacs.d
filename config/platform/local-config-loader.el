(cond
 ((string= system-type "gnu/linux")
  (progn
    (load "local-config-linux")))
 ((string= system-type "darwin")
  (progn
    (load "local-config-mac"))))
