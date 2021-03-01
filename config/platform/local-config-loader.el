(cond
 ((string= system-type "gnu/linux")
  (progn
    (load "local-config-ubuntu")))
 ((string= system-type "darwin")
  (progn
    (load "local-config-mac"))))
