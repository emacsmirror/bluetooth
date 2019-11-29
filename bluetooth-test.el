(package-initialize)
(byte-compile-file "bluetooth.el" t)
(bluetooth-list-devices)
