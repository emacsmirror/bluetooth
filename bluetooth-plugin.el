;;; bluetooth-plugin.el --- Plugin API for bluetooth mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Raffael Stocker

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Keywords: hardware

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'bluetooth-lib)
(require 'bluetooth-device)

(defvar bluetooth-plugin--objects nil)

(defun bluetooth-plugin-register (api new-fn &optional path insert-fn _transient)
  "Register a plugin for object on PATH for API.
If PATH is nil, NEW-FN will be called for every object
implementing API, either when the device is already connected, or
when a suitable new device connects.  Otherwise it is called only
once for the object on PATH.

The optional function INSERT-FN is called when device information
is printed from the device view.  It should call
‘bluetooth-ins-line’ to insert its information.

The optional TRANSIENT can be provided as a device menu to
interact with the device.

The passed functions may be called before this function returns,
so the plugin should be set up and ready to go when this function
is called."
  (unless (hash-table-p bluetooth-plugin--objects)
    (setf bluetooth-plugin--objects (make-hash-table :test #'eq)))
  (if (gethash api bluetooth-plugin--objects)
      (error "A bluetooth plugin is already registered for interface %s"
             (bluetooth-lib-interface api))
    (setf (plist-get (gethash api bluetooth-plugin--objects) :new-fn)
          new-fn)
    (setf (plist-get (gethash api bluetooth-plugin--objects) :insert-fn)
          insert-fn)
    (cl-flet ((install (path)
                (let ((dev-id (bluetooth-device-id-by-path path)))
                  (push dev-id
                        (plist-get (gethash api
                                            bluetooth-plugin--objects)
                                   :dev-ids))
                  (when insert-fn
                    (cl-pushnew insert-fn
                                (bluetooth-device-insert-fn
                                 (bluetooth-device dev-id)))))))
      (if (stringp path)
          (install path)
        (when-let (paths (mapcar #'car
                                 (bluetooth-lib-match (bluetooth-lib-get-objects bluetooth-root)
                                                      (bluetooth-lib-interface api))))
          (mapc #'install paths)))))
  nil)

(defun bluetooth-plugin-unregister (api)
  "Unregister the plugin for API."
  (dolist (id (plist-get (gethash api bluetooth-plugin--objects) :dev-ids))
    (let ((device (bluetooth-device id)))
      (setf  (bluetooth-device-insert-fn device)
             (cl-remove (plist-get (gethash api bluetooth-plugin--objects)
                                   :insert-fn)
                        (bluetooth-device-insert-fn device)))))
  (remhash api bluetooth-plugin--objects))

(provide 'bluetooth-plugin)
;;; bluetooth-plugin.el ends here
