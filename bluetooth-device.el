;;; bluetooth-device.el --- Bluetooth mode device handling functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

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

(require 'cl-lib)
(require 'bluetooth-lib)

(defvar bluetooth-device--info nil "Device info obtained from Bluez.")

(cl-defstruct bluetooth-device
  "A bluetooth device.
This structure holds all the device properties."
  (id nil :read-only t)
  signal-handler
  properties)

(defun bluetooth-device-property (device property)
  "Return DEVICE's property PROPERTY."
  (alist-get property
             (bluetooth-device-properties device)
             nil nil #'equal))

(defun bluetooth-device--property-set (device property value)
  "Set DEVICE's PROPERTY to VALUE."
  (setf (alist-get property (bluetooth-device-properties device)
                   nil nil #'equal)
        value))

(gv-define-simple-setter bluetooth-device-property
                         bluetooth-device--property-set)

(defun bluetooth-device (device-id)
  "Return the device struct for DEVICE-ID."
  (gethash device-id bluetooth-device--info))

;; TODO use hooks so multiple callbacks can be used
(defun bluetooth-device--make-signal-handler (device &optional callback)
  "Make a signal handler for DEVICE, with CALLBACK.
The optional callback function takes a ‘bluetooth-device’ as
argument and is called after the device properties have been
updated."
  (let ((adapter (bluetooth-device-property device "Adapter"))
        (dev-id (bluetooth-device-id device)))
    (cl-flet ((handler
                (_interface changed-props invalidated-props)
                (let ((device (bluetooth-device dev-id)))
                  (mapc (lambda (prop)
                          (cl-destructuring-bind (key (value)) prop
                            (setf (bluetooth-device-property device key)
                                  value)))
                        (append changed-props invalidated-props))
                  (when callback (funcall callback device)))))
      (bluetooth-lib-register-props-signal bluetooth-lib-service
                                           (concat adapter "/" dev-id)
                                           :device
                                           #'handler))))

(defun bluetooth-device--create (adapter dev-id)
  "Create a bluetooth device struct for DEV-ID on ADAPTER."
  (let ((props (dbus-get-all-properties bluetooth-bluez-bus
                                        bluetooth-lib-service
                                        (bluetooth-lib-path adapter dev-id)
                                        (bluetooth-lib-interface :device))))
    (make-bluetooth-device :id dev-id
                           :signal-handler nil
                           :properties props)))

(defun bluetooth-device--remove (dev-id)
  "Remove the device with id DEV-ID from the device info.
This also unregisters any signal handlers."
  (let ((device (bluetooth-device dev-id)))
    (when (bluetooth-device-signal-handler device)
      (dbus-unregister-object (bluetooth-device-signal-handler device))
      (setf (bluetooth-device-signal-handler device) nil)))
  (remhash dev-id bluetooth-device--info))

(defun bluetooth-device--add (dev-id device &optional callback)
  "Add bluetooth DEVICE with id DEV-ID and CALLBACK to device info.
The CALLBACK function is called from the properties signal
handler after device properties have changed."
  (when (bluetooth-device-property device "Paired")
    (setf (bluetooth-device-signal-handler device)
          (bluetooth-device--make-signal-handler device callback)))
  (puthash dev-id device bluetooth-device--info))

;; TODO use hooks so multiple callbacks can be used
(defun bluetooth-device--update (dev-id device &optional callback)
  "Update device info for id DEV-ID with data in DEVICE and CALLBACK.
The CALLBACK function is installed only if no signal handler was
installed before and is otherwise ignored."
  (setf (bluetooth-device-properties (bluetooth-device dev-id))
        (bluetooth-device-properties device))
  (when (and (bluetooth-device-property device "Paired")
             (null (bluetooth-device-signal-handler device)))
    (setf (bluetooth-device-signal-handler device)
          (bluetooth-device--make-signal-handler device callback))))

(defun bluetooth-device-init (&optional callback)
  "Initialize device handling, installing CALLBACK for every device.
The CALLBACK function is called after device properties change.
Call this function only once, usually at mode initialization."
  (setq bluetooth-device--info (make-hash-table :test #'equal))
  (mapc (lambda (adapter)
          (mapc (lambda (dev-id)
                  (bluetooth-device--add dev-id
                                         (bluetooth-device--create adapter
                                                                   dev-id)
                                         callback))
                (bluetooth-lib-query-devices adapter)))
        (bluetooth-lib-query-adapters)))

(defun bluetooth-device-cleanup ()
  "Cleanup the internal device table, removing every known device.
The cleanup will also unregister any installed signal handlers."
  (mapc #'bluetooth-device--remove
        (hash-table-keys bluetooth-device--info))
  (setq bluetooth-device--info nil))

(defun bluetooth-device--update-info (adapter &optional callback)
  "Update the device info for ADAPTER, installing CALLBACK."
  (let ((queried-devices (bluetooth-lib-query-devices adapter)))
    (mapc (lambda (dev-id)
            (bluetooth-device--remove dev-id))
          (cl-set-difference (hash-table-keys bluetooth-device--info)
                             queried-devices))
    (mapc (lambda (dev-id)
            (if-let (device (bluetooth-device dev-id))
                (bluetooth-device--update dev-id device callback)
              (bluetooth-device--add dev-id
                                     (bluetooth-device--create adapter dev-id)
                                     callback)))
          queried-devices)))

(defun bluetooth-device-update-all (&optional callback)
  "Update the device info for all adapters, installing CALLBACK."
  (mapc (lambda (adapter)
          (bluetooth-device--update-info adapter callback))
        (bluetooth-lib-query-adapters)))

(defun bluetooth-device-map (fn)
  "Map FN over all devices.
The function takes the device id and a ‘bluetooth-device’ as
arguments."
  (maphash fn bluetooth-device--info))


(provide 'bluetooth-device)
;;; bluetooth-device.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
