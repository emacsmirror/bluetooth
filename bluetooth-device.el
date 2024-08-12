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

;; This module keeps a store of known devices with information about their ID
;; (e. g. dev_AB_CD_EF_GH_IJ_KL), their properties as key/value pairs and a
;; signal handler for paired devices that keeps the properties up to date.
;; The signal handler also calls a callback function whenever properties
;; change.  This is used to keep the tabulated list view current.
;;

;;; Code:

(require 'cl-lib)
(require 'bluetooth-lib)

(declare-function bluetooth-plugin-dev-remove "bluetooth-plugin")
(declare-function bluetooth-plugin-dev-update "bluetooth-plugin")

(defvar bluetooth-device--info nil "Device info obtained from Bluez.")

(cl-defstruct bluetooth-device
  "A Bluetooth device.
This structure holds all the device properties, the per-device signal handler
and the property hooks."
  (id nil :read-only t)
  signal-handler
  properties
  property-functions)

(defun bluetooth-device-property (device property)
  "Return the value of DEVICE's PROPERTY."
  (alist-get property
             (bluetooth-device-properties device)
             nil nil #'string=))

(defun bluetooth-device--property-set (device property value)
  "Set DEVICE's PROPERTY to VALUE."
  (setf (alist-get property (bluetooth-device-properties device)
                   nil nil #'string=)
        value))

(gv-define-simple-setter bluetooth-device-property
                         bluetooth-device--property-set)

(defun bluetooth-device--prop-fns (device property)
  "Return DEVICE's PROPERTY functions."
  (alist-get property (bluetooth-device-property-functions device)
             nil nil #'string=))

(defun bluetooth-device--prop-fns-set (device property value)
  "Set DEVICE's PROPERTY functions."
  (setf (alist-get property (bluetooth-device-property-functions device)
                   nil nil #'string=)
        value))

(gv-define-simple-setter bluetooth-device--prop-fns
                         bluetooth-device--prop-fns-set)

(defun bluetooth-device-add-prop-hook (device property function)
  "Add to DEVICE's PROPERTY hook the function FUNCTION.
The FUNCTION will be called when the specified PROPERTY changes.

The FUNCTION must take the four arguments DEVICE, PROPERTY, VALUE,
and INTERFACE with the following meaning:
DEVICE: the ‘bluetooth-device’ whose property has change,
PROPERTY: the name of the changed property,
VALUE: the new value of this property,
INTERFACE: the D-Bus interface name of the property."
  (let ((fns (bluetooth-device--prop-fns device property)))
    (if fns
        (cl-pushnew function fns :test #'equal)
      (setf fns (list function)))
    (setf (bluetooth-device--prop-fns device property) fns)))

(defun bluetooth-device-remove-prop-hook (device property function)
  "Remove from DEVICE's PROPERTY hook the function FUNCTION."
  (setf (bluetooth-device--prop-fns device property)
        (cl-delete function
                   (bluetooth-device--prop-fns device property)
                   :test #'equal)))

(defun bluetooth-device (dev-id)
  "Return the device struct for DEV-ID."
  (gethash dev-id bluetooth-device--info))

(defun bluetooth-device-path (device)
  "Return the full D-Bus path of DEVICE."
  (when device
    (concat (bluetooth-device-property device "Adapter")
            "/"
            (bluetooth-device-id device))))

(defun bluetooth-device-id-by-path (path)
  "Return the device id embedded in PATH or nil if there isn't one."
  (when (string-match ".*\\(dev\\(_[[:alnum:]]\\{2\\}\\)\\{6\\}\\)"
                      path)
    (match-string 1 path)))

(defun bluetooth-device--call-prop-functions (device property &rest args)
  "Call all of DEVICE's PROPERTY functions with ARGS."
  (when-let ((functions (bluetooth-device--prop-fns device property)))
    (dolist (fn functions)
      (when (fboundp fn)
        (apply fn device property args)))))

(defun bluetooth-device--make-signal-handler (device &optional callback)
  "Make a signal handler for DEVICE, with CALLBACK.
The optional CALLBACK function takes a ‘bluetooth-device’ as
argument; the signal handler will call this function after any
properties have changed.

For each changed property, the signal handler will also call the
DEVICE's property functions.  See ‘bluetooth-device-add-prop-hook’."
  (let ((dev-id (bluetooth-device-id device)))
    (cl-labels ((handler (interface changed-props &rest _)
                  (let ((dev (bluetooth-device dev-id)))
                    (mapc (lambda (prop)
                            (cl-destructuring-bind (key (value)) prop
                              (setf (bluetooth-device-property dev key)
                                    value)
                              (when (or (string= "Connected" key)
                                        (string= "ServicesResolved" key))
                                    (if value
                                        (bluetooth-plugin-dev-update dev)
                                      (bluetooth-plugin-dev-remove dev)))
                              (bluetooth-device--call-prop-functions
                               device key value interface)))
                          changed-props)
                    (when callback (funcall callback dev)))))
      (bluetooth-lib-register-props-signal bluetooth-service
                                           (bluetooth-device-path device)
                                           #'handler))))

(defun bluetooth-device--remove (dev-id)
  "Remove the device with id DEV-ID from the device info.
This also unregisters any signal handlers."
  (let ((device (bluetooth-device dev-id)))
    (when (bluetooth-device-signal-handler device)
      (dbus-unregister-object (bluetooth-device-signal-handler device))
      (bluetooth-plugin-dev-remove device)))
  (remhash dev-id bluetooth-device--info))

(defun bluetooth-device--add (dev-id adapter &optional callback)
  "Add device with DEV-ID on ADAPTER and CALLBACK to device info.
The device signal handler will call the CALLBACK function after
any device properties have changed."
  (let* ((props (bluetooth-lib-query-properties
                 (bluetooth-lib-path adapter dev-id)
                 :device))
         (device (make-bluetooth-device :id dev-id
                                        :signal-handler nil
                                        :properties props
                                        :property-functions nil)))
    (when (bluetooth-device-property device "Paired")
      (setf (bluetooth-device-signal-handler device)
            (bluetooth-device--make-signal-handler device callback))
      (bluetooth-plugin-dev-update device))
    (setf (gethash dev-id bluetooth-device--info) device)))

(defun bluetooth-device--update (device &optional callback)
  "Update device info for DEVICE, maybe installing CALLBACK.
The CALLBACK function is installed only if no signal handler was
installed before and is otherwise ignored."
  (setf (bluetooth-device-properties device)
        (bluetooth-lib-query-properties (bluetooth-device-path device)
                                        :device))
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
                  (bluetooth-device--add dev-id adapter callback))
                (bluetooth-lib-query-devices adapter)))
        (bluetooth-lib-query-adapters)))

(defun bluetooth-device-cleanup ()
  "Clean up the internal device table, removing every known device.
The cleanup will also unregister any installed signal handlers."
  (when (hash-table-p bluetooth-device--info)
    (mapc #'bluetooth-device--remove
          (hash-table-keys bluetooth-device--info))
    (setq bluetooth-device--info nil)))

(defun bluetooth-device--update-devices (adapter &optional callback)
  "Update the device info for ADAPTER, installing CALLBACK.
This function removes devices that have disappeared and adds
devices that have newly connected."
  (let ((queried-devices (bluetooth-lib-query-devices adapter)))
    (mapc (lambda (dev-id)
            (bluetooth-device--remove dev-id))
          (cl-set-difference (hash-table-keys bluetooth-device--info)
                             queried-devices
                             :test #'string=))
    (mapc (lambda (dev-id)
            (if-let (device (bluetooth-device dev-id))
                (bluetooth-device--update device callback)
              (bluetooth-device--add dev-id adapter callback)))
          queried-devices)))

(defun bluetooth-device-update-all (&optional callback)
  "Update the device info for all adapters, installing CALLBACK."
  (mapc (lambda (adapter)
          (bluetooth-device--update-devices adapter callback))
        (bluetooth-lib-query-adapters)))

(defun bluetooth-device-implements-p (device api)
  "Indicate whether DEVICE offers API.
Returns the corresponding D-Bus interface name or nil, if not
implemented by device."
  (when-let ((res (member (bluetooth-lib-interface api)
                          (dbus-introspect-get-interface-names bluetooth-bluez-bus
                                                               bluetooth-service
                                                               (bluetooth-device-path
                                                                device)))))
    (car res)))

(defun bluetooth-device-map (fn &optional filter-fn &rest args)
  "Map FN over all devices specified by FILTER-FN, passing ARGS.
The function FN takes the device id and the corresponding
‘bluetooth-device’ as arguments.

The argument FILTER-FN, when supplied, must be a predicate that
takes a ‘bluetooth-device’ as its first argument and ARGS as the
rest."
  (maphash (lambda (id dev)
             (if filter-fn
                 (when (apply filter-fn dev args)
                   (funcall fn id dev))
               (funcall fn id dev)))
           bluetooth-device--info))

(provide 'bluetooth-device)
;;; bluetooth-device.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
