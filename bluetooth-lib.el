;;; bluetooth-lib.el --- Library functions for Emacs Bluetooth mode  -*- lexical-binding: t; -*-

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

(require 'dbus)

(declare-function bluetooth-device "bluetooth-device" (device-id))
(declare-function bluetooth-device-property "bluetooth-device" (device property))

(defcustom bluetooth-bluez-bus :system
  "D-Bus bus that Bluez is registered on.
This is usually `:system' if bluetoothd runs as a system service, or
`:session' if it runs as a user service."
  :type '(symbol)
  :group 'bluetooth)

(defcustom bluetooth-timeout 5000
  "Default timeout for Bluez D-Bus access."
  :type '(natnum)
  :group 'bluetooth)

;; Bluez service name as defined by the Bluez API
(defconst bluetooth-lib-service "org.bluez" "D-Bus service name of Bluez.")

;; Bluez root path as defined by the Bluez API
(defconst bluetooth-lib-root "/org/bluez" "D-Bus path root for Bluez.")

;; our path name for the pairing agent
(defconst bluetooth-lib-own-path (concat dbus-path-emacs "/bluetooth")
  "D-Bus object path for the pairing agent.")

(eval-and-compile
  (defun bluetooth-lib-make-function-name (name &optional prefix)
    "Make a function name out of NAME and PREFIX.
The generated function name has the form ‘bluetoothPREFIX-NAME’."
    (let ((case-fold-search nil))
      (concat "bluetooth"
              prefix
              (replace-regexp-in-string "[[:upper:]][[:lower:]]+"
                                        (lambda (x) (concat "-" (downcase x)))
                                        name t)))))

(defvar bluetooth-lib--interfaces
  '((:device . "org.bluez.Device1")
    (:adapter . "org.bluez.Adapter1")
    (:agent-manager . "org.bluez.AgentManager1")
    (:agent . "org.bluez.Agent1")
    (:properties . "org.freedesktop.DBus.Properties"))
  "Bluez D-Bus interfaces.")

(defun bluetooth-lib-interface (api)
  "Return Bluez interface name for SERVICE."
  (alist-get api bluetooth-lib--interfaces))

(defun bluetooth-lib--add-interface (api service-name)
  (cl-pushnew (cons api service-name) bluetooth-lib--interfaces))

(defun bluetooth-lib-path (&rest nodes)
  "Construct path from NODES, prepended by BLUETOOTH--ROOT."
  (mapconcat #'identity (cons bluetooth-lib-root nodes) "/"))

(defun bluetooth-lib-query-adapters ()
  "Return a list of bluetooth adapters."
  (dbus-introspect-get-node-names
   bluetooth-bluez-bus bluetooth-lib-service bluetooth-lib-root))

(defun bluetooth-lib-query-devices (adapter)
  "Return a list of bluetooth devices connected to ADAPTER."
  (dbus-introspect-get-node-names bluetooth-bluez-bus bluetooth-lib-service
                                  (bluetooth-lib-path adapter)))

(defun bluetooth-lib-adapter-properties (adapter)
  "Return the properties of bluetooth ADAPTER.
This function evaluates to an alist of attribute/value pairs."
  (dbus-get-all-properties bluetooth-bluez-bus bluetooth-lib-service
                           (bluetooth-lib-path adapter)
                           (bluetooth-lib-interface :adapter)))

(defun bluetooth-lib-adapter-property (adapter property)
  "Return the value of ADAPTER's PROPERTY."
  (cl-rest (assoc property (bluetooth-lib-adapter-properties adapter))))


(defun bluetooth-lib-call-method (dev-id api function &rest args)
  "For DEV-ID, invoke D-Bus FUNCTION on API, passing ARGS."
  (let ((path (cond ((and (eq :device api)
                          (not (null dev-id)))
                     (concat (bluetooth-device-property
                              (bluetooth-device dev-id)
                              "Adapter")
                             "/" dev-id))
                    ((eq :adapter api)
                     (bluetooth-lib-path (cl-first (bluetooth-lib-query-adapters))))
                    (t nil)))
        (interface (bluetooth-lib-interface api)))
    (when path
      (apply function bluetooth-bluez-bus bluetooth-lib-service path interface
             (mapcar (lambda (x)
                       (if (eq x :path-devid)
                           (concat path "/" dev-id)
                         x))
                     args)))))

(defun bluetooth-lib-dbus-method (dev-id method api &rest args)
  "Invoke METHOD on D-Bus API with ARGS."
  (apply #'bluetooth-lib-call-method dev-id api
         #'dbus-call-method-asynchronously method
         nil :timeout bluetooth-timeout args))

(defun bluetooth-lib-dbus-toggle (dev-id property api)
  "Toggle boolean PROPERTY on D-Bus API."
  (let ((value (bluetooth-lib-call-method dev-id api
                                          #'dbus-get-property property)))
    (bluetooth-lib-call-method dev-id api #'dbus-set-property property
                               (not value))))

(defun bluetooth-lib-dbus-set (dev-id property arg api)
  "Set PROPERTY to ARG on D-Bus API."
  (bluetooth-lib-call-method dev-id api #'dbus-set-property property arg))

(defun bluetooth-lib-register-props-signal (service path api handler-fn)
  "Register signal handler for property changes."
  (dbus-register-signal bluetooth-bluez-bus
                        service
                        path
                        (bluetooth-lib-interface :properties)
                        "PropertiesChanged"
                        handler-fn
                        :arg-namespace
                        (bluetooth-lib-interface api)))

(provide 'bluetooth-lib)
;;; bluetooth-lib.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
