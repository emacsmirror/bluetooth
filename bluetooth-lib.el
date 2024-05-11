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
(defcustom bluetooth-service "org.bluez"
  "D-Bus service name of Bluez."
  :type '(string)
  :group 'bluetooth)

;; our path name for the pairing agent
(defconst bluetooth-lib-own-path (concat dbus-path-emacs "/bluetooth")
  "D-Bus object path for the pairing agent.")

;; Bluez root path as defined by the Bluez API
(defcustom bluetooth-root "/org/bluez"
  "D-Bus path root for Bluez."
  :type '(string)
  :group 'bluetooth)

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

(defvar bluetooth-lib-interfaces
  '((:device . "org.bluez.Device1")
    (:adapter . "org.bluez.Adapter1")
    (:agent-manager . "org.bluez.AgentManager1")
    (:agent . "org.bluez.Agent1")
    (:properties . "org.freedesktop.DBus.Properties"))
  "Bluez D-Bus interfaces.")

(defun bluetooth-lib-interface (api)
  "Return Bluez interface name for API."
  (alist-get api bluetooth-lib-interfaces))

(defun bluetooth-lib--add-interface (interface api)
  "Add new INTERFACE using API (a keyword) as its Lisp name."
  (cl-pushnew (cons api interface) bluetooth-lib-interfaces))

(defun bluetooth-lib-path (&rest nodes)
  "Construct path from NODES, prepended by BLUETOOTH--ROOT."
  (mapconcat #'identity (cons bluetooth-root nodes) "/"))

(defun bluetooth-lib-query-adapters ()
  "Return a list of bluetooth adapters."
  (dbus-introspect-get-node-names
   bluetooth-bluez-bus bluetooth-service bluetooth-root))

(defun bluetooth-lib-query-devices (adapter)
  "Return a list of bluetooth devices connected to ADAPTER."
  (dbus-introspect-get-node-names bluetooth-bluez-bus bluetooth-service
                                  (bluetooth-lib-path adapter)))

(defun bluetooth-lib-query-properties (path api)
  "Return all properties of SERVICE on PATH using API."
  (dbus-get-all-properties bluetooth-bluez-bus bluetooth-service
                           path
                           (bluetooth-lib-interface api)))

(defun bluetooth-lib-adapter-properties (adapter)
  "Return the properties of bluetooth ADAPTER.
This function evaluates to an alist of attribute/value pairs."
  (bluetooth-lib-query-properties (bluetooth-lib-path adapter) :adapter))

(defun bluetooth-lib-adapter-property (adapter property)
  "Return the value of ADAPTER's PROPERTY."
  (cl-rest (assoc property (bluetooth-lib-adapter-properties adapter))))

(defun bluetooth-lib-call-method (path api method &rest args)
  "For DEV-ID, invoke D-Bus FUNCTION on API, passing ARGS."
  (let ((interface (bluetooth-lib-interface api)))
    (when path
      (apply method bluetooth-bluez-bus bluetooth-service path interface args))))

(defun bluetooth-lib-dbus-method (path method api &rest args)
  "Invoke METHOD on D-Bus API with ARGS for device given by DEV-ID.
The method is called asynchronously with the timeout specified by
‘bluetooth-timeout’."
  (apply #'bluetooth-lib-call-method path api
         #'dbus-call-method-asynchronously method
         nil :timeout bluetooth-timeout args))

(defun bluetooth-lib-dbus-toggle (path property api)
  "Toggle boolean PROPERTY on D-Bus API for device given by DEV-ID."
  (let ((value (bluetooth-lib-call-method path api
                                          #'dbus-get-property property)))
    (bluetooth-lib-call-method path api #'dbus-set-property property
                               (not value))))

(defun bluetooth-lib-dbus-set (path property arg api)
  "Set PROPERTY to ARG on D-Bus API for device given by DEV-ID."
  (bluetooth-lib-call-method path api #'dbus-set-property property arg))

(defun bluetooth-lib-register-props-signal (service path api handler-fn)
  "Register signal handler to be notified when properties change.
The argument SERVICE specifies the bluetooth service,
e.g. ‘bluetooth-service’, or is nil for adapter signals.  The
argument PATH specifies the path to the D-Bus object holding the
property.  API specifies the api interface to use, see
‘bluetooth-lib-interfaces’.  HANDLER-FN is signal handler function taking the
arguments described in ‘dbus-unregister-object’."
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
