;;; bluetooth-pa.el --- Bluetooth mode pairing agent code  -*- lexical-binding: t; -*-

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

;; This file contains the pairing agent code for Emacs Bluetooth mode

;;; Code:

(require 'bluetooth-lib)
(require 'bluetooth-device)

(declare-function bluetooth-uuid-parse-service-class-uuid
                  "bluetooth")

(defvar bluetooth-pa--method-objects '() "D-Bus method objects.")

;;;; Bluetooth pairing agent code
;; The release function is not needed at the moment, but needs
;; to be implemented for the agent API.
(defun bluetooth-pa--release ()
  "Clean up after Bluetooth agent release.")

(defmacro bluetooth-pa--with-alias (device &rest body)
  "Evaluate BODY with DEVICE alias bound to ALIAS."
  (declare (indent defun))
  `(let* ((dev-id (cl-first (last (split-string ,device "/"))))
          (dev (bluetooth-device dev-id))
          (alias (if dev
                     (bluetooth-device-property dev "Alias")
                   (replace-regexp-in-string "_" ":" dev-id nil nil nil 4))))
     ,@body))

(defmacro bluetooth-pa--maybe-cancel-reject (&rest body)
  "Invoke BODY and maybe issue cancel and reject errors.
`org.bluez.Error.Canceled' is issued on `keyboard-quit' and
`org.bluez.Error.Rejected' is issued if BODY evaluates to nil."
  (declare (indent defun))
  `(or (condition-case nil
           (progn ,@body)
         (quit (signal 'dbus-error '("org.bluez.Error.Canceled"))))
       (signal 'dbus-error '("org.bluez.Error.Rejected"))))

(defun bluetooth-pa--request-pin-code (device)
  "Request a pin code for DEVICE."
  (bluetooth-pa--maybe-cancel-reject
   (bluetooth-pa--with-alias device
     (let* ((pin (read-from-minibuffer
                  (format "Enter Bluetooth PIN for `%s': " alias)))
            (trimmed-pin (substring pin 0 (min (length pin) 16)))
            (case-fold-search nil))
       (cond ((= 0 (length trimmed-pin))
              (message "PIN has zero length")
              nil)
             ((string-match "[^[:alnum:]]" trimmed-pin)
              (message "PIN contains non-alphanumeric characters")
              nil)
             (t trimmed-pin))))))

(defun bluetooth-pa--display-pin-code (device pincode)
  "Display the PINCODE for DEVICE."
  (bluetooth-pa--with-alias device
    (message "Bluetooth PIN for `%s': %s" alias pincode)
    :ignore))

(defun bluetooth-pa--request-passkey (device)
  "Request passkey for DEVICE."
  (bluetooth-pa--maybe-cancel-reject
   (bluetooth-pa--with-alias device
     (let ((pk (read-from-minibuffer
                (format "Enter Bluetooth Passkey for `%s': (0..999999) "
                        alias))))
       (min (max (string-to-number pk) 0) 999999)))))

(defun bluetooth-pa--display-passkey (device passkey _)
  "Display PASSKEY for DEVICE, ignoring ENTERED (for now)."
  (bluetooth-pa--with-alias device
    (message "Bluetooth Passkey for `%s': %06d" alias passkey)
    :ignore))

(defun bluetooth-pa--request-confirmation (device passkey)
  "Request user confirmation that DEVICE's PASSKEY is correct."
  (bluetooth-pa--maybe-cancel-reject
   (bluetooth-pa--with-alias device
     (y-or-n-p
      (format "Is Bluetooth Passkey %06d for `%s' correct? " passkey alias))))
  :ignore)

(defun bluetooth-pa--request-authorization (device)
  "Authorize Bluetooth DEVICE."
  (bluetooth-pa--maybe-cancel-reject
   (bluetooth-pa--with-alias device
     (y-or-n-p (format "Authorize Bluetooth device `%s'? " alias))))
  :ignore)

(defun bluetooth-pa--authorize-service (device uuid)
  "Authorize Bluetooth service UUID for DEVICE."
  (bluetooth-pa--maybe-cancel-reject
   (bluetooth-pa--with-alias device
     (let ((p-uuid (bluetooth-uuid-parse-service-class-uuid uuid)))
       (y-or-n-p
        (format "Authorize Bluetooth service `%s' for device `%s'? "
                p-uuid alias)))))
  :ignore)

;; This function usually gets called (from D-Bus) while we are
;; in the minibuffer trying to read a passkey or PIN.  Tha call to
;; `keyboard-quit' is used to break out of there.
(defun bluetooth-pa--cancel ()
  "Cancel a pairing process."
  (keyboard-quit)
  (message "Pairing canceled"))

;; This procedure registers the pairing agent.
(defun bluetooth-pa-register-agent ()
  "Register as a pairing agent."
  (let ((methods '("Release" "RequestPinCode" "DisplayPinCode"
                   "RequestPasskey" "DisplayPasskey" "RequestConfirmation"
                   "RequestAuthorization" "AuthorizeService" "Cancel")))
    (setq bluetooth-pa--method-objects
          (cl-loop for method in methods
                   for fname = (bluetooth-lib-make-function-name method "-")
                   collect (dbus-register-method bluetooth-bluez-bus
                                                 dbus-service-emacs
                                                 bluetooth-lib-own-path
                                                 (bluetooth-lib-interface :agent)
                                                 method (intern fname) t)))
    (dbus-register-service :session dbus-service-emacs)
    (dbus-call-method bluetooth-bluez-bus bluetooth-lib-service bluetooth-lib-root
                      (bluetooth-lib-interface :agent-manager)
                      "RegisterAgent"
                      :object-path bluetooth-lib-own-path "KeyboardDisplay")))

(defun bluetooth-pa-unregister-agent ()
  "Unregister agent and clean up."
  (dbus-call-method bluetooth-bluez-bus bluetooth-lib-service bluetooth-lib-root
                    (bluetooth-lib-interface :agent-manager)
                    "UnregisterAgent"
                    :object-path bluetooth-lib-own-path)
  (mapc #'dbus-unregister-object bluetooth-pa--method-objects))

(provide 'bluetooth-pa)
;;; bluetooth-pa.el ends here


;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
