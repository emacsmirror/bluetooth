;;; bluetooth-battery.el --- Bluetooth mode plugin for the battery profile  -*- lexical-binding: t; -*-

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

(require 'bluetooth)
(require 'bluetooth-lib)
(require 'bluetooth-device)
(require 'bluetooth-plugin)

(defgroup bluetooth-battery nil
  "Bluetooth battery plugin."
  :group 'bluetooth)

(defcustom bluetooth-battery-warning-level 40
  "Level below which a low battery warning is issued."
  :type '(natnum))

(defcustom bluetooth-battery-display-warning t
  "Whether a low-battery warning should be displayed."
  :type '(boolean))

(defvar bluetooth-battery--signals nil)

(defun bluetooth-battery--info (device)
  "Insert battery info for DEVICE into buffer at point."
  (let ((level (bluetooth-battery-level device)))
    (when (numberp level)
      (bluetooth-ins-line "Battery level" (number-to-string level)))))

;; TODO test!
(defun bluetooth-battery--make-handler (path)
  "Make a signal handler for object at PATH to indicate battery level.
If the battery level gets low, a message to that effect will be printed."
  (lambda (_interface changed-props &rest _)
    (let* ((device (bluetooth-device (bluetooth-device-id-by-path path)))
           (alias (bluetooth-device-property device "Alias"))
           (level (alist-get "Percentage" changed-props nil nil #'equal)))
      (when (and (numberp level)
                 bluetooth-battery-display-warning
                 (<= level bluetooth-battery-warning-level))
        (delay-warning 'bluetooth-battery
                       (format "Battery level of %s is low (%d %%)"
                               alias level))))))

;; TODO add dev-id or something so it can be cleaned up later
;; dev-id can be obtained from path
(defun bluetooth-battery--new (path)
  "Notify bluetooth-battery about new object at PATH."
  (push (bluetooth-lib-register-props-signal bluetooth-service
                                             path
                                             :battery
                                             (bluetooth-battery--make-handler
                                              path))
        bluetooth-battery--signals))

(defun bluetooth-battery--cleanup ()
  "Clean up the bluetooth battery plugin."
  (mapc #'dbus-unregister-object bluetooth-battery--signals))

(defun bluetooth-battery-level (device)
  "Return the battery level of DEVICE."
  (bluetooth-lib-query-property "Percentage"
                                (bluetooth-device-path device)
                                :battery))

(defun bluetooth-battery-levels ()
  "Return a list of devices by ID, aliases and their battery levels."
  (let (result)
    (bluetooth-device-map (lambda (id dev)
                            (push (list id
                                        (bluetooth-device-property dev "Alias")
                                        (bluetooth-battery-level dev))
                                  result))
                          #'bluetooth-device-implements-p
                          :battery)
    result))

;;;###autoload
(defun bluetooth-battery-init ()
  "Initialize the bluetooth-battery plugin."
  ;; everything must be set up before calling the register function, because
  ;; it will likely call ‘bluetooth-battery--new’ right away
  (bluetooth-plugin-register :battery
                             #'bluetooth-battery--new
                             #'bluetooth-battery--info
                             #'bluetooth-battery--cleanup))

(provide 'bluetooth-battery)
;;; bluetooth-battery.el ends here
