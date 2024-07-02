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

(defcustom bluetooth-battery-warning-level 50
  "Level below which a low battery warning is issued."
  :type '(natnum))

(defcustom bluetooth-battery-display-warning t
  "Whether a low-battery warning should be displayed."
  :type '(boolean))

(defun bluetooth-battery--info (device)
  "Insert battery info for DEVICE into buffer at point."
  (let ((level (bluetooth-battery-level device)))
    (when (numberp level)
      (bluetooth-ins-line "Battery level"
                          (format "%d %%" level)))))

(defun bluetooth-battery--prop-change (device _property percentage interface)
  "Hook function to be called when DEVICE's PROPERTY changes."
  (let ((alias (bluetooth-device-property device "Alias")))
    (when (string= interface (bluetooth-lib-interface :battery))
      (when (and (numberp percentage)
                 bluetooth-battery-display-warning
                 (<= percentage bluetooth-battery-warning-level))
        (delay-warning 'bluetooth-battery
                       (format "Battery percentage of %s is low (%d %%)"
                               alias percentage))))))

(defun bluetooth-battery--new (device)
  "Notify bluetooth-battery about new DEVICE."
  (bluetooth-device-add-prop-hook device
                                  "Percentage"
                                  #'bluetooth-battery--prop-change))

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
                             #'bluetooth-battery--info))

(provide 'bluetooth-battery)
;;; bluetooth-battery.el ends here
