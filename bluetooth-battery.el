;;; bluetooth-battery.el --- Bluetooth mode plugin for the battery profile  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Keywords: hardware
;; URL: https://codeberg.org/rstocker/emacs-bluetooth

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

;; The ‘bluetooth-battery’ package implements a simple plugin for the
;; org.bluez.battery1 interface.  It also serves as an example for how to
;; implement plugins.
;;
;; This plugin shows a device's battery level in the device information view
;; of Bluetooth mode, if the device implements the battery interface.  It also
;; warns about low battery levels if ‘bluetooth-battery-display-warning’ is t.
;; You can customize the warning level with ‘bluetooth-battery-warning-level’.
;; This is an inclusive level, so the warning will be displayed if the battery
;; percentage is at this level or lower.

;;; Code:

(require 'bluetooth)
(require 'bluetooth-lib)
(require 'bluetooth-device)
(require 'bluetooth-plugin)
(require 'transient)

(defgroup bluetooth-battery nil
  "Bluetooth battery plugin."
  :group 'bluetooth)

(defcustom bluetooth-battery-warning-level 50
  "Level below which a low battery warning is issued."
  :type '(natnum))

(defcustom bluetooth-battery-display-warning t
  "Whether a low-battery warning should be displayed."
  :type '(boolean))

(defcustom bluetooth-battery-menu-key "b"
  "Key to use for the battery plugin sub-menu."
  :type '(string))

(defvar bluetooth-battery--devices nil)

(defvar bluetooth-battery--loaded nil)

(defun bluetooth-battery--known-dev-p ()
  (and bluetooth-battery--loaded
       (cl-member (bluetooth-devatpt) bluetooth-battery--devices)))

;; TODO This doesn't do much yet.  Any ideas for menu entries?
(transient-define-prefix bluetooth-battery--menu ()
  "The Bluetooth battery plugin menu."
  ["Battery menu"
   (:info (lambda ()
            (let ((dev (bluetooth-devatpt)))
              (format "Battery percentage of %s: %d%%"
                      (bluetooth-device-property dev "Alias")
                      (bluetooth-battery-percentage dev)))))])

(defun bluetooth-battery--info (device)
  "Insert battery info for DEVICE into buffer at point."
  ;; The info function is called by Bluetooth mode when it displays the device
  ;; information.  It must call ‘bluetooth-ins-line’ to insert a line in the
  ;; device information display.  You should call this function for every
  ;; piece of information to be displayed.
  (let ((percentage (bluetooth-battery-percentage device)))
    (when (numberp percentage)
      (bluetooth-ins-line "Battery percentage"
                          (format "%d%%" percentage)))))

(defun bluetooth-battery--prop-change (device _property percentage interface)
  "Hook function to be called when DEVICE's PROPERTY changes."
  ;; A hook function can be registered for every property of the plugin's
  ;; interface.  This is usually done in ‘bluetooth-battery--new’.  The
  ;; parameters of this function are always the device, the property, its
  ;; value and the interface name.
  (let ((alias (bluetooth-device-property device "Alias")))
    (when (string= interface (bluetooth-lib-interface :battery))
      (when (and (numberp percentage)
                 bluetooth-battery-display-warning
                 (<= percentage bluetooth-battery-warning-level))
        (delay-warning 'bluetooth-battery
                       (format "Battery percentage of %s is low (%d%%)"
                               alias percentage))))))

(defun bluetooth-battery--new (device)
  "Notify bluetooth-battery about new DEVICE."
  (bluetooth-device-add-prop-hook device
                                  "Percentage"
                                  #'bluetooth-battery--prop-change)
  (cl-pushnew device bluetooth-battery--devices))

(defun bluetooth-battery--remove (device)
  "Stop providing battery info for DEVICE."
  (setf bluetooth-battery--devices
        (cl-remove device bluetooth-battery--devices)))

(defun bluetooth-battery--cleanup ()
  "Cleanup the battery plugin."
  (setq bluetooth-battery--loaded nil))

(defun bluetooth-battery-percentage (device)
  "Return the battery level of DEVICE."
  (bluetooth-lib-query-property "Percentage"
                                (bluetooth-device-path device)
                                :battery))

(defun bluetooth-battery-percentages ()
  "Return a list of devices by ID, aliases and their battery levels."
  (let (result)
    (bluetooth-device-map (lambda (id dev)
                            (push (list id
                                        (bluetooth-device-property dev "Alias")
                                        (bluetooth-battery-percentage dev))
                                  result))
                          #'bluetooth-device-implements-p
                          :battery)
    result))

;;;###autoload
(defun bluetooth-battery-init (menu)
  "Initialize the bluetooth-battery plugin.
The Bluetooth mode menu must be passed in MENU.

This plugin shows a device's battery level in the device
information view of Bluetooth mode, if the device implements the
battery interface.  It also warns about low battery levels if
‘bluetooth-battery-display-warning’ is t.  You can customize the
warning level with ‘bluetooth-battery-warning-level’.  This is an
inclusive level, so the warning will be displayed if the battery
percentage is at this level or lower."
  ;; Everything must be set up before calling the register function, because
  ;; it will likely call ‘bluetooth-battery--new’ right away.
  (setf bluetooth-battery--loaded
        (bluetooth-plugin-register :battery
                                   #'bluetooth-battery--new
                                   #'bluetooth-battery--info
                                   #'bluetooth-battery--cleanup
                                   #'bluetooth-battery--remove))
  (transient-append-suffix menu '(0 0 -1)
    (list bluetooth-battery-menu-key
          "Battery menu"
          'bluetooth-battery--menu
          :if 'bluetooth-battery--known-dev-p)))

(provide 'bluetooth-battery)
;;; bluetooth-battery.el ends here
