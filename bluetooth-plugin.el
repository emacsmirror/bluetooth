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

(require 'cl-lib)
(require 'bluetooth-lib)
(require 'bluetooth-device)

(declare-function bluetooth-battery-init "bluetooth-battery")

(defgroup bluetooth-plugin nil
  "Bluetooth plugins."
  :group 'bluetooth)

(defcustom bluetooth-plugin-autoload (list #'bluetooth-battery-init)
  "List of init functions of auto-loaded plugins.
The init functions are called when the bluetooth plugin interface
is initialized."
  :type '(repeat function))

(defvar bluetooth-plugin--objects nil
  "The hash table of registered bluetooth plugins.  The keys are API
names.  Each value of this hash table is a plist with the
following keys and values:

- new-fn: the function to call when a new device offering the api
  for which the plugin registered connects,
- info-fn: a function called from ‘bluetooth-show-device-info’
  to show device information,
- dev-ids: a list of device IDs that the plugin handles.")

(defun bluetooth-plugin-register (api new-fn &optional info-fn
                                      cleanup-fn remove-fn _transient)
  "Register a plugin for API.
NEW-FN will be called for every object implementing API, either
when the device is already connected, or when a suitable new
device connects.

The optional function INFO-FN is called when device information
is printed from the device view.  It takes a ‘bluetooth-device’
as argument and should call ‘bluetooth-ins-line’ to insert its
information.

The optional function REMOVE-FN is called when a device is
removed (unpaired) or disconnects.  It takes a ‘bluetooth-device’
as argument.

The optional TRANSIENT can be provided as a device menu to
interact with the device.

The passed functions may be called before this function returns,
so the plugin should be set up and ready to go when this function
is called."
  (unless (hash-table-p bluetooth-plugin--objects)
    (setf bluetooth-plugin--objects (make-hash-table :test #'eq)))
  (if (gethash api bluetooth-plugin--objects)
      (message "A bluetooth plugin is already registered for interface %s"
               (bluetooth-lib-interface api))
    (cl-mapc (lambda (key fn)
               (when fn
                 (setf (plist-get (gethash api bluetooth-plugin--objects) key)
                       fn)))
             (list :new-fn :info-fn :cleanup-fn :remove-fn)
             (list new-fn info-fn cleanup-fn remove-fn))
    ;; TODO add transient
    )
  nil)

(defun bluetooth-plugin-unregister (api)
  "Unregister the plugin for API."
  (when-let (entry (and (hash-table-p bluetooth-plugin--objects)
                        (gethash api bluetooth-plugin--objects)))
    (when-let ((cleanup-fn (plist-get entry :cleanup-fn)))
      (funcall cleanup-fn))
    (remhash api bluetooth-plugin--objects)))

(defun bluetooth-plugin-unregister-all ()
  "Unregister all plugins."
  (when (hash-table-p bluetooth-plugin--objects)
    (maphash (lambda (key _value)
               (bluetooth-plugin-unregister key))
             bluetooth-plugin--objects)))

(defun bluetooth-plugin-dev-remove (device)
  "Remove DEVICE from the handled devices."
  (when (hash-table-p bluetooth-plugin--objects)
    (let ((dev-id (bluetooth-device-id device)))
      (maphash (lambda (_api entry)
                 (when (member dev-id (plist-get entry :dev-ids))
                   (when-let ((remove-fn (plist-get entry :remove-fn)))
                     (funcall remove-fn device))
                   (setf (plist-get entry :dev-ids)
                         (cl-remove dev-id (plist-get entry :dev-ids)))))
               bluetooth-plugin--objects))))

(defun bluetooth-plugin-dev-update (device)
  "Add DEVICE to the handled devices.
Obtain a list of interfaces provided by DEVICE and notify plugins registered
for these interfaces of the newly added device."
  (when (hash-table-p bluetooth-plugin--objects)
    (cl-labels ((notify (api entry)
                  (when-let ((interface (bluetooth-device-implements-p device api)))
                    (when-let ((new-fn (plist-get entry :new-fn)))
                      (cl-pushnew (bluetooth-device-id device)
                                  (plist-get (gethash api bluetooth-plugin--objects)
                                             :dev-ids))
                      (funcall new-fn device)))))
      (maphash #'notify bluetooth-plugin--objects))))

(defun bluetooth-plugin-insert-infos (device)
  (when (hash-table-p bluetooth-plugin--objects)
    (when-let ((dev-id (and (bluetooth-device-property device "Connected")
                            (bluetooth-device-id device))))
      (maphash (lambda (_api entry)
                 (when (member dev-id (plist-get entry :dev-ids))
                   (funcall (plist-get entry :info-fn) device)))
               bluetooth-plugin--objects))))

(defun bluetooth-plugin-init ()
  "Initialize the bluetooth plugin interface.
Initialize all the auto-load plugins configured in
‘bluetooth-plugin-autoload’.  The init functions should call
‘bluetooth-plugin-register’."
  (dolist (init-fn bluetooth-plugin-autoload)
    (and (fboundp init-fn) (funcall init-fn))))

(provide 'bluetooth-plugin)
;;; bluetooth-plugin.el ends here
