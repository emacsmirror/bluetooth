;;; bluetooth.el --- A Major mode for Bluetooth devices -*- lexical-binding: t -*-

;; Copyright (C) 2019, 2020, 2022, 2024 Free Software Foundation, Inc.

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;;         Etienne Prud’homme <e.e.f.prudhomme@gmail.com>
;;
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 13 Aug 2019
;; Keywords: hardware
;; URL: https://gitlab.com/rstocker/emacs-bluetooth

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements basic Bluetooth management functionality, such as
;; connecting and disconnecting devices, setting properties and aliases,
;; putting the adapter in discovery mode and controlling its power supply.
;; It also includes a pairing agent.
;; It uses the Bluez Bluetooth stack on GNU/Linux via the D-Bus interface.
;; Therefore, it requires an Emacs with D-Bus support compiled-in.
;;
;; To use the package, invoke `bluetooth-list-devices'.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'let-alist)
(require 'dash)
(require 'rx)
(eval-when-compile (require 'subr-x))
(require 'bluetooth-pa)
(require 'bluetooth-lib)
(require 'bluetooth-device)
(require 'bluetooth-uuid)


;;;; customization

(defgroup bluetooth nil
  "Bluetooth device management."
  :group 'comm)

(defcustom bluetooth-update-interval 2
  "Update interval of the device list view."
  :type '(natnum))

(defgroup bluetooth-faces nil
  "Faces used by Bluetooth mode."
  :group 'faces)

(defface bluetooth-info-heading
  '((t . (:foreground "royal blue" :weight bold)))
  "Face for device info headings.")

(defface bluetooth-info-attribute
  '((t . (:slant italic)))
  "Face for device attribute names.")


;;;; internal constants and variables

(defconst bluetooth-buffer-name "*Bluetooth*"
  "Name of the buffer in which to list bluetooth devices.")

(defconst bluetooth-info-buffer-name "*Bluetooth info*"
  "Name of the bluetooth info buffer.")

(defconst bluetooth--mode-name "Bluetooth" "Pretty print mode name.")

(defvar bluetooth--mode-info
  '(:eval (bluetooth--mode-info))
  "Mode info display.")

(put 'bluetooth--mode-info 'risky-local-variable t)

(cl-defstruct bluetooth-property
  "Bluetooth state information for the mode line.
This structure holds the texts shown in active and inactive state
of a property."
  active-p
  (active-text nil :read-only t)
  (inactive-text nil :read-only t))

(defun bluetooth-property-text (property)
  "Return the text describing the state of PROPERTY."
  (if (bluetooth-property-active-p property)
      (bluetooth-property-active-text property)
    (bluetooth-property-inactive-text property)))

(defvar bluetooth--mode-state `(("Powered" . ,(make-bluetooth-property
                                               :inactive-text "off"))
                                ("Discoverable" . ,(make-bluetooth-property
                                                    :active-text "discoverable"))
                                ("Pairable" . ,(make-bluetooth-property
                                                :active-text "pairable"))
                                ("Discovering" . ,(make-bluetooth-property
                                                   :active-text "scan")))
  "Mode line adapter state information.

The state information list defines the kind of adapter state
displayed in the mode-line.  The first element of each sub-list
is an adapter property, the second is a ‘bluetooth-property’
structure containing the
 - current status of the item (t or nil),
 - string displayed if the property is non-nil,
 - string displayed if the property is nil.

If a display element is nil, nothing will be displayed for this
property and state.")

;; this variable holds the adapter signal handler object to allow clean-up in
;; the kill-buffer-hook
(defvar bluetooth--adapter-signal nil "D-Bus adapter signal object.")

(defvar bluetooth--update-timer nil
  "The bluetooth device table update timer.")


;;;; internal functions

(defconst bluetooth--list-format
  [("Alias" 24 t) ("Paired" 8 t) ("Connected" 11 t) ("Address" 18 t)
   ("Blocked" 9 t) ("Trusted" 9 t)]
  "The list view format for bluetooth mode.

NOTE: the strings MUST correspond to Bluez device properties
as they are used to gather the information from Bluez.")

;; This function provides the list entries for the tabulated-list
;; view.  It is called from `tabulated-list-print'.
(defun bluetooth--list-entries ()
  "Provide the list entries for the tabulated view."
  (let (dev-list)
    (bluetooth-device-map
     (lambda (dev-id device)
       (when (bluetooth-device-properties device)
         (push (list dev-id
                     (cl-map 'vector
                             (lambda (key)
                               (let ((value (bluetooth-device-property device key)))
                                 (cond ((stringp value) value)
                                       ((null value) "no")
                                       (t "yes"))))
                             (mapcar #'cl-first bluetooth--list-format)))
               dev-list))))
    dev-list))

(defun bluetooth--print-list (&optional _device)
  "Print the device list."
  (with-current-buffer bluetooth-buffer-name
    (tabulated-list-print t)
    (and (fboundp 'hl-line-highlight)
         (bound-and-true-p hl-line-mode)
         (hl-line-highlight))))

(defun bluetooth--update-with-callback ()
  (bluetooth-device-update-all #'bluetooth--print-list))

(defun bluetooth--update-print ()
  "Update device info and print device list view."
  (ignore-errors
    (bluetooth--update-with-callback)
    (bluetooth--print-list)))

;; Build up the index for Imenu.  This function is used as
;; `imenu-create-index-function'.
(defun bluetooth--create-imenu-index ()
  "Create the Bluetooth device index for Imenu."
  (goto-char (point-min))
  (cl-loop for (pos entry) = (list (point) (tabulated-list-get-entry))
           while entry
           do (forward-line 1)
           collect (cons (elt entry 0) pos)))

(defun bluetooth--initialize-mode-info ()
  "Get the current adapter state and display it.
This function only uses the first adapter reported by Bluez."
  (let* ((adapter (cl-first (bluetooth-lib-query-adapters)))
         (props (bluetooth-lib-adapter-properties adapter))
         (info (--map (list (cl-first it)
                            (list (cl-rest (assoc (cl-first it) props))))
                      bluetooth--mode-state)))
    (bluetooth--handle-prop-change (bluetooth-lib-interface :adapter)
                                   info)))

(defun bluetooth--cleanup ()
  "Clean up when mode buffer is killed."
  ;; This function is registered as a kill-buffer-hook, so we don't
  ;; want any errors to get in the way of killing the buffer
  (ignore-errors
    (bluetooth-pa-unregister-agent)
    (dbus-unregister-object bluetooth--adapter-signal)
    (bluetooth-device-cleanup)
    (remove-hook 'tabulated-list-revert-hook 'bluetooth--update-with-callback)
    (cancel-timer bluetooth--update-timer)
    (setq bluetooth--update-timer nil)))

(defun bluetooth-unload-function ()
  "Clean up when the bluetooth feature is unloaded."
  (when (buffer-live-p (get-buffer bluetooth-buffer-name))
    (bluetooth--cleanup)
    (kill-buffer bluetooth-buffer-name))
  nil)

;; This function is called from Emacs's mode-line update code
;; and must not contain any calls to D-Bus functions.
(defun bluetooth--mode-info ()
  "Update the mode info display."
  (let ((info (mapconcat #'identity
                         (--keep (bluetooth-property-text (cl-rest it))
                                 bluetooth--mode-state)
                         ",")))
    (unless (string-blank-p info)
      (concat " [" info "]"))))

(defun bluetooth--handle-prop-change (interface data &rest _)
  "Handle property change signals on D-Bus INTERFACE as given by DATA.
Only adapter properties are considered.  If an adapter property changes,
update the status display accordingly."
  (when (string= interface (bluetooth-lib-interface :adapter))
    (mapc (lambda (elt)
            (cl-destructuring-bind (prop (value)) elt
              (when-let (property (cl-rest (assoc prop bluetooth--mode-state)))
                (setf (bluetooth-property-active-p property) value))))
          data)
    (force-mode-line-update)))

;; TODO remove this and replace by general path construction function
(defun bluetooth--make-path (api)
  "Return the path of the currently selected device."
  (cond ((eq :device api)
         (bluetooth-device-path (bluetooth-device (tabulated-list-get-id))))
        ((eq :adapter api)
         (bluetooth-lib-path (cl-first (bluetooth-lib-query-adapters))))))


;;;; command definitions

(defun bluetooth--choose-uuid ()
  "Ask for a UUID and return it in a form suitable for ‘interactive’."
  (if current-prefix-arg
      (let* ((device (bluetooth-device (tabulated-list-get-id)))
             (uuids (bluetooth-uuid-interpret
                     (bluetooth-device-property device "UUIDs")))
             (profile (completing-read "Profile: "
                                       (mapcar (lambda (x)
                                                 (let ((desc (cl-second x)))
                                                   (concat (cl-first desc)
                                                           ", "
                                                           (cl-second desc))))
                                               uuids)
                                       nil t)))
        (list (cl-rassoc profile uuids
                         :key (lambda (x)
                                (let ((desc (cl-first x)))
                                  (concat (cl-first desc) ", " (cl-second desc))))
                         :test #'string=)))
    '(nil)))

(defun bluetooth-connect (uuid)
  "Connect to the Bluetooth device at point.
When called with a prefix argument, ask for a profile and
connect only this profile.  Otherwise, or when called
non-interactively with UUID set to nil, connect to all profiles."
  (interactive (bluetooth--choose-uuid))
  (if uuid
      (bluetooth-lib-dbus-method (bluetooth--make-path :device) "ConnectProfile"
                                 :device (cl-first uuid))
    (bluetooth-lib-dbus-method (bluetooth--make-path :device) "Connect" :device)))

(defun bluetooth-disconnect (uuid)
  "Disconnect the Bluetooth device at point.
When called with a prefix argument, ask for a profile and
disconnect only this profile.  Otherwise, or when called
non-interactively with UUID set to nil, disconnect all
profiles."
  (interactive (bluetooth--choose-uuid))
  (if uuid
      (bluetooth-lib-dbus-method (bluetooth--make-path :device) "DisconnectProfile"
                                 :device (cl-first uuid))
    (bluetooth-lib-dbus-method (bluetooth--make-path :device) "Disconnect" :device)))

(defun bluetooth-connect-profile ()
  "Ask for a Bluetooth profile and connect the device at point to it."
  (interactive)
  (let ((prefix-arg (list 4)))
    (command-execute #'bluetooth-connect)))

(defun bluetooth-disconnect-profile ()
  "Ask for a Bluetooth profile and disconnect the device at point from it."
  (interactive)
  (let ((prefix-arg (list 4)))
    (command-execute #'bluetooth-disconnect)))

(defmacro bluetooth-defun-method (method api docstring &rest body)
  "Make a function calling a bluetooth METHOD using API.
The function will have DOCSTRING as its documentation and is
implemented by BODY."
  (declare (doc-string 3) (indent 2))
  (let ((name (bluetooth-lib-make-function-name method)))
    (cl-with-gensyms (gmethod gapi)
      `(defun ,(intern name) () ,docstring
              (interactive)
              (let ((,gmethod ,method)
                    (,gapi ,api))
                (bluetooth-lib-dbus-method (bluetooth--make-path ,gapi)
                                           ,gmethod
                                           ,gapi)
                ,@body)))))

(bluetooth-defun-method "StartDiscovery" :adapter
  "Start discovery mode."
  (unless bluetooth--update-timer
    (setq bluetooth--update-timer
          (run-at-time nil bluetooth-update-interval
                       #'bluetooth--update-print))))

(bluetooth-defun-method "StopDiscovery" :adapter
  "Stop discovery mode."
  (when bluetooth--update-timer
    (cancel-timer bluetooth--update-timer)
    (setq bluetooth--update-timer nil)))

(bluetooth-defun-method "Pair" :device
  "Pair with device at point.")

(defmacro bluetooth-defun-toggle (property api docstring)
  "Make a function to toggle a PROPERTY of a device using API.
The function will have DOCSTRING as its documentation."
  (declare (doc-string 3) (indent 2))
  (let ((name (bluetooth-lib-make-function-name property "-toggle")))
    (cl-with-gensyms (gproperty gapi)
      `(defun ,(intern name) () ,docstring
              (interactive)
              (let ((,gproperty ,property)
                    (,gapi ,api))
                (bluetooth-lib-dbus-toggle (bluetooth--make-path ,gapi)
                                           ,gproperty
                                           ,gapi))))))

(bluetooth-defun-toggle "Blocked" :device
  "Mark Bluetooth device at point blocked.")
(bluetooth-defun-toggle "Trusted" :device
  "Mark Bluetooth device at point trusted.")
(bluetooth-defun-toggle "Powered" :adapter
  "Toggle power supply of adapter.")
(bluetooth-defun-toggle "Discoverable" :adapter
  "Toggle discoverable mode.")
(bluetooth-defun-toggle "Pairable" :adapter
  "Toggle pairable mode.")

(defun bluetooth-set-alias (name)
  "Set alias of Bluetooth device at point to NAME."
  (interactive "MAlias (empty to reset): ")
  (bluetooth-lib-dbus-set (bluetooth--make-path :device) "Alias" name :device))

(defun bluetooth-remove-device (&optional dev-id)
  "Remove Bluetooth device at point or specified by DEV-ID.
Calling this function will unpair device and host."
  (interactive)
  (when-let (device (bluetooth-device (or dev-id (tabulated-list-get-id))))
    (bluetooth-lib-dbus-method (bluetooth-device-property device "Adapter")
                               "RemoveDevice"
                               :adapter
                               :object-path
                               (bluetooth-device-path device))))

(defun bluetooth-end-of-list ()
  "Move cursor to the last list element."
  (interactive)
  (let ((column (current-column)))
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (+ (point)
                  (- column (current-column))))))

(defun bluetooth-beginning-of-list ()
  "Move cursor to the first list element."
  (interactive)
  (let ((column (current-column)))
    (goto-char (point-min))
    (goto-char (+ (point)
                  (- column (current-column))))))


;;;; keymap and menu

(defvar bluetooth-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?c] #'bluetooth-connect)
    (define-key map [?d] #'bluetooth-disconnect)
    (define-key map [?b] #'bluetooth-toggle-blocked)
    (define-key map [?t] #'bluetooth-toggle-trusted)
    (define-key map [?a] #'bluetooth-set-alias)
    (define-key map [?r] #'bluetooth-start-discovery)
    (define-key map [?R] #'bluetooth-stop-discovery)
    (define-key map [?s] #'bluetooth-toggle-powered)
    (define-key map [?P] #'bluetooth-pair)
    (define-key map [?D] #'bluetooth-toggle-discoverable)
    (define-key map [?x] #'bluetooth-toggle-pairable)
    (define-key map [?i] #'bluetooth-show-device-info)
    (define-key map [?A] #'bluetooth-show-adapter-info)
    (define-key map [?k] #'bluetooth-remove-device)
    (define-key map [?<] #'bluetooth-beginning-of-list)
    (define-key map [?>] #'bluetooth-end-of-list)

    (define-key map [menu-bar bluetooth]
                (cons "Bluetooth" (make-sparse-keymap "Bluetooth")))
    (define-key map [menu-bar bluetooth device]
                (cons "Device" (make-sparse-keymap "Device")))

    (define-key map [menu-bar bluetooth stop-discovery]
                '(menu-item "Stop discovery" bluetooth-stop-discovery
                            :help "Stop discovery"))
    (define-key map [menu-bar bluetooth start-discovery]
                '(menu-item "Start discovery" bluetooth-start-discovery
                            :help "Start discovery"))
    (define-key map [menu-bar bluetooth toggle-discoverable]
                '(menu-item "Toggle discoverable" bluetooth-toggle-discoverable
                            :help "Toggle discoverable mode"))
    (define-key map [menu-bar bluetooth toggle-pairable]
                '(menu-item "Toggle pairable" bluetooth-toggle-pairable
                            :help "Toggle pairable mode"))
    (define-key map [menu-bar bluetooth toggle-powered]
                '(menu-item "Toggle powered" bluetooth-toggle-powered
                            :help "Toggle power supply of adapter"))
    (define-key map [menu-bar bluetooth show-adapter-info]
                '(menu-item "Show adapter info" bluetooth-show-adapter-info
                            :help "Show bluetooth adapter info"))

    (define-key map [menu-bar bluetooth device show-info]
                '(menu-item "Show device info" bluetooth-show-device-info
                            :help "Show bluetooth device info"))
    (define-key map [menu-bar bluetooth device set-alias]
                '(menu-item "Set device alias" bluetooth-set-alias
                            :help "Set device alias"))
    (define-key map [menu-bar bluetooth device toggle-trusted]
                '(menu-item "Toggle trusted" bluetooth-toggle-trusted
                            :help "Trust/untrust bluetooth device"))
    (define-key map [menu-bar bluetooth device toggle-blocked]
                '(menu-item "Toggle blocked" bluetooth-toggle-blocked
                            :help "Block/unblock bluetooth device"))
    (define-key map [menu-bar bluetooth device disconnect-profile]
                '(menu-item "Disconnect profile" bluetooth-disconnect-profile
                            :help "Disconnect bluetooth device profile"))
    (define-key map [menu-bar bluetooth device disconnect]
                '(menu-item "Disconnect" bluetooth-disconnect
                            :help "Disconnect bluetooth device"))
    (define-key map [menu-bar bluetooth device connect-profile]
                '(menu-item "Connect profile" bluetooth-connect-profile
                            :help "Connect bluetooth device profile"))
    (define-key map [menu-bar bluetooth device connect]
                '(menu-item "Connect" bluetooth-connect
                            :help "Connect bluetooth device"))
    (define-key map [menu-bar bluetooth device remove]
                '(menu-item "Remove" bluetooth-remove-device
                            :help "Remove bluetooth device"))
    (define-key map [menu-bar bluetooth device pair]
                '(menu-item "Pair" bluetooth-pair
                            :help "Pair bluetooth device"))

    map)
  "The Bluetooth mode keymap.")


;;;; mode definition

(define-derived-mode bluetooth-mode tabulated-list-mode
  bluetooth--mode-name
  "Major mode for managing Bluetooth devices."
  (setq tabulated-list-format bluetooth--list-format
        tabulated-list-entries #'bluetooth--list-entries
        tabulated-list-padding 0
        tabulated-list-sort-key (cons "Alias" nil))
  (add-hook 'tabulated-list-revert-hook 'bluetooth--update-with-callback)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))


;;;; device and adapter info display

(defun bluetooth--ins-heading (heading)
  "Insert HEADING in info view."
  (insert (propertize heading 'face
                      'bluetooth-info-heading)))

(defun bluetooth-ins-line (attr text)
  "Insert attribute ATTR and corresponding TEXT in info view."
  (insert (propertize (format "%21s" attr)
                      'face
                      'bluetooth-info-attribute)
          ": " text "\n"))

(defun bluetooth--ins-attr (props attr)
  "Insert information on attribute ATTR in properties alist PROPS."
  (let ((value (cl-rest (assoc attr props))))
    (bluetooth-ins-line attr
                        (cond ((stringp value) value)
                              ((numberp value)
                               (number-to-string value))
                              ((consp value)
                               (mapconcat #'identity value ", "))
                              ((null value) "no")
                              (t "yes")))))

(defun bluetooth--ins-classes (props)
  "Insert device classes from properties alist PROPS."
  (when-let (class (cl-rest (assoc "Class" props)))
    (let ((p-class (bluetooth-uuid-parse-class class)))
      (bluetooth--ins-heading "\nService and device classes\n")
      (--map (cl-destructuring-bind (type value) it
               (if (listp value)
                   (bluetooth-ins-line type
                                       (mapconcat #'identity
                                                  value
                                                  ", "))
                 (bluetooth-ins-line type value)))
             p-class))))

(defun bluetooth--ins-services (props)
  "Insert device services from properties alist PROPS."
  (when-let (uuids (cl-rest (assoc "UUIDs" props)))
    (bluetooth--ins-heading "\nServices (UUIDs)\n")
    (mapc (lambda (id-pair)
            (--zip-with (insert (format it other))
                        '("%36s  " "%s " "(%s)")
                        (cl-second id-pair))
            (insert "\n"))
          (bluetooth-uuid-interpret uuids))))

(defun bluetooth--ins-rf-info (props)
  "Insert rf information from properties alist PROPS."
  (let* ((rssi (cl-rest (assoc "RSSI" props)))
         (tx-power (cl-rest (assoc "TxPower" props)))
         (loss (when (and rssi tx-power) (- tx-power rssi))))
    (--zip-with (when other
                  (bluetooth-ins-line (cl-first it)
                                      (format (cl-second it) other)))
                '(("RSSI" "%4d dBm") ("Tx Power" "%4d dBm")
                  ("Path loss" "%4d dB"))
                (list rssi tx-power loss))))

(defun bluetooth--ins-mfc-info (props)
  "Insert manufacturer information from properties alist PROPS."
  (when-let (mf-info (cl-second (assoc "ManufacturerData" props)))
    (bluetooth-ins-line "Manufacturer"
                        (or (bluetooth-uuid-manufacturer-from-id
                             (cl-first mf-info))
                            "unknown"))))

(defun bluetooth-show-device-info ()
  "Show detailed information on the device at point."
  (interactive)
  (when-let (device (bluetooth-device (tabulated-list-get-id)))
    (with-current-buffer-window bluetooth-info-buffer-name nil nil
      (let ((props (bluetooth-device-properties device)))
        (bluetooth--ins-heading "Bluetooth device info\n\n")
        (mapc (lambda (it) (bluetooth--ins-attr props it))
              '("Alias" "Address" "AddressType" "Paired" "Trusted"
                "Blocked" "LegacyPairing" "Connected" "Modalias"
                "ServicesResolved" "WakeAllowed" "Adapter"))
        (funcall (-juxt #'bluetooth--ins-rf-info
                        #'bluetooth--ins-mfc-info
                        #'bluetooth--ins-classes
                        #'bluetooth--ins-services)
                 props)
        (bluetooth--ins-heading "\nOther device information\n")
        (dolist (fn (bluetooth-device-insert-fn device))
          (when fn (funcall fn device)))
        (special-mode)))))

(defun bluetooth-show-adapter-info ()
  "Show detailed information on the (first) bluetooth adapter."
  (interactive)
  (let* ((adapter (cl-first (bluetooth-lib-query-adapters)))
         (props (bluetooth-lib-adapter-properties adapter)))
    (with-current-buffer-window bluetooth-info-buffer-name nil nil
      (bluetooth--ins-heading "Bluetooth adapter info\n\n")
      (mapc (lambda (it) (bluetooth--ins-attr props it))
            '("Alias" "Address" "AddressType" "Powered" "Discoverable"
              "DiscoverableTimeout" "Pairable" "PairableTimeout"
              "Discovering" "Roles" "Modalias"))
      (bluetooth-ins-line "Adapter" (bluetooth-lib-path adapter))
      (funcall (-juxt #'bluetooth--ins-mfc-info
                      #'bluetooth--ins-classes
                      #'bluetooth--ins-services)
               props)
      (special-mode))))


;;;; mode entry command

;;;###autoload
(defun bluetooth-init ()
  "Initialize bluetooth mode."
  ;; make sure D-Bus is (made) available
  (dbus-ping bluetooth-bluez-bus bluetooth-service bluetooth-timeout)
  (bluetooth-device-init #'bluetooth--print-list)
  (bluetooth-pa-register-agent))

;;;###autoload
(defun bluetooth-list-devices ()
  "Display a list of Bluetooth devices.
This function starts Bluetooth mode which offers an interface
offering device management functions, e.g. pairing, connecting,
scanning the bus, displaying device info etc."
  (interactive)
  (if (get-buffer bluetooth-buffer-name)
      (pop-to-buffer bluetooth-buffer-name)
    (with-current-buffer (switch-to-buffer bluetooth-buffer-name)
      (unless (derived-mode-p 'bluetooth-mode)
        (bluetooth-init)
        (erase-buffer)
        (bluetooth-mode)
        (cl-pushnew bluetooth--mode-info mode-line-process)
        ;; TODO have an extra unload command instead
        (add-hook 'kill-buffer-hook #'bluetooth--cleanup 0 t)
        (setq imenu-create-index-function #'bluetooth--create-imenu-index)
        (bluetooth--initialize-mode-info)
        (setq bluetooth--update-timer
              (if (bluetooth-lib-adapter-property (cl-first (bluetooth-lib-query-adapters))
                                                  "Discovering")
                  (run-at-time nil bluetooth-update-interval
                               #'bluetooth--update-print)
                nil))
        (setq bluetooth--adapter-signal
              (bluetooth-lib-register-props-signal nil
                                                   (bluetooth-lib-path
                                                    (cl-first (bluetooth-lib-query-adapters)))
                                                   :adapter
                                                   #'bluetooth--handle-prop-change))))))

(provide 'bluetooth)

;;; bluetooth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
