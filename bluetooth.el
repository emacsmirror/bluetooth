;;; bluetooth.el --- A Major mode for Bluetooth devices -*- lexical-binding: t -*-

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc.

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;;         Etienne Prud’homme <e.e.f.prudhomme@gmail.com>
;;
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 13 Aug 2019
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (dash "2.12.0"))
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


;;;; customization

(defgroup bluetooth nil
  "Bluetooth device management."
  :group 'comm)

(defcustom bluetooth-bluez-bus :system
  "D-Bus bus that Bluez is registered on.
This is usually `:system' if bluetoothd runs as a system service, or
`:session' if it runs as a user service."
  :type '(symbol))

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
  "Bluetooth state information for the mode line with texts shown
in active and inactive state of a property."
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

;; Bluez service name as defined by the Bluez API
(defconst bluetooth--service "org.bluez" "D-Bus service name of Bluez.")

;; Bluez root path as defined by the Bluez API
(defconst bluetooth--root "/org/bluez" "D-Bus path root for Bluez.")

;; our path name for the pairing agent
(defconst bluetooth--own-path (concat dbus-path-emacs "/bluetooth")
  "D-Bus object path for the pairing agent.")

;; these two variables hold D-Bus objects to allow clean-up in
;; the kill-buffer-hook
(defvar bluetooth--method-objects '() "D-Bus method objects.")
(defvar bluetooth--adapter-signal nil "D-Bus adapter signal object.")

(eval-and-compile
  (defconst bluetooth--base-uuid "0000-1000-8000-00805f9b34fb"
	"Bluetooth base UUID."))

(defconst bluetooth--interfaces
  '((:device . "org.bluez.Device1")
	(:adapter . "org.bluez.Adapter1")
	(:agent-manager . "org.bluez.AgentManager1")
	(:agent . "org.bluez.Agent1")
	(:properties . "org.freedesktop.DBus.Properties"))
  "Bluez D-Bus interfaces.")

(defvar bluetooth--timeout 5000 "Default timeout for Bluez D-Bus access.")

(defvar bluetooth--device-info nil "Device info obtained from Bluez.")


;;;; command definitions

(eval-and-compile
  (defun bluetooth--function-name (name &optional prefix)
	"Make a function name out of NAME and PREFIX.
The generated function name has the form ‘bluetoothPREFIX-NAME’."
	(let ((case-fold-search nil))
	  (concat "bluetooth"
			  prefix
			  (replace-regexp-in-string "[[:upper:]][[:lower:]]+"
										(lambda (x) (concat "-" (downcase x)))
										name t)))))

(defun bluetooth--choose-uuid ()
  "Ask for a UUID and return it in a form suitable for ‘interactive’."
  (if current-prefix-arg
	  (let* ((device (bluetooth--device (tabulated-list-get-id)))
			 (uuids (bluetooth--device-uuids
					 (bluetooth-device-properties device)))
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
	  (bluetooth--dbus-method "ConnectProfile" :device (cl-first uuid))
	(bluetooth--dbus-method "Connect" :device)))

(defun bluetooth-disconnect (uuid)
  "Disconnect the Bluetooth device at point.
When called with a prefix argument, ask for a profile and
disconnect only this profile.  Otherwise, or when called
non-interactively with UUID set to nil, disconnect all
profiles."
  (interactive (bluetooth--choose-uuid))
  (if uuid
	  (bluetooth--dbus-method "DisconnectProfile" :device (cl-first uuid))
	(bluetooth--dbus-method "Disconnect" :device)))

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


(defmacro bluetooth-defun-method (method api docstring)
  (declare (doc-string 3) (indent 2))
  (let ((name (bluetooth--function-name method)))
	`(defun ,(intern name) () ,docstring
			(interactive)
			(bluetooth--dbus-method ,method ,api))))

(bluetooth-defun-method "StartDiscovery" :adapter
  "Start discovery mode.")
(bluetooth-defun-method "StopDiscovery" :adapter
  "Stop discovery mode.")
(bluetooth-defun-method "Pair" :device
  "Pair with device at point.")

(defmacro bluetooth-defun-toggle (property api docstring)
  (declare (doc-string 3) (indent 2))
  (let ((name (bluetooth--function-name property "-toggle")))
	`(defun ,(intern name) () ,docstring
			(interactive)
			(bluetooth--dbus-toggle ,property ,api))))

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
  (bluetooth--dbus-set "Alias" name :device))

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


;;;; internal functions

(cl-defstruct bluetooth-device
  "A bluetooth device.  This structure holds all the device
properties."
  (id nil :read-only t)
  properties)

(defun bluetooth-device-property (device prop-name)
  "Return DEVICE's property PROP-NAME."
  (cl-rest (assoc prop-name (bluetooth-device-properties device))))

(defun bluetooth--query-adapters ()
  "Return a list of bluetooth adapters."
  (dbus-introspect-get-node-names
   bluetooth-bluez-bus bluetooth--service bluetooth--root))

(defun bluetooth--query-devices (adapter)
  "Return a list of bluetooth devices connected to ADAPTER."
  (dbus-introspect-get-node-names bluetooth-bluez-bus bluetooth--service
								  (concat bluetooth--root "/" adapter)))

(defun bluetooth--device (device-id)
  "Return the device struct for DEVICE-ID."
  (gethash device-id bluetooth--device-info))

(defun bluetooth--dev-state (key device)
  "Return state information regarding KEY for DEVICE."
  (let ((value (bluetooth-device-property device key)))
	(cond ((stringp value) value)
		  ((null value) "no")
		  (t "yes"))))

(defun bluetooth--create-device (adapter dev-id)
  "Create a bluetooth device struct for DEV-ID on ADAPTER."
  (let* ((path (mapconcat #'identity
						  (list bluetooth--root adapter dev-id)
						  "/"))
		 (props (dbus-get-all-properties bluetooth-bluez-bus
										 bluetooth--service
										 path
										 (alist-get
										  :device bluetooth--interfaces))))
	(make-bluetooth-device :id dev-id :properties props)))

(defun bluetooth--adapter-properties (adapter)
  "Return the properties of bluetooth ADAPTER.
This function evaluates to an alist of attribute/value pairs."
  (dbus-get-all-properties bluetooth-bluez-bus bluetooth--service
						   (concat bluetooth--root "/" adapter)
						   (alist-get :adapter
									  bluetooth--interfaces)))

(defconst bluetooth--list-format
  [("Alias" 24 t) ("Paired" 8 t) ("Connected" 11 t) ("Address" 18 t)
   ("Blocked" 9 t) ("Trusted" 9 t)]
  "The list view format for bluetooth mode.

NOTE: the strings MUST correspond to Bluez device properties
as they are used to gather the information from Bluez.")

(defun bluetooth--update-device-info ()
  "Update the bluetooth devices list."
  (mapc (lambda (adapter)
		   (mapc (lambda (dev)
				   (puthash dev
							(bluetooth--create-device adapter dev)
							bluetooth--device-info))
				 (bluetooth--query-devices adapter)))
		(bluetooth--query-adapters)))

;; This function provides the list entries for the tabulated-list
;; view.  It is called from `tabulated-list-print'.
(defun bluetooth--list-entries ()
  "Provide the list entries for the tabulated view."
  (bluetooth--update-device-info)		; TODO this can later be removed when
										; update is by dbus change notifications
  (let (dev-list)
	(maphash (lambda (dev dev-info)
			   (push (list dev
						   (cl-map 'vector (lambda (key)
											 (bluetooth--dev-state key dev-info))
								   (mapcar #'cl-first bluetooth--list-format)))
					 dev-list))
			 bluetooth--device-info)
	dev-list))

(defun bluetooth--update-list ()
  "Update the list view."
  (with-current-buffer bluetooth-buffer-name
	(tabulated-list-print t)
	(and (fboundp 'hl-line-highlight)
		 (bound-and-true-p hl-line-mode)
		 (hl-line-highlight))))

(define-derived-mode bluetooth-mode tabulated-list-mode
  bluetooth--mode-name
  "Major mode for managing Bluetooth devices."
  (setq tabulated-list-format bluetooth--list-format
		tabulated-list-entries #'bluetooth--list-entries
		tabulated-list-padding 0
		tabulated-list-sort-key (cons "Alias" nil))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

;; Build up the index for Imenu.  This function is used as
;; `imenu-create-index-function'.
(defun bluetooth--create-imenu-index ()
  "Create the Bluetooth device index for Imenu."
  (goto-char (point-min))
  (cl-loop for (pos entry) = (list (point) (tabulated-list-get-entry))
		   while entry
		   do (forward-line 1)
		   collect (cons (elt entry 0) pos)))

;; TODO operate on device structs, not ids
(defun bluetooth--call-method (dev-id api function &rest args)
  "For DEV-ID, invoke D-Bus FUNCTION on API, passing ARGS."
  (let ((path (cond ((and (eq :device api)
						  (not (null dev-id)))
					 (concat (bluetooth-device-property
							  (bluetooth--device dev-id)
							  "Adapter")
							 "/" dev-id))
					((eq :adapter api)
					 (concat bluetooth--root
							 "/"
							 (cl-first (bluetooth--query-adapters))))
					(t nil)))
		(interface (alist-get api bluetooth--interfaces)))
	(when path
	  (apply function bluetooth-bluez-bus bluetooth--service path interface
			 (mapcar (lambda (x)
					   (if (eq x :path-devid)
						   (concat path "/" dev-id)
						 x))
					 args)))))

(defun bluetooth--dbus-method (method api &rest args)
  "Invoke METHOD on D-Bus API with ARGS."
  (let ((dev-id (tabulated-list-get-id)))
	(apply #'bluetooth--call-method dev-id api
		   #'dbus-call-method-asynchronously method
		   #'bluetooth--update-list :timeout bluetooth--timeout args)))

(defun bluetooth--dbus-toggle (property api)
  "Toggle boolean PROPERTY on D-Bus API."
  (let* ((dev-id (tabulated-list-get-id))
		 (value (bluetooth--call-method dev-id api
										#'dbus-get-property property)))
	(bluetooth--call-method dev-id api #'dbus-set-property property
							(not value))
	(bluetooth--update-list)))

(defun bluetooth--dbus-set (property arg api)
  "Set PROPERTY to ARG on D-Bus API."
  (let ((dev-id (tabulated-list-get-id)))
	(bluetooth--call-method dev-id api #'dbus-set-property property arg)
	(bluetooth--update-list)))

(defun bluetooth--initialize-mode-info ()
  "Get the current adapter state and display it.
This function only uses the first adapter reported by Bluez."
  (let* ((adapter (cl-first (bluetooth--query-adapters)))
		 (props (bluetooth--adapter-properties adapter))
		 (info (--map (list (cl-first it)
							(list (cl-rest (assoc (cl-first it) props))))
					  bluetooth--mode-state)))
	(bluetooth--handle-prop-change (alist-get :adapter bluetooth--interfaces)
								   info)))

(defun bluetooth--cleanup ()
  "Clean up when mode buffer is killed."
  ;; This function is registered as a kill-buffer-hook, so we don't
  ;; want any errors to get in the way of killing the buffer
  (ignore-errors
	(dbus-call-method bluetooth-bluez-bus bluetooth--service bluetooth--root
					  (alist-get :agent-manager bluetooth--interfaces)
					  "UnregisterAgent"
					  :object-path bluetooth--own-path)
	(mapc #'dbus-unregister-object bluetooth--method-objects)
	(dbus-unregister-object bluetooth--adapter-signal)))

(defun bluetooth-unload-function ()
  "Clean up when the bluetooth feature is unloaded."
  (when (buffer-live-p (get-buffer bluetooth-buffer-name))
	(kill-buffer bluetooth-buffer-name))
  nil)

(defun bluetooth-remove-device ()
  "Remove Bluetooth device at point (unpaires device and host)."
  (interactive)
  (when-let (dev-id (tabulated-list-get-id))
	(bluetooth--call-method dev-id
							:adapter
							#'dbus-call-method-asynchronously
							"RemoveDevice"
							#'bluetooth--update-list
							:timeout bluetooth--timeout
							:object-path :path-devid)))

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
  (when (string= interface (alist-get :adapter bluetooth--interfaces))
	(mapc (lambda (elt)
			(cl-destructuring-bind (prop (value)) elt
			  (when-let (property (cl-rest (assoc prop bluetooth--mode-state)))
				(setf (bluetooth-property-active-p property) value))))
		  data)))

(defun bluetooth--register-signal-handler ()
  "Register a signal handler for adapter property changes.

This function registers a signal handler only for the first
adapter reported by Bluez."
  (let ((adapter (cl-first (bluetooth--query-adapters))))
	(dbus-register-signal bluetooth-bluez-bus
						  nil
						  (concat bluetooth--root "/"
								  adapter)
						  (alist-get :properties
									 bluetooth--interfaces)
						  "PropertiesChanged"
						  #'bluetooth--handle-prop-change
						  :arg-namespace
						  (alist-get :adapter
									 bluetooth--interfaces))))

(defun bluetooth--device-uuids (properties)
  "Extract a UUID alist from device PROPERTIES.
Each list element contains a UUID as the key and the
corresponding description string as the value.  If no description
string is available (e.g. for unknown UUIDs,) the UUID itself is
the value.  The device properties can be obtained in the suitable
form by a call to ‘bluetooth-device-properties’."
  (let ((uuids (cl-rest (assoc "UUIDs" properties)))
		(uuid-alist))
	(when uuids
	  (dolist (id uuids)
		(let ((desc (or (bluetooth--parse-service-class-uuid id)
						(list id))))
		  (push (list id desc) uuid-alist)))
	  (nreverse uuid-alist))))


;;;; mode entry command

;;;###autoload
(defun bluetooth-list-devices ()
  "Display a list of Bluetooth devices.
This function starts Bluetooth mode which offers an interface
offering device management functions, e.g. pairing, connecting,
scanning the bus, displaying device info etc."
  (interactive)
  ;; make sure D-Bus is (made) available
  (dbus-ping bluetooth-bluez-bus bluetooth--service bluetooth--timeout)
  (with-current-buffer (switch-to-buffer bluetooth-buffer-name)
	(unless (derived-mode-p 'bluetooth-mode)
	  (erase-buffer)
	  (bluetooth-mode)
	  (setq bluetooth--device-info (make-hash-table :test #'equal))
	  (bluetooth--update-device-info)
	  (setq bluetooth--method-objects (bluetooth--register-agent))
	  (cl-pushnew bluetooth--mode-info mode-line-process)
	  (add-hook 'kill-buffer-hook #'bluetooth--cleanup nil t)
	  (setq imenu-create-index-function #'bluetooth--create-imenu-index)
	  (bluetooth--initialize-mode-info)
	  (setq bluetooth--adapter-signal
			(bluetooth--register-signal-handler)))))


;;;; Bluetooth pairing agent code

;; The release function is not needed at the moment, but needs
;; to be implemented for the agent API.
(defun bluetooth--release ()
  "Clean up after Bluetooth agent release.")

(defmacro bluetooth--with-alias (device &rest body)
  "Evaluate BODY with DEVICE alias bound to ALIAS."
  (declare (indent defun))
  `(let* ((dev (bluetooth--device (cl-first (last (split-string ,device "/")))))
		  (alias (or (bluetooth-device-property dev "Alias")
					 (replace-regexp-in-string "_" ":" dev nil nil nil 4))))
	 ,@body))

(defmacro bluetooth--maybe-cancel-reject (&rest body)
  "Invoke BODY and maybe issue cancel and reject errors.
`org.bluez.Error.Canceled' is issued on `keyboard-quit' and
`org.bluez.Error.Rejected' is issued if BODY evaluates to nil."
  (declare (indent defun))
  `(or (condition-case nil
		   (progn ,@body)
		 (quit (signal 'dbus-error '("org.bluez.Error.Canceled"))))
	   (signal 'dbus-error '("org.bluez.Error.Rejected"))))

(defun bluetooth--request-pin-code (device)
  "Request a pin code for DEVICE."
  (bluetooth--maybe-cancel-reject
	(bluetooth--with-alias device
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

(defun bluetooth--display-pin-code (device pincode)
  "Display the PINCODE for DEVICE."
  (bluetooth--with-alias device
	(message "Bluetooth PIN for `%s': %s" alias pincode)
	:ignore))

(defun bluetooth--request-passkey (device)
  "Request passkey for DEVICE."
  (bluetooth--maybe-cancel-reject
	(bluetooth--with-alias device
	  (let ((pk (read-from-minibuffer
				 (format "Enter Bluetooth Passkey for `%s': (0..999999) "
						 alias))))
		(min (max (string-to-number pk) 0) 999999)))))

(defun bluetooth--display-passkey (device passkey _)
  "Display PASSKEY for DEVICE, ignoring ENTERED (for now)."
  (bluetooth--with-alias device
	(message "Bluetooth Passkey for `%s': %06d" alias passkey)
	:ignore))

(defun bluetooth--request-confirmation (device passkey)
  "Request user confirmation that DEVICE's PASSKEY is correct."
  (bluetooth--maybe-cancel-reject
	(bluetooth--with-alias device
	  (y-or-n-p
	   (format "Is Bluetooth Passkey %06d for `%s' correct? " passkey alias))))
  :ignore)

(defun bluetooth--request-authorization (device)
  "Authorize Bluetooth DEVICE."
  (bluetooth--maybe-cancel-reject
	(bluetooth--with-alias device
	  (y-or-n-p (format "Authorize Bluetooth device `%s'? " alias))))
  :ignore)

(defun bluetooth--authorize-service (device uuid)
  "Authorize Bluetooth service UUID for DEVICE."
  (bluetooth--maybe-cancel-reject
	(bluetooth--with-alias device
	  (let ((p-uuid (bluetooth--parse-service-class-uuid uuid)))
		(y-or-n-p
		 (format "Authorize Bluetooth service `%s' for device `%s'? "
				 p-uuid alias)))))
  :ignore)

;; This function usually gets called (from D-Bus) while we are
;; in the minibuffer trying to read a passkey or PIN.  Tha call to
;; `keyboard-quit' is used to break out of there.
(defun bluetooth--cancel ()
  "Cancel a pairing process."
  (keyboard-quit)
  (message "Pairing canceled"))

;; This procedure registers the pairing agent.
(defun bluetooth--register-agent ()
  "Register as a pairing agent."
  (let ((methods '("Release" "RequestPinCode" "DisplayPinCode"
				   "RequestPasskey" "DisplayPasskey" "RequestConfirmation"
				   "RequestAuthorization" "AuthorizeService" "Cancel")))
	(prog1 
		(cl-loop for method in methods
				 for fname = (bluetooth--function-name method "-")
				 collect (dbus-register-method bluetooth-bluez-bus
											   dbus-service-emacs
											   bluetooth--own-path
											   (alist-get
												:agent
												bluetooth--interfaces)
											   method (intern fname) t))
	  (dbus-register-service :session dbus-service-emacs)
	  (dbus-call-method bluetooth-bluez-bus bluetooth--service bluetooth--root
						(alist-get :agent-manager bluetooth--interfaces)
						"RegisterAgent"
						:object-path bluetooth--own-path "KeyboardDisplay"))))


;;;; service and class UUID definitions

;; The following constants define the meaning of the Bluetooth
;; CLASS property, which is made up of a number of fields.
;; The following components are used:
;; NAME (string): a name describing the field type, e.g.
;;   "service classes"
;; MASK: the bit mask for the CLASS field
;; SHIFT: a shift value to be applied before interpreting the
;;   CLASS field
;; FN: a function to be invoked on the masked and shifted CLASS
;;   and DATA
;; NEXT: the next field in the class property: NEXT can have
;;   one of three different kinds of values:
;;   - a field specification (e.g. bluetooth--class-major-dev-classes)
;;   - a function returning the next field specification when
;;     invoked with the masked and shifted CLASS and DATA
;;   - nil, if no further processing of CLASS is necessary
;; DATA: the data passed to the parsing (FN) or NEXT functions
;;
;; The information used in all the following lists has been taken
;; from the Bluetooth website:
;; https://www.bluetooth.com/specifications/assigned-numbers/
(defconst bluetooth--class-major-services
  '((name . "major service classes")
	(mask . #xffe000)
	(shift . 0)
	(fn . bluetooth--class-parse-bitfield)
	(next . bluetooth--class-major-dev-classes)
	(data . ((13 . "Limited discoverable mode")
			 (14 . "(Reserved)")
			 (15 . "(Reserved)")
			 (16 . "Positioning")
			 (17 . "Networking")
			 (18 . "Rendering")
			 (19 . "Capturing")
			 (20 . "Object Transfer")
			 (21 . "Audio")
			 (22 . "Telephony")
			 (23 . "Information"))))
  "Bluetooth major service classes.")

(defconst bluetooth--class-major-dev-classes
  '((name . "major device class")
	(mask . #x1f00)
	(shift . -8)
	(fn . bluetooth--class-parse-major)
	(next . bluetooth--class-get-minor)
	(data . ((#x0 . ("Miscellaneous" . nil))
			 (#x1 . ("Computer" . bluetooth--class-computer-minor-classes))
			 (#x2 . ("Phone" . bluetooth--class-phone-minor-classes))
			 (#x3 . ("LAN/Network Access point" .
					 bluetooth--class-network-minor-classes))
			 (#x4 . ("Audio/Video" . bluetooth--class-av-minor-classes))
			 (#x5 . ("Peripheral" . bluetooth--class-peripheral-minor-classes))
			 (#x6 . ("Imaging" . bluetooth--class-imaging-minor-classes))
			 (#x7 . ("Wearable" . bluetooth--class-wearable-minor-classes))
			 (#x8 . ("Toy" . bluetooth--class-toy-minor-classes))
			 (#x9 . ("Health" . bluetooth--class-health-minor-classes))
			 (#xf . ("Unspecified" . nil)))))
  "Bluetooth major device classes.")

(defconst bluetooth--class-computer-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x0 . "Uncategorized")
			 (#x1 . "Desktop workstation")
			 (#x2 . "Server-class computer")
			 (#x3 . "Laptop")
			 (#x4 . "Handheld PC/PDA (clamshell)")
			 (#x5 . "Palm-size PC/PDA")
			 (#x6 . "Wearable computer (watch size)")
			 (#x7 . "Tablet"))))
  "Bluetooth computer minor classes.")

(defconst bluetooth--class-phone-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x0 . "Uncategorized, code for device not assigned")
			 (#x1 . "Cellular")
			 (#x2 . "Cordless")
			 (#x3 . "Smartphone")
			 (#x4 . "Wired modem or voice gateway")
			 (#x5 . "Common ISDN access"))))
  "Bluetooth phone minor classes.")

(defconst bluetooth--class-network-minor-classes
  '((name . "minor device class")
	(mask . #xe0)
	(shift . -5)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x0 . "Fully available")
			 (#x1 . "1% to 17% utilized")
			 (#x2 . "17% to 33% utilized")
			 (#x3 . "33% to 50% utilized")
			 (#x4 . "50% to 67% utilized")
			 (#x5 . "67% to 83% utilized")
			 (#x6 . "83% to 99% utilized")
			 (#x7 . "No service available"))))
  "Bluetooth LAN/network minor classes.")

(defconst bluetooth--class-av-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x0 . "Uncategorized")
			 (#x1 . "Wearable headset device")
			 (#x2 . "Hands-free device")
			 (#x3 . "(Reserved)")
			 (#x4 . "Microphone")
			 (#x5 . "Loudspeaker")
			 (#x6 . "Headphones")
			 (#x7 . "Portable audio")
			 (#x8 . "Car audio")
			 (#x9 . "Set-top box")
			 (#xa . "HiFi audio device")
			 (#xb . "VCR")
			 (#xc . "Video camera")
			 (#xd . "Camcorder")
			 (#xe . "Video monitor")
			 (#xf . "Video display and loudspeaker")
			 (#x10 . "Video conferencing")
			 (#x11 . "(Reserved)")
			 (#x12 . "Gaming/toy"))))
  "Bluetooth audio/video minor classes.")

(defconst bluetooth--class-peripheral-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-peripheral)
	(next . nil)
	(data . ((#x30 . ((#x00 . "Not keyboard/not pointing device")
					  (#x10 . "Keyboard")
					  (#x20 . "Pointing device")
					  (#x30 . "Combo keyboard/pointing device")))
			 (#xf . ((#x0 . "Uncategorized device")
					 (#x1 . "Joystick")
					 (#x2 . "Gamepad")
					 (#x3 . "Remote control")
					 (#x4 . "Sensing device")
					 (#x5 . "Digitizer tablet")
					 (#x6 . "Card reader")
					 (#x7 . "Digital pen")
					 (#x8 . "Handheld scanner")
					 (#x9 . "Handheld gestural input device"))))))
  "Bluetooth peripheral minor classes.")

(defconst bluetooth--class-imaging-minor-classes
  '((name . "minor device class")
	(mask . #xf0)
	(shift . 0)
	(fn . bluetooth--class-parse-bitfield)
	(next . nil)
	(data . ((4 . "Display")
			 (5 . "Camera")
			 (6 . "Scanner")
			 (7 . "Printer"))))
  "Bluetooth imaging minor class bits (inclusive).")

(defconst bluetooth--class-wearable-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x1 . "Wristwatch")
			 (#x2 . "Pager")
			 (#x3 . "Jacket")
			 (#x4 . "Helmet")
			 (#x5 . "Glasses"))))
  "Bluetooth wearable minor classes.")

(defconst bluetooth--class-toy-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x1 . "Robot")
			 (#x2 . "Vehicle")
			 (#x3 . "Doll/action figure")
			 (#x4 . "Controller")
			 (#x5 . "Game"))))
  "Bluetooth toy minor classes.")

(defconst bluetooth--class-health-minor-classes
  '((name . "minor device class")
	(mask . #xfc)
	(shift . -2)
	(fn . bluetooth--class-parse-value)
	(next . nil)
	(data . ((#x0 . "Undefined")
			 (#x1 . "Blood pressure monitor")
			 (#x2 . "Thermometer")
			 (#x3 . "Weighing scale")
			 (#x4 . "Glucose meter")
			 (#x5 . "Pulse oximeter")
			 (#x6 . "Heart/pulse rate monitor")
			 (#x7 . "Health data display")
			 (#x8 . "Step counter")
			 (#x9 . "Body composition analyzer")
			 (#xa . "Peak flow monitor")
			 (#xb . "Medication monitor")
			 (#xc . "Knee prosthesis")
			 (#xd . "Ankle prosthesis")
			 (#xe . "Generic health manager")
			 (#xf . "Personal mobility device"))))
  "Bluetooth health minor classes.")

(define-obsolete-variable-alias 'bluetooth--service-class-uuid-alist
  'bluetooth--service-class-uuids "0.2")

(defconst bluetooth--service-class-uuids
  #s(hash-table
	 size 120 data
	 (#x1000
	  ("ServiceDiscoveryServerServiceClassID"
	   "Bluetooth Core Specification")
	  #x1001 ("BrowseGroupDescriptorServiceClassID"
			  "Bluetooth Core Specification")
	  #x1101 ("SerialPort" "Serial Port Profile (SPP)")
	  #x1102 ("LANAccessUsingPPP" "LAN Access Profile")
	  #x1103 ("DialupNetworking" "Dial-up Networking Profile (DUN)")
	  #x1104 ("IrMCSync" "Synchronization Profile (SYNC)")
	  #x1105 ("OBEXObjectPush" "Object Push Profile (OPP)")
	  #x1106 ("OBEXFileTransfer" "File Transfer Profile (FTP)")
	  #x1107 ("IrMCSyncCommand" "Synchronization Profile (SYNC)")
	  #x1108 ("Headset" "Headset Profile (HSP)")
	  #x1109 ("CordlessTelephony" "Cordless Telephony Profile (CTP)")
	  #x110A ("AudioSource" "Advanced Audio Distribution Profile (A2DP)")
	  #x110B ("AudioSink" "Advanced Audio Distribution Profile (A2DP)")
	  #x110C ("A/V_RemoteControlTarget"
			  "Audio/Video Remote Control Profile (AVRCP)")
	  #x110D ("AdvancedAudioDistribution"
			  "Advanced Audio Distribution Profile (A2DP)")
	  #x110E ("A/V_RemoteControl"
			  "Audio/Video Remote Control Profile (AVRCP)")
	  #x110F ("A/V_RemoteControlController"
			  "Audio/Video Remote Control Profile (AVRCP)")
	  #x1110 ("Intercom" "Intercom Profile (ICP)")
	  #x1111 ("Fax" "Fax Profile (FAX)")
	  #x1112 ("Headset - Audio Gateway (AG)" "Headset Profile (HSP)")
	  #x1113 ("WAP" (concat "Interoperability Requirements for Bluetooth"
							" technology as a WAP, Bluetooth SIG"))
	  #x1114 ("WAP_CLIENT"
			  (concat "Interoperability Requirements for"
					  " Bluetooth technology as a WAP, Bluetooth SIG"))
	  #x1115 ("PANU" "Personal Area Networking Profile (PAN)")
	  #x1116 ("NAP" "Personal Area Networking Profile (PAN)")
	  #x1117 ("GN" "Personal Area Networking Profile (PAN)")
	  #x1118 ("DirectPrinting" "Basic Printing Profile (BPP)")
	  #x1119 ("ReferencePrinting" "See Basic Printing Profile (BPP)")
	  #x111A ("Basic Imaging Profile" "Basic Imaging Profile (BIP)")
	  #x111B ("ImagingResponder" "Basic Imaging Profile (BIP)")
	  #x111C ("ImagingAutomaticArchive" "Basic Imaging Profile (BIP)")
	  #x111D ("ImagingReferencedObjects" "Basic Imaging Profile (BIP)")
	  #x111E ("Handsfree" "Hands-Free Profile (HFP)")
	  #x111F ("HandsfreeAudioGateway" "Hands-free Profile (HFP)")
	  #x1120 ("DirectPrintingReferenceObjectsService"
			  "Basic Printing Profile (BPP)")
	  #x1121 ("ReflectedUI" "Basic Printing Profile (BPP)")
	  #x1122 ("BasicPrinting" "Basic Printing Profile (BPP)")
	  #x1123 ("PrintingStatus" "Basic Printing Profile (BPP)")
	  #x1124 ("HumanInterfaceDeviceService" "Human Interface Device (HID)")
	  #x1125 ("HardcopyCableReplacement"
			  "Hardcopy Cable Replacement Profile (HCRP)")
	  #x1126 ("HCR_Print" "Hardcopy Cable Replacement Profile (HCRP)")
	  #x1127 ("HCR_Scan" "Hardcopy Cable Replacement Profile (HCRP)")
	  #x1128 ("Common_ISDN_Access" "Common ISDN Access Profile (CIP)")
	  #x112D ("SIM_Access" "SIM Access Profile (SAP)")
	  #x112E ("Phonebook Access - PCE" "Phonebook Access Profile (PBAP)")
	  #x112F ("Phonebook Access - PSE" "Phonebook Access Profile (PBAP)")
	  #x1130 ("Phonebook Access" "Phonebook Access Profile (PBAP)")
	  #x1131 ("Headset - HS" "Headset Profile (HSP)")
	  #x1132 ("Message Access Server" "Message Access Profile (MAP)")
	  #x1133 ("Message Notification Server" "Message Access Profile (MAP)")
	  #x1134 ("Message Access Profile" "Message Access Profile (MAP)")
	  #x1135 ("GNSS" "Global Navigation Satellite System Profile (GNSS)")
	  #x1136 ("GNSS_Server"
			  "Global Navigation Satellite System Profile (GNSS)")
	  #x1137 ("​3D Display" "​3D Synchronization Profile (3DSP)")
	  ​#x1138 ("​3D Glasses" "​3D Synchronization Profile (3DSP)")
	  #x1139 ("​3D Synchronization" "​3D Synchronization Profile (3DSP)")
	  ​#x113A ("​MPS Profile UUID" "​Multi-Profile Specification (MPS)")
	  ​#x113B ("​MPS SC UUID" "​Multi-Profile Specification (MPS)")
	  ​#x113C ("​CTN Access Service​"
			  "​Calendar, Task, and Notes (CTN) Profile")
	  ​#x113D ("​CTN Notification Service​"
			  "​​Calendar Tasks and Notes (CTN) Profile")
	  ​#x113E ("​CTN Profile" "​​Calendar Tasks and Notes (CTN) Profile")
	  #x1200 ("PnPInformation" "Device Identification (DID)")
	  #x1201 ("GenericNetworking" "N/A")
	  #x1202 ("GenericFileTransfer" "N/A")
	  #x1203 ("GenericAudio" "N/A")
	  #x1204 ("GenericTelephony" "N/A")
	  #x1205 ("UPNP_Service" "Enhanced Service Discovery Profile (ESDP)")
	  #x1206 ("UPNP_IP_Service" "Enhanced Service Discovery Profile (ESDP)")
	  #x1300 ("ESDP_UPNP_IP_PAN" "Enhanced Service Discovery Profile (ESDP)")
	  #x1301 ("ESDP_UPNP_IP_LAP" "Enhanced Service Discovery Profile (ESDP)")
	  #x1302 ("ESDP_UPNP_L2CAP" "Enhanced Service Discovery Profile (ESDP)")
	  #x1303 ("VideoSource" "Video Distribution Profile (VDP)")
	  #x1304 ("VideoSink" "Video Distribution Profile (VDP)")
	  #x1305 ("VideoDistribution" "Video Distribution Profile (VDP)")
	  #x1400 ("HDP" "Health Device Profile")
	  #x1401 ("HDP Source" "Health Device Profile (HDP)")
	  #x1402 ("HDP Sink" "Health Device Profile (HDP)")))
  "Bluetooth service class UUIDs.")

(define-obsolete-variable-alias 'bluetooth--gatt-service-uuid-alist
  'bluetooth--gatt-service-uuids "0.2")

(defconst bluetooth--gatt-service-uuids
  #s(hash-table
	 size 45 data
	 (#x1800
	  ("Generic Access" "org.bluetooth.service.generic_access" "GSS")
	  #x1811 ("Alert Notification Service"
			  "org.bluetooth.service.alert_notification" "GSS")
	  #x1815 ("Automation IO" "org.bluetooth.service.automation_io" "GSS")
	  #x180F ("Battery Service" "org.bluetooth.service.battery_service"
			  "GSS")
	  #x183B ("Binary Sensor" "GATT Service UUID" "BSS")
	  #x1810 ("Blood Pressure" "org.bluetooth.service.blood_pressure" "GSS")
	  #x181B ("Body Composition" "org.bluetooth.service.body_composition"
			  "GSS")
	  #x181E ("Bond Management Service"
			  "org.bluetooth.service.bond_management" "GSS")
	  #x181F ("Continuous Glucose Monitoring"
			  "org.bluetooth.service.continuous_glucose_monitoring" "GSS")
	  #x1805 ("Current Time Service" "org.bluetooth.service.current_time"
			  "GSS")
	  #x1818 ("Cycling Power" "org.bluetooth.service.cycling_power" "GSS")
	  #x1816 ("Cycling Speed and Cadence"
			  "org.bluetooth.service.cycling_speed_and_cadence" "GSS")
	  #x180A ("Device Information" "org.bluetooth.service.device_information"
			  "GSS")
	  #x183C ("Emergency Configuration" "GATT Service UUID" "EMCS")
	  #x181A ("Environmental Sensing"
			  "org.bluetooth.service.environmental_sensing" "GSS")
	  #x1826 ("Fitness Machine" "org.bluetooth.service.fitness_machine"
			  "GSS")
	  #x1801 ("Generic Attribute" "org.bluetooth.service.generic_attribute"
			  "GSS")
	  #x1808 ("Glucose" "org.bluetooth.service.glucose" "GSS")
	  #x1809 ("Health Thermometer" "org.bluetooth.service.health_thermometer"
			  "GSS")
	  #x180D ("Heart Rate" "org.bluetooth.service.heart_rate" "GSS")
	  #x1823 ("HTTP Proxy" "org.bluetooth.service.http_proxy" "GSS")
	  #x1812 ("Human Interface Device"
			  "org.bluetooth.service.human_interface_device" "GSS")
	  #x1802 ("Immediate Alert" "org.bluetooth.service.immediate_alert"
			  "GSS")
	  #x1821 ("Indoor Positioning" "org.bluetooth.service.indoor_positioning"
			  "GSS")
	  #x183A ("Insulin Delivery" "org.bluetooth.service.insulin_delivery"
			  "GSS")
	  #x1820 ("Internet Protocol Support Service"
			  "org.bluetooth.service.internet_protocol_support" "GSS")
	  #x1803 ("Link Loss" "org.bluetooth.service.link_loss" "GSS")
	  #x1819 ("Location and Navigation"
			  "org.bluetooth.service.location_and_navigation" "GSS")
	  #x1827 ("Mesh Provisioning Service"
			  "org.bluetooth.service.mesh_provisioning" "GSS")
	  #x1828 ("Mesh Proxy Service" "org.bluetooth.service.mesh_proxy" "GSS")
	  #x1807 ("Next DST Change Service"
			  "org.bluetooth.service.next_dst_change" "GSS")
	  #x1825 ("Object Transfer Service"
			  "org.bluetooth.service.object_transfer" "GSS")
	  #x180E ("Phone Alert Status Service"
			  "org.bluetooth.service.phone_alert_status" "GSS")
	  #x1822 ("Pulse Oximeter Service" "org.bluetooth.service.pulse_oximeter"
			  "GSS")
	  #x1829 ("Reconnection Configuration"
			  "org.bluetooth.service.reconnection_configuration" "GSS")
	  #x1806 ("Reference Time Update Service"
			  "org.bluetooth.service.reference_time_update" "GSS")
	  #x1814 ("Running Speed and Cadence"
			  "org.bluetooth.service.running_speed_and_cadence" "GSS")
	  #x1813 ("Scan Parameters" "org.bluetooth.service.scan_parameters"
			  "GSS")
	  #x1824 ("Transport Discovery"
			  "org.bluetooth.service.transport_discovery" "GSS")
	  #x1804 ("Tx Power" "org.bluetooth.service.tx_power" "GSS")
	  #x181C ("User Data" "org.bluetooth.service.user_data" "GSS")
	  #x181D ("Weight Scale" "org.bluetooth.service.weight_scale" "GSS")))
  "Bluetooth GATT service UUIDs.")

(define-obsolete-variable-alias 'bluetooth--sdo-uuid-alist
  'bluetooth--sdo-uuids "0.2")

(defconst bluetooth--sdo-uuids
  #s(hash-table
	 size 10 data
	 (#xFFF9
	  ("Fast IDentity Online Alliance (FIDO)"
	   "FIDO2 secure client-to-authenticator transport")
	  #xFFFA ("ASTM International" "ASTM Remote ID")
	  #xFFFB ("Thread Group, Inc." "Direct Thread Commissioning")
	  #xFFFC ("AirFuel Alliance"
			  "Wireless Power Transfer (WPT) Service")
	  #xFFFD ("Fast IDentity Online Alliance"
			  "Universal Second Factor Authenticator Service")
	  #xFFFE ("AirFuel Alliance" "Wireless Power Transfer Service")))
  "Bluetooth standards development organizations UUIDS.")


;;;; Bluetooth member UUIDs
;; Last updated: 19. Sep 2020

(define-obsolete-variable-alias 'bluetooth--member-uuid-alist
  'bluetooth--member-uuids "0.2")

(defconst bluetooth--member-uuids
  #s(hash-table
	 size 500 data
	 (#xFEFF
	  ("GN Netcom")
	  #xFEFE ("GN ReSound A/S")
	  #xFEFD ("Gimbal, Inc.")
	  #xFEFC ("Gimbal, Inc.")
	  #xFEFB ("Telit Wireless Solutions (Formerly Stollmann E+V GmbH)")
	  #xFEFA ("PayPal, Inc.")
	  #xFEF9 ("PayPal, Inc.")
	  #xFEF8 ("Aplix Corporation")
	  #xFEF7 ("Aplix Corporation")
	  #xFEF6 ("Wicentric, Inc.")
	  #xFEF5 ("Dialog Semiconductor GmbH")
	  #xFEF4 ("Google")
	  #xFEF3 ("Google")
	  #xFEF2 ("CSR")
	  #xFEF1 ("CSR")
	  #xFEF0 ("Intel")
	  #xFEEF ("Polar Electro Oy ")
	  #xFEEE ("Polar Electro Oy ")
	  #xFEED ("Tile, Inc.")
	  #xFEEC ("Tile, Inc.")
	  #xFEEB ("Swirl Networks, Inc.")
	  #xFEEA ("Swirl Networks, Inc.")
	  #xFEE9 ("Quintic Corp.")
	  #xFEE8 ("Quintic Corp.")
	  #xFEE7 ("Tencent Holdings Limited.")
	  #xFEE6 ("Silvair, Inc.")
	  #xFEE5 ("Nordic Semiconductor ASA")
	  #xFEE4 ("Nordic Semiconductor ASA")
	  #xFEE3 ("Anki, Inc.")
	  #xFEE2 ("Anki, Inc.")
	  #xFEE1 ("Anhui Huami Information Technology Co., Ltd. ")
	  #xFEE0 ("Anhui Huami Information Technology Co., Ltd. ")
	  #xFEDF ("Design SHIFT")
	  #xFEDE ("Coin, Inc.")
	  #xFEDD ("Jawbone")
	  #xFEDC ("Jawbone")
	  #xFEDB ("Perka, Inc.")
	  #xFEDA ("ISSC Technologies Corp. ")
	  #xFED9 ("Pebble Technology Corporation")
	  #xFED8 ("Google")
	  #xFED7 ("Broadcom")
	  #xFED6 ("Broadcom")
	  #xFED5 ("Plantronics Inc.")
	  #xFED4 ("Apple, Inc.")
	  #xFED3 ("Apple, Inc.")
	  #xFED2 ("Apple, Inc.")
	  #xFED1 ("Apple, Inc.")
	  #xFED0 ("Apple, Inc.")
	  #xFECF ("Apple, Inc.")
	  #xFECE ("Apple, Inc.")
	  #xFECD ("Apple, Inc.")
	  #xFECC ("Apple, Inc.")
	  #xFECB ("Apple, Inc.")
	  #xFECA ("Apple, Inc.")
	  #xFEC9 ("Apple, Inc.")
	  #xFEC8 ("Apple, Inc.")
	  #xFEC7 ("Apple, Inc.")
	  #xFEC6 ("Kocomojo, LLC")
	  #xFEC5 ("Realtek Semiconductor Corp.")
	  #xFEC4 ("PLUS Location Systems")
	  #xFEC3 ("360fly, Inc.")
	  #xFEC2 ("Blue Spark Technologies, Inc.")
	  #xFEC1 ("KDDI Corporation")
	  #xFEC0 ("KDDI Corporation")
	  #xFEBF ("Nod, Inc.")
	  #xFEBE ("Bose Corporation")
	  #xFEBD ("Clover Network, Inc")
	  #xFEBC ("Dexcom Inc")
	  #xFEBB ("adafruit industries")
	  #xFEBA ("Tencent Holdings Limited")
	  #xFEB9 ("LG Electronics")
	  #xFEB8 ("Facebook, Inc.")
	  #xFEB7 ("Facebook, Inc.")
	  #xFEB6 ("Vencer Co., Ltd")
	  #xFEB5 ("WiSilica Inc.")
	  #xFEB4 ("WiSilica Inc.")
	  #xFEB3 ("Taobao")
	  #xFEB2 ("Microsoft Corporation")
	  #xFEB1 ("Electronics Tomorrow Limited")
	  #xFEB0 ("Nest Labs Inc")
	  #xFEAF ("Nest Labs Inc")
	  #xFEAE ("Nokia")
	  #xFEAD ("Nokia")
	  #xFEAC ("Nokia")
	  #xFEAB ("Nokia")
	  #xFEAA ("Google")
	  #xFEA9 ("Savant Systems LLC")
	  #xFEA8 ("Savant Systems LLC")
	  #xFEA7 ("UTC Fire and Security")
	  #xFEA6 ("GoPro, Inc.")
	  #xFEA5 ("GoPro, Inc.")
	  #xFEA4 ("Paxton Access Ltd")
	  #xFEA3 ("ITT Industries")
	  #xFEA2 ("Intrepid Control Systems, Inc.")
	  #xFEA1 ("Intrepid Control Systems, Inc.")
	  #xFEA0 ("Google")
	  #xFE9F ("Google")
	  #xFE9E ("Dialog Semiconductor B.V.")
	  #xFE9D ("Mobiquity Networks Inc")
	  #xFE9C ("GSI Laboratories, Inc.")
	  #xFE9B ("Samsara Networks, Inc")
	  #xFE9A ("Estimote")
	  #xFE99 ("Currant Inc")
	  #xFE98 ("Currant Inc")
	  #xFE97 ("Tesla Motors Inc.")
	  #xFE96 ("Tesla Motors Inc.")
	  #xFE95 ("Xiaomi Inc.")
	  #xFE94 ("OttoQ In")
	  #xFE93 ("OttoQ In")
	  #xFE92 ("Jarden Safety & Security")
	  #xFE91 ("Shanghai Imilab Technology Co., Ltd")
	  #xFE90 ("JUMA")
	  #xFE8F ("CSR")
	  #xFE8E ("ARM Ltd")
	  #xFE8D ("Interaxon Inc.")
	  #xFE8C ("TRON Forum")
	  #xFE8B ("Apple, Inc.")
	  #xFE8A ("Apple, Inc.")
	  #xFE89 ("B&O Play A/S")
	  #xFE88 ("SALTO SYSTEMS S.L.")
	  #xFE87 ("Qingdao Yeelink Information Technology Co., Ltd. ( 青岛亿联客信息技术有限公司 )")
	  #xFE86 ("HUAWEI Technologies Co., Ltd. ( 华为技术有限公司 )")
	  #xFE85 ("RF Digital Corp")
	  #xFE84 ("RF Digital Corp")
	  #xFE83 ("Blue Bite")
	  #xFE82 ("Medtronic Inc.")
	  #xFE81 ("Medtronic Inc.")
	  #xFE80 ("Doppler Lab")
	  #xFE7F ("Doppler Lab")
	  #xFE7E ("Awear Solutions Ltd")
	  #xFE7D ("Aterica Health Inc.")
	  #xFE7C ("Telit Wireless Solutions (Formerly Stollmann E+V GmbH)")
	  #xFE7B ("Orion Labs, Inc.")
	  #xFE7A ("Bragi GmbH")
	  #xFE79 ("Zebra Technologies")
	  #xFE78 ("Hewlett-Packard Company")
	  #xFE77 ("Hewlett-Packard Company")
	  #xFE76 ("TangoMe")
	  #xFE75 ("TangoMe")
	  #xFE74 ("unwire")
	  #xFE73 ("Abbott (formerly St. Jude Medical, Inc.)")
	  #xFE72 ("Abbott (formerly St. Jude Medical, Inc.)")
	  #xFE71 ("Plume Design Inc")
	  #xFE70 ("Beijing Jingdong Century Trading Co., Ltd.")
	  #xFE6F ("LINE Corporation")
	  #xFE6E ("The University of Tokyo")
	  #xFE6D ("The University of Tokyo")
	  #xFE6C ("TASER International, Inc.")
	  #xFE6B ("TASER International, Inc.")
	  #xFE6A ("Kontakt Micro-Location Sp. z o.o.")
	  #xFE69 ("Capsule Technologies Inc.")
	  #xFE68 ("Capsule Technologies Inc.")
	  #xFE67 ("Lab Sensor Solutions")
	  #xFE66 ("Intel Corporation")
	  #xFE65 ("CHIPOLO d.o.o.")
	  #xFE64 ("Siemens AG")
	  #xFE63 ("Connected Yard, Inc.")
	  #xFE62 ("Indagem Tech LLC")
	  #xFE61 ("Logitech International SA")
	  #xFE60 ("Lierda Science & Technology Group Co., Ltd.")
	  #xFE5F ("Eyefi, Inc.")
	  #xFE5E ("Plastc Corporation")
	  #xFE5D ("Grundfos A/S")
	  #xFE5C ("million hunters GmbH")
	  #xFE5B ("GT-tronics HK Ltd")
	  #xFE5A ("Cronologics Corporation")
	  #xFE59 ("Nordic Semiconductor ASA")
	  #xFE58 ("Nordic Semiconductor ASA")
	  #xFE57 ("Dotted Labs")
	  #xFE56 ("Google Inc.")
	  #xFE55 ("Google Inc.")
	  #xFE54 ("Motiv, Inc.")
	  #xFE53 ("3M")
	  #xFE52 ("SetPoint Medical")
	  #xFE51 ("SRAM")
	  #xFE50 ("Google Inc.")
	  #xFE4F ("Molekule, Inc.")
	  #xFE4E ("NTT docomo")
	  #xFE4D ("Casambi Technologies Oy")
	  #xFE4C ("Volkswagen AG")
	  #xFE4B ("Signify Netherlands B.V. (formerly Philips Lighting B.V.)")
	  #xFE4A ("OMRON HEALTHCARE Co., Ltd.")
	  #xFE49 ("SenionLab AB")
	  #xFE48 ("General Motors")
	  #xFE47 ("General Motors")
	  #xFE46 ("B&O Play A/S")
	  #xFE45 ("Snapchat Inc")
	  #xFE44 ("SK Telecom")
	  #xFE43 ("Andreas Stihl AG & Co. KG")
	  #xFE42 ("Nets A/S")
	  #xFE41 ("Inugo Systems Limited")
	  #xFE40 ("Inugo Systems Limited")
	  #xFE3F ("Friday Labs Limited")
	  #xFE3E ("BD Medical")
	  #xFE3D ("BD Medical")
	  #xFE3C ("alibaba")
	  #xFE3B ("Dobly Laboratories")
	  #xFE3A ("TTS Tooltechnic Systems AG & Co. KG")
	  #xFE39 ("TTS Tooltechnic Systems AG & Co. KG")
	  #xFE38 ("Spaceek LTD")
	  #xFE37 ("Spaceek LTD")
	  #xFE36 ("HUAWEI Technologies Co., Ltd")
	  #xFE35 ("HUAWEI Technologies Co., Ltd")
	  #xFE34 ("SmallLoop LLC")
	  #xFE33 ("CHIPOLO d.o.o.")
	  #xFE32 ("Pro-Mark, Inc.")
	  #xFE31 ("Volkswagen AG")
	  #xFE30 ("Volkswagen AG")
	  #xFE2F ("CRESCO Wireless, Inc")
	  #xFE2E ("ERi,Inc.")
	  #xFE2D ("SMART INNOVATION Co., Ltd")
	  #xFE2C ("Google")
	  #xFE2B ("ITT Industries")
	  #xFE2A ("DaisyWorks, Inc.")
	  #xFE29 ("Gibson Innovations")
	  #xFE28 ("Ayla Networks")
	  #xFE27 ("Google")
	  #xFE26 ("Google")
	  #xFE25 ("Apple, Inc.")
	  #xFE24 ("August Home Inc")
	  #xFE23 ("Zoll Medical Corporation")
	  #xFE22 ("Zoll Medical Corporation")
	  #xFE21 ("Bose Corporation")
	  #xFE20 ("Emerson")
	  #xFE1F ("Garmin International, Inc.")
	  #xFE1E ("Smart Innovations Co., Ltd")
	  #xFE1D ("Illuminati Instrument Corporation")
	  #xFE1C ("NetMedia, Inc.")
	  #xFE1B ("Tyto Life LLC")
	  #xFE1A ("Tyto Life LLC")
	  #xFE19 ("Google, Inc")
	  #xFE18 ("Runtime, Inc.")
	  #xFE17 ("Telit Wireless Solutions GmbH")
	  #xFE16 ("Footmarks, Inc.")
	  #xFE15 ("Amazon.com Services, Inc.")
	  #xFE14 ("Flextronics International USA Inc.")
	  #xFE13 ("Apple Inc.")
	  #xFE12 ("M-Way Solutions GmbH")
	  #xFE11 ("GMC-I Messtechnik GmbH")
	  #xFE10 ("Lapis Semiconductor Co., Ltd.")
	  #xFE0F ("Signify Netherlands B.V. (formerly Philips Lighting B.V.)")
	  #xFE0E ("Setec Pty Ltd")
	  #xFE0D ("Procter & Gamble")
	  #xFE0C ("Procter & Gamble")
	  #xFE0B ("ruwido austria gmbh")
	  #xFE0A ("ruwido austria gmbh")
	  #xFE09 ("Pillsy, Inc.")
	  #xFE08 ("Microsoft")
	  #xFE07 ("Sonos, Inc.")
	  #xFE06 ("Qualcomm Technologies, Inc.")
	  #xFE05 ("CORE Transport Technologies NZ Limited")
	  #xFE04 ("OpenPath Security Inc")
	  #xFE03 ("Amazon.com Services, Inc.")
	  #xFE02 ("Robert Bosch GmbH")
	  #xFE01 ("Duracell U.S. Operations Inc.")
	  #xFE00 ("Amazon.com Services, Inc.")
	  #xFDFF ("OSRAM GmbH")
	  #xFDFE ("ADHERIUM(NZ) LIMITED")
	  #xFDFD ("RecursiveSoft Inc.")
	  #xFDFC ("Optrel AG")
	  #xFDFB ("Tandem Diabetes Care")
	  #xFDFA ("Tandem Diabetes Care")
	  #xFDF9 ("INIA")
	  #xFDF8 ("Onvocal")
	  #xFDF7 ("HP Inc.")
	  #xFDF6 ("AIAIAI ApS")
	  #xFDF5 ("Milwaukee Electric Tools")
	  #xFDF4 ("O. E. M. Controls, Inc.")
	  #xFDF3 ("Amersports")
	  #xFDF2 ("AMICCOM Electronics Corporation")
	  #xFDF1 ("LAMPLIGHT Co., Ltd")
	  #xFDF0 ("Google Inc.")
	  #xFDEF ("ART AND PROGRAM, INC.")
	  #xFDEE ("Huawei Technologies Co., Ltd.")
	  #xFDED ("Pole Star")
	  #xFDEC ("Mannkind Corporation")
	  #xFDEB ("Syntronix Corporation")
	  #xFDEA ("SeeScan, Inc")
	  #xFDE9 ("Spacesaver Corporation")
	  #xFDE8 ("Robert Bosch GmbH")
	  #xFDE7 ("SECOM Co., LTD")
	  #xFDE6 ("Intelletto Technologies Inc")
	  #xFDE5 ("SMK Corporation")
	  #xFDE4 ("JUUL Labs, Inc.")
	  #xFDE3 ("Abbott Diabetes Care")
	  #xFDE2 ("Google Inc.")
	  #xFDE1 ("Fortin Electronic Systems")
	  #xFDE0 ("John Deere")
	  #xFDDF ("Harman International")
	  #xFDDE ("Noodle Technology Inc.")
	  #xFDDD ("Arch Systems Inc")
	  #xFDDC ("4iiii Innovations Inc.")
	  #xFDDB ("Samsung Electronics Co., Ltd.")
	  #xFDDA ("MHCS")
	  #xFDD9 ("Jiangsu Teranovo Tech Co., Ltd.")
	  #xFDD8 ("Jiangsu Teranovo Tech Co., Ltd.")
	  #xFDD7 ("Emerson")
	  #xFDD6 ("Ministry of Supply")
	  #xFDD5 ("Brompton Bicycle Ltd")
	  #xFDD4 ("LX Solutions Pty Limited")
	  #xFDD3 ("FUBA Automotive Electronics GmbH")
	  #xFDD2 ("Bose Corporation")
	  #xFDD1 ("Huawei Technologies Co., Ltd")
	  #xFDD0 ("Huawei Technologies Co., Ltd")
	  #xFDCF ("Nalu Medical, Inc")
	  #xFDCE ("SENNHEISER electronic GmbH & Co. KG")
	  #xFDCD ("Qingping Technology (Beijing) Co., Ltd.")
	  #xFDCC ("Shoof Technologies")
	  #xFDCB ("Meggitt SA")
	  #xFDCA ("Fortin Electronic Systems")
	  #xFDC9 ("Busch-Jaeger Elektro GmbH")
	  #xFDC8 ("Hach – Danaher")
	  #xFDC7 ("Eli Lilly and Company")
	  #xFDC6 ("Eli Lilly and Company")
	  #xFDC5 ("Automatic Labs")
	  #xFDC4 ("Simavita (Aust) Pty Ltd")
	  #xFDC3 ("Baidu Online Network Technology (Beijing) Co., Ltd")
	  #xFDC2 ("Baidu Online Network Technology (Beijing) Co., Ltd")
	  #xFDC1 ("Hunter Douglas")
	  #xFDC0 ("Hunter Douglas")
	  #xFDBF ("California Things Inc.")
	  #xFDBE ("California Things Inc.")
	  #xFDBD ("Clover Network, Inc.")
	  #xFDBC ("Emerson")
	  #xFDBB ("Profoto")
	  #xFDBA ("Comcast Cable Corporation")
	  #xFDB9 ("Comcast Cable Corporation")
	  #xFDB8 ("LivaNova USA Inc.")
	  #xFDB7 ("LivaNova USA Inc.")
	  #xFDB6 ("GWA Hygiene GmbH")
	  #xFDB5 ("ECSG")
	  #xFDB4 ("HP Inc")
	  #xFDB3 ("Audiodo AB")
	  #xFDB2 ("Portable Multimedia Ltd")
	  #xFDB1 ("Proxy Technologies, Inc.")
	  #xFDB0 ("Proxy Technologies, Inc.")
	  #xFDAF ("Wiliot LTD")
	  #xFDAE ("Houwa System Design, k.k.")
	  #xFDAD ("Houwa System Design, k.k.")
	  #xFDAC ("Tentacle Sync GmbH")
	  #xFDAB ("Xiaomi Inc.")
	  #xFDAA ("Xiaomi Inc.")
	  #xFDA9 ("Rhombus Systems, Inc.")
	  #xFDA8 ("PSA Peugeot Citroën")
	  #xFDA7 ("WWZN Information Technology Company Limited")
	  #xFDA6 ("WWZN Information Technology Company Limited")
	  #xFDA5 ("Neurostim OAB, Inc.")
	  #xFDA4 ("Inseego Corp.")
	  #xFDA3 ("Inseego Corp.")
	  #xFDA2 ("Groove X, Inc")
	  #xFDA1 ("Groove X, Inc")
	  #xFDA0 ("Secugen Corporation")
	  #xFD9F ("VitalTech Affiliates LLC")
	  #xFD9E ("The Coca-Cola Company")
	  #xFD9D ("Gastec Corporation")
	  #xFD9C ("Huawei Technologies Co., Ltd.")
	  #xFD9B ("Huawei Technologies Co., Ltd.")
	  #xFD9A ("Huawei Technologies Co., Ltd.")
	  #xFD99 ("ABB Oy")
	  #xFD98 ("Disney Worldwide Services, Inc.")
	  #xFD97 ("June Life, Inc.")
	  #xFD96 ("Google LLC")
	  #xFD95 ("Rigado")
	  #xFD94 ("Hewlett Packard Enterprise")
	  #xFD93 ("Bayerische Motoren Werke AG")
	  #xFD92 ("Qualcomm Technologies International, Ltd. (QTIL)")
	  #xFD91 ("Groove X, Inc.")
	  #xFD90 ("Guangzhou SuperSound Information Technology Co., Ltd")
	  #xFD8F ("Matrix ComSec Pvt. Ltd.")
	  #xFD8E ("Motorola Solutions")
	  #xFD8D ("quip NYC Inc.")
	  #xFD8C ("Google LLC")
	  #xFD8B ("Jigowatts Inc.")
	  #xFD8A ("Signify Netherlands B.V.")
	  #xFD89 ("Urbanminded LTD")
	  #xFD88 ("Urbanminded LTD")
	  #xFD87 ("Google LLC")
	  #xFD86 ("Abbott")
	  #xFD85 ("Husqvarna AB")
	  #xFD84 ("Tile, Inc.")
	  #xFD83 ("iNFORM Technology GmbH")
	  #xFD82 ("Sony Corporation")
	  #xFD81 ("CANDY HOUSE, Inc.")
	  #xFD80 ("Phindex Technologies, Inc")
	  #xFD7F ("Husqvarna AB")
	  #xFD7E ("Samsung Electronics Co., Ltd.")
	  #xFD7D ("Center for Advanced Research Wernher Von Braun")
	  #xFD7C ("Toshiba Information Systems (Japan) Corporation")
	  #xFD7B ("WYZE LABS, INC.")
	  #xFD7A ("Withings")
	  #xFD79 ("Withings")
	  #xFD78 ("Withings")
	  #xFD77 ("Withings")
	  #xFD76 ("Insulet Corporation")
	  #xFD75 ("Insulet Corporation")
	  #xFD74 ("BRControls Products BV")
	  #xFD73 ("BRControls Products BV")
	  #xFD72 ("Logitech International SA")
	  #xFD71 ("GN Hearing A/S")
	  #xFD70 ("GuangDong Oppo Mobile Telecommunications Corp., Ltd.")
	  #xFD6F ("Apple, Inc.")
	  #xFD6E ("Polidea sp. z o.o.")
	  #xFD6D ("Sigma Elektro GmbH")
	  #xFD6C ("Samsung Electronics Co., Ltd.")
	  #xFD6B ("rapitag GmbH")
	  #xFD6A ("Emerson")
	  #xFD69 ("Samsung Electronics Co., Ltd.")
	  #xFD68 ("Ubique Innovation AG")
	  #xFD67 ("Montblanc Simplo GmbH")
	  #xFD66 ("Zebra Technologies Corporation")
	  #xFD65 ("Razer Inc.")
	  #xFD64 ("INRIA")
	  #xFD63 ("Fitbit, Inc.")
	  #xFD62 ("Fitbit, Inc.")
	  #xFD61 ("Arendi AG")
	  #xFD60 ("Sercomm Corporation")
	  #xFD5F ("Oculus VR, LLC")
	  #xFD5E ("Tapkey GmbH")
	  #xFD5D ("maxon motor ltd.")
	  #xFD5C ("React Mobile")
	  #xFD5B ("V2SOFT INC.")
	  #xFD5A ("Samsung Electronics Co., Ltd.")
	  #xFD59 ("Samsung Electronics Co., Ltd.")
	  #xFD58 ("Volvo Car Corporation")
	  #xFD57 ("Volvo Car Corporation")
	  #xFD56 ("Resmed Ltd")
	  #xFD55 ("Braveheart Wireless, Inc.")
	  #xFD54 ("Qingdao Haier Technology Co., Ltd.")
	  #xFD53 ("PCI Private Limited")
	  #xFD52 ("UTC Fire and Security")
	  #xFD51 ("UTC Fire and Security")
	  #xFD50 ("Hangzhou Tuya Information Technology Co., Ltd")
	  #xFD4F ("Forkbeard Technologies AS")
	  #xFD4E ("70mai Co., Ltd.")
	  #xFD4D ("70mai Co., Ltd.")
	  #xFD4C ("Adolf Wuerth GmbH & Co KG")
	  #xFD4B ("Samsung Electronics Co., Ltd.")
	  #xFD4A ("Sigma Elektro GmbH")
	  #xFD49 ("Panasonic Corporation")
	  #xFD48 ("Geberit International AG")
	  #xFD47 ("Liberty Global Inc.")
	  #xFD46 ("Lemco IKE")
	  #xFD45 ("GB Solution co., Ltd")
	  #xFD44 ("Apple Inc.")
	  #xFD43 ("Apple Inc.")
	  #xFD42 ("Globe (Jiangsu) Co., Ltd")))
  "Bluetooth member UUIDs.")


;;;; service and class parsing code

(define-obsolete-variable-alias 'bluetooth--uuid-alists 'bluetooth--uuids "0.2")

(defconst bluetooth--uuids
  `((#xfff0 . ,bluetooth--sdo-uuids)
	(#xfd00 . ,bluetooth--member-uuids)
	(#x1800 . ,bluetooth--gatt-service-uuids)
	(#x0 . ,bluetooth--service-class-uuids))
  "Bluetooth UUID tables sorted by beginning of range.")

(defun bluetooth--parse-service-class-uuid (uuid)
  "Parse UUID and return short and long service class names."
  (let ((uuid-re (rx (seq bos (submatch (= 8 xdigit))
						  "-" (eval bluetooth--base-uuid) eos))))
	(when (string-match uuid-re uuid)
	  (let ((service-id (string-to-number (match-string 1 uuid) 16)))
		(or (gethash service-id
					 (cl-rest (--first (>= service-id (cl-first it))
									   bluetooth--uuids)))
			(list  (format "#x%08x" service-id) "unknown"))))))

(defun bluetooth--parse-class (class)
  "Parse the CLASS property of a Bluetooth device."
  (cl-labels ((parse (field-def acc)
					 (let-alist field-def
					   (let* ((m-field (lsh (logand class .mask) .shift))
							  (res (cons .name
										 (list (funcall .fn m-field .data))))
							  (n-acc (cons res acc)))
						 (cond ((functionp .next)
								(let ((spec (funcall .next m-field .data)))
								  (if spec
									  (parse spec n-acc)
									(nreverse n-acc))))
							   ((not (null .next))
								(parse (symbol-value .next) n-acc))
							   (t (nreverse n-acc)))))))
	(parse bluetooth--class-major-services '())))

(defun bluetooth--class-parse-bitfield (bitfield data)
  "Parse BITFIELD using DATA as specification."
  (or (delq nil (mapcar (lambda (x)
						  (if (/= 0 (logand bitfield (lsh 1 (cl-first x))))
							  (cl-rest x)
							nil))
						data))
	  "unknown"))

(defun bluetooth--class-parse-major (field data)
  "Parse major class FIELD using DATA as specification."
  (or (cl-first (alist-get field data))
	  "unknown"))

(defun bluetooth--class-parse-value (field data)
  "Parse minor class FIELD using DATA as specification."
  (or (alist-get field data)
	  "unknown"))

(defun bluetooth--class-parse-peripheral (field data)
  "Parse peripheral class FIELD using DATA as specification."
  (-let (((cat-mask . categories) (cl-first data))
		 ((sub-mask . sub-groups) (cl-second data)))
	(or (list (bluetooth--class-parse-value (logand cat-mask field)
											categories)
			  (bluetooth--class-parse-value (logand sub-mask field)
											sub-groups))
		"unknown")))

(defun bluetooth--class-get-minor (field data)
  "Get the minor field spec for FIELD using DATA as specification."
  (symbol-value (cl-rest (alist-get field data))))


;;;; Bluetooth company IDs

;; Very long list of manufacturer IDs.
;; Last updated: 19. Sep 2020
(defconst bluetooth--manufacturer-ids
  #s(hash-table
	 size 3000 data
	 (#x0000
	  "Ericsson Technology Licensing"
	  #x0001 "Nokia Mobile Phones"
	  #x0002 "Intel Corp"
	  #x0003 "IBM Corp"
	  #x0004 "Toshiba Corp"
	  #x0005 "3Com"
	  #x0006 "Microsoft"
	  #x0007 "Lucent"
	  #x0008 "Motorola"
	  #x0009 "Infineon Technologies AG"
	  #x000A "Qualcomm Technologies International, Ltd. (QTIL)"
	  #x000B "Silicon Wave"
	  #x000C "Digianswer A/S"
	  #x000D "Texas Instruments Inc"
	  #x000E "Parthus Technologies Inc"
	  #x000F "Broadcom Corporation"
	  #x0010 "Mitel Semiconductor"
	  #x0011 "Widcomm, Inc"
	  #x0012 "Zeevo, Inc"
	  #x0013 "Atmel Corporation"
	  #x0014 "Mitsubishi Electric Corporation"
	  #x0015 "RTX Telecom A/S"
	  #x0016 "KC Technology Inc"
	  #x0017 "Newlogic"
	  #x0018 "Transilica, Inc"
	  #x0019 "Rohde & Schwarz GmbH & Co. KG"
	  #x001A "TTPCom Limited"
	  #x001B "Signia Technologies, Inc"
	  #x001C "Conexant Systems Inc"
	  #x001D "Qualcomm"
	  #x001E "Inventel"
	  #x001F "AVM Berlin"
	  #x0020 "BandSpeed, Inc"
	  #x0021 "Mansella Ltd"
	  #x0022 "NEC Corporation"
	  #x0023 "WavePlus Technology Co., Ltd"
	  #x0024 "Alcatel"
	  #x0025 "NXP Semiconductors (formerly Philips Semiconductors)"
	  #x0026 "C Technologies"
	  #x0027 "Open Interface"
	  #x0028 "R F Micro Devices"
	  #x0029 "Hitachi Ltd"
	  #x002A "Symbol Technologies, Inc"
	  #x002B "Tenovis"
	  #x002C "Macronix International Co. Ltd"
	  #x002D "GCT Semiconductor"
	  #x002E "Norwood Systems"
	  #x002F "MewTel Technology Inc"
	  #x0030 "ST Microelectronics"
	  #x0031 "Synopsys, Inc"
	  #x0032 "Red-M (Communications) Ltd"
	  #x0033 "Commil Ltd"
	  #x0034 "Computer Access Technology Corporation (CATC)"
	  #x0035 "Eclipse (HQ Espana) S.L"
	  #x0036 "Renesas Electronics Corporation"
	  #x0037 "Mobilian Corporation"
	  #x0038 "Syntronix Corporation"
	  #x0039 "Integrated System Solution Corp"
	  #x003A "Panasonic Corporation (formerly Matsushita Electric Industrial Co., Ltd)"
	  #x003B "Gennum Corporation"
	  #x003C "BlackBerry Limited  (formerly Research In Motion)"
	  #x003D "IPextreme, Inc."
	  #x003E "Systems and Chips, Inc"
	  #x003F "Bluetooth SIG, Inc"
	  #x0040 "Seiko Epson Corporation"
	  #x0041 "Integrated Silicon Solution Taiwan, Inc"
	  #x0042 "CONWISE Technology Corporation Ltd"
	  #x0043 "PARROT AUTOMOTIVE SAS"
	  #x0044 "Socket Mobile"
	  #x0045 "Atheros Communications, Inc"
	  #x0046 "MediaTek, Inc"
	  #x0047 "Bluegiga"
	  #x0048 "Marvell Technology Group Ltd"
	  #x0049 "3DSP Corporation"
	  #x004A "Accel Semiconductor Ltd"
	  #x004B "Continental Automotive Systems"
	  #x004C "Apple, Inc"
	  #x004D "Staccato Communications, Inc"
	  #x004E "Avago Technologies"
	  #x004F "APT Ltd"
	  #x0050 "SiRF Technology, Inc"
	  #x0051 "Tzero Technologies, Inc"
	  #x0052 "J&M Corporation"
	  #x0053 "Free2move AB"
	  #x0054 "3DiJoy Corporation"
	  #x0055 "Plantronics, Inc"
	  #x0056 "Sony Ericsson Mobile Communications"
	  #x0057 "Harman International Industries, Inc"
	  #x0058 "Vizio, Inc"
	  #x0059 "Nordic Semiconductor ASA"
	  #x005A "EM Microelectronic-Marin SA"
	  #x005B "Ralink Technology Corporation"
	  #x005C "Belkin International, Inc"
	  #x005D "Realtek Semiconductor Corporation"
	  #x005E "Stonestreet One, LLC"
	  #x005F "Wicentric, Inc"
	  #x0060 "RivieraWaves S.A.S"
	  #x0061 "RDA Microelectronics"
	  #x0062 "Gibson Guitars"
	  #x0063 "MiCommand Inc"
	  #x0064 "Band XI International, LLC"
	  #x0065 "Hewlett-Packard Company"
	  #x0066 "9Solutions Oy"
	  #x0067 "GN Netcom A/S"
	  #x0068 "General Motors"
	  #x0069 "A&D Engineering, Inc"
	  #x006A "MindTree Ltd"
	  #x006B "Polar Electro OY"
	  #x006C "Beautiful Enterprise Co., Ltd"
	  #x006D "BriarTek, Inc"
	  #x006E "Summit Data Communications, Inc"
	  #x006F "Sound ID"
	  #x0070 "Monster, LLC"
	  #x0071 "connectBlue AB"
	  #x0072 "ShangHai Super Smart Electronics Co. Ltd"
	  #x0073 "Group Sense Ltd"
	  #x0074 "Zomm, LLC"
	  #x0075 "Samsung Electronics Co. Ltd"
	  #x0076 "Creative Technology Ltd"
	  #x0077 "Laird Technologies"
	  #x0078 "Nike, Inc"
	  #x0079 "lesswire AG"
	  #x007A "MStar Semiconductor, Inc"
	  #x007B "Hanlynn Technologies"
	  #x007C "A & R Cambridge"
	  #x007D "Seers Technology Co., Ltd"
	  #x007E "Sports Tracking Technologies Ltd"
	  #x007F "Autonet Mobile"
	  #x0080 "DeLorme Publishing Company, Inc"
	  #x0081 "WuXi Vimicro"
	  #x0082 "Sennheiser Communications A/S"
	  #x0083 "TimeKeeping Systems, Inc"
	  #x0084 "Ludus Helsinki Ltd"
	  #x0085 "BlueRadios, Inc"
	  #x0086 "Equinux AG"
	  #x0087 "Garmin International, Inc"
	  #x0088 "Ecotest"
	  #x0089 "GN ReSound A/S"
	  #x008A "Jawbone"
	  #x008B "Topcon Positioning Systems, LLC"
	  #x008C "Gimbal Inc. (formerly Qualcomm Labs, Inc. and Qualcomm Retail Solutions, Inc)"
	  #x008D "Zscan Software"
	  #x008E "Quintic Corp"
	  #x008F "Telit Wireless Solutions GmbH (formerly Stollmann E+V GmbH)"
	  #x0090 "Funai Electric Co., Ltd"
	  #x0091 "Advanced PANMOBIL systems GmbH & Co. KG"
	  #x0092 "ThinkOptics, Inc"
	  #x0093 "Universal Electronics, Inc"
	  #x0094 "Airoha Technology Corp"
	  #x0095 "NEC Lighting, Ltd"
	  #x0096 "ODM Technology, Inc"
	  #x0097 "ConnecteDevice Ltd"
	  #x0098 "zero1.tv GmbH"
	  #x0099 "i.Tech Dynamic Global Distribution Ltd"
	  #x009A "Alpwise"
	  #x009B "Jiangsu Toppower Automotive Electronics Co., Ltd"
	  #x009C "Colorfy, Inc"
	  #x009D "Geoforce Inc"
	  #x009E "Bose Corporation"
	  #x009F "Suunto Oy"
	  #x00A0 "Kensington Computer Products Group"
	  #x00A1 "SR-Medizinelektronik"
	  #x00A2 "Vertu Corporation Limited"
	  #x00A3 "Meta Watch Ltd"
	  #x00A4 "LINAK A/S"
	  #x00A5 "OTL Dynamics LLC"
	  #x00A6 "Panda Ocean Inc"
	  #x00A7 "Visteon Corporation"
	  #x00A8 "ARP Devices Limited"
	  #x00A9 "Magneti Marelli S.p.A"
	  #x00AA "CAEN RFID srl"
	  #x00AB "Ingenieur-Systemgruppe Zahn GmbH"
	  #x00AC "Green Throttle Games"
	  #x00AD "Peter Systemtechnik GmbH"
	  #x00AE "Omegawave Oy"
	  #x00AF "Cinetix"
	  #x00B0 "Passif Semiconductor Corp"
	  #x00B1 "Saris Cycling Group, Inc"
	  #x00B2 "​Bekey A/S"
	  #x00B3 "​Clarinox Technologies Pty. Ltd"
	  #x00B4 "​BDE Technology Co., Ltd"
	  #x00B5 "Swirl Networks"
	  #x00B6 "​Meso international"
	  #x00B7 "​TreLab Ltd"
	  #x00B8 "​Qualcomm Innovation Center, Inc. (QuIC)"
	  #x00B9 "​​Johnson Controls, Inc"
	  #x00BA "​Starkey Laboratories Inc"
	  #x00BB "​​S-Power Electronics Limited"
	  #x00BC "​​Ace Sensor Inc"
	  #x00BD "​​Aplix Corporation"
	  #x00BE "​​AAMP of America"
	  #x00BF "​​Stalmart Technology Limited"
	  #x00C0 "​​AMICCOM Electronics Corporation"
	  #x00C1 "​​Shenzhen Excelsecu Data Technology Co.,Ltd"
	  #x00C2 "​​Geneq Inc"
	  #x00C3 "​​adidas AG"
	  #x00C4 "​​LG Electronics"
	  #x00C5 "​Onset Computer Corporation"
	  #x00C6 "​Selfly BV"
	  #x00C7 "​Quuppa Oy"
	  #x00C8 "GeLo Inc"
	  #x00C9 "Evluma"
	  #x00CA "MC10"
	  #x00CB "Binauric SE"
	  #x00CC "Beats Electronics"
	  #x00CD "Microchip Technology Inc"
	  #x00CE "Elgato Systems GmbH"
	  #x00CF "ARCHOS SA"
	  #x00D0 "Dexcom, Inc"
	  #x00D1 "Polar Electro Europe B.V"
	  #x00D2 "Dialog Semiconductor B.V"
	  #x00D3 "Taixingbang Technology (HK) Co,. LTD"
	  #x00D4 "Kawantech"
	  #x00D5 "Austco Communication Systems"
	  #x00D6 "Timex Group USA, Inc"
	  #x00D7 "Qualcomm Technologies, Inc"
	  #x00D8 "Qualcomm Connected Experiences, Inc"
	  #x00D9 "Voyetra Turtle Beach"
	  #x00DA "txtr GmbH"
	  #x00DB "Biosentronics"
	  #x00DC "Procter & Gamble"
	  #x00DD "Hosiden Corporation"
	  #x00DE "Muzik LLC"
	  #x00DF "Misfit Wearables Corp"
	  #x00E0 "Google"
	  #x00E1 "Danlers Ltd"
	  #x00E2 "Semilink Inc"
	  #x00E3 "inMusic Brands, Inc"
	  #x00E4 "L.S. Research Inc"
	  #x00E5 "Eden Software Consultants Ltd"
	  #x00E6 "Freshtemp"
	  #x00E7 "​KS Technologies"
	  #x00E8 "​ACTS Technologies"
	  #x00E9 "​Vtrack Systems"
	  #x00EA "​Nielsen-Kellerman Company"
	  #x00EB "Server Technology Inc"
	  #x00EC "BioResearch Associates"
	  #x00ED "Jolly Logic, LLC"
	  #x00EE "Above Average Outcomes, Inc"
	  #x00EF "Bitsplitters GmbH"
	  #x00F0 "PayPal, Inc"
	  #x00F1 "Witron Technology Limited"
	  #x00F2 "Morse Project Inc"
	  #x00F3 "Kent Displays Inc"
	  #x00F4 "Nautilus Inc"
	  #x00F5 "Smartifier Oy"
	  #x00F6 "Elcometer Limited"
	  #x00F7 "VSN Technologies, Inc"
	  #x00F8 "AceUni Corp., Ltd"
	  #x00F9 "StickNFind"
	  #x00FA "Crystal Code AB"
	  #x00FB "KOUKAAM a.s"
	  #x00FC "Delphi Corporation"
	  #x00FD "ValenceTech Limited"
	  #x00FE "Stanley Black and Decker"
	  #x00FF "Typo Products, LLC"
	  #x0100 "TomTom International BV"
	  #x0101 "Fugoo, Inc"
	  #x0102 "Keiser Corporation"
	  #x0103 "Bang & Olufsen A/S"
	  #x0104 "PLUS Location Systems Pty Ltd"
	  #x0105 "Ubiquitous Computing Technology Corporation"
	  #x0106 "Innovative Yachtter Solutions"
	  #x0107 "William Demant Holding A/S"
	  #x0108 "Chicony Electronics Co., Ltd"
	  #x0109 "Atus BV"
	  #x010A "Codegate Ltd"
	  #x010B "ERi, Inc"
	  #x010C "Transducers Direct, LLC"
	  #x010D "DENSO TEN LIMITED (formerly Fujitsu Ten LImited)"
	  #x010E "Audi AG"
	  #x010F "HiSilicon Technologies Col, Ltd"
	  #x0110 "Nippon Seiki Co., Ltd"
	  #x0111 "Steelseries ApS"
	  #x0112 "Visybl Inc"
	  #x0113 "Openbrain Technologies, Co., Ltd"
	  #x0114 "Xensr"
	  #x0115 "e.solutions"
	  #x0116 "10AK Technologies"
	  #x0117 "Wimoto Technologies Inc"
	  #x0118 "Radius Networks, Inc"
	  #x0119 "Wize Technology Co., Ltd"
	  #x011A "Qualcomm Labs, Inc"
	  #x011B "Hewlett Packard Enterprise"
	  #x011C "Baidu"
	  #x011D "Arendi AG"
	  #x011E "Skoda Auto a.s"
	  #x011F "Volkswagen AG"
	  #x0120 "Porsche AG"
	  #x0121 "Sino Wealth Electronic Ltd"
	  #x0122 "AirTurn, Inc"
	  #x0123 "Kinsa, Inc"
	  #x0124 "HID Global"
	  #x0125 "SEAT es"
	  #x0126 "Promethean Ltd"
	  #x0127 "Salutica Allied Solutions"
	  #x0128 "GPSI Group Pty Ltd"
	  #x0129 "Nimble Devices Oy"
	  #x012A "Changzhou Yongse Infotech  Co., Ltd"
	  #x012B "SportIQ"
	  #x012C "TEMEC Instruments B.V"
	  #x012D "Sony Corporation"
	  #x012E "ASSA ABLOY"
	  #x012F "Clarion Co. Inc"
	  #x0130 "Warehouse Innovations"
	  #x0131 "Cypress Semiconductor"
	  #x0132 "MADS Inc"
	  #x0133 "Blue Maestro Limited"
	  #x0134 "Resolution Products, Ltd"
	  #x0135 "Aireware LLC"
	  #x0136 "Silvair, Inc"
	  #x0137 "Prestigio Plaza Ltd"
	  #x0138 "NTEO Inc"
	  #x0139 "Focus Systems Corporation"
	  #x013A "Tencent Holdings Ltd"
	  #x013B "Allegion"
	  #x013C "Murata Manufacturing Co., Ltd"
	  #x013D "WirelessWERX"
	  #x013E "Nod, Inc"
	  #x013F "B&B Manufacturing Company"
	  #x0140 "Alpine Electronics (China) Co., Ltd"
	  #x0141 "FedEx Services"
	  #x0142 "Grape Systems Inc"
	  #x0143 "Bkon Connect"
	  #x0144 "Lintech GmbH"
	  #x0145 "Novatel Wireless"
	  #x0146 "Ciright"
	  #x0147 "Mighty Cast, Inc"
	  #x0148 "Ambimat Electronics"
	  #x0149 "Perytons Ltd"
	  #x014A "Tivoli Audio, LLC"
	  #x014B "Master Lock"
	  #x014C "Mesh-Net Ltd"
	  #x014D "HUIZHOU DESAY SV AUTOMOTIVE CO., LTD"
	  #x014E "Tangerine, Inc"
	  #x014F "B&W Group Ltd"
	  #x0150 "Pioneer Corporation"
	  #x0151 "OnBeep"
	  #x0152 "Vernier Software & Technology"
	  #x0153 "ROL Ergo"
	  #x0154 "Pebble Technology"
	  #x0155 "NETATMO"
	  #x0156 "Accumulate AB"
	  #x0157 "Anhui Huami Information Technology Co., Ltd"
	  #x0158 "Inmite s.r.o"
	  #x0159 "ChefSteps, Inc"
	  #x015A "micas AG"
	  #x015B "Biomedical Research Ltd"
	  #x015C "Pitius Tec S.L"
	  #x015D "Estimote, Inc"
	  #x015E "Unikey Technologies, Inc"
	  #x015F "Timer Cap Co"
	  #x0160 "AwoX"
	  #x0161 "yikes"
	  #x0162 "MADSGlobalNZ Ltd"
	  #x0163 "PCH International"
	  #x0164 "Qingdao Yeelink Information Technology Co., Ltd"
	  #x0165 "Milwaukee Tool (Formally Milwaukee Electric Tools)"
	  #x0166 "MISHIK Pte Ltd"
	  #x0167 "Ascensia Diabetes Care US Inc"
	  #x0168 "Spicebox LLC"
	  #x0169 "emberlight"
	  #x016A "Cooper-Atkins Corporation"
	  #x016B "Qblinks"
	  #x016C "MYSPHERA"
	  #x016D "LifeScan Inc"
	  #x016E "Volantic AB"
	  #x016F "Podo Labs, Inc"
	  #x0170 "Roche Diabetes Care AG"
	  #x0171 "Amazon Fulfillment Service"
	  #x0172 "Connovate Technology Private Limited"
	  #x0173 "Kocomojo, LLC"
	  #x0174 "Everykey Inc"
	  #x0175 "Dynamic Controls"
	  #x0176 "SentriLock"
	  #x0177 "I-SYST inc"
	  #x0178 "CASIO COMPUTER CO., LTD"
	  #x0179 "LAPIS Semiconductor Co., Ltd"
	  #x017A "Telemonitor, Inc"
	  #x017B "taskit GmbH"
	  #x017C "Daimler AG"
	  #x017D "BatAndCat"
	  #x017E "BluDotz Ltd"
	  #x017F "XTel Wireless ApS"
	  #x0180 "Gigaset Communications GmbH"
	  #x0181 "Gecko Health Innovations, Inc"
	  #x0182 "HOP Ubiquitous"
	  #x0183 "Walt Disney"
	  #x0184 "Nectar"
	  #x0185 "bel'apps LLC"
	  #x0186 "CORE Lighting Ltd"
	  #x0187 "Seraphim Sense Ltd"
	  #x0188 "Unico RBC"
	  #x0189 "Physical Enterprises Inc"
	  #x018A "Able Trend Technology Limited"
	  #x018B "Konica Minolta, Inc"
	  #x018C "Wilo SE"
	  #x018D "Extron Design Services"
	  #x018E "Fitbit, Inc"
	  #x018F "Fireflies Systems"
	  #x0190 "Intelletto Technologies Inc"
	  #x0191 "FDK CORPORATION"
	  #x0192 "Cloudleaf, Inc"
	  #x0193 "Maveric Automation LLC"
	  #x0194 "Acoustic Stream Corporation"
	  #x0195 "Zuli"
	  #x0196 "Paxton Access Ltd"
	  #x0197 "WiSilica Inc"
	  #x0198 "VENGIT Korlatolt Felelossegu Tarsasag"
	  #x0199 "SALTO SYSTEMS S.L"
	  #x019A "TRON Forum (formerly T-Engine Forum)"
	  #x019B "CUBETECH s.r.o"
	  #x019C "Cokiya Incorporated"
	  #x019D "CVS Health"
	  #x019E "Ceruus"
	  #x019F "Strainstall Ltd"
	  #x01A0 "Channel Enterprises (HK) Ltd"
	  #x01A1 "FIAMM"
	  #x01A2 "GIGALANE.CO.,LTD"
	  #x01A3 "EROAD"
	  #x01A4 "Mine Safety Appliances"
	  #x01A5 "Icon Health and Fitness"
	  #x01A6 "Wille Engineering (formely as Asandoo GmbH)"
	  #x01A7 "ENERGOUS CORPORATION"
	  #x01A8 "Taobao"
	  #x01A9 "Canon Inc"
	  #x01AA "Geophysical Technology Inc"
	  #x01AB "Facebook, Inc"
	  #x01AC "Trividia Health, Inc"
	  #x01AD "FlightSafety International"
	  #x01AE "Earlens Corporation"
	  #x01AF "Sunrise Micro Devices, Inc"
	  #x01B0 "Star Micronics Co., Ltd"
	  #x01B1 "Netizens Sp. z o.o"
	  #x01B2 "Nymi Inc"
	  #x01B3 "Nytec, Inc"
	  #x01B4 "Trineo Sp. z o.o"
	  #x01B5 "Nest Labs Inc"
	  #x01B6 "LM Technologies Ltd"
	  #x01B7 "General Electric Company"
	  #x01B8 "i+D3 S.L"
	  #x01B9 "HANA Micron"
	  #x01BA "Stages Cycling LLC"
	  #x01BB "Cochlear Bone Anchored Solutions AB"
	  #x01BC "SenionLab AB"
	  #x01BD "Syszone Co., Ltd"
	  #x01BE "Pulsate Mobile Ltd"
	  #x01BF "Hong Kong HunterSun Electronic Limited"
	  #x01C0 "pironex GmbH"
	  #x01C1 "BRADATECH Corp"
	  #x01C2 "Transenergooil AG"
	  #x01C3 "Bunch"
	  #x01C4 "DME Microelectronics"
	  #x01C5 "Bitcraze AB"
	  #x01C6 "HASWARE Inc"
	  #x01C7 "Abiogenix Inc"
	  #x01C8 "Poly-Control ApS"
	  #x01C9 "Avi-on"
	  #x01CA "Laerdal Medical AS"
	  #x01CB "Fetch My Pet"
	  #x01CC "Sam Labs Ltd"
	  #x01CD "Chengdu Synwing Technology Ltd"
	  #x01CE "HOUWA SYSTEM DESIGN, k.k"
	  #x01CF "BSH"
	  #x01D0 "Primus Inter Pares Ltd"
	  #x01D1 "August Home, Inc"
	  #x01D2 "Gill Electronics"
	  #x01D3 "Sky Wave Design"
	  #x01D4 "Newlab S.r.l"
	  #x01D5 "ELAD srl"
	  #x01D6 "G-wearables inc"
	  #x01D7 "Squadrone Systems Inc"
	  #x01D8 "Code Corporation"
	  #x01D9 "Savant Systems LLC"
	  #x01DA "Logitech International SA"
	  #x01DB "Innblue Consulting"
	  #x01DC "iParking Ltd"
	  #x01DD "Koninklijke Philips Electronics N.V"
	  #x01DE "Minelab Electronics Pty Limited"
	  #x01DF "Bison Group Ltd"
	  #x01E0 "Widex A/S"
	  #x01E1 "Jolla Ltd"
	  #x01E2 "Lectronix, Inc"
	  #x01E3 "Caterpillar Inc"
	  #x01E4 "Freedom Innovations"
	  #x01E5 "Dynamic Devices Ltd"
	  #x01E6 "Technology Solutions (UK) Ltd"
	  #x01E7 "IPS Group Inc"
	  #x01E8 "STIR"
	  #x01E9 "Sano, Inc"
	  #x01EA "Advanced Application Design, Inc"
	  #x01EB "AutoMap LLC"
	  #x01EC "Spreadtrum Communications Shanghai Ltd"
	  #x01ED "CuteCircuit LTD"
	  #x01EE "Valeo Service"
	  #x01EF "Fullpower Technologies, Inc"
	  #x01F0 "KloudNation"
	  #x01F1 "Zebra Technologies Corporation"
	  #x01F2 "Itron, Inc"
	  #x01F3 "The University of Tokyo"
	  #x01F4 "UTC Fire and Security"
	  #x01F5 "Cool Webthings Limited"
	  #x01F6 "DJO Global"
	  #x01F7 "Gelliner Limited"
	  #x01F8 "Anyka (Guangzhou) Microelectronics Technology Co, LTD"
	  #x01F9 "Medtronic Inc"
	  #x01FA "Gozio Inc"
	  #x01FB "Form Lifting, LLC"
	  #x01FC "Wahoo Fitness, LLC"
	  #x01FD "Kontakt Micro-Location Sp. z o.o"
	  #x01FE "Radio Systems Corporation"
	  #x01FF "Freescale Semiconductor, Inc"
	  #x0200 "Verifone Systems Pte Ltd. Taiwan Branch"
	  #x0201 "AR Timing"
	  #x0202 "Rigado LLC"
	  #x0203 "Kemppi Oy"
	  #x0204 "Tapcentive Inc"
	  #x0205 "Smartbotics Inc"
	  #x0206 "Otter Products, LLC"
	  #x0207 "STEMP Inc"
	  #x0208 "LumiGeek LLC"
	  #x0209 "InvisionHeart Inc"
	  #x020A "Macnica Inc"
	  #x020B "Jaguar Land Rover Limited"
	  #x020C "CoroWare Technologies, Inc"
	  #x020D "Simplo Technology Co., LTD"
	  #x020E "Omron Healthcare Co., LTD"
	  #x020F "Comodule GMBH"
	  #x0210 "ikeGPS"
	  #x0211 "Telink Semiconductor Co. Ltd"
	  #x0212 "Interplan Co., Ltd"
	  #x0213 "Wyler AG"
	  #x0214 "IK Multimedia Production srl"
	  #x0215 "Lukoton Experience Oy"
	  #x0216 "MTI Ltd"
	  #x0217 "Tech4home, Lda"
	  #x0218 "Hiotech AB"
	  #x0219 "DOTT Limited"
	  #x021A "Blue Speck Labs, LLC"
	  #x021B "Cisco Systems, Inc"
	  #x021C "Mobicomm Inc"
	  #x021D "Edamic"
	  #x021E "Goodnet, Ltd"
	  #x021F "Luster Leaf Products	Inc"
	  #x0220 "Manus Machina BV"
	  #x0221 "Mobiquity Networks Inc"
	  #x0222 "Praxis Dynamics"
	  #x0223 "Philip Morris Products S.A"
	  #x0224 "Comarch SA"
	  #x0225 "Nestlé Nespresso S.A"
	  #x0226 "Merlinia A/S"
	  #x0227 "LifeBEAM Technologies"
	  #x0228 "Twocanoes Labs, LLC"
	  #x0229 "Muoverti Limited"
	  #x022A "Stamer Musikanlagen GMBH"
	  #x022B "Tesla Motors"
	  #x022C "Pharynks Corporation"
	  #x022D "Lupine"
	  #x022E "Siemens AG"
	  #x022F "Huami (Shanghai) Culture Communication CO., LTD"
	  #x0230 "Foster Electric Company, Ltd"
	  #x0231 "ETA SA"
	  #x0232 "x-Senso Solutions Kft"
	  #x0233 "Shenzhen SuLong Communication Ltd"
	  #x0234 "FengFan (BeiJing) Technology Co, Ltd"
	  #x0235 "Qrio Inc"
	  #x0236 "Pitpatpet Ltd"
	  #x0237 "MSHeli s.r.l"
	  #x0238 "Trakm8 Ltd"
	  #x0239 "JIN CO, Ltd"
	  #x023A "Alatech Tehnology"
	  #x023B "Beijing CarePulse Electronic Technology Co, Ltd"
	  #x023C "Awarepoint"
	  #x023D "ViCentra B.V"
	  #x023E "Raven Industries"
	  #x023F "WaveWare Technologies Inc"
	  #x0240 "Argenox Technologies"
	  #x0241 "Bragi GmbH"
	  #x0242 "16Lab Inc"
	  #x0243 "Masimo Corp"
	  #x0244 "Iotera Inc"
	  #x0245 "Endress+Hauser"
	  #x0246 "ACKme Networks, Inc"
	  #x0247 "FiftyThree Inc"
	  #x0248 "Parker Hannifin Corp"
	  #x0249 "Transcranial Ltd"
	  #x024A "Uwatec AG"
	  #x024B "Orlan LLC"
	  #x024C "Blue Clover Devices"
	  #x024D "M-Way Solutions GmbH"
	  #x024E "Microtronics Engineering GmbH"
	  #x024F "Schneider Schreibgeräte GmbH"
	  #x0250 "Sapphire Circuits LLC"
	  #x0251 "Lumo Bodytech Inc"
	  #x0252 "UKC Technosolution"
	  #x0253 "Xicato Inc"
	  #x0254 "Playbrush"
	  #x0255 "Dai Nippon Printing Co., Ltd"
	  #x0256 "G24 Power Limited"
	  #x0257 "AdBabble Local Commerce Inc"
	  #x0258 "Devialet SA"
	  #x0259 "ALTYOR"
	  #x025A "University of Applied Sciences Valais/Haute Ecole Valaisanne"
	  #x025B "Five Interactive, LLC dba Zendo"
	  #x025C "NetEase（Hangzhou）Network co.Ltd"
	  #x025D "Lexmark International Inc"
	  #x025E "Fluke Corporation"
	  #x025F "Yardarm Technologies"
	  #x0260 "SensaRx"
	  #x0261 "SECVRE GmbH"
	  #x0262 "Glacial Ridge Technologies"
	  #x0263 "Identiv, Inc"
	  #x0264 "DDS, Inc"
	  #x0265 "SMK Corporation"
	  #x0266 "Schawbel Technologies LLC"
	  #x0267 "XMI Systems SA"
	  #x0268 "Cerevo"
	  #x0269 "Torrox GmbH & Co KG"
	  #x026A "Gemalto"
	  #x026B "DEKA Research & Development Corp"
	  #x026C "Domster Tadeusz Szydlowski"
	  #x026D "Technogym SPA"
	  #x026E "FLEURBAEY BVBA"
	  #x026F "Aptcode Solutions"
	  #x0270 "LSI ADL Technology"
	  #x0271 "Animas Corp"
	  #x0272 "Alps Electric Co., Ltd"
	  #x0273 "OCEASOFT"
	  #x0274 "Motsai Research"
	  #x0275 "Geotab"
	  #x0276 "E.G.O. Elektro-Geraetebau GmbH"
	  #x0277 "bewhere inc"
	  #x0278 "Johnson Outdoors Inc"
	  #x0279 "steute Schaltgerate GmbH & Co. KG"
	  #x027A "Ekomini inc"
	  #x027B "DEFA AS"
	  #x027C "Aseptika Ltd"
	  #x027D "HUAWEI Technologies Co., Ltd"
	  #x027E "HabitAware, LLC"
	  #x027F "ruwido austria gmbh"
	  #x0280 "ITEC corporation"
	  #x0281 "StoneL"
	  #x0282 "Sonova AG"
	  #x0283 "Maven Machines, Inc"
	  #x0284 "Synapse Electronics"
	  #x0285 "Standard Innovation Inc"
	  #x0286 "RF Code, Inc"
	  #x0287 "Wally Ventures S.L"
	  #x0288 "Willowbank Electronics Ltd"
	  #x0289 "SK Telecom"
	  #x028A "Jetro AS"
	  #x028B "Code Gears LTD"
	  #x028C "NANOLINK APS"
	  #x028D "IF, LLC"
	  #x028E "RF Digital Corp"
	  #x028F "Church & Dwight Co., Inc"
	  #x0290 "Multibit Oy"
	  #x0291 "CliniCloud Inc"
	  #x0292 "SwiftSensors"
	  #x0293 "Blue Bite"
	  #x0294 "ELIAS GmbH"
	  #x0295 "Sivantos GmbH"
	  #x0296 "Petzl"
	  #x0297 "storm power ltd"
	  #x0298 "EISST Ltd"
	  #x0299 "Inexess Technology Simma KG"
	  #x029A "Currant, Inc"
	  #x029B "C2 Development, Inc"
	  #x029C "Blue Sky Scientific, LLC"
	  #x029D "ALOTTAZS LABS, LLC"
	  #x029E "Kupson spol. s r.o"
	  #x029F "Areus Engineering GmbH"
	  #x02A0 "Impossible Camera GmbH"
	  #x02A1 "InventureTrack Systems"
	  #x02A2 "LockedUp"
	  #x02A3 "Itude"
	  #x02A4 "Pacific Lock Company"
	  #x02A5 "Tendyron Corporation ( 天地融科技股份有限公司)"
	  #x02A6 "Robert Bosch GmbH"
	  #x02A7 "Illuxtron international B.V"
	  #x02A8 "miSport Ltd"
	  #x02A9 "Chargelib"
	  #x02AA "Doppler Lab"
	  #x02AB "BBPOS Limited"
	  #x02AC "RTB Elektronik GmbH & Co. KG"
	  #x02AD "Rx Networks, Inc"
	  #x02AE "WeatherFlow, Inc"
	  #x02AF "Technicolor USA Inc"
	  #x02B0 "Bestechnic(Shanghai),Ltd"
	  #x02B1 "Raden Inc"
	  #x02B2 "JouZen Oy"
	  #x02B3 "CLABER S.P.A"
	  #x02B4 "Hyginex, Inc"
	  #x02B5 "HANSHIN ELECTRIC RAILWAY CO.,LTD"
	  #x02B6 "Schneider Electric"
	  #x02B7 "Oort Technologies LLC"
	  #x02B8 "Chrono Therapeutics"
	  #x02B9 "Rinnai Corporation"
	  #x02BA "Swissprime Technologies AG"
	  #x02BB "Koha.,Co.Ltd"
	  #x02BC "Genevac Ltd"
	  #x02BD "Chemtronics"
	  #x02BE "Seguro Technology Sp. z o.o"
	  #x02BF "Redbird Flight Simulations"
	  #x02C0 "Dash Robotics"
	  #x02C1 "LINE Corporation"
	  #x02C2 "Guillemot Corporation"
	  #x02C3 "Techtronic Power Tools Technology Limited"
	  #x02C4 "Wilson Sporting Goods"
	  #x02C5 "Lenovo (Singapore) Pte Ltd. ( 联想(新加坡))"
	  #x02C6 "Ayatan Sensors"
	  #x02C7 "Electronics Tomorrow Limited"
	  #x02C8 "VASCO Data Security International, Inc"
	  #x02C9 "PayRange Inc"
	  #x02CA "ABOV Semiconductor"
	  #x02CB "AINA-Wireless Inc"
	  #x02CC "Eijkelkamp Soil & Water"
	  #x02CD "BMA ergonomics b.v"
	  #x02CE "Teva Branded Pharmaceutical Products R&D, Inc"
	  #x02CF "Anima"
	  #x02D0 "3M"
	  #x02D1 "Empatica Srl"
	  #x02D2 "Afero, Inc"
	  #x02D3 "Powercast Corporation"
	  #x02D4 "Secuyou ApS"
	  #x02D5 "OMRON Corporation"
	  #x02D6 "Send Solutions"
	  #x02D7 "NIPPON SYSTEMWARE CO.,LTD"
	  #x02D8 "Neosfar"
	  #x02D9 "Fliegl Agrartechnik GmbH"
	  #x02DA "Gilvader"
	  #x02DB "Digi International Inc (R)"
	  #x02DC "DeWalch Technologies, Inc"
	  #x02DD "Flint Rehabilitation Devices, LLC"
	  #x02DE "Samsung SDS Co., Ltd"
	  #x02DF "Blur Product Development"
	  #x02E0 "University of Michigan"
	  #x02E1 "Victron Energy BV"
	  #x02E2 "NTT docomo"
	  #x02E3 "Carmanah Technologies Corp"
	  #x02E4 "Bytestorm Ltd"
	  #x02E5 "Espressif Incorporated ( 乐鑫信息科技(上海)有限公司)"
	  #x02E6 "Unwire"
	  #x02E7 "Connected Yard, Inc"
	  #x02E8 "American Music Environments"
	  #x02E9 "Sensogram Technologies, Inc"
	  #x02EA "Fujitsu Limited"
	  #x02EB "Ardic Technology"
	  #x02EC "Delta Systems, Inc"
	  #x02ED "HTC Corporation"
	  #x02EE "Citizen Holdings Co., Ltd"
	  #x02EF "SMART-INNOVATION.inc"
	  #x02F0 "Blackrat Software"
	  #x02F1 "The Idea Cave, LLC"
	  #x02F2 "GoPro, Inc"
	  #x02F3 "AuthAir, Inc"
	  #x02F4 "Vensi, Inc"
	  #x02F5 "Indagem Tech LLC"
	  #x02F6 "Intemo Technologies"
	  #x02F7 "DreamVisions co., Ltd"
	  #x02F8 "Runteq Oy Ltd"
	  #x02F9 "IMAGINATION TECHNOLOGIES LTD"
	  #x02FA "CoSTAR TEchnologies"
	  #x02FB "Clarius Mobile Health Corp"
	  #x02FC "Shanghai Frequen Microelectronics Co., Ltd"
	  #x02FD "Uwanna, Inc"
	  #x02FE "Lierda Science & Technology Group Co., Ltd"
	  #x02FF "Silicon Laboratories"
	  #x0300 "World Moto Inc"
	  #x0301 "Giatec Scientific Inc"
	  #x0302 "Loop Devices, Inc"
	  #x0303 "IACA electronique"
	  #x0304 "Proxy Technologies, Inc"
	  #x0305 "Swipp ApS"
	  #x0306 "Life Laboratory Inc"
	  #x0307 "FUJI INDUSTRIAL CO.,LTD"
	  #x0308 "Surefire, LLC"
	  #x0309 "Dolby Labs"
	  #x030A "Ellisys"
	  #x030B "Magnitude Lighting Converters"
	  #x030C "Hilti AG"
	  #x030D "Devdata S.r.l"
	  #x030E "Deviceworx"
	  #x030F "Shortcut Labs"
	  #x0310 "SGL Italia S.r.l"
	  #x0311 "PEEQ DATA"
	  #x0312 "Ducere Technologies Pvt Ltd"
	  #x0313 "DiveNav, Inc"
	  #x0314 "RIIG AI Sp. z o.o"
	  #x0315 "Thermo Fisher Scientific"
	  #x0316 "AG Measurematics Pvt. Ltd"
	  #x0317 "CHUO Electronics CO., LTD"
	  #x0318 "Aspenta International"
	  #x0319 "Eugster Frismag AG"
	  #x031A "Amber wireless GmbH"
	  #x031B "HQ Inc"
	  #x031C "Lab Sensor Solutions"
	  #x031D "Enterlab ApS"
	  #x031E "Eyefi, Inc"
	  #x031F "MetaSystem S.p.A"
	  #x0320 "SONO ELECTRONICS. CO., LTD"
	  #x0321 "Jewelbots"
	  #x0322 "Compumedics Limited"
	  #x0323 "Rotor Bike Components"
	  #x0324 "Astro, Inc"
	  #x0325 "Amotus Solutions"
	  #x0326 "Healthwear Technologies (Changzhou)Ltd"
	  #x0327 "Essex Electronics"
	  #x0328 "Grundfos A/S"
	  #x0329 "Eargo, Inc"
	  #x032A "Electronic Design Lab"
	  #x032B "ESYLUX"
	  #x032C "NIPPON SMT.CO.,Ltd"
	  #x032D "BM innovations GmbH"
	  #x032E "indoormap"
	  #x032F "OttoQ Inc"
	  #x0330 "North Pole Engineering"
	  #x0331 "3flares Technologies Inc"
	  #x0332 "Electrocompaniet A.S"
	  #x0333 "Mul-T-Lock"
	  #x0334 "Corentium AS"
	  #x0335 "Enlighted Inc"
	  #x0336 "GISTIC"
	  #x0337 "AJP2 Holdings, LLC"
	  #x0338 "COBI GmbH"
	  #x0339 "Blue Sky Scientific, LLC"
	  #x033A "Appception, Inc"
	  #x033B "Courtney Thorne Limited"
	  #x033C "Virtuosys"
	  #x033D "TPV Technology Limited"
	  #x033E "Monitra SA"
	  #x033F "Automation Components, Inc"
	  #x0340 "Letsense s.r.l"
	  #x0341 "Etesian Technologies LLC"
	  #x0342 "GERTEC BRASIL LTDA"
	  #x0343 "Drekker Development Pty. Ltd"
	  #x0344 "Whirl Inc"
	  #x0345 "Locus Positioning"
	  #x0346 "Acuity Brands Lighting, Inc"
	  #x0347 "Prevent Biometrics"
	  #x0348 "Arioneo"
	  #x0349 "VersaMe"
	  #x034A "Vaddio"
	  #x034B "Libratone A/S"
	  #x034C "HM Electronics, Inc"
	  #x034D "TASER International, Inc"
	  #x034E "SafeTrust Inc"
	  #x034F "Heartland Payment Systems"
	  #x0350 "Bitstrata Systems Inc"
	  #x0351 "Pieps GmbH"
	  #x0352 "iRiding(Xiamen)Technology Co.,Ltd"
	  #x0353 "Alpha Audiotronics, Inc"
	  #x0354 "TOPPAN FORMS CO.,LTD"
	  #x0355 "Sigma Designs, Inc"
	  #x0356 "Spectrum Brands, Inc"
	  #x0357 "Polymap Wireless"
	  #x0358 "MagniWare Ltd"
	  #x0359 "Novotec Medical GmbH"
	  #x035A "Medicom Innovation Partner a/s"
	  #x035B "Matrix Inc"
	  #x035C "Eaton Corporation"
	  #x035D "KYS"
	  #x035E "Naya Health, Inc"
	  #x035F "Acromag"
	  #x0360 "Insulet Corporation"
	  #x0361 "Wellinks Inc"
	  #x0362 "ON Semiconductor"
	  #x0363 "FREELAP SA"
	  #x0364 "Favero Electronics Srl"
	  #x0365 "BioMech Sensor LLC"
	  #x0366 "BOLTT Sports technologies Private limited"
	  #x0367 "Saphe International"
	  #x0368 "Metormote AB"
	  #x0369 "littleBits"
	  #x036A "SetPoint Medical"
	  #x036B "BRControls Products BV"
	  #x036C "Zipcar"
	  #x036D "AirBolt Pty Ltd"
	  #x036E "KeepTruckin Inc"
	  #x036F "Motiv, Inc"
	  #x0370 "Wazombi Labs OÜ"
	  #x0371 "ORBCOMM"
	  #x0372 "Nixie Labs, Inc"
	  #x0373 "AppNearMe Ltd"
	  #x0374 "Holman Industries"
	  #x0375 "Expain AS"
	  #x0376 "Electronic Temperature Instruments Ltd"
	  #x0377 "Plejd AB"
	  #x0378 "Propeller Health"
	  #x0379 "Shenzhen iMCO Electronic Technology Co.,Ltd"
	  #x037A "Algoria"
	  #x037B "Apption Labs Inc"
	  #x037C "Cronologics Corporation"
	  #x037D "MICRODIA Ltd"
	  #x037E "lulabytes S.L"
	  #x037F "Nestec S.A"
	  #x0380 "LLC MEGA-F service"
	  #x0381 "Sharp Corporation"
	  #x0382 "Precision Outcomes Ltd"
	  #x0383 "Kronos Incorporated"
	  #x0384 "OCOSMOS Co., Ltd"
	  #x0385 "Embedded Electronic Solutions Ltd. dba e2Solutions"
	  #x0386 "Aterica Inc"
	  #x0387 "BluStor PMC, Inc"
	  #x0388 "Kapsch TrafficCom AB"
	  #x0389 "ActiveBlu Corporation"
	  #x038A "Kohler Mira Limited"
	  #x038B "Noke"
	  #x038C "Appion Inc"
	  #x038D "Resmed Ltd"
	  #x038E "Crownstone B.V"
	  #x038F "Xiaomi Inc"
	  #x0390 "INFOTECH s.r.o"
	  #x0391 "Thingsquare AB"
	  #x0392 "T&D"
	  #x0393 "LAVAZZA S.p.A"
	  #x0394 "Netclearance Systems, Inc"
	  #x0395 "SDATAWAY"
	  #x0396 "BLOKS GmbH"
	  #x0397 "LEGO System A/S"
	  #x0398 "Thetatronics Ltd"
	  #x0399 "Nikon Corporation"
	  #x039A "NeST"
	  #x039B "South Silicon Valley Microelectronics"
	  #x039C "ALE International"
	  #x039D "CareView Communications, Inc"
	  #x039E "SchoolBoard Limited"
	  #x039F "Molex Corporation"
	  #x03A0 "IVT Wireless Limited"
	  #x03A1 "Alpine Labs LLC"
	  #x03A2 "Candura Instruments"
	  #x03A3 "SmartMovt Technology Co., Ltd"
	  #x03A4 "Token Zero Ltd"
	  #x03A5 "ACE CAD Enterprise Co., Ltd. (ACECAD)"
	  #x03A6 "Medela, Inc"
	  #x03A7 "AeroScout"
	  #x03A8 "Esrille Inc"
	  #x03A9 "THINKERLY SRL"
	  #x03AA "Exon Sp. z o.o"
	  #x03AB "Meizu Technology Co., Ltd"
	  #x03AC "Smablo LTD"
	  #x03AD "XiQ"
	  #x03AE "Allswell Inc"
	  #x03AF "Comm-N-Sense Corp DBA Verigo"
	  #x03B0 "VIBRADORM GmbH"
	  #x03B1 "Otodata Wireless Network Inc"
	  #x03B2 "Propagation Systems Limited"
	  #x03B3 "Midwest Instruments & Controls"
	  #x03B4 "Alpha Nodus, inc"
	  #x03B5 "petPOMM, Inc"
	  #x03B6 "Mattel"
	  #x03B7 "Airbly Inc"
	  #x03B8 "A-Safe Limited"
	  #x03B9 "FREDERIQUE CONSTANT SA"
	  #x03BA "Maxscend Microelectronics Company Limited"
	  #x03BB "Abbott Diabetes Care"
	  #x03BC "ASB Bank Ltd"
	  #x03BD "amadas"
	  #x03BE "Applied Science, Inc"
	  #x03BF "iLumi Solutions Inc"
	  #x03C0 "Arch Systems Inc"
	  #x03C1 "Ember Technologies, Inc"
	  #x03C2 "Snapchat Inc"
	  #x03C3 "Casambi Technologies Oy"
	  #x03C4 "Pico Technology Inc"
	  #x03C5 "St. Jude Medical, Inc"
	  #x03C6 "Intricon"
	  #x03C7 "Structural Health Systems, Inc"
	  #x03C8 "Avvel International"
	  #x03C9 "Gallagher Group"
	  #x03CA "In2things Automation Pvt. Ltd"
	  #x03CB "SYSDEV Srl"
	  #x03CC "Vonkil Technologies Ltd"
	  #x03CD "Wynd Technologies, Inc"
	  #x03CE "CONTRINEX S.A"
	  #x03CF "MIRA, Inc"
	  #x03D0 "Watteam Ltd"
	  #x03D1 "Density Inc"
	  #x03D2 "IOT Pot India Private Limited"
	  #x03D3 "Sigma Connectivity AB"
	  #x03D4 "PEG PEREGO SPA"
	  #x03D5 "Wyzelink Systems Inc"
	  #x03D6 "Yota Devices LTD"
	  #x03D7 "FINSECUR"
	  #x03D8 "Zen-Me Labs Ltd"
	  #x03D9 "3IWare Co., Ltd"
	  #x03DA "EnOcean GmbH"
	  #x03DB "Instabeat, Inc"
	  #x03DC "Nima Labs"
	  #x03DD "Andreas Stihl AG & Co. KG"
	  #x03DE "Nathan Rhoades LLC"
	  #x03DF "Grob Technologies, LLC"
	  #x03E0 "Actions (Zhuhai) Technology Co., Limited"
	  #x03E1 "SPD Development Company Ltd"
	  #x03E2 "Sensoan Oy"
	  #x03E3 "Qualcomm Life Inc"
	  #x03E4 "Chip-ing AG"
	  #x03E5 "ffly4u"
	  #x03E6 "IoT Instruments Oy"
	  #x03E7 "TRUE Fitness Technology"
	  #x03E8 "Reiner Kartengeraete GmbH & Co. KG"
	  #x03E9 "SHENZHEN LEMONJOY TECHNOLOGY CO., LTD"
	  #x03EA "Hello Inc"
	  #x03EB "Evollve Inc"
	  #x03EC "Jigowatts Inc"
	  #x03ED "BASIC MICRO.COM,INC"
	  #x03EE "CUBE TECHNOLOGIES"
	  #x03EF "foolography GmbH"
	  #x03F0 "CLINK"
	  #x03F1 "Hestan Smart Cooking Inc"
	  #x03F2 "WindowMaster A/S"
	  #x03F3 "Flowscape AB"
	  #x03F4 "PAL Technologies Ltd"
	  #x03F5 "WHERE, Inc"
	  #x03F6 "Iton Technology Corp"
	  #x03F7 "Owl Labs Inc"
	  #x03F8 "Rockford Corp"
	  #x03F9 "Becon Technologies Co.,Ltd"
	  #x03FA "Vyassoft Technologies Inc"
	  #x03FB "Nox Medical"
	  #x03FC "Kimberly-Clark"
	  #x03FD "Trimble Navigation Ltd"
	  #x03FE "Littelfuse"
	  #x03FF "Withings"
	  #x0400 "i-developer IT Beratung UG"
	  #x0401 "Relations Inc"
	  #x0402 "Sears Holdings Corporation"
	  #x0403 "Gantner Electronic GmbH"
	  #x0404 "Authomate Inc"
	  #x0405 "Vertex International, Inc"
	  #x0406 "Airtago"
	  #x0407 "Swiss Audio SA"
	  #x0408 "ToGetHome Inc"
	  #x0409 "AXIS"
	  #x040A "Openmatics"
	  #x040B "Jana Care Inc"
	  #x040C "Senix Corporation"
	  #x040D "NorthStar Battery Company, LLC"
	  #x040E "SKF (U.K.) Limited"
	  #x040F "CO-AX Technology, Inc"
	  #x0410 "Fender Musical Instruments"
	  #x0411 "Luidia Inc"
	  #x0412 "SEFAM"
	  #x0413 "Wireless Cables Inc"
	  #x0414 "Lightning Protection International Pty Ltd"
	  #x0415 "Uber Technologies Inc"
	  #x0416 "SODA GmbH"
	  #x0417 "Fatigue Science"
	  #x0418 "Alpine Electronics Inc"
	  #x0419 "Novalogy LTD"
	  #x041A "Friday Labs Limited"
	  #x041B "OrthoAccel Technologies"
	  #x041C "WaterGuru, Inc"
	  #x041D "Benning Elektrotechnik und Elektronik GmbH & Co. KG"
	  #x041E "Dell Computer Corporation"
	  #x041F "Kopin Corporation"
	  #x0420 "TecBakery GmbH"
	  #x0421 "Backbone Labs, Inc"
	  #x0422 "DELSEY SA"
	  #x0423 "Chargifi Limited"
	  #x0424 "Trainesense Ltd"
	  #x0425 "Unify Software and Solutions GmbH & Co. KG"
	  #x0426 "Husqvarna AB"
	  #x0427 "Focus fleet and fuel management inc"
	  #x0428 "SmallLoop, LLC"
	  #x0429 "Prolon Inc"
	  #x042A "BD Medical"
	  #x042B "iMicroMed Incorporated"
	  #x042C "Ticto N.V"
	  #x042D "Meshtech AS"
	  #x042E "MemCachier Inc"
	  #x042F "Danfoss A/S"
	  #x0430 "SnapStyk Inc"
	  #x0431 "Amway Corporation"
	  #x0432 "Silk Labs, Inc"
	  #x0433 "Pillsy Inc"
	  #x0434 "Hatch Baby, Inc"
	  #x0435 "Blocks Wearables Ltd"
	  #x0436 "Drayson Technologies (Europe) Limited"
	  #x0437 "eBest IOT Inc"
	  #x0438 "Helvar Ltd"
	  #x0439 "Radiance Technologies"
	  #x043A "Nuheara Limited"
	  #x043B "Appside co., ltd"
	  #x043C "DeLaval"
	  #x043D "Coiler Corporation"
	  #x043E "Thermomedics, Inc"
	  #x043F "Tentacle Sync GmbH"
	  #x0440 "Valencell, Inc"
	  #x0441 "iProtoXi Oy"
	  #x0442 "SECOM CO., LTD"
	  #x0443 "Tucker International LLC"
	  #x0444 "Metanate Limited"
	  #x0445 "Kobian Canada Inc"
	  #x0446 "NETGEAR, Inc"
	  #x0447 "Fabtronics Australia Pty Ltd"
	  #x0448 "Grand Centrix GmbH"
	  #x0449 "1UP USA.com llc"
	  #x044A "SHIMANO INC"
	  #x044B "Nain Inc"
	  #x044C "LifeStyle Lock, LLC"
	  #x044D "VEGA Grieshaber KG"
	  #x044E "Xtrava Inc"
	  #x044F "TTS Tooltechnic Systems AG & Co. KG"
	  #x0450 "Teenage Engineering AB"
	  #x0451 "Tunstall Nordic AB"
	  #x0452 "Svep Design Center AB"
	  #x0453 "GreenPeak Technologies BV"
	  #x0454 "Sphinx Electronics GmbH & Co KG"
	  #x0455 "Atomation"
	  #x0456 "Nemik Consulting Inc"
	  #x0457 "RF INNOVATION"
	  #x0458 "Mini Solution Co., Ltd"
	  #x0459 "Lumenetix, Inc"
	  #x045A "2048450 Ontario Inc"
	  #x045B "SPACEEK LTD"
	  #x045C "Delta T Corporation"
	  #x045D "Boston Scientific Corporation"
	  #x045E "Nuviz, Inc"
	  #x045F "Real Time Automation, Inc"
	  #x0460 "Kolibree"
	  #x0461 "vhf elektronik GmbH"
	  #x0462 "Bonsai Systems GmbH"
	  #x0463 "Fathom Systems Inc"
	  #x0464 "Bellman & Symfon"
	  #x0465 "International Forte Group LLC"
	  #x0466 "CycleLabs Solutions inc"
	  #x0467 "Codenex Oy"
	  #x0468 "Kynesim Ltd"
	  #x0469 "Palago AB"
	  #x046A "INSIGMA INC"
	  #x046B "PMD Solutions"
	  #x046C "Qingdao Realtime Technology Co., Ltd"
	  #x046D "BEGA Gantenbrink-Leuchten KG"
	  #x046E "Pambor Ltd"
	  #x046F "Develco Products A/S"
	  #x0470 "iDesign s.r.l"
	  #x0471 "TiVo Corp"
	  #x0472 "Control-J Pty Ltd"
	  #x0473 "Steelcase, Inc"
	  #x0474 "iApartment co., ltd"
	  #x0475 "Icom inc"
	  #x0476 "Oxstren Wearable Technologies Private Limited"
	  #x0477 "Blue Spark Technologies"
	  #x0478 "FarSite Communications Limited"
	  #x0479 "mywerk system GmbH"
	  #x047A "Sinosun Technology Co., Ltd"
	  #x047B "MIYOSHI ELECTRONICS CORPORATION"
	  #x047C "POWERMAT LTD"
	  #x047D "Occly LLC"
	  #x047E "OurHub Dev IvS"
	  #x047F "Pro-Mark, Inc"
	  #x0480 "Dynometrics Inc"
	  #x0481 "Quintrax Limited"
	  #x0482 "POS Tuning Udo Vosshenrich GmbH & Co. KG"
	  #x0483 "Multi Care Systems B.V"
	  #x0484 "Revol Technologies Inc"
	  #x0485 "SKIDATA AG"
	  #x0486 "DEV TECNOLOGIA INDUSTRIA, COMERCIO E MANUTENCAO DE EQUIPAMENTOS LTDA. - ME"
	  #x0487 "Centrica Connected Home"
	  #x0488 "Automotive Data Solutions Inc"
	  #x0489 "Igarashi Engineering"
	  #x048A "Taelek Oy"
	  #x048B "CP Electronics Limited"
	  #x048C "Vectronix AG"
	  #x048D "S-Labs Sp. z o.o"
	  #x048E "Companion Medical, Inc"
	  #x048F "BlueKitchen GmbH"
	  #x0490 "Matting AB"
	  #x0491 "SOREX - Wireless Solutions GmbH"
	  #x0492 "ADC Technology, Inc"
	  #x0493 "Lynxemi Pte Ltd"
	  #x0494 "SENNHEISER electronic GmbH & Co. KG"
	  #x0495 "LMT Mercer Group, Inc"
	  #x0496 "Polymorphic Labs LLC"
	  #x0497 "Cochlear Limited"
	  #x0498 "METER Group, Inc. USA"
	  #x0499 "Ruuvi Innovations Ltd"
	  #x049A "Situne AS"
	  #x049B "nVisti, LLC"
	  #x049C "DyOcean"
	  #x049D "Uhlmann & Zacher GmbH"
	  #x049E "AND!XOR LLC"
	  #x049F "tictote AB"
	  #x04A0 "Vypin, LLC"
	  #x04A1 "PNI Sensor Corporation"
	  #x04A2 "ovrEngineered, LLC"
	  #x04A3 "GT-tronics HK Ltd"
	  #x04A4 "Herbert Waldmann GmbH & Co. KG"
	  #x04A5 "Guangzhou FiiO Electronics Technology Co.,Ltd"
	  #x04A6 "Vinetech Co., Ltd"
	  #x04A7 "Dallas Logic Corporation"
	  #x04A8 "BioTex, Inc"
	  #x04A9 "DISCOVERY SOUND TECHNOLOGY, LLC"
	  #x04AA "LINKIO SAS"
	  #x04AB "Harbortronics, Inc"
	  #x04AC "Undagrid B.V"
	  #x04AD "Shure Inc"
	  #x04AE "ERM Electronic Systems LTD"
	  #x04AF "BIOROWER Handelsagentur GmbH"
	  #x04B0 "Weba Sport und Med. Artikel GmbH"
	  #x04B1 "Kartographers Technologies Pvt. Ltd"
	  #x04B2 "The Shadow on the Moon"
	  #x04B3 "mobike (Hong Kong) Limited"
	  #x04B4 "Inuheat Group AB"
	  #x04B5 "Swiftronix AB"
	  #x04B6 "Diagnoptics Technologies"
	  #x04B7 "Analog Devices, Inc"
	  #x04B8 "Soraa Inc"
	  #x04B9 "CSR Building Products Limited"
	  #x04BA "Crestron Electronics, Inc"
	  #x04BB "Neatebox Ltd"
	  #x04BC "Draegerwerk AG & Co. KGaA"
	  #x04BD "AlbynMedical"
	  #x04BE "Averos FZCO"
	  #x04BF "VIT Initiative, LLC"
	  #x04C0 "Statsports International"
	  #x04C1 "Sospitas, s.r.o"
	  #x04C2 "Dmet Products Corp"
	  #x04C3 "Mantracourt Electronics Limited"
	  #x04C4 "TeAM Hutchins AB"
	  #x04C5 "Seibert Williams Glass, LLC"
	  #x04C6 "Insta GmbH"
	  #x04C7 "Svantek Sp. z o.o"
	  #x04C8 "Shanghai Flyco Electrical Appliance Co., Ltd"
	  #x04C9 "Thornwave Labs Inc"
	  #x04CA "Steiner-Optik GmbH"
	  #x04CB "Novo Nordisk A/S"
	  #x04CC "Enflux Inc"
	  #x04CD "Safetech Products LLC"
	  #x04CE "GOOOLED S.R.L"
	  #x04CF "DOM Sicherheitstechnik GmbH & Co. KG"
	  #x04D0 "Olympus Corporation"
	  #x04D1 "KTS GmbH"
	  #x04D2 "Anloq Technologies Inc"
	  #x04D3 "Queercon, Inc"
	  #x04D4 "5th Element Ltd"
	  #x04D5 "Gooee Limited"
	  #x04D6 "LUGLOC LLC"
	  #x04D7 "Blincam, Inc"
	  #x04D8 "FUJIFILM Corporation"
	  #x04D9 "RandMcNally"
	  #x04DA "Franceschi Marina snc"
	  #x04DB "Engineered Audio, LLC"
	  #x04DC "IOTTIVE (OPC) PRIVATE LIMITED"
	  #x04DD "4MOD Technology"
	  #x04DE "Lutron Electronics Co., Inc"
	  #x04DF "Emerson"
	  #x04E0 "Guardtec, Inc"
	  #x04E1 "REACTEC LIMITED"
	  #x04E2 "EllieGrid"
	  #x04E3 "Under Armour"
	  #x04E4 "Woodenshark"
	  #x04E5 "Avack Oy"
	  #x04E6 "Smart Solution Technology, Inc"
	  #x04E7 "REHABTRONICS INC"
	  #x04E8 "STABILO International"
	  #x04E9 "Busch Jaeger Elektro GmbH"
	  #x04EA "Pacific Bioscience Laboratories, Inc"
	  #x04EB "Bird Home Automation GmbH"
	  #x04EC "Motorola Solutions"
	  #x04ED "R9 Technology, Inc"
	  #x04EE "Auxivia"
	  #x04EF "DaisyWorks, Inc"
	  #x04F0 "Kosi Limited"
	  #x04F1 "Theben AG"
	  #x04F2 "InDreamer Techsol Private Limited"
	  #x04F3 "Cerevast Medical"
	  #x04F4 "ZanCompute Inc"
	  #x04F5 "Pirelli Tyre S.P.A"
	  #x04F6 "McLear Limited"
	  #x04F7 "Shenzhen Huiding Technology Co.,Ltd"
	  #x04F8 "Convergence Systems Limited"
	  #x04F9 "Interactio"
	  #x04FA "Androtec GmbH"
	  #x04FB "Benchmark Drives GmbH & Co. KG"
	  #x04FC "SwingLync L. L. C"
	  #x04FD "Tapkey GmbH"
	  #x04FE "Woosim Systems Inc"
	  #x04FF "Microsemi Corporation"
	  #x0500 "Wiliot LTD"
	  #x0501 "Polaris IND"
	  #x0502 "Specifi-Kali LLC"
	  #x0503 "Locoroll, Inc"
	  #x0504 "PHYPLUS Inc"
	  #x0505 "Inplay Technologies LLC"
	  #x0506 "Hager"
	  #x0507 "Yellowcog"
	  #x0508 "Axes System sp. z o. o"
	  #x0509 "myLIFTER Inc"
	  #x050A "Shake-on B.V"
	  #x050B "Vibrissa Inc"
	  #x050C "OSRAM GmbH"
	  #x050D "TRSystems GmbH"
	  #x050E "Yichip Microelectronics (Hangzhou) Co.,Ltd"
	  #x050F "Foundation Engineering LLC"
	  #x0510 "UNI-ELECTRONICS, INC"
	  #x0511 "Brookfield Equinox LLC"
	  #x0512 "Soprod SA"
	  #x0513 "9974091 Canada Inc"
	  #x0514 "FIBRO GmbH"
	  #x0515 "RB Controls Co., Ltd"
	  #x0516 "Footmarks"
	  #x0517 "Amtronic Sverige AB (formerly Amcore AB)"
	  #x0518 "MAMORIO.inc"
	  #x0519 "Tyto Life LLC"
	  #x051A "Leica Camera AG"
	  #x051B "Angee Technologies Ltd"
	  #x051C "EDPS"
	  #x051D "OFF Line Co., Ltd"
	  #x051E "Detect Blue Limited"
	  #x051F "Setec Pty Ltd"
	  #x0520 "Target Corporation"
	  #x0521 "IAI Corporation"
	  #x0522 "NS Tech, Inc"
	  #x0523 "MTG Co., Ltd"
	  #x0524 "Hangzhou iMagic Technology Co., Ltd"
	  #x0525 "HONGKONG NANO IC TECHNOLOGIES  CO., LIMITED"
	  #x0526 "Honeywell International Inc"
	  #x0527 "Albrecht JUNG"
	  #x0528 "Lunera Lighting Inc"
	  #x0529 "Lumen UAB"
	  #x052A "Keynes Controls Ltd"
	  #x052B "Novartis AG"
	  #x052C "Geosatis SA"
	  #x052D "EXFO, Inc"
	  #x052E "LEDVANCE GmbH"
	  #x052F "Center ID Corp"
	  #x0530 "Adolene, Inc"
	  #x0531 "D&M Holdings Inc"
	  #x0532 "CRESCO Wireless, Inc"
	  #x0533 "Nura Operations Pty Ltd"
	  #x0534 "Frontiergadget, Inc"
	  #x0535 "Smart Component Technologies Limited"
	  #x0536 "ZTR Control Systems LLC"
	  #x0537 "MetaLogics Corporation"
	  #x0538 "Medela AG"
	  #x0539 "OPPLE Lighting Co., Ltd"
	  #x053A "Savitech Corp"
	  #x053B "prodigy"
	  #x053C "Screenovate Technologies Ltd"
	  #x053D "TESA SA"
	  #x053E "CLIM8 LIMITED"
	  #x053F "Silergy Corp"
	  #x0540 "SilverPlus, Inc"
	  #x0541 "Sharknet srl"
	  #x0542 "Mist Systems, Inc"
	  #x0543 "MIWA LOCK CO.,Ltd"
	  #x0544 "OrthoSensor, Inc"
	  #x0545 "Candy Hoover Group s.r.l"
	  #x0546 "Apexar Technologies S.A"
	  #x0547 "LOGICDATA d.o.o"
	  #x0548 "Knick Elektronische Messgeraete GmbH & Co. KG"
	  #x0549 "Smart Technologies and Investment Limited"
	  #x054A "Linough Inc"
	  #x054B "Advanced Electronic Designs, Inc"
	  #x054C "Carefree Scott Fetzer Co Inc"
	  #x054D "Sensome"
	  #x054E "FORTRONIK storitve d.o.o"
	  #x054F "Sinnoz"
	  #x0550 "Versa Networks, Inc"
	  #x0551 "Sylero"
	  #x0552 "Avempace SARL"
	  #x0553 "Nintendo Co., Ltd"
	  #x0554 "National Instruments"
	  #x0555 "KROHNE Messtechnik GmbH"
	  #x0556 "Otodynamics Ltd"
	  #x0557 "Arwin Technology Limited"
	  #x0558 "benegear, inc"
	  #x0559 "Newcon Optik"
	  #x055A "CANDY HOUSE, Inc"
	  #x055B "FRANKLIN TECHNOLOGY INC"
	  #x055C "Lely"
	  #x055D "Valve Corporation"
	  #x055E "Hekatron Vertriebs GmbH"
	  #x055F "PROTECH S.A.S. DI GIRARDI ANDREA & C"
	  #x0560 "Sarita CareTech APS (formerly Sarita CareTech IVS)"
	  #x0561 "Finder S.p.A"
	  #x0562 "Thalmic Labs Inc"
	  #x0563 "Steinel Vertrieb GmbH"
	  #x0564 "Beghelli Spa"
	  #x0565 "Beijing Smartspace Technologies Inc"
	  #x0566 "CORE TRANSPORT TECHNOLOGIES NZ LIMITED"
	  #x0567 "Xiamen Everesports Goods Co., Ltd"
	  #x0568 "Bodyport Inc"
	  #x0569 "Audionics System, INC"
	  #x056A "Flipnavi Co.,Ltd"
	  #x056B "Rion Co., Ltd"
	  #x056C "Long Range Systems, LLC"
	  #x056D "Redmond Industrial Group LLC"
	  #x056E "VIZPIN INC"
	  #x056F "BikeFinder AS"
	  #x0570 "Consumer Sleep Solutions LLC"
	  #x0571 "PSIKICK, INC"
	  #x0572 "AntTail.com"
	  #x0573 "Lighting Science Group Corp"
	  #x0574 "AFFORDABLE ELECTRONICS INC"
	  #x0575 "Integral Memroy Plc"
	  #x0576 "Globalstar, Inc"
	  #x0577 "True Wearables, Inc"
	  #x0578 "Wellington Drive Technologies Ltd"
	  #x0579 "Ensemble Tech Private Limited"
	  #x057A "OMNI Remotes"
	  #x057B "Duracell U.S. Operations Inc"
	  #x057C "Toor Technologies LLC"
	  #x057D "Instinct Performance"
	  #x057E "Beco, Inc"
	  #x057F "Scuf Gaming International, LLC"
	  #x0580 "ARANZ Medical Limited"
	  #x0581 "LYS TECHNOLOGIES LTD"
	  #x0582 "Breakwall Analytics, LLC"
	  #x0583 "Code Blue Communications"
	  #x0584 "Gira Giersiepen GmbH & Co. KG"
	  #x0585 "Hearing Lab Technology"
	  #x0586 "LEGRAND"
	  #x0587 "Derichs GmbH"
	  #x0588 "ALT-TEKNIK LLC"
	  #x0589 "Star Technologies"
	  #x058A "START TODAY CO.,LTD"
	  #x058B "Maxim Integrated Products"
	  #x058C "MERCK Kommanditgesellschaft auf Aktien"
	  #x058D "Jungheinrich Aktiengesellschaft"
	  #x058E "Oculus VR, LLC"
	  #x058F "HENDON SEMICONDUCTORS PTY LTD"
	  #x0590 "Pur3 Ltd"
	  #x0591 "Viasat Group S.p.A"
	  #x0592 "IZITHERM"
	  #x0593 "Spaulding Clinical Research"
	  #x0594 "Kohler Company"
	  #x0595 "Inor Process AB"
	  #x0596 "My Smart Blinds"
	  #x0597 "RadioPulse Inc"
	  #x0598 "rapitag GmbH"
	  #x0599 "Lazlo326, LLC"
	  #x059A "Teledyne Lecroy, Inc"
	  #x059B "Dataflow Systems Limited"
	  #x059C "Macrogiga Electronics"
	  #x059D "Tandem Diabetes Care"
	  #x059E "Polycom, Inc"
	  #x059F "Fisher & Paykel Healthcare"
	  #x05A0 "RCP Software Oy"
	  #x05A1 "Shanghai Xiaoyi Technology Co.,Ltd"
	  #x05A2 "ADHERIUM(NZ) LIMITED"
	  #x05A3 "Axiomware Systems Incorporated"
	  #x05A4 "O. E. M. Controls, Inc"
	  #x05A5 "Kiiroo BV"
	  #x05A6 "Telecon Mobile Limited"
	  #x05A7 "Sonos Inc"
	  #x05A8 "Tom Allebrandi Consulting"
	  #x05A9 "Monidor"
	  #x05AA "Tramex Limited"
	  #x05AB "Nofence AS"
	  #x05AC "GoerTek Dynaudio Co., Ltd"
	  #x05AD "INIA"
	  #x05AE "CARMATE MFG.CO.,LTD"
	  #x05AF "ONvocal"
	  #x05B0 "NewTec GmbH"
	  #x05B1 "Medallion Instrumentation Systems"
	  #x05B2 "CAREL INDUSTRIES S.P.A"
	  #x05B3 "Parabit Systems, Inc"
	  #x05B4 "White Horse Scientific ltd"
	  #x05B5 "verisilicon"
	  #x05B6 "Elecs Industry Co.,Ltd"
	  #x05B7 "Beijing Pinecone Electronics Co.,Ltd"
	  #x05B8 "Ambystoma Labs Inc"
	  #x05B9 "Suzhou Pairlink Network Technology"
	  #x05BA "igloohome"
	  #x05BB "Oxford Metrics plc"
	  #x05BC "Leviton Mfg. Co., Inc"
	  #x05BD "ULC Robotics Inc"
	  #x05BE "RFID Global by Softwork SrL"
	  #x05BF "Real-World-Systems Corporation"
	  #x05C0 "Nalu Medical, Inc"
	  #x05C1 "P.I.Engineering"
	  #x05C2 "Grote Industries"
	  #x05C3 "Runtime, Inc"
	  #x05C4 "Codecoup sp. z o.o. sp. k"
	  #x05C5 "SELVE GmbH & Co. KG"
	  #x05C6 "Smart Animal Training Systems, LLC"
	  #x05C7 "Lippert Components, INC"
	  #x05C8 "SOMFY SAS"
	  #x05C9 "TBS Electronics B.V"
	  #x05CA "MHL Custom Inc"
	  #x05CB "LucentWear LLC"
	  #x05CC "WATTS ELECTRONICS"
	  #x05CD "RJ Brands LLC"
	  #x05CE "V-ZUG Ltd"
	  #x05CF "Biowatch SA"
	  #x05D0 "Anova Applied Electronics"
	  #x05D1 "Lindab AB"
	  #x05D2 "frogblue TECHNOLOGY GmbH"
	  #x05D3 "Acurable Limited"
	  #x05D4 "LAMPLIGHT Co., Ltd"
	  #x05D5 "TEGAM, Inc"
	  #x05D6 "Zhuhai Jieli technology Co.,Ltd"
	  #x05D7 "modum.io AG"
	  #x05D8 "Farm Jenny LLC"
	  #x05D9 "Toyo Electronics Corporation"
	  #x05DA "Applied Neural Research Corp"
	  #x05DB "Avid Identification Systems, Inc"
	  #x05DC "Petronics Inc"
	  #x05DD "essentim GmbH"
	  #x05DE "QT Medical INC"
	  #x05DF "VIRTUALCLINIC.DIRECT LIMITED"
	  #x05E0 "Viper Design LLC"
	  #x05E1 "Human, Incorporated"
	  #x05E2 "stAPPtronics GmbH"
	  #x05E3 "Elemental Machines, Inc"
	  #x05E4 "Taiyo Yuden Co., Ltd"
	  #x05E5 "INEO ENERGY& SYSTEMS"
	  #x05E6 "Motion Instruments Inc"
	  #x05E7 "PressurePro"
	  #x05E8 "COWBOY"
	  #x05E9 "iconmobile GmbH"
	  #x05EA "ACS-Control-System GmbH"
	  #x05EB "Bayerische Motoren Werke AG"
	  #x05EC "Gycom Svenska AB"
	  #x05ED "Fuji Xerox Co., Ltd"
	  #x05EE "Glide Inc"
	  #x05EF "SIKOM AS"
	  #x05F0 "beken"
	  #x05F1 "The Linux Foundation"
	  #x05F2 "Try and E CO.,LTD"
	  #x05F3 "SeeScan"
	  #x05F4 "Clearity, LLC"
	  #x05F5 "GS TAG"
	  #x05F6 "DPTechnics"
	  #x05F7 "TRACMO, INC"
	  #x05F8 "Anki Inc"
	  #x05F9 "Hagleitner Hygiene International GmbH"
	  #x05FA "Konami Sports Life Co., Ltd"
	  #x05FB "Arblet Inc"
	  #x05FC "Masbando GmbH"
	  #x05FD "Innoseis"
	  #x05FE "Niko"
	  #x05FF "Wellnomics Ltd"
	  #x0600 "iRobot Corporation"
	  #x0601 "Schrader Electronics"
	  #x0602 "Geberit International AG"
	  #x0603 "Fourth Evolution Inc"
	  #x0604 "Cell2Jack LLC"
	  #x0605 "FMW electronic Futterer u. Maier-Wolf OHG"
	  #x0606 "John Deere"
	  #x0607 "Rookery Technology Ltd"
	  #x0608 "KeySafe-Cloud"
	  #x0609 "BUCHI Labortechnik AG"
	  #x060A "IQAir AG"
	  #x060B "Triax Technologies Inc"
	  #x060C "Vuzix Corporation"
	  #x060D "TDK Corporation"
	  #x060E "Blueair AB"
	  #x060F "Signify Netherlands"
	  #x0610 "ADH GUARDIAN USA LLC"
	  #x0611 "Beurer GmbH"
	  #x0612 "Playfinity AS"
	  #x0613 "Hans Dinslage GmbH"
	  #x0614 "OnAsset Intelligence, Inc"
	  #x0615 "INTER ACTION Corporation"
	  #x0616 "OS42 UG (haftungsbeschraenkt)"
	  #x0617 "WIZCONNECTED COMPANY LIMITED"
	  #x0618 "Audio-Technica Corporation"
	  #x0619 "Six Guys Labs, s.r.o"
	  #x061A "R.W. Beckett Corporation"
	  #x061B "silex technology, inc"
	  #x061C "Univations Limited"
	  #x061D "SENS Innovation ApS"
	  #x061E "Diamond Kinetics, Inc"
	  #x061F "Phrame Inc"
	  #x0620 "Forciot Oy"
	  #x0621 "Noordung d.o.o"
	  #x0622 "Beam Labs, LLC"
	  #x0623 "Philadelphia Scientific (U.K.) Limited"
	  #x0624 "Biovotion AG"
	  #x0625 "Square Panda, Inc"
	  #x0626 "Amplifico"
	  #x0627 "WEG S.A"
	  #x0628 "Ensto Oy"
	  #x0629 "PHONEPE PVT LTD"
	  #x062A "Lunatico Astronomia SL"
	  #x062B "MinebeaMitsumi Inc"
	  #x062C "ASPion GmbH"
	  #x062D "Vossloh-Schwabe Deutschland GmbH"
	  #x062E "Procept"
	  #x062F "ONKYO Corporation"
	  #x0630 "Asthrea D.O.O"
	  #x0631 "Fortiori Design LLC"
	  #x0632 "Hugo Muller GmbH & Co KG"
	  #x0633 "Wangi Lai PLT"
	  #x0634 "Fanstel Corp"
	  #x0635 "Crookwood"
	  #x0636 "ELECTRONICA INTEGRAL DE SONIDO S.A"
	  #x0637 "GiP Innovation Tools GmbH"
	  #x0638 "LX SOLUTIONS PTY LIMITED"
	  #x0639 "Shenzhen Minew Technologies Co., Ltd"
	  #x063A "Prolojik Limited"
	  #x063B "Kromek Group Plc"
	  #x063C "Contec Medical Systems Co., Ltd"
	  #x063D "Xradio Technology Co.,Ltd"
	  #x063E "The Indoor Lab, LLC"
	  #x063F "LDL TECHNOLOGY"
	  #x0640 "Parkifi"
	  #x0641 "Revenue Collection Systems FRANCE SAS"
	  #x0642 "Bluetrum Technology Co.,Ltd"
	  #x0643 "makita corporation"
	  #x0644 "Apogee Instruments"
	  #x0645 "BM3"
	  #x0646 "SGV Group Holding GmbH & Co. KG"
	  #x0647 "MED-EL"
	  #x0648 "Ultune Technologies"
	  #x0649 "Ryeex Technology Co.,Ltd"
	  #x064A "Open Research Institute, Inc"
	  #x064B "Scale-Tec, Ltd"
	  #x064C "Zumtobel Group AG"
	  #x064D "iLOQ Oy"
	  #x064E "KRUXWorks Technologies Private Limited"
	  #x064F "Digital Matter Pty Ltd"
	  #x0650 "Coravin, Inc"
	  #x0651 "Stasis Labs, Inc"
	  #x0652 "ITZ Innovations- und Technologiezentrum GmbH"
	  #x0653 "Meggitt SA"
	  #x0654 "Ledlenser GmbH & Co. KG"
	  #x0655 "Renishaw PLC"
	  #x0656 "ZhuHai AdvanPro Technology Company Limited"
	  #x0657 "Meshtronix Limited"
	  #x0658 "Payex Norge AS"
	  #x0659 "UnSeen Technologies Oy"
	  #x065A "Zound Industries International AB"
	  #x065B "Sesam Solutions BV"
	  #x065C "PixArt Imaging Inc"
	  #x065D "Panduit Corp"
	  #x065E "Alo AB"
	  #x065F "Ricoh Company Ltd"
	  #x0660 "RTC Industries, Inc"
	  #x0661 "Mode Lighting Limited"
	  #x0662 "Particle Industries, Inc"
	  #x0663 "Advanced Telemetry Systems, Inc"
	  #x0664 "RHA TECHNOLOGIES LTD"
	  #x0665 "Pure International Limited"
	  #x0666 "WTO Werkzeug-Einrichtungen GmbH"
	  #x0667 "Spark Technology Labs Inc"
	  #x0668 "Bleb Technology srl"
	  #x0669 "Livanova USA, Inc"
	  #x066A "Brady Worldwide Inc"
	  #x066B "DewertOkin GmbH"
	  #x066C "Ztove ApS"
	  #x066D "Venso EcoSolutions AB"
	  #x066E "Eurotronik Kranj d.o.o"
	  #x066F "Hug Technology Ltd"
	  #x0670 "Gema Switzerland GmbH"
	  #x0671 "Buzz Products Ltd"
	  #x0672 "Kopi"
	  #x0673 "Innova Ideas Limited"
	  #x0674 "BeSpoon"
	  #x0675 "Deco Enterprises, Inc"
	  #x0676 "Expai Solutions Private Limited"
	  #x0677 "Innovation First, Inc"
	  #x0678 "SABIK Offshore GmbH"
	  #x0679 "4iiii Innovations Inc"
	  #x067A "The Energy Conservatory, Inc"
	  #x067B "I.FARM, INC"
	  #x067C "Tile, Inc"
	  #x067D "Form Athletica Inc"
	  #x067E "MbientLab Inc"
	  #x067F "NETGRID S.N.C. DI BISSOLI MATTEO, CAMPOREALE SIMONE, TOGNETTI FEDERICO"
	  #x0680 "Mannkind Corporation"
	  #x0681 "Trade FIDES a.s"
	  #x0682 "Photron Limited"
	  #x0683 "Eltako GmbH"
	  #x0684 "Dermalapps, LLC"
	  #x0685 "Greenwald Industries"
	  #x0686 "inQs Co., Ltd"
	  #x0687 "Cherry GmbH"
	  #x0688 "Amsted Digital Solutions Inc"
	  #x0689 "Tacx b.v"
	  #x068A "Raytac Corporation"
	  #x068B "Jiangsu Teranovo Tech Co., Ltd"
	  #x068C "Changzhou Sound Dragon Electronics and Acoustics Co., Ltd"
	  #x068D "JetBeep Inc"
	  #x068E "Razer Inc"
	  #x068F "JRM Group Limited"
	  #x0690 "Eccrine Systems, Inc"
	  #x0691 "Curie Point AB"
	  #x0692 "Georg Fischer AG"
	  #x0693 "Hach - Danaher"
	  #x0694 "T&A Laboratories LLC"
	  #x0695 "Koki Holdings Co., Ltd"
	  #x0696 "Gunakar Private Limited"
	  #x0697 "Stemco Products Inc"
	  #x0698 "Wood IT Security, LLC"
	  #x0699 "RandomLab SAS"
	  #x069A "Adero, Inc. (formerly as TrackR, Inc)"
	  #x069B "Dragonchip Limited"
	  #x069C "Noomi AB"
	  #x069D "Vakaros LLC"
	  #x069E "Delta Electronics, Inc"
	  #x069F "FlowMotion Technologies AS"
	  #x06A0 "OBIQ Location Technology Inc"
	  #x06A1 "Cardo Systems, Ltd"
	  #x06A2 "Globalworx GmbH"
	  #x06A3 "Nymbus, LLC"
	  #x06A4 "Sanyo Techno Solutions Tottori Co., Ltd"
	  #x06A5 "TEKZITEL PTY LTD"
	  #x06A6 "Roambee Corporation"
	  #x06A7 "Chipsea Technologies (ShenZhen) Corp"
	  #x06A8 "GD Midea Air-Conditioning Equipment Co., Ltd"
	  #x06A9 "Soundmax Electronics Limited"
	  #x06AA "Produal Oy"
	  #x06AB "HMS Industrial Networks AB"
	  #x06AC "Ingchips Technology Co., Ltd"
	  #x06AD "InnovaSea Systems Inc"
	  #x06AE "SenseQ Inc"
	  #x06AF "Shoof Technologies"
	  #x06B0 "BRK Brands, Inc"
	  #x06B1 "SimpliSafe, Inc"
	  #x06B2 "Tussock Innovation 2013 Limited"
	  #x06B3 "The Hablab ApS"
	  #x06B4 "Sencilion Oy"
	  #x06B5 "Wabilogic Ltd"
	  #x06B6 "Sociometric Solutions, Inc"
	  #x06B7 "iCOGNIZE GmbH"
	  #x06B8 "ShadeCraft, Inc"
	  #x06B9 "Beflex Inc"
	  #x06BA "Beaconzone Ltd"
	  #x06BB "Leaftronix Analogic Solutions Private Limited"
	  #x06BC "TWS Srl"
	  #x06BD "ABB Oy"
	  #x06BE "HitSeed Oy"
	  #x06BF "Delcom Products Inc"
	  #x06C0 "CAME S.p.A"
	  #x06C1 "Alarm.com Holdings, Inc"
	  #x06C2 "Measurlogic Inc"
	  #x06C3 "King I Electronics.Co.,Ltd"
	  #x06C4 "Dream Labs GmbH"
	  #x06C5 "Urban Compass, Inc"
	  #x06C6 "Simm Tronic Limited"
	  #x06C7 "Somatix Inc"
	  #x06C8 "Storz & Bickel GmbH & Co. KG"
	  #x06C9 "MYLAPS B.V"
	  #x06CA "Shenzhen Zhongguang Infotech Technology Development Co., Ltd"
	  #x06CB "Dyeware, LLC"
	  #x06CC "Dongguan SmartAction Technology Co.,Ltd"
	  #x06CD "DIG Corporation"
	  #x06CE "FIOR & GENTZ"
	  #x06CF "Belparts N.V"
	  #x06D0 "Etekcity Corporation"
	  #x06D1 "Meyer Sound Laboratories, Incorporated"
	  #x06D2 "CeoTronics AG"
	  #x06D3 "TriTeq Lock and Security, LLC"
	  #x06D4 "DYNAKODE TECHNOLOGY PRIVATE LIMITED"
	  #x06D5 "Sensirion AG"
	  #x06D6 "JCT Healthcare Pty Ltd"
	  #x06D7 "FUBA Automotive Electronics GmbH"
	  #x06D8 "AW Company"
	  #x06D9 "Shanghai Mountain View Silicon Co.,Ltd"
	  #x06DA "Zliide Technologies ApS"
	  #x06DB "Automatic Labs, Inc"
	  #x06DC "Industrial Network Controls, LLC"
	  #x06DD "Intellithings Ltd"
	  #x06DE "Navcast, Inc"
	  #x06DF "Hubbell Lighting, Inc"
	  #x06E0 "Avaya"
	  #x06E1 "Milestone AV Technologies LLC"
	  #x06E2 "Alango Technologies Ltd"
	  #x06E3 "Spinlock Ltd"
	  #x06E4 "Aluna"
	  #x06E5 "OPTEX CO.,LTD"
	  #x06E6 "NIHON DENGYO KOUSAKU"
	  #x06E7 "VELUX A/S"
	  #x06E8 "Almendo Technologies GmbH"
	  #x06E9 "Zmartfun Electronics, Inc"
	  #x06EA "SafeLine Sweden AB"
	  #x06EB "Houston Radar LLC"
	  #x06EC "Sigur"
	  #x06ED "J Neades Ltd"
	  #x06EE "Avantis Systems Limited"
	  #x06EF "ALCARE Co., Ltd"
	  #x06F0 "Chargy Technologies, SL"
	  #x06F1 "Shibutani Co., Ltd"
	  #x06F2 "Trapper Data AB"
	  #x06F3 "Alfred International Inc"
	  #x06F4 "Near Field Solutions Ltd"
	  #x06F5 "Vigil Technologies Inc"
	  #x06F6 "Vitulo Plus BV"
	  #x06F7 "WILKA Schliesstechnik GmbH"
	  #x06F8 "BodyPlus Technology Co.,Ltd"
	  #x06F9 "happybrush GmbH"
	  #x06FA "Enequi AB"
	  #x06FB "Sartorius AG"
	  #x06FC "Tom Communication Industrial Co.,Ltd"
	  #x06FD "ESS Embedded System Solutions Inc"
	  #x06FE "Mahr GmbH"
	  #x06FF "Redpine Signals Inc"
	  #x0700 "TraqFreq LLC"
	  #x0701 "PAFERS TECH"
	  #x0702 "Akciju sabiedriba SAF TEHNIKA"
	  #x0703 "Beijing Jingdong Century Trading Co., Ltd"
	  #x0704 "JBX Designs Inc"
	  #x0705 "AB Electrolux"
	  #x0706 "Wernher von Braun Center for ASdvanced Research"
	  #x0707 "Essity Hygiene and Health Aktiebolag"
	  #x0708 "Be Interactive Co., Ltd"
	  #x0709 "Carewear Corp"
	  #x070A "Huf Hülsbeck & Fürst GmbH & Co. KG"
	  #x070B "Element Products, Inc"
	  #x070C "Beijing Winner Microelectronics Co.,Ltd"
	  #x070D "SmartSnugg Pty Ltd"
	  #x070E "FiveCo Sarl"
	  #x070F "California Things Inc"
	  #x0710 "Audiodo AB"
	  #x0711 "ABAX AS"
	  #x0712 "Bull Group Company Limited"
	  #x0713 "Respiri Limited"
	  #x0714 "MindPeace Safety LLC"
	  #x0715 "Vgyan Solutions"
	  #x0716 "Altonics"
	  #x0717 "iQsquare BV"
	  #x0718 "IDIBAIX enginneering"
	  #x0719 "ECSG"
	  #x071A "REVSMART WEARABLE HK CO LTD"
	  #x071B "Precor"
	  #x071C "F5 Sports, Inc"
	  #x071D "exoTIC Systems"
	  #x071E "DONGGUAN HELE ELECTRONICS CO., LTD"
	  #x071F "Dongguan Liesheng Electronic Co.Ltd"
	  #x0720 "Oculeve, Inc"
	  #x0721 "Clover Network, Inc"
	  #x0722 "Xiamen Eholder Electronics Co.Ltd"
	  #x0723 "Ford Motor Company"
	  #x0724 "Guangzhou SuperSound Information Technology Co.,Ltd"
	  #x0725 "Tedee Sp. z o.o"
	  #x0726 "PHC Corporation"
	  #x0727 "STALKIT AS"
	  #x0728 "Eli Lilly and Company"
	  #x0729 "SwaraLink Technologies"
	  #x072A "JMR embedded systems GmbH"
	  #x072B "Bitkey Inc"
	  #x072C "GWA Hygiene GmbH"
	  #x072D "Safera Oy"
	  #x072E "Open Platform Systems LLC"
	  #x072F "OnePlus Electronics (Shenzhen) Co., Ltd"
	  #x0730 "Wildlife Acoustics, Inc"
	  #x0731 "ABLIC Inc"
	  #x0732 "Dairy Tech, Inc"
	  #x0733 "Iguanavation, Inc"
	  #x0734 "DiUS Computing Pty Ltd"
	  #x0735 "UpRight Technologies LTD"
	  #x0736 "FrancisFund, LLC"
	  #x0737 "LLC Navitek"
	  #x0738 "Glass Security Pte Ltd"
	  #x0739 "Jiangsu Qinheng Co., Ltd"
	  #x073A "Chandler Systems Inc"
	  #x073B "Fantini Cosmi s.p.a"
	  #x073C "Acubit ApS"
	  #x073D "Beijing Hao Heng Tian Tech Co., Ltd"
	  #x073E "Bluepack S.R.L"
	  #x073F "Beijing Unisoc Technologies Co., Ltd"
	  #x0740 "HITIQ LIMITED"
	  #x0741 "MAC SRL"
	  #x0742 "DML LLC"
	  #x0743 "Sanofi"
	  #x0744 "SOCOMEC"
	  #x0745 "WIZNOVA, Inc"
	  #x0746 "Seitec Elektronik GmbH"
	  #x0747 "OR Technologies Pty Ltd"
	  #x0748 "GuangZhou KuGou Computer Technology Co.Ltd"
	  #x0749 "DIAODIAO (Beijing) Technology Co., Ltd"
	  #x074A "Illusory Studios LLC"
	  #x074B "Sarvavid Software Solutions LLP"
	  #x074C "iopool s.a"
	  #x074D "Amtech Systems, LLC"
	  #x074E "EAGLE DETECTION SA"
	  #x074F "MEDIATECH S.R.L"
	  #x0750 "Hamilton Professional Services of Canada Incorporated"
	  #x0751 "Changsha JEMO IC Design Co.,Ltd"
	  #x0752 "Elatec GmbH"
	  #x0753 "JLG Industries, Inc"
	  #x0754 "Michael Parkin"
	  #x0755 "Brother Industries, Ltd"
	  #x0756 "Lumens For Less, Inc"
	  #x0757 "ELA Innovation"
	  #x0758 "umanSense AB"
	  #x0759 "Shanghai InGeek Cyber Security Co., Ltd"
	  #x075A "HARMAN CO.,LTD"
	  #x075B "Smart Sensor Devices AB"
	  #x075C "Antitronics Inc"
	  #x075D "RHOMBUS SYSTEMS, INC"
	  #x075E "Katerra Inc"
	  #x075F "Remote Solution Co., LTD"
	  #x0760 "Vimar SpA"
	  #x0761 "Mantis Tech LLC"
	  #x0762 "TerOpta Ltd"
	  #x0763 "PIKOLIN S.L"
	  #x0764 "WWZN Information Technology Company Limited"
	  #x0765 "Voxx International"
	  #x0766 "ART AND PROGRAM, INC"
	  #x0767 "NITTO DENKO ASIA TECHNICAL CENTRE PTE. LTD"
	  #x0768 "Peloton Interactive Inc"
	  #x0769 "Force Impact Technologies"
	  #x076A "Dmac Mobile Developments, LLC"
	  #x076B "Engineered Medical Technologies"
	  #x076C "Noodle Technology inc"
	  #x076D "Graesslin GmbH"
	  #x076E "WuQi technologies, Inc"
	  #x076F "Successful Endeavours Pty Ltd"
	  #x0770 "InnoCon Medical ApS"
	  #x0771 "Corvex Connected Safety"
	  #x0772 "Thirdwayv Inc"
	  #x0773 "Echoflex Solutions Inc"
	  #x0774 "C-MAX Asia Limited"
	  #x0775 "4eBusiness GmbH"
	  #x0776 "Cyber Transport Control GmbH"
	  #x0777 "Cue"
	  #x0778 "KOAMTAC INC"
	  #x0779 "Loopshore Oy"
	  #x077A "Niruha Systems Private Limited"
	  #x077B "AmaterZ, Inc"
	  #x077C "radius co., ltd"
	  #x077D "Sensority, s.r.o"
	  #x077E "Sparkage Inc"
	  #x077F "Glenview Software Corporation"
	  #x0780 "Finch Technologies Ltd"
	  #x0781 "Qingping Technology (Beijing) Co., Ltd"
	  #x0782 "DeviceDrive AS"
	  #x0783 "ESEMBER LIMITED LIABILITY COMPANY"
	  #x0784 "audifon GmbH & Co. KG"
	  #x0785 "O2 Micro, Inc"
	  #x0786 "HLP Controls Pty Limited"
	  #x0787 "Pangaea Solution"
	  #x0788 "BubblyNet, LLC"
	  #x078A "The Wildflower Foundation"
	  #x078B "Optikam Tech Inc"
	  #x078C "MINIBREW HOLDING B.V"
	  #x078D "Cybex GmbH"
	  #x078E "FUJIMIC NIIGATA, INC"
	  #x078F "Hanna Instruments, Inc"
	  #x0790 "KOMPAN A/S"
	  #x0791 "Scosche Industries, Inc"
	  #x0792 "Provo Craft"
	  #x0793 "AEV spol. s r.o"
	  #x0794 "The Coca-Cola Company"
	  #x0795 "GASTEC CORPORATION"
	  #x0796 "StarLeaf Ltd"
	  #x0797 "Water-i.d. GmbH"
	  #x0798 "HoloKit, Inc"
	  #x0799 "PlantChoir Inc"
	  #x079A "GuangDong Oppo Mobile Telecommunications Corp., Ltd"
	  #x079B "CST ELECTRONICS (PROPRIETARY) LIMITED"
	  #x079C "Sky UK Limited"
	  #x079D "Digibale Pty Ltd"
	  #x079E "Smartloxx GmbH"
	  #x079F "Pune Scientific LLP"
	  #x07A0 "Regent Beleuchtungskorper AG"
	  #x07A1 "Apollo Neuroscience, Inc"
	  #x07A2 "Roku, Inc"
	  #x07A3 "Comcast Cable"
	  #x07A4 "Xiamen Mage Information Technology Co., Ltd"
	  #x07A5 "RAB Lighting, Inc"
	  #x07A6 "Musen Connect, Inc"
	  #x07A7 "Zume, Inc"
	  #x07A8 "conbee GmbH"
	  #x07A9 "Bruel & Kjaer Sound & Vibration"
	  #x07AA "The Kroger Co"
	  #x07AB "Granite River Solutions, Inc"
	  #x07AC "LoupeDeck Oy"
	  #x07AD "New H3C Technologies Co.,Ltd"
	  #x07AE "Aurea Solucoes Tecnologicas Ltda"
	  #x07AF "Hong Kong Bouffalo Lab Limited"
	  #x07B0 "GV Concepts Inc"
	  #x07B1 "Thomas Dynamics, LLC"
	  #x07B2 "Moeco IOT Inc"
	  #x07B3 "2N TELEKOMUNIKACE a.s"
	  #x07B4 "Hormann KG Antriebstechnik"
	  #x07B5 "CRONO CHIP, S.L"
	  #x07B6 "Soundbrenner Limited"
	  #x07B7 "ETABLISSEMENTS GEORGES RENAULT"
	  #x07B8 "iSwip"
	  #x07B9 "Epona Biotec Limited"
	  #x07BA "Battery-Biz Inc"
	  #x07BB "EPIC S.R.L"
	  #x07BC "KD CIRCUITS LLC"
	  #x07BD "Genedrive Diagnostics Ltd"
	  #x07BE "Axentia Technologies AB"
	  #x07BF "REGULA Ltd"
	  #x07C0 "Biral AG"
	  #x07C1 "A.W. Chesterton Company"
	  #x07C2 "Radinn AB"
	  #x07C3 "CIMTechniques, Inc"
	  #x07C4 "Johnson Health Tech NA"
	  #x07C5 "June Life, Inc"
	  #x07C6 "Bluenetics GmbH"
	  #x07C7 "iaconicDesign Inc"
	  #x07C8 "WRLDS Creations AB"
	  #x07C9 "Skullcandy, Inc"
	  #x07CA "Modul-System HH AB"
	  #x07CB "West Pharmaceutical Services, Inc"
	  #x07CC "Barnacle Systems Inc"
	  #x07CD "Smart Wave Technologies Canada Inc"
	  #x07CE "Shanghai Top-Chip Microelectronics Tech. Co., LTD"
	  #x07CF "NeoSensory, Inc"
	  #x07D0 "Hangzhou Tuya Information  Technology Co., Ltd"
	  #x07D1 "Shanghai Panchip Microelectronics Co., Ltd"
	  #x07D2 "React Accessibility Limited"
	  #x07D3 "LIVNEX Co.,Ltd"
	  #x07D4 "Kano Computing Limited"
	  #x07D5 "hoots classic GmbH"
	  #x07D6 "ecobee Inc"
	  #x07D7 "Nanjing Qinheng Microelectronics Co., Ltd"
	  #x07D8 "SOLUTIONS AMBRA INC"
	  #x07D9 "Micro-Design, Inc"
	  #x07DA "STARLITE Co., Ltd"
	  #x07DB "Remedee Labs"
	  #x07DC "ThingOS GmbH"
	  #x07DD "Linear Circuits"
	  #x07DE "Unlimited Engineering SL"
	  #x07DF "Snap-on Incorporated"
	  #x07E0 "Edifier International Limited"
	  #x07E1 "Lucie Labs"
	  #x07E2 "Alfred Kaercher SE & Co. KG"
	  #x07E3 "Audiowise Technology Inc"
	  #x07E4 "Geeksme S.L"
	  #x07E5 "Minut, Inc"
	  #x07E6 "Autogrow Systems Limited"
	  #x07E7 "Komfort IQ, Inc"
	  #x07E8 "Packetcraft, Inc"
	  #x07E9 "Häfele GmbH & Co KG"
	  #x07EA "ShapeLog, Inc"
	  #x07EB "NOVABASE S.R.L"
	  #x07EC "Frecce LLC"
	  #x07ED "Joule IQ, INC"
	  #x07EE "KidzTek LLC"
	  #x07EF "Aktiebolaget Sandvik Coromant"
	  #x07F0 "e-moola.com Pty Ltd"
	  #x07F1 "GSM Innovations Pty Ltd"
	  #x07F2 "SERENE GROUP, INC"
	  #x07F3 "DIGISINE ENERGYTECH CO. LTD"
	  #x07F4 "MEDIRLAB Orvosbiologiai Fejleszto Korlatolt Felelossegu Tarsasag"
	  #x07F5 "Byton North America Corporation"
	  #x07F6 "Shenzhen TonliScience and Technology Development Co.,Ltd"
	  #x07F7 "Cesar Systems Ltd"
	  #x07F8 "quip NYC Inc"
	  #x07F9 "Direct Communication Solutions, Inc"
	  #x07FA "Klipsch Group, Inc"
	  #x07FB "Access Co., Ltd"
	  #x07FC "Renault SA"
	  #x07FD "JSK CO., LTD"
	  #x07FE "BIROTA"
	  #x07FF "maxon motor ltd"
	  #x0800 "Optek"
	  #x0801 "CRONUS ELECTRONICS LTD"
	  #x0802 "NantSound, Inc"
	  #x0803 "Domintell s.a"
	  #x0804 "Andon Health Co.,Ltd"
	  #x0805 "Urbanminded Ltd"
	  #x0806 "TYRI Sweden AB"
	  #x0807 "ECD Electronic Components GmbH Dresden"
	  #x0808 "SISTEMAS KERN, SOCIEDAD ANÓMINA"
	  #x0809 "Trulli Audio"
	  #x080A "Altaneos"
	  #x080B "Nanoleaf Canada Limited"
	  #x080C "Ingy B.V"
	  #x080D "Azbil Co"
	  #x080E "TATTCOM LLC"
	  #x080F "Paradox Engineering SA"
	  #x0810 "LECO Corporation"
	  #x0811 "Becker Antriebe GmbH"
	  #x0812 "Mstream Technologies., Inc"
	  #x0813 "Flextronics International USA Inc"
	  #x0814 "Ossur hf"
	  #x0815 "SKC Inc"
	  #x0816 "SPICA SYSTEMS LLC"
	  #x0817 "Wangs Alliance Corporation"
	  #x0818 "tatwah SA"
	  #x0819 "Hunter Douglas Inc"
	  #x081A "Shenzhen Conex"
	  #x081B "DIM3"
	  #x081C "Bobrick Washroom Equipment, Inc"
	  #x081D "Potrykus Holdings and Development LLC"
	  #x081E "iNFORM Technology GmbH"
	  #x081F "eSenseLab LTD"
	  #x0820 "Brilliant Home Technology, Inc"
	  #x0821 "INOVA Geophysical, Inc"
	  #x0822 "adafruit industries"
	  #x0823 "Nexite Ltd"
	  #x0824 "8Power Limited"
	  #x0825 "CME PTE. LTD."
	  #x0826 "Hyundai Motor Company"
	  #x0827 "Kickmaker"
	  #x0828 "Shanghai Suisheng Information Technology Co., Ltd."
	  #x0829 "HEXAGON METROLOGY DIVISION ROMER"
	  #x082A "Mitutoyo Corporation"
	  #x082B "shenzhen fitcare electronics Co.,Ltd"
	  #x082C "INGICS TECHNOLOGY CO., LTD."
	  #x082D "INCUS PERFORMANCE LTD."
	  #x082E "ABB S.p.A."
	  #x082F "Blippit AB"
	  #x0830 "Core Health and Fitness LLC"
	  #x0831 "Foxble, LLC"
	  #x0832 "Intermotive,Inc."
	  #x0833 "Conneqtech B.V."
	  #x0834 "RIKEN KEIKI CO., LTD."
	  #x0835 "Canopy Growth Corporation"
	  #x0836 "Bitwards Oy"
	  #x0837 "vivo Mobile Communication Co., Ltd."
	  #x0838 "Etymotic Research, Inc."
	  #x0839 "A puissance 3"
	  #x083A "BPW Bergische Achsen Kommanditgesellschaft"
	  #x083B "Piaggio Fast Forward"
	  #x083C "BeerTech LTD"
	  #x083D "Tokenize, Inc."
	  #x083E "Zorachka LTD"
	  #x083F "D-Link Corp."
	  #x0840 "Down Range Systems LLC"
	  #x0841 "General Luminaire (Shanghai) Co., Ltd."
	  #x0842 "Tangshan HongJia electronic technology co., LTD."
	  #x0843 "FRAGRANCE DELIVERY TECHNOLOGIES LTD"
	  #x0844 "Pepperl + Fuchs GmbH"
	  #x0845 "Dometic Corporation"
	  #x0846 "USound GmbH"
	  #x0847 "DNANUDGE LIMITED"
	  #x0848 "JUJU JOINTS CANADA CORP."
	  #x0849 "Dopple Technologies B.V."
	  #x084A "ARCOM"
	  #x084B "Biotechware SRL"
	  #x084C "ORSO Inc."
	  #x084D "SafePort"
	  #x084E "Carol Cole Company"
	  #x084F "Embedded Fitness B.V."
	  #x0850 "Yealink (Xiamen) Network Technology Co.,LTD"
	  #x0851 "Subeca, Inc."
	  #x0852 "Cognosos, Inc."
	  #x0853 "Pektron Group Limited"
	  #x0854 "Tap Sound System"
	  #x0855 "Helios Hockey, Inc."
	  #x0856 "Canopy Growth Corporation"
	  #x0857 "Parsyl Inc"
	  #x0858 "SOUNDBOKS"
	  #x0859 "BlueUp"
	  #x085A "DAKATECH"
	  #x085B "RICOH ELECTRONIC DEVICES CO., LTD."
	  #x085C "ACOS CO.,LTD."
	  #x085D "Guilin Zhishen Information Technology Co.,Ltd."
	  #x085E "Krog Systems LLC"
	  #x085F "COMPEGPS TEAM,SOCIEDAD LIMITADA"
	  #x0860 "Alflex Products B.V."
	  #x0861 "SmartSensor Labs Ltd"
	  #x0862 "SmartDrive Inc."
	  #x0863 "Yo-tronics Technology Co., Ltd."
	  #x0864 "Rafaelmicro"
	  #x0865 "Emergency Lighting Products Limited"
	  #x0866 "LAONZ Co.,Ltd"
	  #x0867 "Western Digital Techologies, Inc."
	  #x0868 "WIOsense GmbH & Co. KG"
	  #x0869 "EVVA Sicherheitstechnologie GmbH"
	  #x086A "Odic Incorporated"
	  #x086B "Pacific Track, LLC"
	  #x086C "Revvo Technologies, Inc."
	  #x086D "Biometrika d.o.o."
	  #x086E "Vorwerk Elektrowerke GmbH & Co. KG"
	  #x086F "Trackunit A/S"
	  #x0870 "Wyze Labs, Inc"
	  #x0871 "Dension Elektronikai Kft. (formerly: Dension Audio Systems Ltd.)"
	  #x0872 "11 Health & Technologies Limited"
	  #x0873 "Innophase Incorporated"
	  #x0874 "Treegreen Limited"
	  #x0875 "Berner International LLC"
	  #x0876 "SmartResQ ApS"
	  #x0877 "Tome, Inc."
	  #x0878 "The Chamberlain Group, Inc."
	  #x0879 "MIZUNO Corporation"
	  #x087A "ZRF, LLC"
	  #x087B "BYSTAMP"
	  #x087C "Crosscan GmbH"
	  #x087D "Konftel AB"
	  #x087E "1bar.net Limited"
	  #x087F "Phillips Connect Technologies LLC"
	  #x0880 "imagiLabs AB"
	  #x0881 "Optalert"
	  #x0882 "PSYONIC, Inc."
	  #x0883 "Wintersteiger AG"
	  #x0884 "Controlid Industria, Comercio de Hardware e Servicos de Tecnologia Ltda"
	  #x0885 "LEVOLOR, INC."
	  #x0886 "Xsens Technologies B.V."
	  #x0887 "Hydro-Gear Limited Partnership"
	  #x0888 "EnPointe Fencing Pty Ltd"
	  #x0889 "XANTHIO"
	  #x088A "sclak s.r.l."
	  #x088B "Tricorder Arraay Technologies LLC"
	  #x088C "GB Solution co.,Ltd"
	  #x088D "Soliton Systems K.K."
	  #x088E "GIGA-TMS INC"
	  #x088F "Tait International Limited"
	  #x0890 "NICHIEI INTEC CO., LTD."
	  #x0891 "SmartWireless GmbH & Co. KG"
	  #x0892 "Ingenieurbuero Birnfeld UG (haftungsbeschraenkt)"
	  #x0893 "Maytronics Ltd"
	  #x0894 "EPIFIT"
	  #x0895 "Gimer medical"
	  #x0896 "Nokian Renkaat Oyj"
	  #x0897 "Current Lighting Solutions LLC"
	  #x0898 "Sensibo, Inc."
	  #x0899 "SFS unimarket AG"
	  #x089A "Private limited company \"Teltonika\""
	  #x089B "Saucon Technologies"
	  #x089C "Embedded Devices Co. Company"
	  #x089D "J-J.A.D.E. Enterprise LLC"
	  #x089E "i-SENS, inc."
	  #x089F "Witschi Electronic Ltd"
	  #x08A0 "Aclara Technologies LLC"
	  #x08A1 "EXEO TECH CORPORATION"
	  #x08A2 "Epic Systems Co., Ltd."
	  #x08A3 "Hoffmann SE"
	  #x08A4 "Realme Chongqing Mobile Telecommunications Corp., Ltd."
	  #x08A5 "UMEHEAL Ltd"
	  #x08A6 "Intelligenceworks Inc."
	  #x08A7 "TGR 1.618 Limited"
	  #x08A8 "Shanghai Kfcube Inc"
	  #x08A9 "Fraunhofer IIS"
	  #x08AA "SZ DJI TECHNOLOGY CO.,LTD"
	  #x08AB "Coburn Technology, LLC"
	  #x08AC "Topre Corporation"
	  #x08AD "Kayamatics Limited"
	  #x08AE "Moticon ReGo AG"
	  #x08AF " Polidea Sp. z o.o."
	  #x08B0 "Trivedi Advanced Technologies LLC"
	  #x08B1 "CORE|vision BV"
	  #x08B2 "PF SCHWEISSTECHNOLOGIE GMBH"
	  #x08B3 "IONIQ Skincare GmbH & Co. KG"
	  #x08B4 "Sengled Co., Ltd."
	  #x08B5 "TransferFi"
	  #x08B6 "Boehringer Ingelheim Vetmedica GmbH"
	  #x08B7 "ABB Inc"
	  #x08B8 "Check Technology Solutions LLC"
	  #x08B9 "U-Shin Ltd."
	  #x08BA "HYPER ICE, INC."
	  #x08BB "Tokai-rika co.,ltd."
	  #x08BC "Prevayl Limited"
	  #x08BD "bf1systems limited"
	  #x08BE "ubisys technologies GmbH"
	  #x08BF "SIRC Co., Ltd."
	  #x08C0 "Accent Advanced Systems SLU"
	  #x08C1 "Rayden.Earth LTD"
	  #x08C2 "Lindinvent AB"
	  #x08C3 "CHIPOLO d.o.o."
	  #x08C4 "CellAssist, LLC"
	  #x08C5 "J. Wagner GmbH"
	  #x08C6 "Integra Optics Inc"
	  #x08C7 "Monadnock Systems Ltd."
	  #x08C8 "Liteboxer Technologies Inc."
	  #x08C9 "Noventa AG"
	  #x08CA "Nubia Technology Co.,Ltd."
	  #x08CB "JT INNOVATIONS LIMITED"
	  #x08CC "TGM TECHNOLOGY CO., LTD."
	  #x08CD "ifly"
	  #x08CE "ZIMI CORPORATION"
	  #x08CF "betternotstealmybike UG (with limited liability)"
	  #x08D0 "ESTOM Infotech Kft."
	  #x08D1 "Sensovium Inc."
	  #x08D2 "Virscient Limited"
	  #x08D3 "Novel Bits, LLC"
	  #x08D4 "ADATA Technology Co., LTD."
	  #x08D5 "KEYes"
	  #x08D6 "Nome Oy"
	  #x08D7 "Inovonics Corp"
	  #x08D8 "WARES"
	  #x08D9 "Pointr Labs Limited"
	  #x08DA "Miridia Technology Incorporated"
	  #x08DB "Tertium Technology"
	  #x08DC "SHENZHEN AUKEY E BUSINESS CO., LTD"
	  #x08DD "code-Q"
	  #x08DE "Tyco Electronics Corporation a TE Connectivity Ltd Company"
	  #x08DF "IRIS OHYAMA CO.,LTD."
	  #x08E0 "Philia Technology"
	  #x08E1 "KOZO KEIKAKU ENGINEERING Inc."
	  #x08E2 "Shenzhen Simo Technology co. LTD"
	  #x08E3 "Republic Wireless, Inc."
	  #x08E4 "Rashidov ltd"
	  #x08E5 "Crowd Connected Ltd"
	  #x08E6 "Eneso Tecnologia de Adaptacion S.L."
	  #x08E7 "Barrot Technology Limited"
	  #x08E8 "Naonext"
	  #x08E9 "Taiwan Intelligent Home Corp."
	  #x08EA "COWBELL ENGINEERING CO.,LTD."
	  #x08EB "Beijing Big Moment Technology Co., Ltd."
	  #x08EC "Denso Corporation"
	  #x08ED "IMI Hydronic Engineering International SA"
	  #x08EE "ASKEY"
	  #x08EF "Cumulus Digital Systems, Inc"
	  #x08F0 "Joovv, Inc."
	  #x08F1 "The L.S. Starrett Company"
	  #x08F2 "Microoled"
	  #x08F3 "PSP - Pauli Services & Products GmbH"
	  #x08F4 "Kodimo Technologies Company Limited"
	  #x08F5 "Tymtix Technologies Private Limited"
	  #x08F6 "Dermal Photonics Corporation"
	  #x08F7 "MTD Products Inc & Affiliates"
	  #x08F8 "instagrid GmbH"
	  #x08F9 "Spacelabs Medical Inc."
	  #x08FA "Troo Corporation"
	  #x08FB "Darkglass Electronics Oy"
	  #x08FC "Hill-Rom"
	  #x08FD "BioIntelliSense, Inc."
	  #x08FE "Ketronixs Sdn Bhd"
	  #x0904 "SUNCORPORATION"
	  #x0905 "Yandex Services AG"
	  #x0906 "Scope Logistical Solutions"
	  #x0907 "User Hello, LLC"
	  #x0908 "Pinpoint Innovations Limited"
	  #x0909 "70mai Co.,Ltd."
	  #x090A "Zhuhai Hoksi Technology CO.,LTD"
	  #x090B "EMBR labs, INC"
	  #x090C "Radiawave Technologies Co.,Ltd."
	  #x090D "IOT Invent GmbH"
	  #x090E "OPTIMUSIOT TECH LLP"
	  #x090F "VC Inc."
	  #x0910 "ASR Microelectronics (Shanghai) Co., Ltd."
	  #x0911 "Douglas Lighting Controls Inc."
	  #x0912 "Nerbio Medical Software Platforms Inc"
	  #x0913 "Braveheart Wireless, Inc."
	  #x0914 "INEO-SENSE"
	  #x0915 "Honda Motor Co., Ltd."
	  #x0916 "Ambient Sensors LLC"
	  #x0917 "ASR Microelectronics(ShenZhen)Co., Ltd."
	  #x0918 "Technosphere Labs Pvt. Ltd."
	  #x0919 "NO SMD LIMITED"
	  #x091A "Albertronic BV"
	  #x091B "Luminostics, Inc."
	  #x091C "Oblamatik AG"
	  #x091D "Innokind, Inc."
	  #x091E "Melbot Studios, Sociedad Limitada"
	  #x091F "Myzee Technology"
	  #x0920 "Omnisense Limited"
	  #x0921 "KAHA PTE. LTD."
	  #x0922 "Shanghai MXCHIP Information Technology Co., Ltd."
	  #x0923 "JSB TECH PTE LTD"
	  #x0924 "Fundacion Tecnalia Research and Innovation"
	  #x0925 "Yukai Engineering Inc."
	  #x0926 "Gooligum Technologies Pty Ltd"
	  #x0927 "ROOQ GmbH"
	  #x0928 "AiRISTA"
	  #x0929 "Qingdao Haier Technology Co., Ltd."
	  #x092A "Sappl Verwaltungs- und Betriebs GmbH"
	  #x092B "TekHome"
	  #x092C "PCI Private Limited"
	  #x092D "Leggett & Platt, Incorporated"
	  #x092E "PS GmbH"
	  #x092F "C.O.B.O. SpA"
	  #x0930 "James Walker RotaBolt Limited"
	  #x0931 "BREATHINGS Co., Ltd."
	  #x0932 "BarVision, LLC"
	  #x0933 "SRAM"
	  #x0934 "KiteSpring Inc."
	  #x0935 "Reconnect, Inc."
	  #x0936 "Elekon AG"
	  #x0937 "RealThingks GmbH"
	  #x0938 "Henway Technologies, LTD."
	  #x0939 "ASTEM Co.,Ltd."
	  #x093A "LinkedSemi Microelectronics (Xiamen) Co., Ltd"
	  #x093B "ENSESO LLC"
	  #x093C "Xenoma Inc."
	  #x093D "Adolf Wuerth GmbH & Co KG"
	  #x093E "Catalyft Labs, Inc."
	  #x093F "JEPICO Corporation"
	  #x0940 "Hero Workout GmbH"
	  #x0941 "Rivian Automotive, LLC"
	  #x0942 "TRANSSION HOLDINGS LIMITED"
	  #x0943 "Inovonics Corp."
	  #x0944 "Agitron d.o.o."
	  #x0945 "Globe (Jiangsu) Co., Ltd"
	  #x0946 "AMC International Alfa Metalcraft Corporation AG"
	  #x0947 "First Light Technologies Ltd."
	  #x0948 "Wearable Link Limited"
	  #x0949 "Metronom Health Europe"
	  #x094A "Zwift, Inc."
	  #x094B "Kindeva Drug Delivery L.P."
	  #x094C "GimmiSys GmbH"
	  #x094D "tkLABS INC."
	  #x094E "PassiveBolt, Inc."
	  #x094F "Limited Liability Company \"Mikrotikls\""
	  #x0950 "Capetech"
	  #x0951 "PPRS"
	  #x0952 "Apptricity Corporation"
	  #x0953 "LogiLube, LLC"
	  #x0954 "Julbo"
	  #x0955 "Breville Group"
	  #x0956 "Kerlink"
	  #x0957 "Ohsung Electronics"
	  #x0958 "ZTE Corporation"
	  #x0959 "HerdDogg, Inc"
	  #x095A "Selekt Bilgisayar, lletisim Urunleri lnsaat Sanayi ve Ticaret Limited Sirketi"
	  #x095B "Lismore Instruments Limited"
	  #x095C "LogiLube, LLC"
	  #x095D "ETC"
	  #x095E "BioEchoNet inc."
	  #x095F "NUANCE HEARING LTD"
	  #x0960 "Sena Technologies Inc."
	  #x0961 "Linkura AB"
	  #x0962 "GL Solutions K.K."
	  #x0963 "Moonbird BV"))
  "Bluetooth manufacturer IDs.")


;;;; device and adapter info display

(defun bluetooth--ins-heading (heading)
  "Insert HEADING in info view."
  (insert (propertize heading 'face
					  'bluetooth-info-heading)))

(defun bluetooth--ins-line (attr text)
  "Insert attribute ATTR and corresponding TEXT in info view."
  (insert (propertize (format "%21s" attr)
					  'face
					  'bluetooth-info-attribute)
		  ": " text "\n"))

(defun bluetooth--ins-attr (props attr)
  "Insert information on attribute ATTR in properties alist PROPS."
  (let ((value (cl-rest (assoc attr props))))
	(bluetooth--ins-line attr
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
	(let ((p-class (bluetooth--parse-class class)))
	  (bluetooth--ins-heading "\nService and device classes\n")
	  (--map (cl-destructuring-bind (type value) it
			   (if (listp value)
				   (bluetooth--ins-line type
										(mapconcat #'identity
												   value
												   ", "))
				 (bluetooth--ins-line type value)))
			 p-class))))

(defun bluetooth--ins-services (props)
  "Insert device services from properties alist PROPS."
  (when (cl-rest (assoc "UUIDs" props))
	(bluetooth--ins-heading "\nServices (UUIDs)\n")
	(mapc (lambda (id-pair)
			(--zip-with (insert (format it other))
						'("%36s  " "%s " "(%s)")
						(cl-second id-pair))
			(insert "\n"))
		  (bluetooth--device-uuids props))))

(defun bluetooth--ins-rf-info (props)
  "Insert rf information from properties alist PROPS."
  (let* ((rssi (cl-rest (assoc "RSSI" props)))
		 (tx-power (cl-rest (assoc "TxPower" props)))
		 (loss (when (and rssi tx-power) (- tx-power rssi))))
	(--zip-with (when other
				  (bluetooth--ins-line (cl-first it)
									   (format (cl-second it) other)))
				'(("RSSI" "%4d dBm") ("Tx Power" "%4d dBm")
				  ("Path loss" "%4d dB"))
				(list rssi tx-power loss))))

(defun bluetooth--ins-mfc-info (props)
  "Insert manufacturer information from properties alist PROPS."
  (when-let (mf-info (cl-second (assoc "ManufacturerData" props)))
	(bluetooth--ins-line "Manufacturer"
						 (or (gethash (cl-first mf-info)
									  bluetooth--manufacturer-ids)
							 "unknown"))))

(defun bluetooth-show-device-info ()
  "Show detailed information on the device at point."
  (interactive)
  (when-let (device (bluetooth--device (tabulated-list-get-id)))
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
		(special-mode)))))

(defun bluetooth-show-adapter-info ()
  "Show detailed information on the (first) bluetooth adapter."
  (interactive)
  (let* ((adapter (cl-first (bluetooth--query-adapters)))
		 (props (bluetooth--adapter-properties adapter)))
	(with-current-buffer-window bluetooth-info-buffer-name nil nil
	  (bluetooth--ins-heading "Bluetooth adapter info\n\n")
	  (mapc (lambda (it) (bluetooth--ins-attr props it))
			 '("Alias" "Address" "AddressType" "Powered" "Discoverable"
			   "DiscoverableTimeout" "Pairable" "PairableTimeout"
			   "Discovering" "Roles" "Modalias"))
	  (bluetooth--ins-line "Adapter" (concat bluetooth--root "/"
											 adapter))
	  (funcall (-juxt #'bluetooth--ins-mfc-info
					  #'bluetooth--ins-classes
					  #'bluetooth--ins-services)
			   props)
	  (special-mode))))

(provide 'bluetooth)

;;; bluetooth.el ends here

;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 4
;; End:
