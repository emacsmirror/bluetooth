;;; bluetooth.el --- A Major mode for Bluetooth devices -*- lexical-binding: t -*-

;; Copyright (C) 2019 Raffael Stocker

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 13 Aug 2019
;; Version: 0.1
;; Package-Version:
;; Package-Requires: ((dash) (cl-lib))
;; Keywords: hardware
;; URL: https://gitlab.com/rstocker/emacs-bluetooth

;; This file is NOT part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package implements basic Bluetooth management functionality, such as
;; connecting and disconnecting devices, setting properties and aliases,
;; putting the adapter in discovery mode and controlling its power supply.
;; It also includes a pairing agent.  No configuration is necessary.
;; It uses the Bluez Bluetooth stack on GNU/Linux via the DBus interface.
;; Therefore, it requires an Emacs with DBus support compiled-in.
;;
;; To use the package, invoke `list-bluetooth-devices'.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'let-alist)
(require 'dash)
(require 'rx)

(defgroup bluetooth nil
  "Bluetooth device management."
  :group 'comm)

(defcustom bluetooth-bluez-bus :system
  "D-Bus bus that Bluez is registered on.
This is usually `:system' if bluetoothd runs as a system service, or
`:session' if it runs as a user service."
  :type '(symbol))

(defconst bluetooth-buffer-name "*Bluetooth*"
  "Name of the buffer in which to list bluetooth devices.")

(defconst bluetooth--mode-name "Bluetooth" "Pretty print mode name.")

(defconst bluetooth--mode-info '(:eval (bluetooth--display-state))
  "Mode info list.")

;;; The state information list defines the kind of adapter state displayed
;;; in the mode-line.  The first element of a sublist is an adapter property,
;;; the second is the displayed string if the property is non-nil and
;;; the third is the displayed string if the property is nil.  If a
;;; display element is nil, nothing will be displayed for this property.
(defconst bluetooth--mode-state '(("Powered" nil "off")
				  ("Discoverable" "discoverable" nil)
				  ("Pairable" "pairable" nil)
				  ("Discovering" "scan" nil))
  "Mode line adapter state information.")

;;; Bluez service name as defined by the Bluez API
(defconst bluetooth--service "org.bluez" "D-Bus service name of Bluez.")

;;; Bluez root path as defined by the Bluez API
(defconst bluetooth--root "/org/bluez" "D-Bus path root for Bluez.")

;;; our path name for the pairing agent
(defconst bluetooth--own-path (concat dbus-path-emacs "/bluetooth")
  "D-Bus object path for the pairing agent.")

;;; the interface name for the pairing agent
(defconst bluetooth--own-intf (concat dbus-interface-emacs ".bluetooth")
  "D-Bus interface name for the pairing agent.")

(defconst bluetooth--agent-mngr-intf "org.bluez.AgentManager1"
  "D-Bus interface name for the agent manager.")

(defconst bluetooth--agent-intf "org.bluez.Agent1"
  "D-Bus interface name for the agent.")

(defvar bluetooth--method-objects '() "D-Bus method objects.")

(eval-and-compile
  (defconst bluetooth--base-uuid "0000-1000-8000-00805f9b34fb"
    "Bluetooth base UUID."))

;;; API description:
;;;
;;; This is a plist of plists, providing API information for the
;;; implemented D-Bus APIs.
;;;
;;; For instance, API :device has path-spec (bluetooth--adapter
;;; bluetooth--device) and interface "org.bluez.Device1".
;;;
;;; The path-spec is a list of functions, defined below, that
;;; return the designated constituents of the D-Bus path.
(defconst bluetooth--api-info '(:device
				(:path
				 (bluetooth--adapter bluetooth--device)
				 :interface "org.bluez.Device1")
				:adapter
				(:path
				 (bluetooth--adapter)
				 :interface "org.bluez.Adapter1"))
  "Bluez D-Bus API information about paths and interfaces.")

;;; The following functions provide the constituents of the path
;;; spec in `bluetooth--api-info'.

(defun bluetooth--adapter (dev-id)
  "Return the adapter of DEV-ID."
  (bluetooth--dev-state "Adapter" (assoc dev-id bluetooth--device-info)))

(defun bluetooth--root (_)
  "Return the root, ignoring DEV-ID."
  bluetooth--root)

(defun bluetooth--device (dev-id)
  "Return the device name of DEV-ID."
  dev-id)

;;; end of path spec functions

;;; Default timeout for D-Bus commands
(defvar bluetooth--timeout 5000 "Default timeout for Bluez D-Bus access.")

;;; This variable holds the device information as obtained from D-Bus.
(defvar bluetooth--device-info nil "Device info obtained from Bluez.")

;;; This alist specifies all the commands. The format is as follows:
;;;
;;; command . COMMAND specifies the command name
;;; key . [?c] specifies the key binding in `bluetooth-mode-map'
;;; method . "Method" specifies a D-Bus method "Method"
;;; toggle . "Tprop" specifies a D-Bus property "Tprop" that is toggled
;;; by the command
;;; set . "Prop" specifies a D-Bus property "Prop" that can be set by
;;; the command
;;; query . "Query" specifies a query issued in the minibuffer; this
;;; makes sense only if `set' is also specified
;;; api . [:device|:adapter] specifies the Bluez API to be used
;;; for the D-Bus command
;;; args . ARG-LIST adds ARG-LIST to the D-Bus method invocation; the
;;; ARG-LIST will be spliced and evaluated before the method call
;;;    The following special keywords are interpreted:
;;;    :path-devid replace by full object path
;;;                (e.g. "/org/bluez/hci0/dev_...")
;;; run . (FORM) runs the lisp FORM after the D-Bus command
;;; docstring . "STRING" adds STRING as documentation to the command
(defvar bluetooth--commands
  '(((command . connect) (key . [?c]) (method . "Connect") (api . :device)
     (docstring . "Connect to the Bluetooth device at point."))
    ((command . disconnect) (key . [?d]) (method . "Disconnect")
     (api . :device) (docstring . "Disconnect Bluetooth device at point."))
    ((command . toggle-blocked) (key . [?b]) (toggle . "Blocked")
     (api . :device) (docstring . "Mark Bluetooth device at point blocked."))
    ((command . toggle-trusted) (key . [?t]) (toggle . "Trusted")
     (api . :device) (docstring . "Mark Bluetooth device at point trusted."))
    ((command . set-alias) (key . [?a]) (set . "Alias")
     (query . "Alias (empty to reset): ") (api . :device)
     (docstring . "Set alias of Bluetooth device at point."))
    ((command . start-discovery) (key . [?r]) (method . "StartDiscovery")
     (api . :adapter) (docstring . "Start discovery mode."))
    ((command . stop-discovery) (key . [?R]) (method . "StopDiscovery")
     (api . :adapter) (docstring . "Stop discovery mode."))
    ((command . remove-device) (key . [?k]) (method . "RemoveDevice")
     (api . :adapter) (args :object-path :path-devid)
     (docstring . "Remove Bluetooth device at point."))
    ((command . toggle-power) (key . [?s]) (toggle . "Powered")
     (api . :adapter) (docstring . "Toggle power supply of adapter."))
    ((command . pair) (key . [?P]) (method . "Pair")
     (api . :device) (docstring . "Pair with a device."))
    ((command . toggle-discoverable) (key . [?D]) (toggle . "Discoverable")
     (api . :adapter) (docstring . "Toggle discoverable mode."))
    ((command . toggle-pairable) (key . [?x]) (toggle . "Pairable")
     (api . :adapter) (docstring . "Toggle pairable mode."))))

;;; This function provides the list entries for the tabulated-list
;;; view.  It is called from `tabulated-list-print'.
(defun bluetooth--list-entries ()
  "Provide the list entries for the tabulated view."
  (setq bluetooth--device-info
	(bluetooth--get-device-info (bluetooth--get-devices)))
  (bluetooth--compile-list-entries bluetooth--device-info))

;;; This function updates the list view.
(defun bluetooth--update-list ()
  "Update the list view."
  (with-current-buffer bluetooth-buffer-name
    (tabulated-list-print t)))

;;; List format for the main display buffer.
;;; NOTE: the strings MUST correspond to Bluez device properties
;;; as they are used to gather the information from Bluez.
(defconst bluetooth--list-format
  [("Alias" 30 t) ("Paired" 6 t) ("Connected" 9 t) ("Address" 17 t)
   ("Blocked" 7 t) ("Trusted" 7 t)] "The list view format for bluetooth mode.")

;;; This defines the major mode.
(define-derived-mode bluetooth-mode tabulated-list-mode
  bluetooth--mode-name
  "Major mode for managing Bluetooth devices.
This mode allows pairing with and connecting to Bluetooth
devices, as well as setting properties."
  (setq tabulated-list-format bluetooth--list-format
	tabulated-list-entries #'bluetooth--list-entries
	tabulated-list-padding 1)
  (bluetooth--make-commands)
  (define-key bluetooth-mode-map [?i] #'bluetooth-show-device-info)
  (tabulated-list-init-header))

;;; This function returns a list of bluetooth adapters and devices
;;; in the form
;;; (("hci0"
;;;   ("dev_78_AB_BB_DA_6C_7E" "dev_90_F1_AA_06_24_72")))
;;;
;;; The first element of each (sub-) list is an adapter name, followed
;;; by a list of devices known to this adapter.
(defun bluetooth--get-devices ()
  "Return a list of bluetooth adapters and devices connected to them."
  (mapcar (lambda (a)
	    (list a (dbus-introspect-get-node-names
		     bluetooth-bluez-bus bluetooth--service
		     (concat bluetooth--root "/" a))))
	  (dbus-introspect-get-node-names
	   bluetooth-bluez-bus bluetooth--service bluetooth--root)))

;;; Given a device list as obtained from `bluetooth--get-devices'
;;; this function gathers all the properties of each device.
;;; The data is returned in the following structure (alist of alists):
;;;
;; (("dev_78_AB_BB_DA_6C_7E"
;;   (("Address" . "78:AB:BB:DA:6C:7E")
;;    ("AddressType" . "public")
;;    ...
;;    ("ServicesResolved")))
;;  ("dev_90_F1_AA_06_24_72"
;;   (("Address" . "90:F1:AA:06:24:72")
;;    ("AddressType" . "public")
;;    ...
;;    ("ServicesResolved"))))
;;
(defun bluetooth--get-device-info (devices)
  "Return a list with information about DEVICES."
  (mapcan
   (lambda (devlist)
     (cl-loop for dev in (cadr devlist)
	      for path = (mapconcat #'identity
				    (list bluetooth--root (car devlist) dev)
				    "/")
	      collect (cons dev (list (dbus-get-all-properties
				       bluetooth-bluez-bus
				       bluetooth--service path
				       "org.bluez.Device1")))))
   devices))

(defun bluetooth--dev-state (key device)
  "Return state information regarding KEY for DEVICE."
  (let ((value (cdr (assoc key (cadr device)))))
    (cond ((stringp value) value)
	  ((eq nil value) "no")
	  (t "yes"))))

;;; This function compiles a list of device information in the
;;; format needed by `tabulated-list-print'.
(defun bluetooth--compile-list-entries (device-info)
  "Compile list entries based on previously gathered DEVICE-INFO."
  (mapcar (lambda (dev)
	    (list (car dev)
		  (cl-map 'vector (lambda (key) (bluetooth--dev-state key dev))
			  (mapcar #'car bluetooth--list-format))))
	  device-info))

;;; Build up the index for Imenu.  This function is used as
;;; `imenu-create-index-function'.
(defun bluetooth--create-imenu-index ()
  "Create the index for Imenu."
  (goto-char (point-min))
  (cl-loop for (pos entry) = (list (point) (tabulated-list-get-entry))
	   while entry
	   do (forward-line 1)
	   collect (cons (elt entry 0) pos)))

;;; This function calls FUNCTION with ARGS given the device-id DEV-ID and
;;; Bluez API.  This is used on D-Bus functions.
(defun bluetooth--call-method (dev-id api function &rest args)
  "For DEV-ID, invoke D-Bus FUNCTION on API, passing ARGS."
  (let ((path (mapconcat (lambda (f) (funcall f dev-id))
			 (plist-get (plist-get bluetooth--api-info api) :path)
			 "/"))
	(interface (plist-get (plist-get bluetooth--api-info api) :interface)))
    (apply function bluetooth-bluez-bus bluetooth--service path interface
	   (mapcar (lambda (x) (if (eq x :path-devid) (concat path "/" dev-id) x))
		   args))))

;;; The following functions are the workers for the commands.
;;; They are used by `bluetooth--make-commands'.

;;; Invoke a D-Bus method with or without parameters.
(defun bluetooth--dbus-method (method api &rest args)
  "Invoke METHOD on D-Bus API with ARGS."
  (let ((dev-id (tabulated-list-get-id)))
    (when dev-id
      (apply #'bluetooth--call-method dev-id api
	     #'dbus-call-method-asynchronously method
	     #'bluetooth--update-list :timeout bluetooth--timeout args))))

;;; Toggle a property.
(defun bluetooth--dbus-toggle (property api)
  "Toggle boolean PROPERTY on D-Bus API."
  (let ((dev-id (tabulated-list-get-id)))
    (when dev-id
      (let ((value (bluetooth--call-method dev-id api
					   #'dbus-get-property property)))
	(bluetooth--call-method dev-id api #'dbus-set-property property
				(not value))
	(bluetooth--update-list)))))

;;; Set a property.
(defun bluetooth--dbus-set (property arg api)
  "Set PROPERTY to ARG on D-Bus API."
  (let ((dev-id (tabulated-list-get-id)))
    (when dev-id
      (bluetooth--call-method dev-id api #'dbus-set-property property arg)
      (bluetooth--update-list))))

;;; end of worker function definitions

;;; This function generates all the commands.
;;; NOTE: The spaces after the unquotes are necessary for let-alist to expand
;;; the dotted names.
(defun bluetooth--make-commands ()
  "Generate the commands specified in `bluetooth--commands'."
  (dolist (cmd bluetooth--commands)
    (let-alist cmd
      (let ((command (intern (concat "bluetooth-" (symbol-name .command)))))
	(defalias command
	  (cond
	   (.method `(lambda () (interactive)
		       (bluetooth--dbus-method , .method , .api ,@ .args)
		       ,@ .run))
	   (.toggle `(lambda () (interactive)
		       (let ((value (bluetooth--dbus-toggle , .toggle , .api)))
			 ,@ .run)))
	   (.set `(lambda (arg) (interactive ,(concat "M" .query))
		    (bluetooth--dbus-set , .set arg , .api)
		    ,@ .run)))
	  .docstring)
	(define-key bluetooth-mode-map .key command)))))

(defun bluetooth--display-state ()
  "Get the current adapter state and display it.
This function only uses the first adapter reported by Bluez."
  (let* ((adapters (dbus-introspect-get-node-names
		    bluetooth-bluez-bus bluetooth--service bluetooth--root))
	 (resp (dbus-get-all-properties bluetooth-bluez-bus bluetooth--service
					(concat bluetooth--root "/"
						(car adapters))
					"org.bluez.Adapter1"))
	 (info (mapconcat #'identity
			  (-keep (lambda (x) (if (cdr (assoc (car x) resp))
					    (cadr x) (caddr x)))
				 bluetooth--mode-state)
			  ",")))
    (unless (string-blank-p info)
      (concat "[" info "] "))))

(defun bluetooth--cleanup ()
  "Clean up when mode buffer is killed."
  (bluetooth--unregister-agent))

;;; This command is the main entry point.  It is meant to be called by
;;; the user.
;;;
;;; Note that this command will redefine the commands and key bindings
;;; as specified in `bluetooth--commands'.  If you want to have
;;; different key bindings, either edit this variable or change the
;;; key bindings in a hook.

;;;###autoload
(defun list-bluetooth-devices ()
  "Display a list of Bluetooth devices that are available."
  (interactive)
  ;; make sure D-Bus is (made) available
  (dbus-ping bluetooth-bluez-bus bluetooth--service bluetooth--timeout)
  (let ((buffer-exists (get-buffer bluetooth-buffer-name)))
    (with-current-buffer (switch-to-buffer bluetooth-buffer-name)
      (unless buffer-exists
	(erase-buffer)
	(bluetooth-mode)
	(bluetooth--register-agent)
	(add-hook 'kill-buffer-hook #'bluetooth--cleanup nil t)
	(make-local-variable 'mode-line-misc-info)
	(cl-pushnew bluetooth--mode-info mode-line-misc-info)
	(setq imenu-create-index-function #'bluetooth--create-imenu-index))
      (tabulated-list-print t))))

;;; Bluetooth pairing agent code

(defun bluetooth--release ()
  "Agent release method.")

(defmacro bluetooth--with-alias (device &rest body)
  "Evaluate BODY with DEVICE alias bound to ALIAS."
  (declare (indent defun))
  `(let ((alias (bluetooth--call-method
		 (car (last (split-string ,device "/"))) :device
		 #'dbus-get-property "Alias")))
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
      (save-match-data
	(let* ((pin (read-passwd (format "Enter Bluetooth PIN for `%s': " alias)))
	       (trimmed-pin (substring pin 0 (min (length pin) 16)))
	       (case-fold-search nil))
	  (unwind-protect
	      (cond ((= 0 (length trimmed-pin))
		     (message "PIN has zero length")
		     nil)
		    ((string-match "[^[:alnum:]]" trimmed-pin)
		     (message "PIN contains non-alphanumeric characters")
		     nil)
		    (t trimmed-pin))
	    (clear-string pin)
	    (clear-string trimmed-pin)))))))

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

(defun bluetooth--cancel ()
  "Cancel a pairing process."
  (keyboard-quit)
  (message "Pairing canceled"))

(defconst bluetooth--methods
  '("Release" "RequestPinCode" "DisplayPinCode" "RequestPasskey"
    "DisplayPasskey" "RequestConfirmation" "RequestAuthorization"
    "AuthorizeService" "Cancel")
  "D-Bus method names for the agent interface.")

;;; This procedure registers the pairing agent.
(defun bluetooth--register-agent ()
  "Register as a pairing agent."
  ;; register all the methods
  (save-match-data
    (setq bluetooth--method-objects
	  (cl-loop for method in bluetooth--methods
		   with case-fold-search = nil
		   for fname = (concat "bluetooth-"
				       (replace-regexp-in-string
					"[A-Z][a-z]+"
					(lambda (x) (concat "-" (downcase x)))
					method t))
		   collect (dbus-register-method bluetooth-bluez-bus
						 dbus-service-emacs
						 bluetooth--own-path
						 bluetooth--agent-intf
						 method (intern fname) t))))
  (dbus-register-service :session dbus-service-emacs)
  (dbus-call-method bluetooth-bluez-bus bluetooth--service bluetooth--root
		    bluetooth--agent-mngr-intf "RegisterAgent"
		    :object-path bluetooth--own-path "KeyboardDisplay"))

(defun bluetooth--unregister-agent ()
  "Unregister the pairing agent."
  (ignore-errors
    (dbus-call-method bluetooth-bluez-bus bluetooth--service bluetooth--root
		      bluetooth--agent-mngr-intf "UnregisterAgent"
		      :object-path bluetooth--own-path)
    (mapc #'dbus-unregister-object bluetooth--method-objects)))

;;; Application layer

(defun bluetooth--parse-service-class-uuid (uuid)
  "Parse UUID and return short and long service class names."
  (let ((uuid-re (rx (seq bos (submatch (= 8 xdigit))
			  "-" (eval bluetooth--base-uuid) eos))))
    (save-match-data
      (when (string-match uuid-re uuid)
	(let ((service-id (string-to-number (match-string 1 uuid) 16)))
	  (or (alist-get service-id
			 (symbol-value
			  (cdr (-find (lambda (x) (>= service-id (car x)))
				  bluetooth--uuid-alists))))
	      (list  (format "#x%08x" service-id) "unknown")))))))

(defun bluetooth--parse-class (class)
  "Parse the CLASS property of a Bluetooth device."
  (cl-labels ((parse (field-def acc)
		     (let-alist field-def
		       (let* ((m-field (lsh (logand class .mask) .shift))
			      (res (cons .name
					 (list (funcall .fn m-field .data))))
			      (n-acc (push res acc)))
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
		       (if (/= 0 (logand bitfield (lsh 1 (car x))))
			   (cdr x)
			 nil))
			data))
      "unknown"))

(defun bluetooth--class-parse-major (field data)
  "Parse major class FIELD using DATA as specification."
  (or (car (alist-get field data))
      "unknown"))

(defun bluetooth--class-parse-value (field data)
  "Parse minor class FIELD using DATA as specification."
  (or (alist-get field data)
      "unknown"))

(defun bluetooth--class-parse-peripheral (field data)
  "Parse peripheral class FIELD using DATA as specification."
  (or (list (bluetooth--class-parse-value (logand (caar data) field)
					  (cdar data))
	    (bluetooth--class-parse-value (logand (caadr data) field)
					  (cdadr data)))
      "unknown"))

(defun bluetooth--class-get-minor (field data)
  "Get the minor field spec for FIELD using DATA as specification."
  (symbol-value (cdr (alist-get field data))))

(defun bluetooth-show-device-info ()
  "Show detail information on the device at point."
  (interactive)
  (bluetooth--show-device-info (tabulated-list-get-id)))

(defun bluetooth--show-device-info (device)
  "Show information about DEVICE in a temp buffer"
  (bluetooth--with-alias device
    (with-current-buffer-window
     "*Bluetooth device info*" nil nil
     (let* ((props (bluetooth--call-method
		     (car (last (split-string device "/"))) :device
		     #'dbus-get-all-properties))
	   (address (cdr (assoc "Address" props)))
	   (rssi (cdr (assoc "RSSI" props)))
	   (class (cdr (assoc "Class" props)))
	   (uuids (cdr (assoc "UUIDs" props))))
       (insert "Alias:\t\t" alias "\n")
       (when address
	 (insert "Address:\t" address "\n"))
       (when rssi
	 (insert "RSSI:\t\t" (number-to-string rssi) "\n"))
       (when class
	 (let ((p-class (bluetooth--parse-class class)))
	   (insert "\nService and device classes:\n")
	   (mapc (lambda (x)
		   (insert (car x) ":\n")
		   (if (listp (cadr x))
		       (dolist (elt (cadr x))
			 (insert "\t" elt "\n"))
		     (insert "\t" (cadr x) "\n")))
		 p-class)))
       (when uuids
	 (insert "\nServices (UUIDs):\n")
	 (dolist (id uuids)
	   (insert (mapconcat #'identity
			      (or (bluetooth--parse-service-class-uuid id)
				  (list id))
			      " -- ")
		   "\n")))))))

;;; The following constants define the meaning of the Bluetooth
;;; CLASS property, which is made up of a number of fields.
;;; The following components are used:
;;; NAME (string): a name describing the field type, e.g.
;;;   "service classes"
;;; MASK: the bit mask for the CLASS field
;;; SHIFT: a shift value to be applied before interpreting the
;;;   CLASS field
;;; FN: a function to be invoked on the masked and shifted CLASS
;;;   and DATA
;;; NEXT: the next field in the class property: NEXT can have
;;;   one of three different kinds of values:
;;;   - a field specification (e.g. bluetooth--class-major-dev-classes)
;;;   - a function returning the next field specification when
;;;     invoked with the masked and shifted CLASS and DATA
;;;   - nil, if no further processing of CLASS is necessary
;;; DATA: the data passed to the parsing (FN) or NEXT functions
(defconst bluetooth--class-major-services
  '((name . "major service classes")
    (mask . #xffe000)
    (shift . 0)
    (fn . bluetooth--class-parse-bitfield)
    (next . bluetooth--class-major-dev-classes)
    (data . ((13 . "Limited Discoverable Mode")
	     (14 . "(reserved)")
	     (15 . "(reserved)")
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
	     (#x3 . ("LAN /Network Access point" .
		     bluetooth--class-network-minor-classes))
	     (#x4 . ("Audio/Video" . bluetooth--class-av-minor-classes))
	     (#x5 . ("Peripheral" . bluetooth--class-peripheral-minor-classes))
	     (#x6 . ("Imaging" . bluetooth--class-imaging-minor-classes))
	     (#x7 . ("Wearable" . bluetooth--class-wearable-minor-classes))
	     (#x8 . ("Toy" . bluetooth--class-toy-minor-classes))
	     (#x9 . ("Health" . bluetooth--class-health-minor-classes))
	     (#xf . ("Unspecified" . nil)))))
  "Bluetooth major device classes")

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
  "Bluetooth computer minor classes")

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
	     (#x1 . "Wearable Headset Device")
	     (#x2 . "Hands-free Device")
	     (#x3 . "(Reserved)")
	     (#x4 . "Microphone")
	     (#x5 . "Loudspeaker")
	     (#x6 . "Headphones")
	     (#x7 . "Portable Audio")
	     (#x8 . "Car audio")
	     (#x9 . "Set-top box")
	     (#xa . "HiFi Audio Device")
	     (#xb . "VCR")
	     (#xc . "Video Camera")
	     (#xd . "Camcorder")
	     (#xe . "Video Monitor")
	     (#xf . "Video Display and Loudspeaker")
	     (#x10 . "Video Conferencing")
	     (#x11 . "(Reserved)")
	     (#x12 . "Gaming/Toy"))))
  "Bluetooth audio/video minor classes.")

(defconst bluetooth--class-peripheral-minor-classes
  '((name . "minor device class")
    (mask . #xfc)
    (shift . -2)
    (fn . bluetooth--class-parse-peripheral)
    (next . nil)
    (data . ((#x30 . ((#x00 . "Not Keyboard / Not Pointing Device")
		      (#x10 . "Keyboard")
		      (#x20 . "Pointing device")
		      (#x30 . "Combo keyboard/pointing device")))
	     (#xf . ((#x0 . "Uncategorized device")
		     (#x1 . "Joystick")
		     (#x2 . "Gamepad")
		     (#x3 . "Remote control")
		     (#x4 . "Sensing device")
		     (#x5 . "Digitizer tablet")
		     (#x6 . "Card Reader")
		     (#x7 . "Digital Pen")
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
	     (#x3 . "Doll / Action figure")
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
	     (#x1 . "Blood Pressure Monitor")
	     (#x2 . "Thermometer")
	     (#x3 . "Weighing Scale")
	     (#x4 . "Glucose Meter")
	     (#x5 . "Pulse Oximeter")
	     (#x6 . "Heart/Pulse Rate Monitor")
	     (#x7 . "Health Data Display")
	     (#x8 . "Step Counter")
	     (#x9 . "Body Composition Analyzer")
	     (#xa . "Peak Flow Monitor")
	     (#xb . "Medication Monitor")
	     (#xc . "Knee Prosthesis")
	     (#xd . "Ankle Prosthesis")
	     (#xe . "Generic Health Manager")
	     (#xf . "Personal Mobility Device"))))
  "Bluetooth health minor classes.")

(defconst bluetooth--uuid-alists
  '((#xfff0 . bluetooth--sdo-uuid-alist)
    (#xfd00 . bluetooth--member-uuid-alist)
    (#x1800 . bluetooth--gatt-service-uuid-alist)
    (#x0 . bluetooth--service-class-uuid-alist))
  "Bluetooth UUID alists sorted by beginning of range.")

(defconst bluetooth--service-class-uuid-alist
  '((#x1000 . ("ServiceDiscoveryServerServiceClassID"
	       "Bluetooth Core Specification"))
    (#x1001 . ("BrowseGroupDescriptorServiceClassID"
	       "Bluetooth Core Specification"))
    (#x1101 . ("SerialPort" "Serial Port Profile (SPP)"))
    (#x1102 . ("LANAccessUsingPPP" "LAN Access Profile"))
    (#x1103 . ("DialupNetworking" "Dial-up Networking Profile (DUN)"))
    (#x1104 . ("IrMCSync" "Synchronization Profile (SYNC)"))
    (#x1105 . ("OBEXObjectPush" "Object Push Profile (OPP)"))
    (#x1106 . ("OBEXFileTransfer" "File Transfer Profile (FTP)"))
    (#x1107 . ("IrMCSyncCommand" "Synchronization Profile (SYNC)"))
    (#x1108 . ("Headset" "Headset Profile (HSP)"))
    (#x1109 . ("CordlessTelephony" "Cordless Telephony Profile (CTP)"))
    (#x110A . ("AudioSource" "Advanced Audio Distribution Profile (A2DP)"))
    (#x110B . ("AudioSink" "Advanced Audio Distribution Profile (A2DP)"))
    (#x110C . ("A/V_RemoteControlTarget"
	       "Audio/Video Remote Control Profile (AVRCP)"))
    (#x110D . ("AdvancedAudioDistribution"
	       "Advanced Audio Distribution Profile (A2DP)"))
    (#x110E . ("A/V_RemoteControl"
	       "Audio/Video Remote Control Profile (AVRCP)"))
    (#x110F . ("A/V_RemoteControlController"
	       "Audio/Video Remote Control Profile (AVRCP)"))
    (#x1110 . ("Intercom" "Intercom Profile (ICP)"))
    (#x1111 . ("Fax" "Fax Profile (FAX)"))
    (#x1112 . ("Headset - Audio Gateway (AG)" "Headset Profile (HSP)"))
    (#x1113 . ("WAP" (concat "Interoperability Requirements for Bluetooth"
			     " technology as a WAP, Bluetooth SIG")))
    (#x1114 . ("WAP_CLIENT"
	       (concat "Interoperability Requirements for"
		       " Bluetooth technology as a WAP, Bluetooth SIG")))
    (#x1115 . ("PANU" "Personal Area Networking Profile (PAN)"))
    (#x1116 . ("NAP" "Personal Area Networking Profile (PAN)"))
    (#x1117 . ("GN" "Personal Area Networking Profile (PAN)"))
    (#x1118 . ("DirectPrinting" "Basic Printing Profile (BPP)"))
    (#x1119 . ("ReferencePrinting" "See Basic Printing Profile (BPP)"))
    (#x111A . ("Basic Imaging Profile" "Basic Imaging Profile (BIP)"))
    (#x111B . ("ImagingResponder" "Basic Imaging Profile (BIP)"))
    (#x111C . ("ImagingAutomaticArchive" "Basic Imaging Profile (BIP)"))
    (#x111D . ("ImagingReferencedObjects" "Basic Imaging Profile (BIP)"))
    (#x111E . ("Handsfree" "Hands-Free Profile (HFP)"))
    (#x111F . ("HandsfreeAudioGateway" "Hands-free Profile (HFP)"))
    (#x1120 . ("DirectPrintingReferenceObjectsService"
	       "Basic Printing Profile (BPP)"))
    (#x1121 . ("ReflectedUI" "Basic Printing Profile (BPP)"))
    (#x1122 . ("BasicPrinting" "Basic Printing Profile (BPP)"))
    (#x1123 . ("PrintingStatus" "Basic Printing Profile (BPP)"))
    (#x1124 . ("HumanInterfaceDeviceService" "Human Interface Device (HID)"))
    (#x1125 . ("HardcopyCableReplacement"
	       "Hardcopy Cable Replacement Profile (HCRP)"))
    (#x1126 . ("HCR_Print" "Hardcopy Cable Replacement Profile (HCRP)"))
    (#x1127 . ("HCR_Scan" "Hardcopy Cable Replacement Profile (HCRP)"))
    (#x1128 . ("Common_ISDN_Access" "Common ISDN Access Profile (CIP)"))
    (#x112D . ("SIM_Access" "SIM Access Profile (SAP)"))
    (#x112E . ("Phonebook Access - PCE" "Phonebook Access Profile (PBAP)"))
    (#x112F . ("Phonebook Access - PSE" "Phonebook Access Profile (PBAP)"))
    (#x1130 . ("Phonebook Access" "Phonebook Access Profile (PBAP)"))
    (#x1131 . ("Headset - HS" "Headset Profile (HSP)"))
    (#x1132 . ("Message Access Server" "Message Access Profile (MAP)"))
    (#x1133 . ("Message Notification Server" "Message Access Profile (MAP)"))
    (#x1134 . ("Message Access Profile" "Message Access Profile (MAP)"))
    (#x1135 . ("GNSS" "Global Navigation Satellite System Profile (GNSS)"))
    (#x1136 . ("GNSS_Server"
	       "Global Navigation Satellite System Profile (GNSS)"))
    (#x1137​ . ("​3D Display" "​3D Synchronization Profile (3DSP)"))
    (​#x1138 . ("​3D Glasses" "​3D Synchronization Profile (3DSP)"))
    (#x1139​ . ("​3D Synchronization" "​3D Synchronization Profile (3DSP)"))
    (​#x113A . ("​MPS Profile UUID" "​Multi-Profile Specification (MPS)"))
    (​#x113B . ("​MPS SC UUID" "​Multi-Profile Specification (MPS)"))
    (​#x113C​ . ("​CTN Access Service​"
	       "​Calendar, Task, and Notes (CTN) Profile"))
    (​#x113D . ("​CTN Notification Service​"
	       "​​Calendar Tasks and Notes (CTN) Profile"))
    (​#x113E . ("​CTN Profile" "​​Calendar Tasks and Notes (CTN) Profile"))
    (#x1200 . ("PnPInformation" "Device Identification (DID)"))
    (#x1201 . ("GenericNetworking" "N/A"))
    (#x1202 . ("GenericFileTransfer" "N/A"))
    (#x1203 . ("GenericAudio" "N/A"))
    (#x1204 . ("GenericTelephony" "N/A"))
    (#x1205 . ("UPNP_Service" "Enhanced Service Discovery Profile (ESDP)"))
    (#x1206 . ("UPNP_IP_Service" "Enhanced Service Discovery Profile (ESDP)"))
    (#x1300 . ("ESDP_UPNP_IP_PAN" "Enhanced Service Discovery Profile (ESDP)"))
    (#x1301 . ("ESDP_UPNP_IP_LAP" "Enhanced Service Discovery Profile (ESDP)"))
    (#x1302 . ("ESDP_UPNP_L2CAP" "Enhanced Service Discovery Profile (ESDP)"))
    (#x1303 . ("VideoSource" "Video Distribution Profile (VDP)"))
    (#x1304 . ("VideoSink" "Video Distribution Profile (VDP)"))
    (#x1305 . ("VideoDistribution" "Video Distribution Profile (VDP)"))
    (#x1400 . ("HDP" "Health Device Profile"))
    (#x1401 . ("HDP Source" "Health Device Profile (HDP)"))
    (#x1402 . ("HDP Sink" "Health Device Profile (HDP)")))
  "Bluetooth service class UUIDs.")

(defconst bluetooth--gatt-service-uuid-alist
  '((#x1800 . ("Generic Access" "org.bluetooth.service.generic_access" "GSS"))
    (#x1811 . ("Alert Notification Service"
	       "org.bluetooth.service.alert_notification" "GSS"))
    (#x1815 . ("Automation IO" "org.bluetooth.service.automation_io" "GSS"))
    (#x180F . ("Battery Service" "org.bluetooth.service.battery_service"
	       "GSS"))
    (#x183B . ("Binary Sensor" "GATT Service UUID" "BSS"))
    (#x1810 . ("Blood Pressure" "org.bluetooth.service.blood_pressure" "GSS"))
    (#x181B . ("Body Composition" "org.bluetooth.service.body_composition"
	       "GSS"))
    (#x181E . ("Bond Management Service"
	       "org.bluetooth.service.bond_management" "GSS"))
    (#x181F . ("Continuous Glucose Monitoring"
	       "org.bluetooth.service.continuous_glucose_monitoring" "GSS"))
    (#x1805 . ("Current Time Service" "org.bluetooth.service.current_time"
	       "GSS"))
    (#x1818 . ("Cycling Power" "org.bluetooth.service.cycling_power" "GSS"))
    (#x1816 . ("Cycling Speed and Cadence"
	       "org.bluetooth.service.cycling_speed_and_cadence" "GSS"))
    (#x180A . ("Device Information" "org.bluetooth.service.device_information"
	       "GSS"))
    (#x183C . ("Emergency Configuration" "GATT Service UUID" "EMCS"))
    (#x181A . ("Environmental Sensing"
	       "org.bluetooth.service.environmental_sensing" "GSS"))
    (#x1826 . ("Fitness Machine" "org.bluetooth.service.fitness_machine"
	       "GSS"))
    (#x1801 . ("Generic Attribute" "org.bluetooth.service.generic_attribute"
	       "GSS"))
    (#x1808 . ("Glucose" "org.bluetooth.service.glucose" "GSS"))
    (#x1809 . ("Health Thermometer" "org.bluetooth.service.health_thermometer"
	       "GSS"))
    (#x180D . ("Heart Rate" "org.bluetooth.service.heart_rate" "GSS"))
    (#x1823 . ("HTTP Proxy" "org.bluetooth.service.http_proxy" "GSS"))
    (#x1812 . ("Human Interface Device"
	       "org.bluetooth.service.human_interface_device" "GSS"))
    (#x1802 . ("Immediate Alert" "org.bluetooth.service.immediate_alert"
	       "GSS"))
    (#x1821 . ("Indoor Positioning" "org.bluetooth.service.indoor_positioning"
	       "GSS"))
    (#x183A . ("Insulin Delivery" "org.bluetooth.service.insulin_delivery"
	       "GSS"))
    (#x1820 . ("Internet Protocol Support Service"
	       "org.bluetooth.service.internet_protocol_support" "GSS"))
    (#x1803 . ("Link Loss" "org.bluetooth.service.link_loss" "GSS"))
    (#x1819 . ("Location and Navigation"
	       "org.bluetooth.service.location_and_navigation" "GSS"))
    (#x1827 . ("Mesh Provisioning Service"
	       "org.bluetooth.service.mesh_provisioning" "GSS"))
    (#x1828 . ("Mesh Proxy Service" "org.bluetooth.service.mesh_proxy" "GSS"))
    (#x1807 . ("Next DST Change Service"
	       "org.bluetooth.service.next_dst_change" "GSS"))
    (#x1825 . ("Object Transfer Service"
	       "org.bluetooth.service.object_transfer" "GSS"))
    (#x180E . ("Phone Alert Status Service"
	       "org.bluetooth.service.phone_alert_status" "GSS"))
    (#x1822 . ("Pulse Oximeter Service" "org.bluetooth.service.pulse_oximeter"
	       "GSS"))
    (#x1829 . ("Reconnection Configuration"
	       "org.bluetooth.service.reconnection_configuration" "GSS"))
    (#x1806 . ("Reference Time Update Service"
	       "org.bluetooth.service.reference_time_update" "GSS"))
    (#x1814 . ("Running Speed and Cadence"
	       "org.bluetooth.service.running_speed_and_cadence" "GSS"))
    (#x1813 . ("Scan Parameters" "org.bluetooth.service.scan_parameters"
	       "GSS"))
    (#x1824 . ("Transport Discovery"
	       "org.bluetooth.service.transport_discovery" "GSS"))
    (#x1804 . ("Tx Power" "org.bluetooth.service.tx_power" "GSS"))
    (#x181C . ("User Data" "org.bluetooth.service.user_data" "GSS"))
    (#x181D . ("Weight Scale" "org.bluetooth.service.weight_scale" "GSS")))
  "Bluetooth GATT service UUIDs.")

(defconst bluetooth--sdo-uuid-alist
  '((#xFFF9 . ("Fast IDentity Online Alliance (FIDO)"
	       "FIDO2 secure client-to-authenticator transport"))
    (#xFFFA . ("ASTM International" "ASTM Remote ID"))
    (#xFFFB . ("Thread Group, Inc." "Direct Thread Commissioning"))
    (#xFFFC . ("AirFuel Alliance"
	       "Wireless Power Transfer (WPT) Service"))
    (#xFFFD . ("Fast IDentity Online Alliance"
	       "Universal Second Factor Authenticator Service"))
    (#xFFFE . ("AirFuel Alliance" "Wireless Power Transfer Service")))
  "Bluetooth standards development organizations UUIDS.")

;;; This is a very long list of manufacturer UUIDs and therefore
;;; the last thing in this file.
(defconst bluetooth--member-uuid-alist
  '((#xFEFF . ("GN Netcom"))
    (#xFEFE . ("GN ReSound A/S"))
    (#xFEFD . ("Gimbal, Inc."))
    (#xFEFC . ("Gimbal, Inc."))
    (#xFEFB . ("Telit Wireless Solutions (Formerly Stollmann E+V GmbH)"))
    (#xFEFA . ("PayPal, Inc."))
    (#xFEF9 . ("PayPal, Inc."))
    (#xFEF8 . ("Aplix Corporation"))
    (#xFEF7 . ("Aplix Corporation"))
    (#xFEF6 . ("Wicentric, Inc."))
    (#xFEF5 . ("Dialog Semiconductor GmbH"))
    (#xFEF4 . ("Google"))
    (#xFEF3 . ("Google"))
    (#xFEF2 . ("CSR"))
    (#xFEF1 . ("CSR"))
    (#xFEF0 . ("Intel"))
    (#xFEEF . ("Polar Electro Oy "))
    (#xFEEE . ("Polar Electro Oy "))
    (#xFEED . ("Tile, Inc."))
    (#xFEEC . ("Tile, Inc."))
    (#xFEEB . ("Swirl Networks, Inc."))
    (#xFEEA . ("Swirl Networks, Inc."))
    (#xFEE9 . ("Quintic Corp."))
    (#xFEE8 . ("Quintic Corp."))
    (#xFEE7 . ("Tencent Holdings Limited."))
    (#xFEE6 . ("Silvair, Inc."))
    (#xFEE5 . ("Nordic Semiconductor ASA"))
    (#xFEE4 . ("Nordic Semiconductor ASA"))
    (#xFEE3 . ("Anki, Inc."))
    (#xFEE2 . ("Anki, Inc."))
    (#xFEE1 . ("Anhui Huami Information Technology Co., Ltd. "))
    (#xFEE0 . ("Anhui Huami Information Technology Co., Ltd. "))
    (#xFEDF . ("Design SHIFT"))
    (#xFEDE . ("Coin, Inc."))
    (#xFEDD . ("Jawbone"))
    (#xFEDC . ("Jawbone"))
    (#xFEDB . ("Perka, Inc."))
    (#xFEDA . ("ISSC Technologies Corp. "))
    (#xFED9 . ("Pebble Technology Corporation"))
    (#xFED8 . ("Google"))
    (#xFED7 . ("Broadcom"))
    (#xFED6 . ("Broadcom"))
    (#xFED5 . ("Plantronics Inc."))
    (#xFED4 . ("Apple, Inc."))
    (#xFED3 . ("Apple, Inc."))
    (#xFED2 . ("Apple, Inc."))
    (#xFED1 . ("Apple, Inc."))
    (#xFED0 . ("Apple, Inc."))
    (#xFECF . ("Apple, Inc."))
    (#xFECE . ("Apple, Inc."))
    (#xFECD . ("Apple, Inc."))
    (#xFECC . ("Apple, Inc."))
    (#xFECB . ("Apple, Inc."))
    (#xFECA . ("Apple, Inc."))
    (#xFEC9 . ("Apple, Inc."))
    (#xFEC8 . ("Apple, Inc."))
    (#xFEC7 . ("Apple, Inc."))
    (#xFEC6 . ("Kocomojo, LLC"))
    (#xFEC5 . ("Realtek Semiconductor Corp."))
    (#xFEC4 . ("PLUS Location Systems"))
    (#xFEC3 . ("360fly, Inc."))
    (#xFEC2 . ("Blue Spark Technologies, Inc."))
    (#xFEC1 . ("KDDI Corporation"))
    (#xFEC0 . ("KDDI Corporation"))
    (#xFEBF . ("Nod, Inc."))
    (#xFEBE . ("Bose Corporation"))
    (#xFEBD . ("Clover Network, Inc"))
    (#xFEBC . ("Dexcom Inc"))
    (#xFEBB . ("adafruit industries"))
    (#xFEBA . ("Tencent Holdings Limited"))
    (#xFEB9 . ("LG Electronics"))
    (#xFEB8 . ("Facebook, Inc."))
    (#xFEB7 . ("Facebook, Inc."))
    (#xFEB6 . ("Vencer Co., Ltd"))
    (#xFEB5 . ("WiSilica Inc."))
    (#xFEB4 . ("WiSilica Inc."))
    (#xFEB3 . ("Taobao"))
    (#xFEB2 . ("Microsoft Corporation"))
    (#xFEB1 . ("Electronics Tomorrow Limited"))
    (#xFEB0 . ("Nest Labs Inc"))
    (#xFEAF . ("Nest Labs Inc"))
    (#xFEAE . ("Nokia"))
    (#xFEAD . ("Nokia"))
    (#xFEAC . ("Nokia"))
    (#xFEAB . ("Nokia"))
    (#xFEAA . ("Google"))
    (#xFEA9 . ("Savant Systems LLC"))
    (#xFEA8 . ("Savant Systems LLC"))
    (#xFEA7 . ("UTC Fire and Security"))
    (#xFEA6 . ("GoPro, Inc."))
    (#xFEA5 . ("GoPro, Inc."))
    (#xFEA4 . ("Paxton Access Ltd"))
    (#xFEA3 . ("ITT Industries"))
    (#xFEA2 . ("Intrepid Control Systems, Inc."))
    (#xFEA1 . ("Intrepid Control Systems, Inc."))
    (#xFEA0 . ("Google"))
    (#xFE9F . ("Google"))
    (#xFE9E . ("Dialog Semiconductor B.V."))
    (#xFE9D . ("Mobiquity Networks Inc"))
    (#xFE9C . ("GSI Laboratories, Inc."))
    (#xFE9B . ("Samsara Networks, Inc"))
    (#xFE9A . ("Estimote"))
    (#xFE99 . ("Currant Inc"))
    (#xFE98 . ("Currant Inc"))
    (#xFE97 . ("Tesla Motors Inc."))
    (#xFE96 . ("Tesla Motors Inc."))
    (#xFE95 . ("Xiaomi Inc."))
    (#xFE94 . ("OttoQ In"))
    (#xFE93 . ("OttoQ In"))
    (#xFE92 . ("Jarden Safety & Security"))
    (#xFE91 . ("Shanghai Imilab Technology Co., Ltd"))
    (#xFE90 . ("JUMA"))
    (#xFE8F . ("CSR"))
    (#xFE8E . ("ARM Ltd"))
    (#xFE8D . ("Interaxon Inc."))
    (#xFE8C . ("TRON Forum"))
    (#xFE8B . ("Apple, Inc."))
    (#xFE8A . ("Apple, Inc."))
    (#xFE89 . ("B&O Play A/S"))
    (#xFE88 . ("SALTO SYSTEMS S.L."))
    (#xFE87 . ("Qingdao Yeelink Information Technology Co., Ltd. ( 青岛亿联客信息技术有限公司 )"))
    (#xFE86 . ("HUAWEI Technologies Co., Ltd. ( 华为技术有限公司 )"))
    (#xFE85 . ("RF Digital Corp"))
    (#xFE84 . ("RF Digital Corp"))
    (#xFE83 . ("Blue Bite"))
    (#xFE82 . ("Medtronic Inc."))
    (#xFE81 . ("Medtronic Inc."))
    (#xFE80 . ("Doppler Lab"))
    (#xFE7F . ("Doppler Lab"))
    (#xFE7E . ("Awear Solutions Ltd"))
    (#xFE7D . ("Aterica Health Inc."))
    (#xFE7C . ("Telit Wireless Solutions (Formerly Stollmann E+V GmbH)"))
    (#xFE7B . ("Orion Labs, Inc."))
    (#xFE7A . ("Bragi GmbH"))
    (#xFE79 . ("Zebra Technologies"))
    (#xFE78 . ("Hewlett-Packard Company"))
    (#xFE77 . ("Hewlett-Packard Company"))
    (#xFE76 . ("TangoMe"))
    (#xFE75 . ("TangoMe"))
    (#xFE74 . ("unwire"))
    (#xFE73 . ("Abbott (formerly St. Jude Medical, Inc.)"))
    (#xFE72 . ("Abbott (formerly St. Jude Medical, Inc.)"))
    (#xFE71 . ("Plume Design Inc"))
    (#xFE70 . ("Beijing Jingdong Century Trading Co., Ltd."))
    (#xFE6F . ("LINE Corporation"))
    (#xFE6E . ("The University of Tokyo"))
    (#xFE6D . ("The University of Tokyo"))
    (#xFE6C . ("TASER International, Inc."))
    (#xFE6B . ("TASER International, Inc."))
    (#xFE6A . ("Kontakt Micro-Location Sp. z o.o."))
    (#xFE69 . ("Capsule Technologies Inc."))
    (#xFE68 . ("Capsule Technologies Inc."))
    (#xFE67 . ("Lab Sensor Solutions"))
    (#xFE66 . ("Intel Corporation"))
    (#xFE65 . ("CHIPOLO d.o.o."))
    (#xFE64 . ("Siemens AG"))
    (#xFE63 . ("Connected Yard, Inc."))
    (#xFE62 . ("Indagem Tech LLC"))
    (#xFE61 . ("Logitech International SA"))
    (#xFE60 . ("Lierda Science & Technology Group Co., Ltd."))
    (#xFE5F . ("Eyefi, Inc."))
    (#xFE5E . ("Plastc Corporation"))
    (#xFE5D . ("Grundfos A/S"))
    (#xFE5C . ("million hunters GmbH"))
    (#xFE5B . ("GT-tronics HK Ltd"))
    (#xFE5A . ("Cronologics Corporation"))
    (#xFE59 . ("Nordic Semiconductor ASA"))
    (#xFE58 . ("Nordic Semiconductor ASA"))
    (#xFE57 . ("Dotted Labs"))
    (#xFE56 . ("Google Inc."))
    (#xFE55 . ("Google Inc."))
    (#xFE54 . ("Motiv, Inc."))
    (#xFE53 . ("3M"))
    (#xFE52 . ("SetPoint Medical"))
    (#xFE51 . ("SRAM"))
    (#xFE50 . ("Google Inc."))
    (#xFE4F . ("Molekule, Inc."))
    (#xFE4E . ("NTT docomo"))
    (#xFE4D . ("Casambi Technologies Oy"))
    (#xFE4C . ("Volkswagen AG"))
    (#xFE4B . ("Signify Netherlands B.V. (formerly Philips Lighting B.V.)"))
    (#xFE4A . ("OMRON HEALTHCARE Co., Ltd."))
    (#xFE49 . ("SenionLab AB"))
    (#xFE48 . ("General Motors"))
    (#xFE47 . ("General Motors"))
    (#xFE46 . ("B&O Play A/S"))
    (#xFE45 . ("Snapchat Inc"))
    (#xFE44 . ("SK Telecom"))
    (#xFE43 . ("Andreas Stihl AG & Co. KG"))
    (#xFE42 . ("Nets A/S"))
    (#xFE41 . ("Inugo Systems Limited"))
    (#xFE40 . ("Inugo Systems Limited"))
    (#xFE3F . ("Friday Labs Limited"))
    (#xFE3E . ("BD Medical"))
    (#xFE3D . ("BD Medical"))
    (#xFE3C . ("alibaba"))
    (#xFE3B . ("Dobly Laboratories"))
    (#xFE3A . ("TTS Tooltechnic Systems AG & Co. KG"))
    (#xFE39 . ("TTS Tooltechnic Systems AG & Co. KG"))
    (#xFE38 . ("Spaceek LTD"))
    (#xFE37 . ("Spaceek LTD"))
    (#xFE36 . ("HUAWEI Technologies Co., Ltd"))
    (#xFE35 . ("HUAWEI Technologies Co., Ltd"))
    (#xFE34 . ("SmallLoop LLC"))
    (#xFE33 . ("CHIPOLO d.o.o."))
    (#xFE32 . ("Pro-Mark, Inc."))
    (#xFE31 . ("Volkswagen AG"))
    (#xFE30 . ("Volkswagen AG"))
    (#xFE2F . ("CRESCO Wireless, Inc"))
    (#xFE2E . ("ERi,Inc."))
    (#xFE2D . ("SMART INNOVATION Co., Ltd"))
    (#xFE2C . ("Google"))
    (#xFE2B . ("ITT Industries"))
    (#xFE2A . ("DaisyWorks, Inc."))
    (#xFE29 . ("Gibson Innovations"))
    (#xFE28 . ("Ayla Networks"))
    (#xFE27 . ("Google"))
    (#xFE26 . ("Google"))
    (#xFE25 . ("Apple, Inc."))
    (#xFE24 . ("August Home Inc"))
    (#xFE23 . ("Zoll Medical Corporation"))
    (#xFE22 . ("Zoll Medical Corporation"))
    (#xFE21 . ("Bose Corporation"))
    (#xFE20 . ("Emerson"))
    (#xFE1F . ("Garmin International, Inc."))
    (#xFE1E . ("Smart Innovations Co., Ltd"))
    (#xFE1D . ("Illuminati Instrument Corporation"))
    (#xFE1C . ("NetMedia, Inc."))
    (#xFE1B . ("Tyto Life LLC"))
    (#xFE1A . ("Tyto Life LLC"))
    (#xFE19 . ("Google, Inc"))
    (#xFE18 . ("Runtime, Inc."))
    (#xFE17 . ("Telit Wireless Solutions GmbH"))
    (#xFE16 . ("Footmarks, Inc."))
    (#xFE15 . ("Amazon.com Services, Inc."))
    (#xFE14 . ("Flextronics International USA Inc."))
    (#xFE13 . ("Apple Inc."))
    (#xFE12 . ("M-Way Solutions GmbH"))
    (#xFE11 . ("GMC-I Messtechnik GmbH"))
    (#xFE10 . ("Lapis Semiconductor Co., Ltd."))
    (#xFE0F . ("Signify Netherlands B.V. (formerly Philips Lighting B.V.)"))
    (#xFE0E . ("Setec Pty Ltd"))
    (#xFE0D . ("Procter & Gamble"))
    (#xFE0C . ("Procter & Gamble"))
    (#xFE0B . ("ruwido austria gmbh"))
    (#xFE0A . ("ruwido austria gmbh"))
    (#xFE09 . ("Pillsy, Inc."))
    (#xFE08 . ("Microsoft"))
    (#xFE07 . ("Sonos, Inc."))
    (#xFE06 . ("Qualcomm Technologies, Inc."))
    (#xFE05 . ("CORE Transport Technologies NZ Limited"))
    (#xFE04 . ("OpenPath Security Inc"))
    (#xFE03 . ("Amazon.com Services, Inc."))
    (#xFE02 . ("Robert Bosch GmbH"))
    (#xFE01 . ("Duracell U.S. Operations Inc."))
    (#xFE00 . ("Amazon.com Services, Inc."))
    (#xFDFF . ("OSRAM GmbH"))
    (#xFDFE . ("ADHERIUM(NZ) LIMITED"))
    (#xFDFD . ("RecursiveSoft Inc."))
    (#xFDFC . ("Optrel AG"))
    (#xFDFB . ("Tandem Diabetes Care"))
    (#xFDFA . ("Tandem Diabetes Care"))
    (#xFDF9 . ("INIA"))
    (#xFDF8 . ("Onvocal"))
    (#xFDF7 . ("HP Inc."))
    (#xFDF6 . ("AIAIAI ApS"))
    (#xFDF5 . ("Milwaukee Electric Tools"))
    (#xFDF4 . ("O. E. M. Controls, Inc."))
    (#xFDF3 . ("Amersports"))
    (#xFDF2 . ("AMICCOM Electronics Corporation"))
    (#xFDF1 . ("LAMPLIGHT Co., Ltd"))
    (#xFDF0 . ("Google Inc."))
    (#xFDEF . ("ART AND PROGRAM, INC."))
    (#xFDEE . ("Huawei Technologies Co., Ltd."))
    (#xFDED . ("Pole Star"))
    (#xFDEC . ("Mannkind Corporation"))
    (#xFDEB . ("Syntronix Corporation"))
    (#xFDEA . ("SeeScan, Inc"))
    (#xFDE9 . ("Spacesaver Corporation"))
    (#xFDE8 . ("Robert Bosch GmbH"))
    (#xFDE7 . ("SECOM Co., LTD"))
    (#xFDE6 . ("Intelletto Technologies Inc"))
    (#xFDE5 . ("SMK Corporation"))
    (#xFDE4 . ("JUUL Labs, Inc."))
    (#xFDE3 . ("Abbott Diabetes Care"))
    (#xFDE2 . ("Google Inc."))
    (#xFDE1 . ("Fortin Electronic Systems"))
    (#xFDE0 . ("John Deere"))
    (#xFDDF . ("Harman International"))
    (#xFDDE . ("Noodle Technology Inc."))
    (#xFDDD . ("Arch Systems Inc"))
    (#xFDDC . ("4iiii Innovations Inc."))
    (#xFDDB . ("Samsung Electronics Co., Ltd."))
    (#xFDDA . ("MHCS"))
    (#xFDD9 . ("Jiangsu Teranovo Tech Co., Ltd."))
    (#xFDD8 . ("Jiangsu Teranovo Tech Co., Ltd."))
    (#xFDD7 . ("Emerson"))
    (#xFDD6 . ("Ministry of Supply"))
    (#xFDD5 . ("Brompton Bicycle Ltd"))
    (#xFDD4 . ("LX Solutions Pty Limited"))
    (#xFDD3 . ("FUBA Automotive Electronics GmbH"))
    (#xFDD2 . ("Bose Corporation"))
    (#xFDD1 . ("Huawei Technologies Co., Ltd"))
    (#xFDD0 . ("Huawei Technologies Co., Ltd"))
    (#xFDCF . ("Nalu Medical, Inc"))
    (#xFDCE . ("SENNHEISER electronic GmbH & Co. KG"))
    (#xFDCD . ("Qingping Technology (Beijing) Co., Ltd."))
    (#xFDCC . ("Shoof Technologies"))
    (#xFDCB . ("Meggitt SA"))
    (#xFDCA . ("Fortin Electronic Systems"))
    (#xFDC9 . ("Busch-Jaeger Elektro GmbH"))
    (#xFDC8 . ("Hach – Danaher"))
    (#xFDC7 . ("Eli Lilly and Company"))
    (#xFDC6 . ("Eli Lilly and Company"))
    (#xFDC5 . ("Automatic Labs"))
    (#xFDC4 . ("Simavita (Aust) Pty Ltd"))
    (#xFDC3 . ("Baidu Online Network Technology (Beijing) Co., Ltd"))
    (#xFDC2 . ("Baidu Online Network Technology (Beijing) Co., Ltd"))
    (#xFDC1 . ("Hunter Douglas"))
    (#xFDC0 . ("Hunter Douglas"))
    (#xFDBF . ("California Things Inc."))
    (#xFDBE . ("California Things Inc."))
    (#xFDBD . ("Clover Network, Inc."))
    (#xFDBC . ("Emerson"))
    (#xFDBB . ("Profoto"))
    (#xFDBA . ("Comcast Cable Corporation"))
    (#xFDB9 . ("Comcast Cable Corporation"))
    (#xFDB8 . ("LivaNova USA Inc."))
    (#xFDB7 . ("LivaNova USA Inc."))
    (#xFDB6 . ("GWA Hygiene GmbH"))
    (#xFDB5 . ("ECSG"))
    (#xFDB4 . ("HP Inc"))
    (#xFDB3 . ("Audiodo AB"))
    (#xFDB2 . ("Portable Multimedia Ltd"))
    (#xFDB1 . ("Proxy Technologies, Inc."))
    (#xFDB0 . ("Proxy Technologies, Inc."))
    (#xFDAF . ("Wiliot LTD"))
    (#xFDAE . ("Houwa System Design, k.k."))
    (#xFDAD . ("Houwa System Design, k.k."))
    (#xFDAC . ("Tentacle Sync GmbH"))
    (#xFDAB . ("Xiaomi Inc."))
    (#xFDAA . ("Xiaomi Inc."))
    (#xFDA9 . ("Rhombus Systems, Inc."))
    (#xFDA8 . ("PSA Peugeot Citroën"))
    (#xFDA7 . ("WWZN Information Technology Company Limited"))
    (#xFDA6 . ("WWZN Information Technology Company Limited"))
    (#xFDA5 . ("Neurostim OAB, Inc."))
    (#xFDA4 . ("Inseego Corp."))
    (#xFDA3 . ("Inseego Corp."))
    (#xFDA2 . ("Groove X, Inc"))
    (#xFDA1 . ("Groove X, Inc"))
    (#xFDA0 . ("Secugen Corporation"))
    (#xFD9F . ("VitalTech Affiliates LLC"))
    (#xFD9E . ("The Coca-Cola Company"))
    (#xFD9D . ("Gastec Corporation"))
    (#xFD9C . ("Huawei Technologies Co., Ltd."))
    (#xFD9B . ("Huawei Technologies Co., Ltd."))
    (#xFD9A . ("Huawei Technologies Co., Ltd."))
    (#xFD99 . ("ABB Oy"))
    (#xFD98 . ("Disney Worldwide Services, Inc."))
    (#xFD97 . ("June Life, Inc."))
    (#xFD96 . ("Google LLC"))
    (#xFD95 . ("Rigado"))
    (#xFD94 . ("Hewlett Packard Enterprise"))
    (#xFD93 . ("Bayerische Motoren Werke AG"))
    (#xFD92 . ("Qualcomm Technologies International, Ltd. (QTIL)"))
    (#xFD91 . ("Groove X, Inc."))
    (#xFD90 . ("Guangzhou SuperSound Information Technology Co., Ltd"))
    (#xFD8F . ("Matrix ComSec Pvt. Ltd."))
    (#xFD8E . ("Motorola Solutions"))
    (#xFD8D . ("quip NYC Inc."))
    (#xFD8C . ("Google LLC"))
    (#xFD8B . ("Jigowatts Inc."))
    (#xFD8A . ("Signify Netherlands B.V."))
    (#xFD89 . ("Urbanminded LTD"))
    (#xFD88 . ("Urbanminded LTD"))
    (#xFD87 . ("Google LLC")))
  "Bluetooth manufacturer UUIDs.")

(provide 'bluetooth)

;;; bluetooth.el ends here
