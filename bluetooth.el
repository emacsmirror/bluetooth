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

(defun bluetooth--root (dev-id)
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
(defun bluetooth--list-entry-fcn ()
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
	tabulated-list-entries #'bluetooth--list-entry-fcn
	tabulated-list-padding 1)
  (bluetooth--make-commands)
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
		     :system bluetooth--service (concat bluetooth--root "/" a))))
	  (dbus-introspect-get-node-names
	   :system bluetooth--service bluetooth--root)))

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
				       :system bluetooth--service path
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
    (apply function :system bluetooth--service path interface
	   (mapcar (lambda (x) (if (eq x :path-devid) (concat path "/" dev-id) x))
		   args))))

;;; The following functions are the workers for the commands.
;;; They are used by `bluetooth--make-commands'.

;;; Invoke a D-Bus method with or without parameters.
(defun bluetooth--dbus-method (method api &rest args)
  "Invoke METHOD on D-Bus API with ARGS."
  (apply #'bluetooth--call-method (tabulated-list-get-id) api
	 #'dbus-call-method-asynchronously method
	 (lambda () (tabulated-list-print t)) :timeout bluetooth--timeout args))

;;; Toggle a property.
(defun bluetooth--dbus-toggle (property api)
  "Toggle boolean PROPERTY on D-Bus API."
  (let* ((dev-id (tabulated-list-get-id))
	 (value (bluetooth--call-method dev-id api
					#'dbus-get-property property)))
    (bluetooth--call-method dev-id api #'dbus-set-property property (not value))
    (tabulated-list-print t)))

;;; Set a property.
(defun bluetooth--dbus-set (property arg api)
  "Set PROPERTY to ARG on D-Bus API."
  (bluetooth--call-method (tabulated-list-get-id)
			  api #'dbus-set-property property arg)
  (tabulated-list-print t))

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
		    :system bluetooth--service bluetooth--root))
	 (resp (dbus-get-all-properties :system bluetooth--service
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
  (dbus-ping :system bluetooth--service bluetooth--timeout)
  (let ((buffer-exists (get-buffer bluetooth-buffer-name)))
    (with-current-buffer (switch-to-buffer bluetooth-buffer-name)
      (unless buffer-exists
	(erase-buffer)
	(bluetooth-mode)
	(bluetooth--register-agent)
	(add-hook 'kill-buffer-hook #'bluetooth--cleanup nil t)
	(setq-local mode-line-misc-info
		    (cl-pushnew bluetooth--mode-info mode-line-misc-info))
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
	   ,@body
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

(defun bluetooth--display-passkey (device passkey entered)
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
      (y-or-n-p
       (format "Authorize Bluetooth service `%s' for device `%s'? " uuid alias))))
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
		   collect (dbus-register-method :session dbus-service-emacs
						 bluetooth--own-path
						 bluetooth--agent-intf
						 method (intern fname) t))))
  (dbus-register-service :session dbus-service-emacs)
  (dbus-call-method :system bluetooth--service bluetooth--root
		    bluetooth--agent-mngr-intf "RegisterAgent"
		    :object-path bluetooth--own-path "KeyboardDisplay"))

(defun bluetooth--unregister-agent ()
  "Unregister the pairing agent."
  (ignore-errors
    (dbus-call-method :system bluetooth--service bluetooth--root
		      bluetooth--agent-mngr-intf "UnregisterAgent"
		      :object-path bluetooth--own-path)
    (mapc #'dbus-unregister-object bluetooth--method-objects)))

(provide 'bluetooth)

;;; bluetooth.el ends here
