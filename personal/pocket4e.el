;;; pocket4e.el --- Minor mode for interfacing with getpocket.com
;; Copyright (C) 2013 Chris Bilson
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; pocket4e is a minor mode for working with the getpocket.com API, which
;;; lets one save URLs for reading later, read them, etc.  See getpocket.com
;;; for more information.

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'request)
(require 'browse-url)

(defconst pocket4e-api-host "https://getpocket.com/v3/")
(defconst pocket4e-oauth-request-endpoint (concat pocket4e-api-host "oauth/request"))
(defconst pocket4e-oauth-authorize-endpoint (concat pocket4e-api-host "oauth/authorize"))

(defvar pocket4e-consumer-key "19005-ac921c26923a78a736150c13")
(defvar pocket4e-access-token nil)

(setq request-log-level 'debug)
(setq request-message-level 'debug)

(defun pocket4e--browser-authorize-url (code)
  (format "https://getpocket.com/auth/authorize?request_token=%s&redirect_uri=%s"
          code "pocket4emacs:authorizationFinished"))

(defun pocket4e--initial-auth-request-data ()
  (json-encode
   `(("consumer_key" . ,pocket4e-consumer-key)
     ("redirect_uri" . "pocket4emacs:authorizationFinished"))))

(defun pocket4e--2nd-auth-request-data (code)
  (json-encode
   `(("consumer_key" . ,pocket4e-consumer-key)
     ("code" . ,code))))

(defun* pocket4e--handle-2nd-authenticate-response (&key data &allow-other-keys)
  (let* ((token (assoc-default 'access_token data))
         (user-name (assoc-default 'user_name data)))
    (message "done. token=%s, user-name=%s" token user-name)))

(defun* pocket4e--handle-initial-authenticate-response (&key data &allow-other-keys)
  (let* ((code (assoc-default 'code data))
         (continue-url (pocket4e--browser-authorize-url code)))
    (browse-url continue-url)
    (read-string  "continue authenticating in the browser. Press enter when done.")
    (request
     pocket4e-oauth-authorize-endpoint
     :type "POST"
     :headers '(("Content-Type" . "application/json; charset=UTF-8")
                ("X-Accept" . "application/json"))
     :data (pocket4e--2nd-auth-request-data)
     :parser 'json-read
     :success #'pocket4e--handle-2nd-authenticate-response
     :error (function* (lambda (&key error-thrown &allow-other-keys)
                         (message "Got error: %S" error-thrown))))))

(defun pocket4e--authenticate ()
  "Authenticate with getpocket to get an access token."
  (request
   pocket4e-oauth-request-endpoint
   :type "POST"
   :headers '(("Content-Type" . "application/json; charset=UTF-8")
              ("X-Accept" . "application/json"))
   :data (pocket4e--initial-auth-request-data)
   :parser 'json-read
   :success #'pocket4e--handle-initial-authenticate-response
   :error (function* (lambda (&key error-thrown &allow-other-keys)
                       (message "Got error: %S" error-thrown)))))

(defun pocket4e-add (url &optional title)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json; charset=UTF-8")
           ("X-Accept" . "application/json")))
        (url-request-data (json-encode-alist
                           '(("url" . "http://getpocket.com/developer/docs/v3/add")
                             ("title" . "some title")
                             ("consumer_key" . pocket4e-consumer-key)
                             ("access_token" . "")))))))


(provide 'pocket4e)
;;; pocket4e ends here


