;;; org-fc-forvo.el --- Forvo API integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  org-fc contributors

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
;; Helper for talking to the Forvo API.  Requests can optionally be
;; routed through an HTTP proxy.
;;
;;; Code:

(require 'url)

(defgroup org-fc-forvo nil
  "Forvo API integration for org-fc."
  :group 'org-fc)

(defcustom org-fc-forvo-http-proxy nil
  "HTTP proxy used for requests to the Forvo API.

When non-nil, should be a URL string like
"http://127.0.0.1:8080".  The proxy will be used for both HTTP and
HTTPS requests."
  :type '(choice (const :tag "None" nil)
                 (string :tag "URL"))
  :group 'org-fc-forvo)

(defun org-fc-forvo-request (url)
  "Retrieve URL from the Forvo API.
The request honours `org-fc-forvo-http-proxy'.  Returns the buffer
containing the response."
  (let ((url-proxy-services
         (when org-fc-forvo-http-proxy
           `(("http" . ,org-fc-forvo-http-proxy)
             ("https" . ,org-fc-forvo-http-proxy)))))
    (url-retrieve-synchronously url)))

(provide 'org-fc-forvo)

;;; org-fc-forvo.el ends here
