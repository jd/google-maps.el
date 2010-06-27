;;; google-maps-static.el --- Access Google Maps Static from Emacs

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Functions and data used by Google Maps sub modules.
;;
;;; Code:

(defgroup google-maps nil
  "Google Maps."
  :group 'comm)

(defcustom google-maps-default-sensor nil
  "Default sensor value for map request."
  :group 'google-maps)

(defun google-maps-build-plist (plist)
  "Build a property list based on PLIST."
  (unless (plist-member plist :sensor)
    (plist-put plist :sensor google-maps-default-sensor))
  plist)

(defun google-maps-skip-http-headers (buffer)
  "Remove HTTP headers from BUFFER, and return it.
Assumes headers are indeed present!"
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    buffer))

(defun google-maps-retrieve-data (url)
  "Retrieve image and return its data as string, using URL to the
image."
  (let* ((image-buffer (google-maps-skip-http-headers
                        (url-retrieve-synchronously url)))
         (data (with-current-buffer image-buffer
                 (buffer-string))))
    (kill-buffer image-buffer)
    data))

(provide 'google-maps)
