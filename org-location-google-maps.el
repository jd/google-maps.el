;;; org-location-google-maps.el --- Show Google Maps' map for an Org entry location

;; Copyright (C) 2010 Julien Danjou

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
;; Integrate google-maps into org-mode.
;;
;; This allows you to press C-c M-l on an Org entry to get the Google Map of
;; your appointment.
;;
;;; Code:

(require 'google-maps)
(require 'google-maps-geocode)
(require 'org)
(require 'org-agenda)

(defun org-google-maps (location &optional with-current-location)
  "Run Google Maps for LOCATION.
If WITH-CURRENT-LOCATION prefix is set, add a marker with current
location."
  (let ((buffer (google-maps location)))
    (when with-current-location
      (with-current-buffer buffer
        (google-maps-static-add-home-marker)))))

(defun org-location-google-maps (&optional with-current-location)
  "Show Google Map for location of an Org entry in an org buffer.
If WITH-CURRENT-LOCATION prefix is set, add a marker with current
location."
  (interactive "P")
  (let ((location (org-entry-get nil "LOCATION" t)))
    (when location
      (org-google-maps location with-current-location))))

(define-key org-mode-map "\C-c\M-l" 'org-location-google-maps)

(defun org-agenda-location-google-maps (&optional with-current-location)
  "Show Google Map for location of an Org entry in an org-agenda buffer."
  (interactive "P")
  (let ((location
         (save-window-excursion
           (org-agenda-goto)
           (org-entry-get nil
                          (if (boundp 'org-contacts-address-property)
                              org-contacts-address-property
                            "LOCATION" t)))))
    (when location
      (org-google-maps location with-current-location))))

(define-key org-agenda-mode-map "\C-c\M-l" 'org-agenda-location-google-maps)

(defun org-location-google-geocode-set (location)
  "Set location property to LOCATION for current entry using Google Geocoding API."
  (interactive
   (list (read-string "Location: ")))
  (org-set-property "LOCATION" (cdr (assoc 'formatted_address
                                           (google-maps-geocode-location location)))))

(define-key org-mode-map "\C-c\M-L" 'org-location-google-geocode-set)

(provide 'org-location-google-maps)
