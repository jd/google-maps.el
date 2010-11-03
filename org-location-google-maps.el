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

(defun org-location-google-maps (&optional with-current-location)
  "Show Google Map for location of an Org entry in an org buffer.
If WITH-CURRENT-LOCATION prefix is set, add a marker with current
location. Current location is determined using
`calendar-location-name', `calendar-latitude' and
`calendar-longitude' variable."
  (interactive "P")
  (let ((location (org-entry-get nil "LOCATION" t)))
    (when location
      (let ((buffer (google-maps location)))
        (when (and with-current-location
                   calendar-latitude
                   calendar-longitude
                   calendar-location-name)
          (google-maps-static-add-marker `(,(eval calendar-location-name)
                                            ((lat . ,calendar-latitude)
                                             (lng . ,calendar-longitude)))
                                         ?H))))))

(define-key org-mode-map "\C-c\M-l" 'org-location-google-maps)

(defun org-agenda-location-google-maps ()
  "Show Google Map for location of an Org entry in an org-agenda buffer."
  (interactive)
  (let ((location
         (save-window-excursion
           (org-agenda-goto)
           (org-entry-get nil "LOCATION" t))))
    (when location
      (google-maps location))))

(define-key org-agenda-mode-map "\C-c\M-l" 'org-agenda-location-google-maps)

(defun org-location-google-geocode-set (location)
  "Set location property to LOCATION for current entry using Google Geocoding API."
  (interactive
   (list (read-string "Location: ")))
  (org-set-property "LOCATION" (cdr (assoc 'formatted_address
                                           (google-maps-geocode-location location)))))

(define-key org-mode-map "\C-c\M-L" 'org-location-google-geocode-set)

(provide 'org-location-google-maps)
