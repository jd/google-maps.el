;;; google-maps.el --- Access Google Maps from Emacs

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
;; None
;;; Code:
(eval-when-compile
  (require 'cl))

(require 'google-maps-geocode)
(require 'google-maps-static)

;;;###autoload
(defun google-maps (location &optional no-geocoding)
  "Run Google Maps on LOCATION.
If NO-GEOCODING is t, then does not try to geocode the address
and do not ask the user for a more precise location."
  (interactive
   (list
    (if (use-region-p)
	(buffer-substring-no-properties
         (region-beginning) (region-end))
      (read-string "Location: "))))
  (let ((location (if no-geocoding
                      location
                    (google-maps-geocode-location location))))
    (google-maps-static-show :markers `(((,location))))))

(provide 'google-maps)
