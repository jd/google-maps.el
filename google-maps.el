;;; google-maps.el --- Access Google Maps from Emacs

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
;; All arguments are optional. Here is a full call example:
;;
;; (google-maps-show
;;  :center "Cimetière du Montparnasse"
;;  :maptype 'hybrid
;;  ;; :zoom 5
;;  :markers '((("Place Saint-Michel, Paris") . (:label ?M :color "blue"))
;;             (("Jardin du Luxembourg, Paris" "Parc Montsouris, Paris") . (:label ?P :color "green")))
;;  :visible '("44 rue de l'Ouest, Paris" "Montrouge")
;;  :paths '((("Tour Eiffel, Paris" "Arc de triomphe, Paris" "Panthéon, Paris")
;;            . (:weight 3 :color "black" :fillcolor "yellow"))))
;;
;;; TODO:
;; - Add current map information
;; - Resize map if frame is resized
;; - Add interactive code to build path
;;
;;; Code:
(eval-when-compile
  (require 'cl))

(require 'url-util)
(require 'url-http)

(defgroup google-maps nil
  "Google Maps."
  :group 'comm)

(defcustom google-maps-buffer-name "*Google Maps*"
  "Name of the Google Maps buffer."
  :group 'google-maps)

(defcustom google-maps-default-sensor nil
  "Default sensor value for map request."
  :group 'google-maps)

(defcustom google-maps-default-zoom 5
  "Default zoom level when calling `google-maps-zoom' with no argument."
  :group 'google-maps)

(defconst google-maps-uri
  "http://maps.google.com/maps/api/staticmap"
  "Google Maps API server.")

(defconst google-maps-minimum-zoom 0
  "Minimum zoom level.")

(defconst google-maps-maximum-zoom 21
  "Minimum zoom level.")

(defconst google-maps-maptypes '("roadmap" "satellite" "hybrid" "terrain")
  "Available map types.")

(defvar google-maps-mode-hook nil
  "Hook run by `google-maps-mode'.")

(defvar google-maps-params nil
  "Current parameters of the map.")
(make-variable-buffer-local 'google-maps-params)

(defun mapconcat-if-not-nil (function sequence separator)
  "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
This only concat result if result is not nil. In between each
pair of results, stick in SEPARATOR.  Thus, " " as SEPARATOR
results in spaces between the values returned by FUNCTION.
SEQUENCE may be a list, a vector, a bool-vector, or a string."
  (mapconcat
   'identity
   (remove-if 'null
              (mapcar
               function
               sequence))
   separator))

(defun google-maps-symbol-to-property (symbol)
  "Transform SYMBOL to :SYMBOL."
  (intern-soft (concat ":" (symbol-name symbol))))

(defun google-maps-urlencode-plist (plist properties &optional eqs separator)
  "Encode PLIST for a URL using PROPERTIES.
PROPERTIES should have form '((property-name . format))."
  (let ((eqs (or eqs "="))
        (separator (or separator "&")))
    (mapconcat-if-not-nil
     (lambda (entry)
       (let* ((property (car entry))
              (propsym (google-maps-symbol-to-property property))
              (value (plist-get plist propsym))
              (value-format (or (cdr entry) 'identity))
              ;; If value-format is list or function, eval
              (value (cond ((functionp value-format) (funcall
                                                      value-format
                                                      value))
                           (t (eval value-format)))))
         (when value
           (format "%s%s%s" property eqs value))))
     properties
     separator)))

(defun google-maps-marker-to-url-parameters (marker)
  (let ((prop (google-maps-urlencode-plist
               (cdr marker)
               '((size . (lambda (size)
                           (when size (number-to-string size))))
                 (color)
                 (label . (lambda (label)
                            (when label (char-to-string label)))))
               ":"
               "|")))
    (concat
     (when prop (concat prop "|"))
     (mapconcat
      'url-hexify-string
      (car marker)
      "|"))))

(defun google-maps-markers-to-url-parameters (markers)
  "From MARKERS, build parameters for a Google Static Maps URL.
MARKERS should have the form
'(((\"loc1\" \"loc2\") . (:size tiny :color \"blue\" :label ?X)))"
  (mapconcat 'google-maps-marker-to-url-parameters
             markers
             "&markers="))

(defun google-maps-visible-to-url-parameters (visible)
  "From VISIBLE, build parameters for a Google Static Maps URL.
VISIBLE should have the form '(\"loc1\" \"loc2\" ... \"locN\")."
  (mapconcat 'url-hexify-string
             visible
             "|"))

(defun google-maps-path-to-url-parameters (path)
  (let ((prop (google-maps-urlencode-plist
               (cdr path)
               '((weight . (lambda (weight)
                             (when weight
                               (number-to-string weight))))
                 (color)
                 (fillcolor))
               ":"
               "|")))
    (concat
     (when prop (concat prop "|"))
     (mapconcat
      'url-hexify-string
      (car path)
      "|"))))

(defun google-maps-paths-to-url-parameters (paths)
  "From PATH, build parameters for a Google Static Maps URL.
PATHS should have the form
'(((\"loc1\" \"loc2\") . (:weight 5 :color \"red\" :fillcolor \"black\")))"
  (mapconcat 'google-maps-path-to-url-parameters
             paths
             "&path="))

(defun google-maps-set-size (plist)
  "adapt size to current window settings"
  (let ((edges (window-inside-pixel-edges)))
    (plist-put plist :width (- (nth 2 edges) (nth 0 edges) ))
    (plist-put plist :height (- (nth 3 edges) (nth 1 edges)))
    plist))

(defun google-maps-refresh ()
  "Redisplay the map."
  (interactive)
  (apply 'google-maps-show google-maps-params))

(defun google-maps-build-plist (plist)
  "Build a property list based on PLIST."
  ;; Make all markers upper case
  (let ((markers (plist-get plist :markers)))
    (when markers
      (plist-put plist :markers
                 (mapcar
                  (lambda (marker)
                    (let ((props (cdr marker)))
                      (plist-put props :label (upcase (plist-get props :label)))
                      marker))
                  markers))))
  (unless (plist-member plist :sensor)
    (plist-put plist :sensor google-maps-default-sensor))
  (google-maps-set-size plist)
  plist)

(defun google-maps-build-url (plist)
  "Build a URL to request a static Google Map."
  (concat
   google-maps-uri "?"
   (google-maps-urlencode-plist
    plist
    `((format)
      (center . url-hexify-string)
      (size . ,(format "%dx%d"
                       (plist-get plist :width)
                       (plist-get plist :height)))
      (maptype)
      (mobile . ,(lambda (v)
                  (if v "true" nil)))
      ;; Sensor is MANDATORY
      (sensor . ,(lambda (v)
                  (if v "true" "false")))
      (zoom . ,(lambda (zoom)
                (when zoom number-to-string (zoom))))
      (format)
      (language)
      (markers . ,(google-maps-markers-to-url-parameters (plist-get plist :markers)))
      (visible . ,(google-maps-visible-to-url-parameters (plist-get plist :visible)))))
   (let ((paths (plist-get plist :paths)))
     (if paths
         (concat "&path=" (google-maps-paths-to-url-parameters paths))
     ""))))

(defun google-maps-skip-http-headers (buffer)
  "Remove HTTP headers from BUFFER, and return it.
Assumes headers are indeed present!"
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    buffer))

(defun google-maps-retrieve-image (url)
  "Retrieve image and return its data as string, using URL to the
image."
  (let* ((image-buffer (google-maps-skip-http-headers
                        (url-retrieve-synchronously url)))
         (data (with-current-buffer image-buffer
                 (buffer-string))))
    (kill-buffer image-buffer)
    data))

(defun google-maps-insert-image-at-point (start image format)
  "Insert an IMAGE with FORMAT at point START."
  (goto-char start)
  (insert "Map")
  (add-text-properties
   start (point)
   `(display
     ,(create-image image format t)
     read-only t
     rear-nonsticky (display read-only))))

(defun google-maps-show (&rest plist)
  "XXX DOC."
  (let ((buffer (get-buffer-create google-maps-buffer-name)))
    (unless (eq (current-buffer) buffer)
      (switch-to-buffer-other-window buffer))
    (google-maps-mode)
    (let* ((inhibit-read-only t)
           (plist (google-maps-build-plist plist))
           (url (google-maps-build-url plist)))
      (setq google-maps-params plist)
      (delete-region (point-min) (point-max))
      (google-maps-insert-image-at-point
       (point-min)
       (google-maps-retrieve-image url)
       (plist-get plist :format)))))

(defvar google-maps-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'google-maps-zoom-in)
    (define-key map (kbd ">") 'google-maps-zoom-in)
    (define-key map (kbd ".") 'google-maps-zoom-in)
    (define-key map (kbd "-") 'google-maps-zoom-out)
    (define-key map (kbd "<") 'google-maps-zoom-out)
    (define-key map (kbd ",") 'google-maps-zoom-out)
    (define-key map (kbd "z") 'google-maps-zoom)
    (define-key map (kbd "q") 'google-maps-quit)
    (define-key map (kbd "w") 'google-maps-copy-url)
    (define-key map (kbd "m") 'google-maps-manage-marker)
    (define-key map (kbd "v") 'google-maps-manage-visible)
    (define-key map (kbd "c") 'google-maps-center)
    (define-key map (kbd "t") 'google-maps-set-maptype)
    (define-key map (kbd "g") 'google-maps-refresh)
    (define-key map [mouse-4] 'google-maps-zoom-mouse-in)
    (define-key map [mouse-5] 'google-maps-zoom-mouse-out)
    map)
  "Keymap for `google-maps-mode'.")

;;;###autoload
(define-derived-mode google-maps-mode fundamental-mode "Google Maps"
  "A major mode for Google Maps service"
  :group 'comm
  (setq cursor-type nil)
  (setq buffer-read-only t))

(defun google-maps-zoom (level)
  "Zoom a Google map."
  (interactive "P")
  (let ((plist google-maps-params)
        (level (or level google-maps-default-zoom)))
    (plist-put plist
               :zoom
               (max (min level google-maps-maximum-zoom) google-maps-minimum-zoom))
    (apply 'google-maps-show plist)))

(defun google-maps-zoom-in ()
  "Zoom a Google map in."
  (interactive)
  (unless (plist-member google-maps-params :zoom)
    (error "Current zoom level is unknown, cannot zoom in."))
  (google-maps-zoom (1+ (plist-get google-maps-params :zoom))))

(defun google-maps-zoom-out ()
  "Zoom a Google map out."
  (interactive)
  (unless (plist-member google-maps-params :zoom)
    (error "Current zoom level is unknown, cannot zoom out."))
  (google-maps-zoom (1- (plist-get google-maps-params :zoom))))

(defun google-maps-quit ()
  "Kill Google maps buffer."
  (interactive)
  (kill-buffer))

(defun google-maps-copy-url ()
  "Kill Google maps buffer."
  (interactive)
  (kill-new (google-maps-build-url google-maps-params)))

(defun google-maps-add-visible (location)
  "Make LOCATION visible on the map."
  (interactive
   (list
    (read-string "Location to set visible: ")))
  (let* ((plist google-maps-params)
         (visible (plist-get plist :visible)))
    (plist-put plist :visible (add-to-list 'visible location))
    (apply 'google-maps-show plist)))

(defun google-maps-remove-visible (location)
  "Remove a visible LOCATION on the map."
  (interactive
   (let* ((plist google-maps-params)
          (visible (plist-get plist :visible)))
     (list
      (completing-read "Location to unset visible: " visible nil t))))
  (let* ((plist google-maps-params)
         (visible (plist-get plist :visible)))
    (plist-put plist :visible
               (remove-if `(lambda (l) (string= l ,location)) visible))
    (apply 'google-maps-show plist)))

(defun google-maps-manage-visible (remove)
  "Add or remove a visible location. If REMOVE is set, remove it."
  (interactive "P")
  (if remove
      (call-interactively 'google-maps-remove-visible)
    (call-interactively 'google-maps-add-visible)))

(defun google-maps-add-marker (location label &optional size color)
  "Add a marker on LOCATION on the map with LABEL. You can
specify SIZE and COLOR of the LABEL."
  (interactive
   (list
    (read-string "Location to mark: ")
    (read-char "Type a character to use as mark for location.")))
  (let* ((plist google-maps-params)
         (markers (plist-get plist :markers)))
    (add-to-list 'markers `((,location) . (:label ,label :size ,size :color ,color)))
    (plist-put plist :markers markers)
    (apply 'google-maps-show plist)))

(defun google-maps-remove-marker (label)
  "Remove a marker from the map."
  (interactive
   (list
    (read-char "Type the mark character to remove from the map.")))
  (let ((label (upcase label)))
    (let* ((plist google-maps-params)
           (markers (plist-get plist :markers)))
      (plist-put plist :markers
                 (remove-if
                  (lambda (marker)
                    (eq (plist-get (cdr marker) :label) label))
                  markers))
      (apply 'google-maps-show plist))))

(defun google-maps-manage-marker (times)
  "Remove or add markers on the map.
If TIMES is positive, add this number of marker.
If TIMES is negative, then remove this number of markers."
  (interactive "p")
  (if (> times 0)
      (dotimes (x times)
        (call-interactively 'google-maps-add-marker))
    (dotimes (x (abs times))
      (call-interactively 'google-maps-remove-marker))))

(defun google-maps-center (location)
  "Center the map on a LOCATION. If LOCATION is nil or an empty
string, it will remove centering."
  (interactive
   (list
    (read-string "Location to center the map on: ")))
  (let ((plist google-maps-params))
    (plist-put plist :center location)
    (apply 'google-maps-show plist)))

(defun google-maps-event-to-buffer (event)
  (window-buffer (posn-window (event-start event))))

(defun google-maps-zoom-mouse-in (event)
  "Zoom with the mouse."
  (interactive (list last-input-event))
    (with-current-buffer (google-maps-event-to-buffer event)
      (funcall 'google-maps-zoom-in)))

(defun google-maps-zoom-mouse-out (event)
  "Zoom with the mouse."
  (interactive (list last-input-event))
    (with-current-buffer (google-maps-event-to-buffer event)
      (funcall 'google-maps-zoom-out)))

(defun google-maps-set-maptype (maptype)
  "Set map type to MAPTYPE."
  (interactive
   (list
    (intern
     (completing-read "Map type: " google-maps-maptypes nil t))))
  (let ((plist google-maps-params))
    (plist-put plist :maptype maptype)
    (apply 'google-maps-show plist)))

(defun google-maps (location)
  "Run Google Maps on LOCATION."
  (interactive (list (read-string "Location: ")))
  (google-maps-show :center location))

(provide 'google-maps)
