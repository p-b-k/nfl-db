;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "clim-examples")

(defpackage #:nfl-db (:use #:clim-lisp #:clim))
(in-package #:nfl-db)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Team names and information (name, conference, division, etc)
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar league '(
    ( :BUF           "Buffalo"          "Bills"         afc . east )
    ( :MIA           "Miami"            "Dolphins"      afc . east )
    ( :NE            "New England"      "Patriots"      afc . east )
    ( :NYJ           "New York"         "Jets"          afc . east )
    ( :BAL            "Baltimore"       "Ravens"        afc . north )
    ( :CIN            "Cincinnati"      "Bengals"       afc . north )
    ( :CLE            "Cleveland"       "Browns"        afc . north )
    ( :PIT            "Pittsburg"       "Steelers"      afc . north )
    ( :HOU            "Houston"         "Texans"        afc . south )
    ( :IND            "Indianapolis"    "Colts"         afc . south )
    ( :JAX            "Jacksonville"    "Jaguars"       afc . south )
    ( :TEN            "Tennessee"       "Titans"        afc . south )
    ( :DEN            "Denver"          "Broncos"       afc . west )
    ( :KC             "Kansas City"     "Chiefs"        afc . west )
    ( :LV             "Las Vegas"       "Raiders"       afc . west )
    ( :LAC            "Los Angeles"     "Chargers"      afc . west )
    ( :DAL            "Dallas"          "Cowboys"       nfc . east )
    ( :NYG            "New York"        "Giants"        nfc . east )
    ( :PHI            "Philadelphia"    "Eagles"        nfc . east )
    ( :WAS            "Washington"      "Commanders"    nfc . east )
    ( :CHI            "Chicago"         "Bears"         nfc . north )
    ( :DET            "Detroit"         "Lions"         nfc . north )
    ( :GB             "Green Bay"       "Packers"       nfc . north )
    ( :MIN            "Minisota"        "Vikings"       nfc . north )
    ( :ATL            "Atlanta"         "Falcons"       nfc . south )
    ( :CAR            "Carolina"        "Panthers"      nfc . south )
    ( :NO             "New Orleans"     "Saints"        nfc . south )
    ( :TB             "Tamba Bay"       "Buccaneers"    nfc . south )
    ( :ARI            "Arizona"         "Cardinals"     nfc . west )
    ( :LA             "Los Angeles"     "Rams"          nfc . west )
    ( :SF             "San Fransisco"   "49ers"         nfc . west )
    ( :SEA            "Seattle"         "Seahawks"      nfc . west )))

(defun team-data-rec (team todo)
  (if todo
    (let ( (next (car todo)) )
      (if (equalp team (car next))
        (cdr next)
        (team-data-rec team (cdr todo))))
    nil))

(defun team-data (team) (team-data-rec team league))

(defun team-home (team)
  (let ( (data (team-data team)) )
    (if data (car data) nil)))

(defun team-name (team)
  (let ( (data (team-data team)) )
    (if data (car (cdr data)) nil)))

(defun team-division (team)
  (let ( (data (team-data team)) )
    (if data (cdr (cdr data)) nil)))

(defun team-title (team)
  (let ( (data (team-data team)) )
    (if data (concatenate 'string (car data) " " (car (cdr data))) nil)))

(export 'team-home)
(export 'team-name)
(export 'team-division)
(export 'team-title)

; (defun get-rgb-string (c) (multiple-value-bind (r g b) (color-rgb c) (format nil "rbg:~a/~a/~a" r g b)))

; (defun color-slider-value-changed (slider new-value)
;   (with-slots (current-color-pane red green blue) *application-frame*
;     (let ( (id (gadget-id slider)) )
;       ;; The gadget-id symbols match the slot names in color-editor
;       (setf (slot-value *application-frame* id) new-value)
;       (let ( (new-color (make-rgb-color red green blue)) )
;         (setf (pane-background current-color-pane) new-color)
;         (setf (medium-background current-color-pane) new-color)))
;     (redisplay-frame-pane *application-frame* current-color-pane)))

; ;; Create the slider constructor function
; (defun make-color-slider (id initval label)
;   (labelling
;     (:label label)
;     (make-pane ':slider
;       :id id
;       :orientation :horizontal
;       :value initval
;       :max-value 1
;       :min-value 0
;       :drag-callback #'color-slider-dragged
;       :value-changed-callback #'color-slider-value-changed)))

; ;; Define the main application frame
; (define-application-frame color-editor ()
;   ( current-color-pane
;     drag-feedback-pane
;     (red    :initform 0.0)
;     (green  :initform 1.0)
;     (blue   :initform 0.0) )
;   (:pane (with-slots (drag-feedback-pane current-color-pane red green blue) *application-frame*
;                      (vertically ()
;                         (setf current-color-pane
;                               (make-pane  'application-pane
;                                           :min-height 100
;                                           :max-height 100
;                                           :background (make-rgb-color red green blue)))
;                         (horizontally (:min-height 200 :max-height 200)
;                                       (1/2 (make-color-slider 'red    red    "Red"))
;                                       (1/4 (make-color-slider 'green  green  "Green"))
;                                       (1/4 (make-color-slider 'blue   blue   "Blue")))
;                         +fill+
;                         (setf drag-feedback-pane
;                               (make-pane  'application-pane
;                                           :min-height 100
;                                           :max-height 100
;                                           :background (make-rgb-color red green blue))))))
;   (:menu-bar t))

; (define-color-editor-command (com-quit :name "Quit" :menu t) ()
;   (frame-exit *application-frame*))

; (defun run () (run-frame-top-level (make-application-frame 'color-editor)))
; (export 'run)

