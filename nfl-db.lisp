;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (ql:quickload "clim-examples")

(defpackage #:nfl-db (:use #:cl))
(in-package #:nfl-db)

(defun team-title (team)
  (let ( (data (team-data team)) )
    (if data (concatenate 'string (team-home data) " " (team-name data)) nil)))

(export 'team-home)     ;; getter for where a team plays (e.g. "Philadelphia")
(export 'team-name)     ;; getter for the name of a team (e.g. "Eagles")
(export 'team-conf)     ;; getter for the conference of a team (e.g. "NFC")
(export 'team-div)      ;; getter for the division of a team (e.g. "East")
(export 'team-title)    ;; getter for the combined title of a team (e.g. "Philadelphia Eagles")
(export 'team-data)     ;; looks up a team from a team id and returns it, or nil

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Define the class for a team
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass team ()
  ( (id     :initarg  :id
            :accessor team-id)
    (home   :initarg  :home
            :accessor team-home)
    (name   :initarg  :name
            :accessor team-name)
    (colors :initargs :colors
            :accessor team-colors)
    (conf   :initarg  :conf
            :accessor team-conf)
    (div    :initarg  :div 
            :accessor team-div) )
)

(defvar team-map (make-hash-table))

(defun make-team (id home name conf div)
  (let ( (inst (make-instance 'team :id id :home home :name name :conf conf :div div)) )
    (setf (gethash id team-map) inst)
    inst))

(mapcar (lambda (x) (make-team (aref x 0) (aref x 1) (aref x 2) (aref x 3) (aref x 4))) league)

(defun team-data (id) (gethash id team-map))

(defclass game ()
  ( (week   :initarg    :week
            :accessor   game-week)
    (home   :initarg    :home
            :accessor   game-home)
    (away   :initarg    :away
            :accessor   game-away)
    (date   :initarg    :data
            :initform   nil
            :accessor   game-time)
    (time   :initarg    :time
            :initform   nil
            :accessor   game-time)
    (airer  :initargs   :airer
            :accessor   game-airer)
    (score  :initform   nil
            :accessor   game-score) )
)

(defun game-id (g) (list (game-week g) (game-home g) (game-away g)))

(defclass game-date ()
  ( (year   :initarg    :year
            :accessor   match-year)
    (month  :initarg    :month
            :accessor   match-month)
    (day    :initarg    :day 
            :accessor   match-day) )
)

(defclass game-time ()
  ( (hour   :initarg    :hour
            :accessor   match-hour)
    (minute :initarg    :minute
            :accessor   match-minute) )
)

(defclass game-score ()
  ( (home   :initform   #(0 0 0 0 0)
            :accessor   score-home)
    (away   :initform   #(0 0 0 0 0)
            :accessor   score-away) ))

