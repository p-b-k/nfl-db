;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database of NFL Season info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (ql:quickload "clim-examples")

(defpackage #:nfl-db (:use #:cl))
(in-package #:nfl-db)

(export 'team-id)            ;; getter for the team id
(export 'team-home)          ;; getter for where a team plays (e.g. "Philadelphia")
(export 'team-name)          ;; getter for the name of a team (e.g. "Eagles")
(export 'team-colors)        ;; getter for the color scheme of a team (e.g. "(dark-green white grey black)")
(export 'team-conf)          ;; getter for the conference of a team (e.g. :NFC)
(export 'team-div)           ;; getter for the division of a team (e.g. :EAST)
(export 'team-title)         ;; psuedo getter for the combined title of a team (e.g. "Philadelphia Eagles")
(export 'team-div-name)      ;; psuedo getter for the combined conference and division of a team (e.g. "NFC East")
(export 'team-lookup)        ;; looks up a team from a team id and returns it, or nil

(export 'game-day)           ;; accessor or the date of the game
(export 'game-time)          ;; accessor or the time of the game
(export 'game-score)         ;; accessor or the score of the game
(export 'game-airer)         ;; accessor or the airer of the game

(export 'score-totals)       ;; the total scores (away . home) for a game

(export 'division-teams)     ;; The teams in a given division
(export 'conference-teams)   ;; The teams in a givin conference

(export 'team-schedule)      ;; the list of games in a teams schedule, in order, with NIL for a bye
(export 'schedule-week)      ;; Get the week from the week number, with a week being a sorted list of games

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Reference classes -- just enough data to look identifity the objects, but not the associated data
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass team ()
  ( (team-id :initarg  :team-id
             :accessor team-id) )
)

(defclass game ()
  ( (week-no :initarg  :week
             :accessor week-no)
    (away-id :initarg  :away-team
             :accessor away-team)
    (home-id :initarg  :home-team
             :accessor home-team) )
)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Data classes -- The actual data held by the objects
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass team-data (team)
  ( (home     :initarg  :home
              :accessor team-home)
    (name     :initarg  :name
              :accessor team-name)
    (colors   :initarg  :colors
              :accessor team-colors)
    (conf     :initarg  :conf
              :accessor team-conf)
    (div      :initarg  :div
              :accessor team-div) )
)

;; psuedo getters
(defmethod team-title ((d team-data)) (concatenate 'string (team-home d) " " (team-name d)))
(defmethod team-div-name ((data team-data))
  (let ( (c (symbol-name (team-conf data)))
         (d (symbol-name (team-div data))) )
    (concatenate 'string c " " (subseq d 0 1) (string-downcase (subseq d 1)))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Access the defined teams by id
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar team-map (make-hash-table))

(defun team-lookup (id) (gethash id team-map))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Access the team data
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod team-home ((k symbol))     (let ( (d (team-lookup k)) ) (if d (team-home d) nil)))
(defmethod team-name ((k symbol))     (let ( (d (team-lookup k)) ) (if d (team-name d) nil)))
(defmethod team-colors ((k symbol))   (let ( (d (team-lookup k)) ) (if d (team-colors d) nil)))
(defmethod team-conf ((k symbol))     (let ( (d (team-lookup k)) ) (if d (team-conf d) nil)))
(defmethod team-div ((k symbol))      (let ( (d (team-lookup k)) ) (if d (team-div d) nil)))
(defmethod team-title ((k symbol))    (let ( (d (team-lookup k)) ) (if d (team-title d) nil)))
(defmethod team-div-name ((k symbol)) (let ( (d (team-lookup k)) ) (if d (team-div-name d) nil)))

(defmethod team-home ((c team))     (team-home (team-id c)))
(defmethod team-name ((c team))     (team-name (team-id c)))
(defmethod team-colors ((c team))   (team-colors (team-id c)))
(defmethod team-conf ((c team))     (team-conf (team-id c)))
(defmethod team-div ((c team))      (team-div (team-id c)))
(defmethod team-title ((c team))    (team-title (team-id c)))
(defmethod team-div-name ((c team)) (team-div-name (team-id c)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; The data associated with a game
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass game-data (game)
  ( (date   :initarg    :data
            :initform   nil
            :accessor   game-day)
    (time   :initarg    :time
            :initform   nil
            :accessor   game-time)
    (airer  :initargs   :airer
            :initform   nil
            :accessor   game-airer)
    (score  :initform   nil
            :accessor   game-score) )
)

(defun game-id (g) (list (week-no g) (home-team g) (away-team g)))

(defclass game-date-data ()
  ( (year   :initarg    :year
            :accessor   game-date-year)
    (month  :initarg    :month
            :accessor   game-date-month)
    (day    :initarg    :day
            :accessor   game-date-day) )
)

(defmethod game-date-eqp ( (d1 game-date-data) (d2 game-date-data) )
  (and (eq (game-date-year d1) (game-date-year d2))
       (eq (game-date-month d1) (game-date-month d2))
       (eq (game-date-day d1) (game-date-day d2))))

(defmethod game-date-earlier ( (d1 game-date-data) (d2 game-date-data) )
  (if (eq (game-date-year d1) (game-date-year d2))
    (if (eq (game-date-month d1) (game-date-month d2))
      (< (game-date-day d1) (game-date-day d2))
      (< (game-date-month d1) (game-date-month d2)))
    (< (game-date-year d1) (game-date-year d2))))

(defclass game-time-data ()
  ( (hour   :initarg    :hour
            :accessor   game-time-hour)
    (minute :initarg    :minute
            :accessor   game-time-minute) )
)

(defmethod game-time-earlier ( (d1 game-time-data) (d2 game-time-data) )
  (if (eq (game-time-hour d1) (game-time-hour d2))
    (< (game-time-minute d1) (game-time-minute d2))
    (< (game-time-hour d1) (game-time-hour d2))))

(defclass game-score ()
  ( (home   :initform   '()
            :initarg    :home
            :accessor   score-home)
    (away   :initform   '()
            :initarg    :away
            :accessor   score-away) ))

(defmethod score-totals ((s game-score))
  (with-slots (home away) s
    (format t "score-totals: home = ~a, away = ~a~%" home away)
    (cons (apply #'+ away) (apply #'+ home))))

(defmethod game-earlier ( (g1 game-data) (g2 game-data) )
  (let ( (d1 (game-day g1))
         (d2 (game-day g2)) )
    (if (and d1 d2)
      (if (game-date-eqp (game-day g1) (game-day g2))
        (let ( (t1 (game-time g1))
               (t2 (game-time g2)) )
          (if (and t1 t2)
            (game-time-earlier (game-time g1) (game-time g2))
            (if (and (not t1) (not t2))
              nil
              t1)))
        (game-date-earlier (game-day g1) (game-day g2)))
      (if (and (not d1) (not d2))
        nil
        d1))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Methods to store and retrive accumulated data from the season
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun read-value-from-file (file)
  (let ( (f (open file :if-does-not-exist nil)) )
    (if f
      (let ( (value (read f)) )
        (close f)
        value)
      nil)))

(defmethod get-data-for-game ( (g game) field )
; (format t "get-data-for-game: called on ~a, ~a~%" g field)
; (format t "get-data-for-game: game away is ~a~%" (away-team g))
; (format t "get-data-for-game: game home is ~a~%" (home-team g))
; (format t "get-data-for-game: game week is ~a~%" (week-no g))
  (with-slots ( week-no away-id home-id ) g
    (let ( (game-day-file (format nil "data/cumulative/games/~2,'0d-~a-~a.game.~a.lisp"
                week-no away-id home-id field)) )
;     (format t "get-data-for-game: game-day-file = ~a~%" game-day-file)
      (let ( (file (probe-file game-day-file)) )
        (if file
          (let ( (result (read-value-from-file file)) )
;           (format t "get-data-for-game: result = ~a~%" result)
            result)
          nil)))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; The game weeks
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar game-week-array (make-array 18 :initial-element nil))

(defun schedule-week (week-no) (aref game-week-array week-no))
(defun sort-week (week-no)
  (setf (aref game-week-array week-no)
        (sort (aref game-week-array week-no)
              (lambda (x y) (game-earlier x y)))))
(defun push-game (game week-no)
  (setf (aref game-week-array week-no) (push game (aref game-week-array week-no))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Support loading teams into db from static data
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun make-team (id home name colors conf div)
  (let ( (inst (make-instance 'team-data :team-id id :home home :name name :conf conf :div div :colors colors)) )
    (setf (gethash id team-map) inst)
    inst))

(defun find-team-colors (id clist)
  (if clist
    (let ( (next (car clist)) )
      (if (eq id (car next))
        (cdr next)
        (find-team-colors id (cdr clist))))
    ;; If no colors found, default to black and white
    (list #(0 0 0) #(255 255 255))))

(defun load-teams-with-colors (tarray clist)
  (let ( (id      (aref tarray 0))
         (from    (aref tarray 1))
         (name    (aref tarray 2))
         (colors  (find-team-colors (aref tarray 0) clist))
         (conf    (aref tarray 3))
         (div     (aref tarray 4)) )
    (make-team id from name colors conf div)))

(defun add-game-to-week (game week-no)
  (push-game game week-no)
  (sort-week week-no))

(defun game-from-record (week-no record)
  (let ( (teams (aref record 0))
         (date  (aref record 1))
         (time  (aref record 2))
         (airer (aref record 3)) )
    (let ( (data (make-instance 'game-data :week week-no :away-team (car teams) :home-team (cdr teams))) )
      (let ( (saved-date  (get-data-for-game data 'date))
             (saved-time  (get-data-for-game data 'time))
             (saved-away  (get-data-for-game data 'away-score))
             (saved-home  (get-data-for-game data 'home-score))
             (saved-airer (get-data-for-game data 'airer)) )
        (if date (setf (game-day data)
                       (make-instance 'game-date-data :year (aref date 0) :month (aref date 1) :day (aref date 2))))
        (if saved-date (setf game-day data) saved-date)
        (if time (setf (game-time data)
                       (make-instance 'game-time-data :hour (aref time 0) :minute (aref time 1))))
        (if saved-time (setf game-day time) saved-time)
        (if airer (setf (game-airer data)
                        (if (symbolp airer) (list airer) airer)))
        (if saved-airer (setf game-airer time) saved-airer)
        (setf (game-score data) (make-instance 'game-score :home saved-home :away saved-away))
        data))))

(defun rec-add-games-to-week (week-no games)
  (if games
    (let ( (record (car games)) )
      (add-game-to-week (game-from-record week-no record) week-no)
      (rec-add-games-to-week week-no (cdr games)))))

(defun proc-sched (week-no sched)
  (if (< week-no 18)
    (let ( (week (aref sched week-no)) )
      (rec-add-games-to-week week-no week)
      (proc-sched (+ 1 week-no) sched))))

(defun find-teams-game-in-week (todo team)
  (if (null todo)
    nil
      (with-slots (away-id home-id) (car todo)
        (if (or (eq team away-id) (eq team home-id))
          (car todo)
          (find-teams-game-in-week (cdr todo) team)))))

(defun accum-sched (team week-no sched sofar)
  (if (< week-no 18)
    (accum-sched team (+ 1 week-no) sched (cons (find-teams-game-in-week (aref sched week-no) team) sofar))
    (reverse sofar)))

(defun team-schedule (team)
  (accum-sched team 0 game-week-array nil))

(defun filter-teams (todo sofar &key conf div)
  (if todo
    (with-slots ((c conf) (d div)) (car todo)
      (if (and (or (not conf) (eq c conf))
               (or (not div) (eq d div)))
        (filter-teams (cdr todo) (cons (car todo) sofar) :conf conf :div div)
        (filter-teams (cdr todo) sofar :conf conf :div div)))
    sofar))

(defun division-teams (conf div)
  (let ( (l nil) )
    (maphash (lambda (a b) (setf l (cons b l))) team-map)
    (filter-teams l '() :conf conf :div div)))

(defun conference-teams (conf)
  (let ( (l nil) )
    (maphash (lambda (a b) (setf l (cons b l))) team-map)
    (filter-teams l '() :conf conf)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Define a single point to load all the data
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun load-data (schedule teams colors)
  ;; Start with the static data

  ;; The teams firstdefun
  (mapcar (lambda (x) (load-teams-with-colors x colors)) teams)

  ;; The the base schedule as is it known to us before the start of the season
  (proc-sched 0 schedule)
)

