;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "clim-examples")
(ql:quickload "local-time")

(defpackage #:nfl-tool (:use #:nfl-db #:clim #:clim-lisp #:clim-render))
(in-package #:nfl-tool)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Some helper functions
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defconstant +file-root/logos+ "data/static/png")

(defconstant +icon-xsmall+ 32)
(defconstant +icon-small+  64)
(defconstant +icon-large+  128)
(defconstant +icon-xlarge+ 256)

(defconstant +border-off+ 4)
(defconstant +border-width+ 4)
(defconstant +border-thick+ 4)
(defconstant +border-adjust+ (/ +border-thick+ 2))

; (defconstant +main-bg-color+ (make-rgb-color (/ 155 255) (/ 155 255) (/ 155 255)))
; (defconstant +bye-bg-color+ (make-rgb-color  (/ 150 255) (/ 150 255) (/ 150 255)))
(defconstant +main-bg-color+ (make-rgb-color 0.9 0.9 0.83))
(defconstant +bye-bg-color+ (make-rgb-color  0.83 0.83 0.78))

(defconstant +light-bg-color+ (make-rgb-color  (/ 175 255) (/ 175 255) (/ 175 255)))
(defconstant +dark-bg-color+ (make-rgb-color  (/ 140 255) (/ 140 255) (/ 140 255)))

(defconstant +game-list-item-top-border-size+ 4)
(defconstant +game-list-item-bottom-border-size+ 4)
(defconstant +game-list-item-west-border-size+ 2)
(defconstant +game-list-item-inner-border-size+ 2)
(defconstant +game-list-item-date-padding+ 4)

(defconstant +game-day-info-x-offset+ (+ (* +icon-small+ 2)
                                         +game-list-item-west-border-size+
                                         +game-list-item-inner-border-size+
                                         +game-list-item-date-padding+))
(defconstant +game-airer-info-x-offset+ 150)

(defconstant nfc_color (make-rgb-color 0.0 (/ 59 256) (/ 37 102)))
(defconstant afc_color (make-rgb-color (/ 206 256) (/ 19 256) (/ 102 256)))

(defconstant +days-of-week+ #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defconstant +months+ #("January" "February" "March" "April" "May" "June"
                        "July" "August" "September" "October" "November" "December"))

(defun team-logo-file (team size)
  (format nil "~a/teams/logos/~ax~a/~a.png" +file-root/logos+ size size (symbol-name team)))

(defun get-team-color-main (team)
  (let ( (color (car (team-colors team))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun get-team-color-highlight (team)
  (let ( (color (car (cdr (team-colors team)))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun resolve-airer-png-name (airer)
  (cond
    ( (eq airer :espn+) "ESPN_plus" )
    ( (eq airer :netflix) "Netflix" )
    ( (eq airer :peacock) "Peacock" )
    ( (eq airer :prime) "Prime" )
    ( t (symbol-name airer) )))

(defun airer-logo-file (airer size)
  (format nil "~a/networks/~a/~a.png" +file-root/logos+ size (resolve-airer-png-name airer)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Create Presentation Methods
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-presentation-method present (object (type nfl-db::team) stream view &key)
  (format stream "~a" (team-title object)))

(define-presentation-method present (object (type nfl-db::game) stream view &key)
  (with-slots ( (w nfl-db::week-no) (a nfl-db::away-id) (h nfl-db::home-id) ) object
    (format stream "[~d] ~a @ ~a" w a h)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Create basic pane classes
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; %% TEAM ICON PANE ---------------------------------------------------------------------------------------------------

(defclass team-icon-pane (clim-stream-pane)
  ( (team  :initarg :team)
    (size  :initarg :size)
    (north :initarg :north)
    (south :initarg :south)
    (east  :initarg :east)
    (west  :initarg :west)
    (bg    :initarg :bg)
    (hl    :initarg :hl) )
)

(defun make-team-icon-pane (team size &optional invert &key (north 0) (east 0) (south 0) (west 0))
  (let ( (main-color    (if invert (get-team-color-highlight team) (get-team-color-main team)))
         (second-color  (if invert (get-team-color-main team) (get-team-color-highlight team))) )
    (make-pane 'team-icon-pane :size size
                               :team team
                               :north north
                               :south south
                               :east east
                               :west west
                               :bg main-color
                               :hl second-color
                               :background main-color
                               :max-width (+ east west size)
                               :min-width (+ east west size)
                               :max-height (+ north south size)
                               :min-height size)))

(defmethod handle-repaint ((pane team-icon-pane) region)
  (with-slots (team size hl north south east west) pane
    (let ( (image (make-pattern-from-bitmap-file (team-logo-file team size))) )
      (clim:updating-output (pane)
        (let ( (far-off-x (- (+ size east west) 7))
               (far-off-y (- (+ size north south) 7)) )
          (draw-rectangle* pane 6 6 far-off-x far-off-y
                                :line-joint-shape :bevel
                                :filled nil :line-thickness 4 :ink hl)
          (draw-image* pane image east north))))))

;; %% TEAM BANNER PANE -------------------------------------------------------------------------------------------------

(defclass team-banner-pane (clim-stream-pane)
  ( (team :initarg :team) (hl :initarg :hl) (bg :initarg :bg) ))

(defun make-team-banner-pane (team &optional invert)
  (let ( (main-color    (if invert (get-team-color-highlight team) (get-team-color-main team)))
         (second-color  (if invert (get-team-color-main team) (get-team-color-highlight team))) )
    (make-pane 'team-banner-pane :team team
                                 :min-height +icon-large+
                                 :max-height +icon-large+
                                 :min-width  (* 6 +icon-large+)
                                 :foreground second-color
                                 :background main-color)))

(defmethod handle-repaint ((pane team-banner-pane) region)
  (with-slots (team) pane
    (let ( (w (bounding-rectangle-width  pane))
           (h (bounding-rectangle-height pane)) )
      (clim:updating-output (pane)
        (draw-text* pane (team-home team) 10 30 :text-size 20)
        (draw-text* pane (team-name team) (floor w 2) (floor h 2)
                         :text-size 80
                         :text-face :italic
                         :align-y :center
                         :align-x :center)))))

;; %% GAME PANE --------------------------------------------------------------------------------------------------------

(defclass game-pane (clim-stream-pane)
  ( (game :initarg :game) )
)

(defun make-game-pane (game)
  (let ( (total-height (+ +icon-small+ +game-list-item-top-border-size+ +game-list-item-bottom-border-size+)) )
    (make-pane 'game-pane :game game
                          :background (if game +main-bg-color+ +bye-bg-color+)
                          :min-height (if game total-height (/ +icon-small+ 2))
                          :max-height (if game total-height (/ +icon-small+ 2))
                          :min-width  (* 6 +icon-small+)
    )))

(defun draw-game-score-info (pane w h game)
  (if (game-score game)
    (let* ( (totals (score-totals (game-score game)))
            (text (format nil "~2d ~2d" (car totals) (cdr totals))) )
      (draw-text* pane text (floor w 2) (floor h 2) :align-y :center :x 200))))

(defun draw-game-day-info (pane w h game)
  (let ( (day (game-day game)) )
    (if day
      (multiple-value-bind (secs mins hours day month year day-of-week dst tz)
                           (decode-universal-time
                              (encode-universal-time 0 0 0 (nfl-db::game-date-day day)
                                                           (nfl-db::game-date-month day)
                                                           (nfl-db::game-date-year day)))
        (let ( (text (format nil "~a, ~a ~d ~d"
                             (aref +days-of-week+ day-of-week)
                             (aref +months+ (1- month))
                             day
                             year)) )
          (draw-text* pane text +game-day-info-x-offset+ (/ (* 3 h) 4)))))))

(defun draw-game-time-info (pane w h game)
  (let ( (tm (game-time game)) )
    (if tm
      (let* ( (hr (nfl-db::game-time-hour tm))
              (mn (nfl-db::game-time-minute tm))
              (text (format nil "~d:~2,'0d ~a" (if (< hr 12) hr (- hr 12)) mn (if (< hr 12) 'AM 'PM))) )
        (draw-text* pane text +game-day-info-x-offset+ (/ h 3))))))

(defun draw-game-airer-info (pane w h game)
  (let ( (airer (nfl-db::game-airer game)) )
    (if airer
      (if (cdr airer)
        (let ( (primary (car airer))
               (secondary (car (cdr airer))) )
            (let ( (airer-icon-file-1 (airer-logo-file primary +icon-xsmall+))
                   (airer-icon-file-2 (airer-logo-file secondary +icon-xsmall+)) )
                (let ( (icon-1 (make-pattern-from-bitmap-file airer-icon-file-1))
                       (icon-2 (make-pattern-from-bitmap-file airer-icon-file-2)) )
                (draw-image* pane icon-1 (- w +game-airer-info-x-offset+) (+ +game-list-item-top-border-size+
                                                                             (/ +icon-xsmall+ 2)))
                (draw-image* pane icon-2 (- w (* 2 +game-airer-info-x-offset+)) (+ +game-list-item-top-border-size+
                                                                                         (/ +icon-xsmall+ 2))))))
        (let ( (airer-icon-file (airer-logo-file (car airer) +icon-xsmall+)) )
          (let ( (airer-icon (make-pattern-from-bitmap-file airer-icon-file)) )
            (draw-image* pane airer-icon (- w +game-airer-info-x-offset+) (+ +game-list-item-top-border-size+
                                                                          (/ +icon-xsmall+ 2)))))))))

(defmethod handle-repaint ((pane game-pane) region)
  (with-slots (game) pane
    (let ( (w (bounding-rectangle-width  pane))
           (h (bounding-rectangle-height pane)) )
      (if game
        (with-slots ( (week nfl-db::week-no) (away nfl-db::away-id) (home nfl-db::home-id) ) game
          (let ( (home-icon-file (team-logo-file home +icon-small+))
                 (away-icon-file (team-logo-file away +icon-small+)) )
            (let ( (home-icon (make-pattern-from-bitmap-file home-icon-file))
                   (away-icon (make-pattern-from-bitmap-file away-icon-file))
                   (home-bg (get-team-color-main home))
                   (away-bg (get-team-color-main away)) )
              (clim:updating-output (pane)
;               (draw-rectangle* pane 0 0 (+ +game-list-item-inner-border-size+ (* 2 +icon-small+)) h
;                                :ink (make-rgb-color 1 1 1)) 
                (draw-rectangle* pane +game-list-item-west-border-size+
                                      +game-list-item-bottom-border-size+
                                      (+ +game-list-item-west-border-size+ +icon-small+)
                                      (+ +game-list-item-top-border-size+ +icon-small+)
;                                     :line-joint-shape :bevel
                                      :filled t
                                      :ink away-bg)
                (draw-rectangle* pane (+ +icon-small+
                                         +game-list-item-west-border-size+
                                         +game-list-item-inner-border-size+)
                                      +game-list-item-bottom-border-size+
                                      (+ (* 2 +icon-small+)
                                              +game-list-item-west-border-size+
                                              +game-list-item-inner-border-size+)
                                      (+ +game-list-item-top-border-size+ +icon-small+)
;                                     :line-joint-shape :bevel
                                      :filled t
                                      :ink home-bg)
                (draw-line* pane 0 0 w 0 :ink +light-bg-color+)
                (draw-line* pane 0 (- h 1) w (- h 1) :ink +dark-bg-color+)
;               (draw-game-score-info pane w h game)
                (draw-game-day-info pane w h game)
                (draw-game-time-info pane w h game)
                (draw-game-airer-info pane w h game)
                (draw-image* pane away-icon +game-list-item-west-border-size+ +game-list-item-top-border-size+)
                (draw-image* pane home-icon (+ +icon-small+
                                               +game-list-item-west-border-size+
                                               +game-list-item-inner-border-size+)
                                               +game-list-item-top-border-size+)))))
        (draw-text* pane "BYE" (floor w 2) (floor h 2) :text-size 20 :align-y :center :align-x :center)))))


;; %% SCHEDULE (LIST) PANE ---------------------------------------------------------------------------------------------

(defun make-team-schedule-list-pane (team)
  (make-pane :list-pane
             :mode :exclusive
             :items (team-schedule team)
             :value-changed-callback
               (lambda (pane item)
                 (if item
                   (format t "item changed: Game ~a @ ~a~%" (nfl-db::away-team item) (nfl-db::home-team item))
                   (format t "item changed: BYE ~%")))))

(defun make-team-schedule-pane (team)
  (let ( (games (team-schedule team)) )
    (make-pane :vbox-pane
               :contents (mapcar #'make-game-pane games))))

;; %% LEAGUE (ALL THE TEAMS) PANE --------------------------------------------------------------------------------------

(defconstant +team-icon-border+ 30)

(defun make-division-pane (teams)
  (make-pane :hbox
             :min-width (+ +icon-small+ (* 2 +team-icon-border+))
             :max-width (+ +icon-small+ (* 2 +team-icon-border+))
             :min-height (+ +icon-small+ (* 2 +team-icon-border+))
             :max-height (+ +icon-small+ (* 2 +team-icon-border+))
             :contents (mapcar
                          (lambda (x) (make-team-icon-pane (team-id x) +icon-small+ nil
                                                           :north +team-icon-border+ :east +team-icon-border+
                                                           :west +team-icon-border+ :south +team-icon-border+))
                          teams)))

(defun make-conference-pane (conf)
  (let ( (n (division-teams conf :north))
         (e (division-teams conf :east))
         (s (division-teams conf :south))
         (w (division-teams conf :west)) )
    (make-pane :vbox-pane
               :contents (mapcar #'make-division-pane (list n e s w)))))

;; %% TOP LEVEL APPLICATION FRAMES -------------------------------------------------------------------------------------

(define-application-frame team-info ()
  ( (team  :initarg :team :initform :phi) )
  (:panes (team-icon    (with-slots (team) *application-frame*
                          (make-team-icon-pane team +icon-large+ nil
                                               :west +game-list-item-west-border-size+
                                               :east +game-list-item-inner-border-size+)))
          (team-banner  (with-slots (team) *application-frame* (make-team-banner-pane team nil)))
          (team-sched   (with-slots (team) *application-frame*
                          (scrolling (:scroll-bar :vertical :height 400) (make-team-schedule-pane team)))))
  (:layouts (default (vertically () (horizontally () team-icon team-banner) team-sched)))
  (:menu-bar nil))

(define-application-frame game-info ()
  ( (game :initarg :game) )
  (:pane (with-slots (game) *application-frame* (make-game-pane game)))
  (:menu-bar nil))

(define-application-frame league-info ()
  ( )
  (:panes (nfc (make-conference-pane :nfc))
          (afc (make-conference-pane :afc)))
  (:layouts (default (horizontally () afc nfc)))
  (:menu-bar nil))

(defun run-team (team) (run-frame-top-level (make-application-frame 'team-info :team team)))
(defun run-game (game) (run-frame-top-level (make-application-frame 'game-info :game game)))
(defun run-league () (run-frame-top-level (make-application-frame 'league-info)))

