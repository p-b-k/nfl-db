;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "clim-examples")

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

(defconstant +main-bg-color+ (make-rgb-color  (/ 155 255) (/ 155 255) (/ 155 255)))
(defconstant +bye-bg-color+ (make-rgb-color  (/ 150 255) (/ 150 255) (/ 150 255)))
(defconstant +light-bg-color+ (make-rgb-color  (/ 175 255) (/ 175 255) (/ 175 255)))
(defconstant +dark-bg-color+ (make-rgb-color  (/ 140 255) (/ 140 255) (/ 140 255)))

(defconstant +game-list-item-top-border-size+ 4)
(defconstant +game-list-item-bottom-border-size+ 4)
(defconstant +game-list-item-inner-border-size+ 4)

(defconstant +game-day-info-x-offset+ 300)
(defconstant +game-airer-info-x-offset+ 800)

(defconstant nfc_color (make-rgb-color 0.0 (/ 59 256) (/ 37 102)))
(defconstant afc_color (make-rgb-color (/ 206 256) (/ 19 256) (/ 102 256)))

(defun team-logo-file (team size)
  (format nil "~a/teams/logos/~ax~a/~a.png" +file-root/logos+ size size (symbol-name team)))

(defun get-team-color-main (team)
  (let ( (color (car (team-colors team))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun get-team-color-highlight (team)
  (let ( (color (car (cdr (team-colors team)))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun make-team-icon-pane-xsmall (team) (make-team-icon-pane team +icon-xsmall+))
(defun make-team-icon-pane-small  (team) (make-team-icon-pane team +icon-small+))
(defun make-team-icon-pane-large  (team) (make-team-icon-pane team +icon-large+))
(defun make-team-icon-pane-xlarge (team) (make-team-icon-pane team +icon-xlarge+))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Create basic pane classes
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; %% TEAM ICON PANE ---------------------------------------------------------------------------------------------------

(defclass team-icon-pane (clim-stream-pane)
  ( (team :initarg :team)
    (size :initarg :size)
    (logo :initarg :logo)
    (bg   :initarg :bg)
    (hl   :initarg :hl) )
)

(defun make-team-icon-pane (team size &optional invert)
  (let ( (main-color    (if invert (get-team-color-highlight team) (get-team-color-main team)))
         (second-color  (if invert (get-team-color-main team) (get-team-color-highlight team))) )
    (make-pane 'team-icon-pane :size size
                               :team team
                               :bg main-color
                               :hl second-color
                               :background main-color
                               :max-width size
                               :min-width size
                               :max-height size
                               :min-height size)))

(defmethod handle-repaint ((pane team-icon-pane) region)
  (with-slots (team size hl) pane
    (let ( (image (make-pattern-from-bitmap-file (team-logo-file team size))) )
      (clim:updating-output (pane)
        (let ( (near-off (+ +border-adjust+ +border-off+))
               (far-off (- size (- (* 2 +border-off+) +border-adjust+))) )
          (draw-rectangle* pane +border-off+ +border-off+ far-off far-off
                                :line-joint-shape :bevel
                                :filled nil :line-thickness 4 :ink hl)
          (draw-image* pane image 0 0))))))

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
        (draw-text* pane (team-name team) (floor w 2) (floor h 2) :text-size 80 :align-y :center :align-x :center)))))

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
      (let ( (text (format nil "~d/~d/~d"
                           (nfl-db::game-date-month day) (nfl-db::game-date-day day) (nfl-db::game-date-year day))) )
        (draw-text* pane text +game-day-info-x-offset+ (/ (* 3 h) 4))))))

(defun draw-game-time-info (pane w h game)
  (let ( (tm (game-time game)) )
    (if tm
      (let* ( (hr (nfl-db::game-time-hour tm))
              (mn (nfl-db::game-time-minute tm))
              (text (format nil "~d:~2,'0d ~a" (if (< hr 12) hr (- hr 12)) mn (if (< hr 12) 'AM 'PM))) )
        (draw-text* pane text +game-day-info-x-offset+ (/ h 3))))))

(defun draw-game-airer-info (pane w h game)
  (let ( (airer (nfl-db::game-airer game)) )
    (if (cdr airer)
      (let ( (primary (car airer))
             (secondary (car (cdr airer))) )
        (draw-text* pane (symbol-name primary) (- +game-airer-info-x-offset+ 100) (floor h 2)
                         :align-y :center
                         :align-x :left)
        (draw-text* pane (symbol-name secondary) +game-airer-info-x-offset+ (floor h 2)
                         :align-y :center
                         :align-x :left))
      (draw-text* pane (symbol-name (car airer)) +game-airer-info-x-offset+ (floor h 2)
                       :align-y :center :align-x :left))))

(defmethod handle-repaint ((pane game-pane) region)
  (with-slots (game) pane
    (let ( (w (bounding-rectangle-width  pane))
           (h (bounding-rectangle-height pane)) )
      (if game
        (with-slots ( (week nfl-db::week-no) (away nfl-db::away-id) (home nfl-db::home-id) ) game
          (let ( (home-icon-file (team-logo-file home +icon-small+))
                 (away-icon-file (team-logo-file away +icon-small+)) )
            (let ( (home-icon (make-pattern-from-bitmap-file home-icon-file))
                   (away-icon (make-pattern-from-bitmap-file away-icon-file)) )
              (clim:updating-output (pane)
                (draw-line* pane 0 0 w 0 :ink +light-bg-color+)
                (draw-line* pane 0 (- h 2) w h :ink +dark-bg-color+)
;               (draw-game-score-info pane w h game)
                (draw-game-day-info pane w h game)
                (draw-game-time-info pane w h game)
                (draw-game-airer-info pane w h game)
                (draw-image* pane away-icon 0 +game-list-item-top-border-size+)
                (draw-image* pane home-icon (+ +icon-small+ +game-list-item-inner-border-size+)
                                               +game-list-item-top-border-size+)))))
        (draw-text* pane "BYE" (floor w 2) (floor h 2) :text-size 20 :align-y :center :align-x :center)))))


;; %% SCHEDULE (LIST) PANE ---------------------------------------------------------------------------------------------

(defun make-team-schedule-pane (team)
; (make-pane :list-pane
;            :mode :exclusive
;            :items (team-schedule team)
;            :value-changed-callback
;              (lambda (pane item)
;                (if item
;                  (format t "item changed: Game ~a @ ~a~%" (nfl-db::away-team item) (nfl-db::home-team item))
;                  (format t "item changed: BYE ~%"))))
  (let ( (games (team-schedule team)) )
    (make-pane :vbox-pane
               :contents (mapcar #'make-game-pane games))))

;; %% TOP LEVEL APPLICATION FRAMES -------------------------------------------------------------------------------------

(define-application-frame team-info ()
  ( (team  :initarg :team :initform :phi) )
  (:panes (team-icon    (with-slots (team) *application-frame* (make-team-icon-pane-large team)))
          (team-banner  (with-slots (team) *application-frame* (make-team-banner-pane team t)))
          (team-sched   (with-slots (team) *application-frame* (make-team-schedule-pane team))))
  (:layouts (default (vertically () (horizontally () team-icon team-banner) team-sched)))
  (:menu-bar nil))

(define-application-frame game-info ()
  ( (game :initarg :game) )
  (:pane (with-slots (game) *application-frame* (make-game-pane game)))
  (:menu-bar nil))

(defun run-team (team) (run-frame-top-level (make-application-frame 'team-info :team team)))
(defun run-game (game) (run-frame-top-level (make-application-frame 'game-info :game game)))

