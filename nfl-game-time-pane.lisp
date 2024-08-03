;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pane to display the date and time of the game on the schedule list's game-pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-time-pane (:use #:nfl-db
                                  #:nfl-constants
                                  #:clim
                                  #:clim-lisp
                                  #:clim-render))
(in-package #:nfl-game-time-pane)

(export 'game-time-pane)
(export 'make-game-time-pane)

;; %% GAME TIME ICON PANE ----------------------------------------------------------------------------------------------

(defclass game-time-pane (clim-stream-pane)
  ( (date   :initarg     :date
            :initform     nil) )
)

(defmethod handle-repaint ((pane game-time-pane) region)
  (let ( (w (bounding-rectangle-width  pane))
         (h (bounding-rectangle-height pane)) )
    (with-slots (date) pane
      (if date
        (multiple-value-bind (secs mins hours day month year day-of-week dst tz)
                             (decode-universal-time date)
          (let ( (day-text  (aref +days-of-week+ day-of-week))
                 (date-text (format nil "~a ~a" (aref +months+ (- month 1)) day))
                 (time-text (format nil "~2d:~2,'0d" hours mins))
                 (row (floor h 2))
                 (col (floor w 2)) )
;           (format t "handle-repaint (game-time-pane): text = ~s~%" date-text)
            (draw-text* pane date-text col row
                                       :text-size 14
;                                      :ink (make-rgb-color 0.3 0.9 0.8)
                                       :x-align :right
                                       :y-align :bottom)))
        (draw-text* pane "TBD" (floor 2 w) (floor 2 h)
                               :x-align :center
                               :y-align :center)))))

(defun make-game-time-pane (game)
  (let ( (total-height (+ +icon-small+ +game-list-item-top-border-size+ +game-list-item-bottom-border-size+)) )
    (make-pane 'game-time-pane :date (game-dday game)
;                              :background (make-rgb-color 0.9 0.9 0.2)
                               :min-width 200
                               :max-height +icon-small+
                               :min-height +icon-small+)))

