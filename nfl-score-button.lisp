;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-score-button (:use #:nfl-db
                                     #:nfl-constants
                                     #:clim
                                     #:clim-lisp
                                     #:clim-render))
(in-package #:nfl-score-button)

(defclass score-button (push-button-pane)
  ( (game  :initarg :game)  ;; The game data
    (side  :initarg :side)  ;; Which "Side" (:home or :away)
    (align :initarg :align) ;; Alight to the left or the right (for formatting)
    (north :initarg :north :initform 0)
    (east  :initarg :east  :initform 0)
    (south :initarg :south :initform 0)
    (west  :initarg :west  :initform 0) ))

(defmethod handle-repaint ((button score-button) region)
  (let ( (w (bounding-rectangle-width  pane))
         (h (bounding-rectangle-height pane)) )
    (with-slots (game side align north east south west) button
      (let ( (score (if (eq side :home) (car game-score) (cdr game-score))) )
        (let ( (far-off-x (- (+ size east west) 7))
               (far-off-y (- (+ size north south) 7)) )
          (draw-text* button east (floor h 2) :x-align :center :y-align :center))))))

(defun on-score-button-click (team-button)
  (with-slots (team) team-button
    (run-frame-top-level (make-application-frame 'team-info :team team))))

(defun make-score-button (side game)
  (let ( (total-height (+ +icon-small+ +game-list-item-top-border-size+ +game-list-item-bottom-border-size+)) )
    (make-pane 'score-button :game game :side side)))

