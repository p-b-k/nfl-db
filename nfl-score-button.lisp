;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-score-button (:use #:nfl-db
                                     #:nfl-constants
                                     #:clim
                                     #:clim-lisp
                                     #:clim-render))
(in-package #:nfl-score-button)

(export 'make-score-pane)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Define a button for each score, home or away
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass score-button (push-button-pane)
  ( (game  :initarg :game)  ;; The game data
    (side  :initarg :side)  ;; Which "Side" (:home or :away)
    (align :initarg :align) ;; Alight to the left or the right (for formatting)
    (size  :initarg :size)
    (north :initarg :north :initform 0)
    (east  :initarg :east  :initform 0)
    (south :initarg :south :initform 0)
    (west  :initarg :west  :initform 0) ))

(defmethod handle-repaint ((button score-button) region)
  (let ( (w (bounding-rectangle-width  button))
         (h (bounding-rectangle-height button)) )
    (with-slots (game side align size north east south west) button
      (let ( (score (if (eq side :home) (score-home (game-score game)) (score-away (game-score game)))) )
        (let ( (far-off-x (- (+ size east west) 7))
               (far-off-y (- (+ size north south) 7))
               (score-text (format nil "~d" (apply #'+ score))) )
          (draw-text* button score-text east (floor h 2) :align-x :left :align-y :center))))))

(defun on-score-button-click (team-button)
  (with-slots (team) team-button
    (run-frame-top-level (make-application-frame 'team-info :team team))))

(defun make-score-button (side game)
  (let ( (total-height (+ +icon-small+ +game-list-item-top-border-size+ +game-list-item-bottom-border-size+)) )
    (make-pane 'score-button :game game :side side
                             :background (make-rgb-color 0.3 0.84 0.23)
                             :size +icon-small+)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Define a button for each score, home or away
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass score-pane (hbox-pane)
  ( (game         :initarg  :game)  ))

;; Defines a panel class for our hello world gadget
(defclass no-score-pane (clim-stream-pane) ())

(defmethod handle-repaint ((pane no-score-pane) region)
  (let ( (w (bounding-rectangle-width  pane))
         (h (bounding-rectangle-height pane)) )
    ;; Draw a greeting in the center
    (draw-text* pane "To Be Played" (floor w 2) (floor h 2) :align-x :center :align-y :center)))

(defun make-score-pane (game)
  (with-slots (score) game
    (if score
      (let ( (home-score (make-score-button :home game))
             (away-score (make-score-button :home game)) )
        (make-pane :hbox-pane
                   :min-height +icon-small+
                   :contents (list away-score home-score)))
      (make-pane 'no-score-pane
                 :min-height +icon-small+
                 :max-height +icon-small+
                 :background (make-rgb-color 0.8 0.7 0.8)))))
