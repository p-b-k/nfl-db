;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-team-pane (:use #:nfl-db
                                  #:nfl-constants
                                  #:clim
                                  #:clim-lisp
                                  #:clim-render))
(in-package #:nfl-team-pane)

;; %% EXPORTS ----------------------------------------------------------------------------------------------------------

(export 'team-icon-pane)
(export 'make-team-icon-pane)

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
                               :bg second-color
                               :hl main-color
;                              :background main-color
                               :background (make-rgb-color 1 1 1)
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
          (draw-image* pane image (/ (* 3 east) 4) north))))))


