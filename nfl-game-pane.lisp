;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-pane (:use #:nfl-db
                                  #:nfl-constants
                                  #:nfl-team-pane
                                  #:nfl-score-button
                                  #:clim
                                  #:clim-lisp
                                  #:clim-render))
(in-package #:nfl-game-pane)

(export 'game-pane)
(export 'make-game-pane)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass game-pane (vbox-pane)
  ( (game         :initarg  :game)
    (home-icon    :initarg :home-icon)
    (away-icon    :initarg :away-icon)
    (home-score   :initarg :home-score)
    (away-score   :initarg :away-score)
    (date-time    :initarg :date-time)
    (status       :initarg :status)
    (airers       :initarg :airers) )
; (:layouts (default (horizontally () away-icon away-score home-score home-icon)))
; (:menu-bar nil)
)

(defun make-game-pane (game)
  (let ( (home-icon    (with-slots (make-team-icon (home-team game) +icon-xsmall+)))
         (away-icon    (with-slots (make-team-icon (away-team game) +icon-xsmall+)))
         (home-score   (with-slots (make-score-button game :home)))
         (away-score   (with-slots (make-score-button game :away)))
         (date-time    (with-slots (make-game-time-pane game)))
         (status       (with-slots (make-status-pane game)))
         (airers       (with-slots (make-airers-pane game))) )
    (make-instance 'game-pane :game         game
                              :home-icon    home-icon
                              :away-icon    away-icon
                              :home-score   home-score
                              :away-score   away-score
                              :date-time    date-time
                              :status       status
                              :airers       airers)))
