;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-pane (:use #:nfl-db
                                  #:nfl-constants
                                  #:nfl-team-pane
                                  #:nfl-game-time-pane
                                  #:nfl-score-button
                                  #:nfl-game-airer-pane
                                  #:clim
                                  #:clim-lisp
                                  #:clim-render))
(in-package #:nfl-game-pane)

(export 'game-pane)
(export 'make-game-pane)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defclass game-pane (hbox-pane)
  ( (game         :initarg  :game)
    (home-icon    :initarg :home-icon)
    (away-icon    :initarg :away-icon)
    (home-score   :initarg :home-score)
    (away-score   :initarg :away-score)
    (date-time    :initarg :date-time)
    (status       :initarg :status)
    (airer        :initarg :airer) )
; (:layouts (default (horizontally () away-icon away-score home-score home-icon)))
; (:menu-bar nil)
)

(defun make-game-pane (game)
  (format t "calling make-game-pane on game ~a~%" game)
  (if game
    (let ( (home-icon    (make-team-icon-pane (home-team game) +icon-small+))
           (away-icon    (make-team-icon-pane (away-team game) +icon-small+))
           (home-score   (make-score-button game :home))
           (away-score   (make-score-button game :away))
           (date-time    (make-game-time-pane game))
           (status       (make-status-pane game))
           (airer        (make-airer-pane game)) )
      (make-instance 'game-pane :game         game
                                :home-icon    home-icon
                                :away-icon    away-icon
                                :home-score   home-score
                                :away-score   away-score
                                :date-time    date-time
                                :status       status
                                :airer        airer
                                :contents     (list ;; Start List
                                                    away-icon
;                                                   away-score
;                                                   home-score
                                                    home-icon
;                                                   date-time
;                                                   status
;                                                   airer
                                              )))
     (make-instance 'clim-stream-pane)))
