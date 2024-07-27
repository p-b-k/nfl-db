;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-frame (:use #:nfl-db
                                  #:nfl-constants
                                  #:nfl-team-pane
                                  #:nfl-score-button
                                  #:clim
                                  #:clim-lisp
                                  #:clim-render))
(in-package #:nfl-game-frame)

(export 'game-frame)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-application-frame game-frame (clim-stream-pane)
  ( (game :initarg :game) )
  (:panes (home-icon    (with-slots (game) *appliction-frame*
                                    (make-team-icon (home-team game))))
          (away-icon    (with-slots (game) *appliction-frame*
                                    (make-team-icon (away-team game))))
          (home-score   (with-slots (game) *appliction-frame*
                                    (make-score-button game :home)))
          (away-score   (with-slots (game) *appliction-frame*
                                    (make-score-button game :away)))
          (date-time    (with-slots (game) *appliction-frame*
                                    (make-game-time-pane game)))
          (status       (with-slots (game) *appliction-frame*
                                    (make-status-pane game)))
          (airers       (with-slots (game) *appliction-frame*
                                    (make-airers-pane game))))
  (:layouts (default (horizontally () away-icon away-score home-score home-icon)))
  (:menu-bar nil))

