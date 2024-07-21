;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game panel and sub-panels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-pane (:use #:nfl-db
                                  #:nfl-constants
                                  #:clim
                                  #:clim-lisp
                                  #:clim-render))
(in-package #:nfl-game-pane)

(define-application-frame game-score (clim-stream-pane)
  ( (game :initarg :game) )
  (:panes (home-icon    (with-slots (game) *appliction-frame*
                                    (make-team-icon (home-team game))))
          (away-icon    (with-slots (game) *appliction-frame*
                                    (make-team-icon (away-team game))))
          (home-score   (with-slots (game) *appliction-frame*
                                    (make-score-pane game :home)))
          (away-score   (with-slots (game) *appliction-frame*
                                    (make-score-pane game :away)))
          (date-time    (with-slots (game) *appliction-frame*
                                    (make-game-day-pane game)))
          (status       (with-slots (game) *appliction-frame*
                                    (make-status-pane game)))
          (airers       (with-slots (game) *appliction-frame*
                                    (make-airers-pane game)))))



