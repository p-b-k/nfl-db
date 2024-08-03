;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pane to display the status of a game (i.e. pending, over-due, complete), based on various factors as yet to be
;; determined. The status logic will of course live in the nfl-db package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-time-pane (:use #:nfl-db
                                       #:nfl-constants
                                       #:clim
                                       #:clim-lisp
                                       #:clim-render))
(in-package #:nfl-game-time-pane)

(export 'game-status-pane)
(export 'make-status-pane)

(defclass game-status-pane (clim-stream-pane)
  ( (game     :initarg    :game) ))

(defun make-status-pane (game)
  (let ( (total-height (+ +icon-small+ +game-list-item-top-border-size+ +game-list-item-bottom-border-size+)) )
    (make-pane 'game-status-pane :game game)))

