;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pane to display the broadcaster(s) of the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-game-airer-pane (:use #:nfl-db
                                        #:nfl-constants
                                        #:clim
                                        #:clim-lisp
                                        #:clim-render))
(in-package #:nfl-game-airer-pane)

(export 'game-airer-pane)
(export 'make-airer-pane)

(defclass game-airer-pane (clim-stream-pane)
  ( (airer     :initarg    :airer    :initform nil) ))

(defun make-airer-pane (game)
  (make-pane 'game-airer-pane :airer (game-airer game)))

