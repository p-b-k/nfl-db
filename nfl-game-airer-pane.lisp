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
  (make-pane 'game-airer-pane :airer (game-airer game)
                              :max-height +icon-small+
                              :min-height +icon-small+))

(defmethod handle-repaint ((pane game-airer-pane) region)
  (let ( (w (bounding-rectangle-width  pane))
         (h (bounding-rectangle-height pane)) )
    (with-slots (airer) pane
      (if airer
        (if (cdr airer)
          (let ( (primary (car airer))
                 (secondary (car (cdr airer))) )
              (let ( (airer-icon-file-1 (airer-logo-file primary +icon-xsmall+))
                     (airer-icon-file-2 (airer-logo-file secondary +icon-xsmall+)) )
                  (let ( (icon-1 (make-pattern-from-bitmap-file airer-icon-file-1))
                         (icon-2 (make-pattern-from-bitmap-file airer-icon-file-2)) )
                  (draw-image* pane icon-1 (- w +game-airer-info-x-offset+) (+ +game-list-item-top-border-size+
                                                                               (/ +icon-xsmall+ 2)))
                  (draw-image* pane icon-2 (- w (* 2 +game-airer-info-x-offset+)) (+ +game-list-item-top-border-size+
                                                                                           (/ +icon-xsmall+ 2))))))
          (let ( (airer-icon-file (airer-logo-file (car airer) +icon-xsmall+)) )
            (let ( (airer-icon (make-pattern-from-bitmap-file airer-icon-file)) )
              (draw-image* pane airer-icon (- w +game-airer-info-x-offset+) (+ +game-list-item-top-border-size+
                                                                            (/ +icon-xsmall+ 2))))))))))

