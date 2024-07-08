;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "clim-examples")

(defpackage #:nfl-tool (:use #:nfl-db #:clim #:clim-lisp #:clim-render))
(in-package #:nfl-tool)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Some helper functions
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defconstant +file-root/logos+ "data/static/png")
(defconstant +icon-xsmall+ 32)
(defconstant +icon-small+  64)
(defconstant +icon-large+  128)
(defconstant +icon-xlarge+ 256)
(defconstant nfc_color (make-rgb-color 0.0 (/ 59 256) (/ 37 102)))
(defconstant afc_color (make-rgb-color (/ 206 256) (/ 19 256) (/ 102 256)))

(defun team-logo-file (team size)
  (format nil "~a/teams/~ax~a/~a.png" +file-root/logos+ size size (symbol-name team)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Define a function to create a pane class from a game
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar icon-size +icon-xlarge+)

(defclass team-icon-pane (clim-stream-pane) ())

(defun display-team-icon (frame stream)
  (with-slots (team) *application-frame*
    (let ( (image (make-pattern-from-bitmap-file (team-logo-file team icon-size)))
           (bg    (car (team-colors team))) )
      (clim:updating-output (stream)
        (draw-image* stream image 0 0)))))

(defun display-eagles-icon (frame stream)
  (clim:updating-output (stream)
    (let ( (image (make-pattern-from-bitmap-file (team-logo-file :phi icon-size))) )
      (draw-image* stream image 0 0))))

(define-application-frame team-info ()
  ( (team  :initarg :team :initform :phi) )
  (:panes (team-icon :application
;                      :background (make-rgb-color 0.0 (/ 72 256) (/ 81 256))
;                      :background (make-rgb-color (/ (aref (team-colors team) 0) 256)
;                                                  (/ (aref (team-colors team) 1) 256)
;                                                  (/ (aref (team-colors team) 2) 256))
                       :background (with-slots (team) *application-frame*
                                     (let ( (color1 (car (team-colors team)))
                                            (color2 (car (cdr (team-colors team)))) )
                                        (make-rgb-color (/ (aref color1 0) 256)
                                                        (/ (aref color1 1) 256)
                                                        (/ (aref color1 2) 256))))
                       :min-width icon-size
                       :max-width icon-size
                       :min-height icon-size
                       :max-height icon-size
;                      :display-function 'display-eagles-icon
                       :display-function 'display-team-icon
                       :scroll-bars nil
          ))
  (:layouts (default team-icon))
  (:menu-bar nil))

(defun run (team) (run-frame-top-level (make-application-frame 'team-info :team team)))

