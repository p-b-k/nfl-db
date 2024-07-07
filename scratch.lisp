;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scratch file for NFL stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require "adsf")
(ql:quickload "clim-examples")

(defpackage :app (:use :clim :clim-lisp) (:export run-app))

(in-package :app)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Main application frame, with an application window and an interactor window
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-application-frame superapp ()
  ()
  (:pointer-documentation t)
  (:panes
    ;; Let's add an additional pane
    (app :application
          ;; :DISPLAY-TIME specifies when this pane should be displayed
          ;; in the command loop. Note that the refresh is
          ;; pane-specific, not application-wide.
          :display-time nil
          :height 400
          :width 600)
    (int  :interactor
          :height 200
          :width 600))
  (:layouts
    ;; This time we explicitly specify that the 2 defined panes
    ;; should be stacked vertically.
    (default
      (vertically () app int))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Let's also define commands that will act on the application.
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; How to leave the application.
;; Note the '-superapp-' part of the command definition, coming from
;; the name of the application frame.
(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;; This is an additional command that will be used in the next
;; example, so its content is not important. However, it is useful to
;; describe some aspects of the command loop. See below.
(define-superapp-command (com-parity :name t) ((number 'integer))
  (format t "~a is ~a~%" number (if (oddp number) "odd" "even")))

(defun run-app () (run-frame-top-level (make-application-frame 'superapp)))

(load "nfl-db.lisp")
(load "nfl-static-data.lisp")

(defun load-data () (nfl-db::load-data nfl-static-data:schedule nfl-static-data:league nfl-static-data:colors))
(export 'load-data)

