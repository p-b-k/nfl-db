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

(defconstant +border-off+ 4)
(defconstant +border-width+ 4)
(defconstant +border-thick+ 4)
(defconstant +border-adjust+ (/ +border-thick+ 2))

(defconstant nfc_color (make-rgb-color 0.0 (/ 59 256) (/ 37 102)))
(defconstant afc_color (make-rgb-color (/ 206 256) (/ 19 256) (/ 102 256)))

(defun team-logo-file (team size)
  (format nil "~a/teams/~ax~a/~a.png" +file-root/logos+ size size (symbol-name team)))

(defvar icon-size +icon-xlarge+)

(defun get-team-color-main (team)
  (let ( (color (car (team-colors team))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun get-team-color-highlight (team)
  (let ( (color (car (cdr (team-colors team)))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun make-team-icon-pane-xsmall (team) (make-team-icon-pane team +icon-xsmall+))
(defun make-team-icon-pane-small  (team) (make-team-icon-pane team +icon-small+))
(defun make-team-icon-pane-large  (team) (make-team-icon-pane team +icon-large+))
(defun make-team-icon-pane-xlarge (team) (make-team-icon-pane team +icon-xlarge+))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Create basic pane classes
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; %% TEAM ICON PANE ---------------------------------------------------------------------------------------------------

(defclass team-icon-pane (clim-stream-pane)
  ( (team :initarg :team)
    (size :initarg :size)
    (logo :initarg :logo)
    (bg   :initarg :bg)
    (hl   :initarg :nl) )
)

(defun make-team-icon-pane (team size)
  (make-pane 'team-icon-pane :size size
                             :team team
                             :background (get-team-color-main team)
                             :max-width size
                             :min-width size
                             :max-height size
                             :min-height size))

(defmethod handle-repaint ((pane team-icon-pane) region)
  (with-slots (team size) pane
    (let ( (image (make-pattern-from-bitmap-file (team-logo-file team size))) )
      (clim:updating-output (pane)
        (let ( (near-off (+ +border-adjust+ +border-off+))
               (far-off (- size (- (* 2 +border-off+) +border-adjust+))) )
          (draw-rectangle* pane +border-off+ +border-off+ far-off far-off
                                :line-joint-shape :bevel
                                :filled nil :line-thickness 4 :ink (get-team-color-highlight team))
          (draw-image* pane image 0 0))))))

;; %% TEAM BANNER PANE -------------------------------------------------------------------------------------------------

(defclass team-banner-pane (clim-stream-pane)
  ( (team :initarg :team) ))

(defun make-team-banner-pane (team)
  (make-pane 'team-banner-pane :min-height +icon-xlarge+
                               :max-height +icon-xlarge+
                               :min-width  (* 4 +icon-xlarge+)
                               :foreground (get-team-color-highlight team)
                               :background (get-team-color-main team)))

; (defun display-team-icon (frame stream)
;   (with-slots (team) *application-frame*
;     (let ( (image (make-pattern-from-bitmap-file (team-logo-file team icon-size)))
;            (bg    (car (team-colors team))) )
;       (clim:updating-output (stream)
;         (draw-image* stream image 0 0)))))

; (defun display-eagles-icon (frame stream)
;   (clim:updating-output (stream)
;     (let ( (image (make-pattern-from-bitmap-file (team-logo-file :phi icon-size))) )
;       (draw-image* stream image 0 0))))

(define-application-frame team-info ()
  ( (team  :initarg :team :initform :phi) )
  (:panes (team-icon    (with-slots (team) *application-frame* (make-team-icon-pane-xlarge team)))
          (team-banner  (with-slots (team) *application-frame* (make-team-banner-pane team))))
  (:layouts (default (horizontally () team-icon team-banner)))
  (:menu-bar nil))

(define-application-frame game-info ()
  ( (home  :initarg :home :initform :phi) (away  :initarg :away :initform :dal) )
  (:panes (away-icon (with-slots (away) *application-frame* (make-team-icon-pane-large away)))
          (home-icon (with-slots (home) *application-frame* (make-team-icon-pane-large home))))
  (:layouts (default (horizontally () away-icon home-icon)))
  (:menu-bar nil))

(defun run-team (team) (run-frame-top-level (make-application-frame 'team-info :team team)))
(defun run-game (away home) (run-frame-top-level (make-application-frame 'game-info :home home :away away)))

