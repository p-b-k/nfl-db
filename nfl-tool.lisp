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
    (hl   :initarg :hl) )
)

(defun make-team-icon-pane (team size &optional invert)
  (let ( (main-color    (if invert (get-team-color-highlight team) (get-team-color-main team)))
         (second-color  (if invert (get-team-color-main team) (get-team-color-highlight team))) )
    (make-pane 'team-icon-pane :size size
                               :team team
                               :bg main-color
                               :hl second-color
                               :background main-color
                               :max-width size
                               :min-width size
                               :max-height size
                               :min-height size)))

(defmethod handle-repaint ((pane team-icon-pane) region)
  (with-slots (team size hl) pane
    (let ( (image (make-pattern-from-bitmap-file (team-logo-file team size))) )
      (clim:updating-output (pane)
        (let ( (near-off (+ +border-adjust+ +border-off+))
               (far-off (- size (- (* 2 +border-off+) +border-adjust+))) )
          (draw-rectangle* pane +border-off+ +border-off+ far-off far-off
                                :line-joint-shape :bevel
                                :filled nil :line-thickness 4 :ink hl)
          (draw-image* pane image 0 0))))))

;; %% TEAM BANNER PANE -------------------------------------------------------------------------------------------------

(defclass team-banner-pane (clim-stream-pane)
  ( (team :initarg :team) (hl :initarg :hl) (bg :initarg :bg) ))

(defun make-team-banner-pane (team &optional invert)
  (let ( (main-color    (if invert (get-team-color-highlight team) (get-team-color-main team)))
         (second-color  (if invert (get-team-color-main team) (get-team-color-highlight team))) )
    (make-pane 'team-banner-pane :team team
                                 :min-height +icon-xlarge+
                                 :max-height +icon-xlarge+
                                 :min-width  (* 4 +icon-xlarge+)
                                 :foreground second-color
                                 :background main-color)))

(defmethod handle-repaint ((pane team-banner-pane) region)
  (with-slots (team) pane
    (let ( (w (bounding-rectangle-width  pane))
           (h (bounding-rectangle-height pane)) )
      (clim:updating-output (pane)
        (draw-text* pane (team-home team) 10 30 :text-size 20)
        (draw-text* pane (team-name team) (floor w 2) (floor h 2) :text-size 120 :align-y :center :align-x :center)))))

;; %% SCHEDULE (LIST) PANE ---------------------------------------------------------------------------------------------

(defun make-team-schedule-pane (team)
   (make-pane :list-pane
              :mode :exclusive
              :items (team-schedule team)
              :value-changed-callback (lambda (pane item) (format t "item changed: ~a~%" item))
;             :value-changed-callback
;               (lambda (pane item)
;                 (declare (ignore pane))
;                 (let ((output (get-frame-pane *application-frame* 'output))
;                    (description (get-frame-pane *application-frame* 'description)))
;                (window-clear output)
;                (window-clear description)
;                (with-text-style (description (make-text-style :sans-serif :roman :normal))
;                  (write-string (misc-test-description item) description)
;                  (finish-output description))
;                (with-application-frame (frame)
;                  (setf (display-fn frame) (misc-test-drawer item))
;                  (redisplay-frame-pane frame output :force-p t))))))
   ))

;; %% TOP LEVEL APPLICATION FRAMES -------------------------------------------------------------------------------------

(define-application-frame team-info ()
  ( (team  :initarg :team :initform :phi) )
  (:panes (team-icon    (with-slots (team) *application-frame* (make-team-icon-pane-xlarge team)))
          (team-banner  (with-slots (team) *application-frame* (make-team-banner-pane team t)))
          (team-sched   (with-slots (team) *application-frame* (make-team-schedule-pane team))))
  (:layouts (default (vertically () (horizontally () team-icon team-banner) team-sched)))
  (:menu-bar nil))

(define-application-frame game-info ()
  ( (home  :initarg :home :initform :phi) (away  :initarg :away :initform :dal) )
  (:panes (away-icon (with-slots (away) *application-frame* (make-team-icon-pane-large away)))
          (home-icon (with-slots (home) *application-frame* (make-team-icon-pane-large home))))
  (:layouts (default (horizontally () away-icon home-icon)))
  (:menu-bar nil))

(defun run-team (team) (run-frame-top-level (make-application-frame 'team-info :team team)))
(defun run-game (away home) (run-frame-top-level (make-application-frame 'game-info :home home :away away)))

