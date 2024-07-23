;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "clim-examples")
(ql:quickload "local-time")

(defpackage #:nfl-tool (:use #:nfl-db
                             #:nfl-constants
                             #:nfl-team-pane
                             #:clim
                             #:clim-lisp
                             #:clim-render))
(in-package #:nfl-tool)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Create Presentation Methods
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-presentation-method present (object (type nfl-db::team) stream view &key)
  (format stream "~a" (team-title object)))

(define-presentation-method present (object (type nfl-db::game) stream view &key)
  (with-slots ( (w nfl-db::week-no) (a nfl-db::away-id) (h nfl-db::home-id) ) object
    (format stream "[~d] ~a @ ~a" w a h)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Create basic pane classes
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; %% TEAM BUTTON ------------------------------------------------------------------------------------------------------

(defclass team-button (push-button-pane)
  ( (team  :initarg :team)
    (size  :initarg :size)
    (north :initarg :north :initform 0)
    (east  :initarg :east  :initform 0)
    (south :initarg :south :initform 0)
    (west  :initarg :west  :initform 0) ))

(defmethod handle-repaint ((button team-button) region)
  (with-slots (team size north east south west) button
    (let ( (main-color    (get-team-color-main team))
           (second-color  (get-team-color-highlight team))
           (image (make-pattern-from-bitmap-file (team-logo-file team size))) )
      (let ( (far-off-x (- (+ size east west) 7))
             (far-off-y (- (+ size north south) 7)) )
        (draw-rectangle* button 6 6 far-off-x far-off-y
                              :line-joint-shape :bevel
                              :filled nil :line-thickness 4 :ink main-color)
        (draw-image* button image east north)))))

(defun on-team-icon-click (team-button)
  (with-slots (team) team-button
    (run-frame-top-level (make-application-frame 'team-info :team team))))

(defun make-team-button (team size &optional invert &key (north 0) (east 0) (south 0) (west 0))
  (make-pane 'team-button :label (team-name team) :team team :size size
                                                  :background (make-rgb-color 1 1 1)
                                                  :north north :south south :east east :west west
                                                  :min-width (+ east west size) :max-width (+ east west size)
                                                  :min-height (+ north south size) :max-height (+ north south size)
                                                  :activate-callback #'on-team-icon-click)
)


;; %% TEAM BANNER PANE -------------------------------------------------------------------------------------------------

(defclass team-banner-pane (clim-stream-pane)
  ( (team :initarg :team) (hl :initarg :hl) (bg :initarg :bg) ))

(defun make-team-banner-pane (team &optional invert)
  (let ( (main-color    (if invert (get-team-color-highlight team) (get-team-color-main team)))
         (second-color  (if invert (get-team-color-main team) (get-team-color-highlight team))) )
    (make-pane 'team-banner-pane :team team
                                 :min-height +icon-large+
                                 :max-height +icon-large+
                                 :min-width  (* 6 +icon-large+)
                                 :foreground second-color
                                 :background main-color)))

(defmethod handle-repaint ((pane team-banner-pane) region)
  (with-slots (team) pane
    (let ( (w (bounding-rectangle-width  pane))
           (h (bounding-rectangle-height pane)) )
      (clim:updating-output (pane)
        (draw-text* pane (team-home team) 10 30 :text-size 20)
        (draw-text* pane (team-name team) (floor w 2) (floor h 2)
                         :text-size 80
                         :text-face :italic
                         :align-y :center
                         :align-x :center)))))

;; %% GAME PANE --------------------------------------------------------------------------------------------------------

(defclass old-game-pane (clim-stream-pane)
  ( (game :initarg :game) )
)

(defun make-old-game-pane (game)
  (let ( (total-height (+ +icon-small+ +game-list-item-top-border-size+ +game-list-item-bottom-border-size+)) )
    (make-pane 'old-game-pane :game game
                          :background (if game +main-bg-color+ +bye-bg-color+)
                          :min-height (if game total-height (/ +icon-small+ 2))
                          :max-height (if game total-height (/ +icon-small+ 2))
                          :min-width  (* 6 +icon-small+)
    )))

(defun draw-game-score-info (pane w h game)
  (if (game-score game)
    (let* ( (totals (score-totals (game-score game)))
            (text (format nil "~2d : ~2d" (car totals) (cdr totals))) )
      (draw-text* pane text (+ +icon-small+ (/ +game-list-item-inner-border-size+ 2)) (floor h 2)
                  :align-y :center
                  :align-x :center
                  :text-size 30))
    (draw-text* pane "To Be Played" (+ +icon-small+ (/ +game-list-item-inner-border-size+ 2)) (floor h 2)
                  :align-y :center
                  :align-x :center
                  :text-size 20)))

(defun draw-game-day-info (pane w h game)
  (let ( (dday (game-dday game)) )
    (if dday
      (multiple-value-bind (secs mins hours day month year day-of-week dst tz)
                           (decode-universal-time dday)
        (let ( (text (format nil "~a, ~a ~d ~d"
                             (aref +days-of-week+ day-of-week)
                             (aref +months+ (1- month))
                             day
                             year)) )
          (draw-text* pane text +game-day-info-x-offset+ (/ (* 3 h) 4)))))))

(defun draw-game-time-info (pane w h game)
  (let ( (dday (game-dday game)) )
    (if dday
      (multiple-value-bind (secs mn hr day month year day-of-week dst tz)
                           (decode-universal-time dday)
        (let ( (text (format nil "~d:~2,'0d ~a" (if (< hr 12) hr (- hr 12)) mn (if (< hr 12) 'AM 'PM))) )
          (draw-text* pane text +game-day-info-x-offset+ (/ h 3)))))))

(defun draw-game-airer-info (pane w h game)
  (let ( (airer (nfl-db::game-airer game)) )
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
                                                                          (/ +icon-xsmall+ 2)))))))))

(defmethod handle-repaint ((pane old-game-pane) region)
  (with-slots (game) pane
    (let ( (w (bounding-rectangle-width  pane))
           (h (bounding-rectangle-height pane)) )
      (if game
        (with-slots ( (week nfl-db::week-no) (away nfl-db::away-id) (home nfl-db::home-id) ) game
          (let ( (home-icon-file (team-logo-file home +icon-small+))
                 (away-icon-file (team-logo-file away +icon-small+)) )
            (let ( (home-icon (make-pattern-from-bitmap-file home-icon-file))
                   (away-icon (make-pattern-from-bitmap-file away-icon-file))
                   (home-bg (get-team-color-main home))
                   (away-bg (get-team-color-main away)) )
              (clim:updating-output (pane)
                (draw-rectangle* pane +game-list-item-west-border-size+
                                      +game-list-item-bottom-border-size+
                                      (+ +game-list-item-west-border-size+ +icon-small+)
                                      (+ +game-list-item-top-border-size+ +icon-small+)
                                      :line-joint-shape :bevel
                                      :filled t
                                      :ink (make-rgb-color 1 1 1))
                (draw-rectangle* pane +game-list-item-west-border-size+
                                      +game-list-item-bottom-border-size+
                                      (+ +game-list-item-west-border-size+ +icon-small+)
                                      (+ +game-list-item-top-border-size+ +icon-small+)
                                      :line-thickness 2
                                      :line-joint-shape :bevel
                                      :filled nil
                                      :ink away-bg)
                (draw-rectangle* pane (+ +icon-small+
                                         +game-list-item-west-border-size+
                                         +game-list-item-inner-border-size+)
                                      +game-list-item-bottom-border-size+
                                      (+ (* 2 +icon-small+)
                                              +game-list-item-west-border-size+
                                              +game-list-item-inner-border-size+)
                                      (+ +game-list-item-top-border-size+ +icon-small+)
                                      :line-joint-shape :bevel
                                      :filled t
                                      :ink (make-rgb-color 1 1 1))
                (draw-rectangle* pane (+ +icon-small+
                                         +game-list-item-west-border-size+
                                         +game-list-item-inner-border-size+)
                                      +game-list-item-bottom-border-size+
                                      (+ (* 2 +icon-small+)
                                              +game-list-item-west-border-size+
                                              +game-list-item-inner-border-size+)
                                      (+ +game-list-item-top-border-size+ +icon-small+)
                                      :line-thickness 2
                                      :line-joint-shape :bevel
                                      :filled nil
                                      :ink home-bg)
                (draw-line* pane 0 0 w 0 :ink +light-bg-color+)
                (draw-line* pane 0 (- h 1) w (- h 1) :ink +dark-bg-color+)
                (draw-game-score-info pane w h game)
                (draw-game-day-info pane w h game)
                (draw-game-time-info pane w h game)
                (draw-game-airer-info pane w h game)
                (draw-image* pane away-icon +game-list-item-west-border-size+ +game-list-item-top-border-size+)
                (draw-image* pane home-icon (+ +icon-small+
                                               +game-list-item-west-border-size+
                                               +game-list-item-inner-border-size+)
                                               +game-list-item-top-border-size+)))))
        (draw-text* pane "BYE" (floor w 2) (floor h 2) :text-size 20 :align-y :center :align-x :center)))))


;; %% SCHEDULE (LIST) PANE ---------------------------------------------------------------------------------------------

(defun make-team-schedule-list-pane (team)
  (make-pane :list-pane
             :mode :exclusive
             :items (team-schedule team)
             :value-changed-callback
               (lambda (pane item)
                 (if item
                   (format t "item changed: Game ~a @ ~a~%" (nfl-db::away-team item) (nfl-db::home-team item))
                   (format t "item changed: BYE ~%")))))

(defun make-team-schedule-pane (team)
  (let ( (games (team-schedule team)) )
    (make-pane :vbox-pane
               :contents (mapcar #'make-old-game-pane games))))

;; %% LEAGUE (ALL THE TEAMS) PANE --------------------------------------------------------------------------------------

(defconstant +team-icon-border+ 30)

(defun make-division-pane (teams)
  (make-pane :hbox
             :min-width (+ +icon-small+ (* 2 +team-icon-border+))
             :max-width (+ +icon-small+ (* 2 +team-icon-border+))
             :min-height (+ +icon-small+ (* 2 +team-icon-border+))
             :max-height (+ +icon-small+ (* 2 +team-icon-border+))
             :contents (mapcar
                          (lambda (x) (make-team-button (team-id x) +icon-small+ nil
                                                        :north +team-icon-border+ :east +team-icon-border+
                                                        :west +team-icon-border+ :south +team-icon-border+))
                          teams)
  ))

(defun make-conference-pane (conf)
  (let ( (n (division-teams conf :north))
         (e (division-teams conf :east))
         (s (division-teams conf :south))
         (w (division-teams conf :west)) )
    (make-pane :vbox-pane
               :contents (mapcar #'make-division-pane (list n e s w)))))

;; %% TOP LEVEL APPLICATION FRAMES -------------------------------------------------------------------------------------

(define-application-frame team-info ()
  ( (team  :initarg :team :initform :phi) )
  (:panes (team-icon    (with-slots (team) *application-frame*
                          (make-team-icon-pane team +icon-large+ nil
                                               :west +game-list-item-west-border-size+
                                               :east +game-list-item-inner-border-size+)))
          (team-banner  (with-slots (team) *application-frame* (make-team-banner-pane team nil)))
          (team-sched   (with-slots (team) *application-frame*
                          (scrolling (:scroll-bar :vertical :height 400) (make-team-schedule-pane team)))))
  (:layouts (default (vertically () (horizontally () team-icon team-banner) team-sched)))
  (:menu-bar nil))

(define-application-frame game-info ()
  ( (game :initarg :game) )
  (:pane (with-slots (game) *application-frame* (make-old-game-pane game)))
  (:menu-bar nil))

(define-application-frame league-info ()
  ( )
  (:panes (nfc (make-conference-pane :nfc))
          (afc (make-conference-pane :afc))
          (interactor :interactor))
  (:layouts (default (vertically () interactor (horizontally () afc nfc))))
  (:menu-bar t))

(defun run-team (team) (run-frame-top-level (make-application-frame 'team-info :team team)))
(defun run-game (game) (run-frame-top-level (make-application-frame 'game-info :game game)))
(defun run-league () (run-frame-top-level (make-application-frame 'league-info)))

(define-league-info-command (com-show-team-schedule :menu t :name "Show Sched")
  ( (team 'keyword) )
  (format t "com-show-team-schedule: team = ~a~%" team)
  (run-frame-top-level (make-application-frame 'team-info :team team))
)

