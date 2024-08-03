;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NFL Tool Constants
;; ---------------------------------------------------------------------------------------------------------------------
;; Constants for UI spacing and such
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:nfl-constants (:use #:nfl-db #:clim #:clim-lisp))
(in-package #:nfl-constants)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Exports
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(export '+icon-xsmall+)
(export '+icon-small+)
(export '+icon-large+)
(export '+icon-xlarge+)

(export '+border-off+)
(export '+border-width+)
(export '+border-thick+)
(export '+border-adjust+)

(export '+main-bg-color+)
(export '+bye-bg-color+)

(export '+light-bg-color+)
(export '+dark-bg-color+)

(export '+game-list-item-top-border-size+)
(export '+game-list-item-bottom-border-size+)
(export '+game-list-item-west-border-size+)
(export '+game-list-item-inner-border-size+)
(export '+game-list-item-date-padding+)

(export '+game-day-info-x-offset+)
(export '+game-airer-info-x-offset+)

(export 'nfc_color)
(export 'afc_color)

(export '+days-of-week+)
(export '+months+)

(export 'conference-logo-file)
(export 'afc-logo-file)
(export 'nfc-logo-file)
(export 'team-logo-file)
(export 'get-team-color-main)
(export 'get-team-color-highlight)
(export 'resolve-airer-png-name)
(export 'airer-logo-file)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Definitions
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

(defconstant +main-bg-color+ (make-rgb-color 0.9 0.9 0.83))
(defconstant +bye-bg-color+ (make-rgb-color  0.83 0.83 0.78))

(defconstant +light-bg-color+ (make-rgb-color  (/ 175 255) (/ 175 255) (/ 175 255)))
(defconstant +dark-bg-color+ (make-rgb-color  (/ 140 255) (/ 140 255) (/ 140 255)))

(defconstant +game-list-item-top-border-size+ 4)
(defconstant +game-list-item-bottom-border-size+ 4)
(defconstant +game-list-item-west-border-size+ 2)
(defconstant +game-list-item-inner-border-size+ 200)
(defconstant +game-list-item-date-padding+ 12)

(defconstant +game-day-info-x-offset+ (+ (* +icon-small+ 2)
                                         +game-list-item-west-border-size+
                                         +game-list-item-inner-border-size+
                                         +game-list-item-date-padding+))
(defconstant +game-airer-info-x-offset+ 150)

(defconstant nfc_color (make-rgb-color 0.0 (/ 59 256) (/ 37 102)))
(defconstant afc_color (make-rgb-color (/ 206 256) (/ 19 256) (/ 102 256)))

(defconstant +days-of-week+ #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defconstant +months+ #("January" "February" "March" "April" "May" "June"
                        "July" "August" "September" "October" "November" "December"))

(defun conference-logo-file (conference size)
  (format nil "~a/nfl/~a/~a.png" +file-root/logos+ size (symbol-name conference)))

(defun afc-logo-file (size) (conference-logo-file :afc size))
(defun nfc-logo-file (size) (conference-logo-file :nfc size))

(defun team-logo-file (team size)
  (format nil "~a/teams/logos/~ax~a/~a.png" +file-root/logos+ size size (symbol-name team)))

(defun get-team-color-main (team)
  (let ( (color (car (team-colors team))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun get-team-color-highlight (team)
  (let ( (color (car (cdr (team-colors team)))) )
     (make-rgb-color (/ (aref color 0) 255) (/ (aref color 1) 255) (/ (aref color 2) 255))))

(defun resolve-airer-png-name (airer)
  (cond
    ( (eq airer :espn+) "ESPN_plus" )
    ( (eq airer :netflix) "Netflix" )
    ( (eq airer :peacock) "Peacock" )
    ( (eq airer :prime) "Prime" )
    ( t (symbol-name airer) )))

(defun airer-logo-file (airer size)
  (format nil "~a/networks/~a/~a.png" +file-root/logos+ size (resolve-airer-png-name airer)))

