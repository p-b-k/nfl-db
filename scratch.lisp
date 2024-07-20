;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scratch file for NFL stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require "adsf")
(ql:quickload "clim-examples")
(ql:quickload "clim-listener")

(load "nfl-db.lisp")
(load "nfl-static-data.lisp")

(defpackage :app (:use :clim :clim-lisp :nfl-static-data) (:export run-app))

(in-package :app)

(defun run-app () (run-frame-top-level (make-application-frame 'superapp)))

(defun load-data () (nfl-db::load-data schedule league colors))
(export 'load-data)

(load-data)
(load "nfl-tool.lisp")


(defun start-listener () (clim-listener:run-listener))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Populate mock score data

(defun apply-to-week (f idx weeks)
; (format t "apply-to-week: called on idx = ~a~%" idx)
  (if (< idx 18)
    ;; Figure out how to do this better
    (let ( (func (lambda (x)
                    (format t "calling lambda on ~a~%" x)
                    (apply f (list idx (car (aref x 0)) (cdr (aref x 0)))))) )
      (mapcar func (aref weeks idx))
      (format t "apply-to-week: finished apply function for week ~a~%" idx)
      (apply-to-week f (+ 1 idx) weeks))))

(defun week-map (f weeks)
  (apply-to-week f 0 weeks))

(defun next-score ()
  (let ( (rstate (make-random-state t)) )
    ;; Field Goal?
    (if (> 40 (random 100 rstate))
      3
      ;; Touchdown?
      (if (> 20 (random 100))
        ;; Extra Point
        (if (> 98 (random 100))
          7
          ;; Go for 2, or just take the 6
          (if (> 70 (random 100)) 8 6))
        ;; A Safety perhaps?
        (if (> 8 (random 100)) 2 0)))))
          
(defun rand-quarter-score (sofar)
  (let ( (pts (next-score)) )
    (if (> pts 0)
      (rand-quarter-score (+ pts sofar))
      sofar)))

(defun generate-random-score ()
  `(,(rand-quarter-score 0) ,(rand-quarter-score 0) ,(rand-quarter-score 0) ,(rand-quarter-score 0)))

(defun write-random-game-score (week-no away-id home-id)
  (format t "write-random-game-score: calling on ~a, ~a, ~a~%" week-no away-id home-id)
  (let ( (g (make-instance 'nfl-db::game :week week-no :home-team home-id :away-team away-id)) )
    (format t "write-random-game-score: game = ~s~%" g)
    (let ( (home-score-file (nfl-db::file-for-game-data-field g 'home-score))
           (away-score-file (nfl-db::file-for-game-data-field g 'away-score)) )
      (nfl-db::write-value-to-file home-score-file (generate-random-score))
      (nfl-db::write-value-to-file away-score-file (generate-random-score)))))

(defun create-demo-data ()
  (week-map #'write-random-game-score schedule))

