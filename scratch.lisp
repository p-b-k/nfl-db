;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scratch file for NFL stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require "adsf")
(ql:quickload "clim-examples")

(defpackage :app (:use :clim :clim-lisp) (:export run-app))

(in-package :app)

(defun run-app () (run-frame-top-level (make-application-frame 'superapp)))

(load "nfl-db.lisp")
(load "nfl-static-data.lisp")

(defun load-data () (nfl-db::load-data nfl-static-data:schedule nfl-static-data:league nfl-static-data:colors))
(export 'load-data)

(load-data)
(load "nfl-tool.lisp")


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Try out presentation types
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-presentation-method accept ((type nfl-db::team) stream view &key)
  (let ( (team-id (accept '(member buf mia ne nyj bal cin cle pit hou ind jax ten den kc lv lac
                                   dal nyg phi was chi det gb min atl car no tb ari la sf sea)
                          :stream stream
                          :prompt "?")) )
    (make-team team-id)))

; (in-package :clim-user)

; (define-presentation-type ticket ())

; (setf (get 'bush 'party) 'republican)
; (setf (get 'quayle 'party) 'republican)
; (setf (get 'clinton 'party) 'democrat)
; (setf (get 'gore 'party) 'democrat)

; ;;; separated by comma version
; (define-presentation-method accept ((type ticket) stream view &key &allow-other-keys)
;   (declare (ignore view))
;   (let ( (president (accept '(member bush clinton)
;                            :stream stream
;                            :prompt nil
;                            ;; add comma as a completing delimiter
;                            :blip-characters '(#\,))) )
;     ;; Make sure that the names were separated by a comma
;     (unless (eql (read-gesture :stream stream) #\,) (simple-parse-error "Ticket members must be separated by commas"))

;     (let ((veep (accept '(member quayle gore) :stream stream :prompt nil)))
;       ;; Validate party affiliations
;       (unless (eql (get president 'party) (get veep 'party))
;         (simple-parse-error "Ticket members must be of the same party"))

;       (list president veep))))

; ;;; Separated by Return version
; (define-presentation-method accept ((type ticket) stream view &key
;                                    &allow-other-keys)
;  (declare (ignore view))
;  (let ((president (accept '(member bush clinton)
;                           :stream stream
;                           :prompt nil
;                           ;; Remove Newline from activation characters
;                           :activation-characters `()
;                           ;; Add Newline as a delimiter, so that we get
;                           ;; completion and move-to-next-field behavior
;                           ;; when Return is typed.
;                           :blip-characters `(#\Return #\Newline))))
;    (unless (eql (read-gesture :stream stream) #\Newline)
;      (simple-parse-error "Ticket members must be entered on separate lines"))

;    (let ( (veep (accept '(member quayle gore) :stream stream :prompt nil)) )
;      ;; Validate party affiliations
;      (unless (eql (get president 'party) (get veep 'party))
;        (simple-parse-error "Ticket members must be of the same party"))

;      (list president veep))))
