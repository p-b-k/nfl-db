;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A File to load all the other files for nfl-tool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do not need clim here
(load "nfl-db.lisp")
(load "nfl-static-data.lisp")

; (require "adsf")
(ql:quickload "clim-examples")
(ql:quickload "clim-listener")

(nfl-db::load-data nfl-static-data:schedule nfl-static-data:league nfl-static-data:colors)

;; Load constants first, because everything depends on it
(load "nfl-constants.lisp")

;; Load the various components that depend on db and constants, but not each other
(load "nfl-team-pane.lisp")
(load "nfl-score-button.lisp")
(load "nfl-game-time-pane.lisp")
(load "nfl-game-status-pane.lisp")
(load "nfl-game-airer-pane.lisp")

;; Now that all of the sub-components are loaded, load the game-pane
(load "nfl-game-pane.lisp")

;; Finally, load the tool
(load "nfl-tool.lisp")

