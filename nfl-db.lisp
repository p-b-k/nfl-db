;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIM Color Editor example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "clim-examples")

(defpackage #:nfl-db (:use #:clim-lisp #:clim))
(in-package #:nfl-db)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Team names and information (name, conference, division, etc)
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defvar league '(
    ( :BUF           "Buffalo"          "Bills"         afc . east )
    ( :MIA           "Miami"            "Dolphins"      afc . east )
    ( :NE            "New England"      "Patriots"      afc . east )
    ( :NYJ           "New York"         "Jets"          afc . east )
    ( :BAL            "Baltimore"       "Ravens"        afc . north )
    ( :CIN            "Cincinnati"      "Bengals"       afc . north )
    ( :CLE            "Cleveland"       "Browns"        afc . north )
    ( :PIT            "Pittsburg"       "Steelers"      afc . north )
    ( :HOU            "Houston"         "Texans"        afc . south )
    ( :IND            "Indianapolis"    "Colts"         afc . south )
    ( :JAX            "Jacksonville"    "Jaguars"       afc . south )
    ( :TEN            "Tennessee"       "Titans"        afc . south )
    ( :DEN            "Denver"          "Broncos"       afc . west )
    ( :KC             "Kansas City"     "Chiefs"        afc . west )
    ( :LV             "Las Vegas"       "Raiders"       afc . west )
    ( :LAC            "Los Angeles"     "Chargers"      afc . west )
    ( :DAL            "Dallas"          "Cowboys"       nfc . east )
    ( :NYG            "New York"        "Giants"        nfc . east )
    ( :PHI            "Philadelphia"    "Eagles"        nfc . east )
    ( :WAS            "Washington"      "Commanders"    nfc . east )
    ( :CHI            "Chicago"         "Bears"         nfc . north )
    ( :DET            "Detroit"         "Lions"         nfc . north )
    ( :GB             "Green Bay"       "Packers"       nfc . north )
    ( :MIN            "Minisota"        "Vikings"       nfc . north )
    ( :ATL            "Atlanta"         "Falcons"       nfc . south )
    ( :CAR            "Carolina"        "Panthers"      nfc . south )
    ( :NO             "New Orleans"     "Saints"        nfc . south )
    ( :TB             "Tamba Bay"       "Buccaneers"    nfc . south )
    ( :ARI            "Arizona"         "Cardinals"     nfc . west )
    ( :LA             "Los Angeles"     "Rams"          nfc . west )
    ( :SF             "San Fransisco"   "49ers"         nfc . west )
    ( :SEA            "Seattle"         "Seahawks"      nfc . west )))

(defun team-data-rec (team todo)
  (if todo
    (let ( (next (car todo)) )
      (if (equalp team (car next))
        (cdr next)
        (team-data-rec team (cdr todo))))
    nil))

(defun team-data (team) (team-data-rec team league))

(defun team-home (team)
  (let ( (data (team-data team)) )
    (if data (car data) nil)))

(defun team-name (team)
  (let ( (data (team-data team)) )
    (if data (car (cdr data)) nil)))

(defun team-division (team)
  (let ( (data (team-data team)) )
    (if data (cdr (cdr data)) nil)))

(defun team-title (team)
  (let ( (data (team-data team)) )
    (if data (concatenate 'string (car data) " " (car (cdr data))) nil)))

(export 'team-home)
(export 'team-name)
(export 'team-division)
(export 'team-title)

(defval matches #( 
  ;;     TEAMS           DATE            TIME   ON
  ;; Week 1
  '( #( (:BAL . :KC )  #(2024  9  5)   #(10 20)  nbc)
     #( (:GB  . :PHI)  #(2024  9  6)   #(10 15)  peacock)
     #( (:PIT . :CAR)  #(2024  9  8)   #(13 00)  fox)
     #( (:ARI . :BUF)  #(2024  9  8)   #(13 00)  cbs)
     #( (:TEN . :CHI)  #(2024  9  8)   #(13 00)  fox)
     #( (:NE  . :CIN)  #(2024  9  8)   #(13 00)  cbs)
     #( (:HOU . :IND)  #(2024  9  8)   #(13 00)  cbs)
     #( (:JAX . :MIA)  #(2024  9  8)   #(13 00)  cbs)
     #( (:CAR . :NO )  #(2024  9  8)   #(13 00)  fox)
     #( (:MIN . :NYG)  #(2024  9  8)   #(13 00)  fox)
     #( (:LV  . :LAC)  #(2024  9  8)   #(16 05)  cbs)
     #( (:DEN . :SEA)  #(2024  9  8)   #(16 05)  cbs)
     #( (:DAL . :CLE)  #(2024  9  8)   #(16 25)  fox)
     #( (:WAS . :TB )  #(2024  9  8)   #(16 25)  fox)
     #( (:LA  . :DET)  #(2024  9  8)   #(20 20)  nbc)
     #( (:NYJ . :SF )  #(2024  9  9)   #(20 15)  espn abc) )

  ;; Week 2
  '( #( (:BUF . :MIA)  #(2024  9 12)   #(20 15)  prime)
     #( (:LV  . :BAL)  #(2024  9 15)   #(13 00)  cbs)
     #( (:LAC . :CAR)  #(2024  9 15)   #(13 00)  cbs)
     #( (:NO  . :DAL)  #(2024  9 15)   #(13 00)  fox)
     #( (:TB  . :DET)  #(2024  9 15)   #(13 00)  fox)
     #( (:IND . :GB )  #(2024  9 15)   #(13 00)  fox)
     #( (:CLE . :JAX)  #(2024  9 15)   #(13 00)  cbs)
     #( (:SF  . :MIN)  #(2024  9 15)   #(13 00)  cbs)
     #( (:SEA . :NE )  #(2024  9 15)   #(13 00)  fox)
     #( (:NYJ . :TEN)  #(2024  9 15)   #(13 00)  cbs)
     #( (:NYG . :WAS)  #(2024  9 15)   #(13 00)  fox)
     #( (:LA  . :ARI)  #(2024  9 15)   #(16 05)  fox)
     #( (:PIT . :DEN)  #(2024  9 15)   #(16 25)  cbs)
     #( (:CIN . :KC )  #(2024  9 15)   #(16 25)  cbs)
     #( (:CHI . :HOU)  #(2024  9 15)   #(20 20)  nbc)
     #( (:ATL . :PHI)  #(2024  9 16)   #(20 15)  espn) )

  ;; Week 3
  '( #( (:NE  . :NYJ)  #(2024  9 19)   #(20 15)  prime)
     #( (:NYG . :CLE)  #(2024  9 22)   #(13 00)  fox)
     #( (:CHI . :IND)  #(2024  9 22)   #(13 00)  cbs)
     #( (:HOU . :MIN)  #(2024  9 22)   #(13 00)  cbs)
     #( (:PHI . :NO )  #(2024  9 22)   #(13 00)  fox)
     #( (:LAC . :PIT)  #(2024  9 22)   #(13 00)  cbs)
     #( (:DEN . :TB )  #(2024  9 22)   #(13 00)  fox)
     #( (:GB  . :TEN)  #(2024  9 22)   #(13 00)  fox)
     #( (:CAR . :LV )  #(2024  9 22)   #(13 05)  cbs)
     #( (:MIA . :SEA)  #(2024  9 22)   #(13 05)  cbs)
     #( (:DET . :ARI)  #(2024  9 22)   #(13 25)  fox)
     #( (:BAL . :DAL)  #(2024  9 22)   #(13 25)  fox)
     #( (:SF  . :LA )  #(2024  9 22)   #(13 25)  fox)
     #( (:KC  . :ATL)  #(2024  9 22)   #(20 20)  nbc)
     #( (:JAX . :BUF)  #(2024  9 23)   #(19 30)  espn)
     #( (:WAS . :CIN)  #(2024  9 23)   #(20 15)  abc) )

  ;; Week 4
  '( #( (:DAL . :NYG)  #(2024  9 26)   #(20 15)  prime)
     #( (:NO  . :ATL)  #(2024  9 29)   #(13 00)  fox)
     #( (:CIN . :CAR)  #(2024  9 29)   #(13 00)  fox)
     #( (:LA  . :CHI)  #(2024  9 29)   #(13 00)  fox)
     #( (:MIN . :GB )  #(2024  9 29)   #(13 00)  cbs)
     #( (:JAX . :HOU)  #(2024  9 29)   #(13 00)  cbs)
     #( (:PIT . :IND)  #(2024  9 29)   #(13 00)  cbs)
     #( (:DEN . :NYJ)  #(2024  9 29)   #(13 00)  cbs)
     #( (:PHI . :TB )  #(2024  9 29)   #(13 00)  fox)
     #( (:WAS . :ARI)  #(2024  9 29)   #(16 05)  fox)
     #( (:NE  . :SF )  #(2024  9 29)   #(16 05)  fox)
     #( (:KC  . :LAC)  #(2024  9 29)   #(16 25)  cbs)
     #( (:CLE . :LV )  #(2024  9 29)   #(16 25)  cbs)
     #( (:BUF . :BAL)  #(2024  9 29)   #(20 20)  nbc)
     #( (:TEN . :MIA)  #(2024  9 30)   #(19 30)  espn)
     #( (:SEA . :DET)  #(2024  9 30)   #(20 15)  abc) )

  ;; Week 5
  '( #( (:TB  . :ATL)  #(2024 10  3)   #(20 15)  prime)
     #( (:NYJ . :MIN)  #(2024 10  6)   #( 9 30)  nfl)
     #( (:CAR . :CHI)  #(2024 10  6)   #(13 00)  fox)
     #( (:BAL . :CIN)  #(2024 10  6)   #(13 00)  cbs)
     #( (:BUF . :HOU)  #(2024 10  6)   #(13 00)  cbs)
     #( (:IND . :JAX)  #(2024 10  6)   #(13 00)  cbs)
     #( (:MIA . :NE )  #(2024 10  6)   #(13 00)  fox)
     #( (:CLE . :WAS)  #(2024 10  6)   #(13 00)  fox)
     #( (:LV  . :DEN)  #(2024 10  6)   #(16 05)  fox)
     #( (:ARI . :SF )  #(2024 10  6)   #(16 05)  fox)
     #( (:GB  . :LA )  #(2024 10  6)   #(16 25)  cbs)
     #( (:NYG . :SEA)  #(2024 10  6)   #(16 25)  cbs)
     #( (:DAL . :PIT)  #(2024 10  6)   #(20 2 )  nbc)
     #( (:NO  . :KC )  #(2024 10  7)   #(20 2 )  espn) )

  ;; Week 6
  '( #( (:SF  . :SEA)  #(2024 10 10)   #(20 15)  prime)
     #( (:JAX . :CHI)  #(2024 10 13)   #( 9 30)  nfl)
     #( (:WAS . :BAL)  #(2024 10 13)   #(13 00)  cbs)
     #( (:ARI . :GB )  #(2024 10 13)   #(13 00)  fox)
     #( (:HOU . :NE )  #(2024 10 13)   #(13 00)  cbs)
     #( (:TB  . :NO )  #(2024 10 13)   #(13 00)  fox)
     #( (:CLE . :PHI)  #(2024 10 13)   #(13 00)  fox)
     #( (:IND . :TEN)  #(2024 10 13)   #(13 00)  cbs)
     #( (:LAC . :DEN)  #(2024 10 13)   #(16 05)  cbs)
     #( (:PIT . :LV )  #(2024 10 13)   #(16 05)  cbs)
     #( (:ATL . :CAR)  #(2024 10 13)   #(16 25)  fox)
     #( (:DET . :DAL)  #(2024 10 13)   #(16 25)  fox)
     #( (:CIN . :NYG)  #(2024 10 13)   #(20 20)  nbc)
     #( (:BUF . :NYJ)  #(2024 10 14)   #(20 15)  espn) )
