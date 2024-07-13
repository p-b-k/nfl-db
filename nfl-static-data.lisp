;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the root data I'm going to use to generate the teams and the schedule.
;; It's also going to have some support functions, but it's really a boot strap to generate the actaul files.
;; This will give me an opportunity to get familiar again with common lisp
;; 
;; It all runs in whatever package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Team names and information (name, conference, division, etc)
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defpackage #:nfl-static-data (:use #:cl))
(in-package #:nfl-static-data)

(export 'league)
(export 'colors)
(export 'schedule)

(defvar league '(
    #( :BUF           "Buffalo"          "Bills"        :afc  :east  )
    #( :MIA           "Miami"            "Dolphins"     :afc  :east  )
    #( :NE            "New England"      "Patriots"     :afc  :east  )
    #( :NYJ           "New York"         "Jets"         :afc  :east  )
    #( :BAL           "Baltimore"        "Ravens"       :afc  :north )
    #( :CIN           "Cincinnati"       "Bengals"      :afc  :north )
    #( :CLE           "Cleveland"        "Browns"       :afc  :north )
    #( :PIT           "Pittsburg"        "Steelers"     :afc  :north )
    #( :HOU           "Houston"          "Texans"       :afc  :south )
    #( :IND           "Indianapolis"     "Colts"        :afc  :south )
    #( :JAX           "Jacksonville"     "Jaguars"      :afc  :south )
    #( :TEN           "Tennessee"        "Titans"       :afc  :south )
    #( :DEN           "Denver"           "Broncos"      :afc  :west  )
    #( :KC            "Kansas City"      "Chiefs"       :afc  :west  )
    #( :LV            "Las Vegas"        "Raiders"      :afc  :west  )
    #( :LAC           "Los Angeles"      "Chargers"     :afc  :west  )
    #( :DAL           "Dallas"           "Cowboys"      :nfc  :east  )
    #( :NYG           "New York"         "Giants"       :nfc  :east  )
    #( :PHI           "Philadelphia"     "Eagles"       :nfc  :east  )
    #( :WAS           "Washington"       "Commanders"   :nfc  :east  )
    #( :CHI           "Chicago"          "Bears"        :nfc  :north )
    #( :DET           "Detroit"          "Lions"        :nfc  :north )
    #( :GB            "Green Bay"        "Packers"      :nfc  :north )
    #( :MIN           "Minisota"         "Vikings"      :nfc  :north )
    #( :ATL           "Atlanta"          "Falcons"      :nfc  :south )
    #( :CAR           "Carolina"         "Panthers"     :nfc  :south )
    #( :NO            "New Orleans"      "Saints"       :nfc  :south )
    #( :TB            "Tamba Bay"        "Buccaneers"   :nfc  :south )
    #( :ARI           "Arizona"          "Cardinals"    :nfc  :west  )
    #( :LA            "Los Angeles"      "Rams"         :nfc  :west  )
    #( :SF            "San Fransisco"    "49ers"        :nfc  :west  )
    #( :SEA           "Seattle"          "Seahawks"     :nfc  :west  )))

(defvar colors '(
    (:BUF           #(  0  51 141)   #(198  12  48)   #(255 255 255)   #(  0  39 377))
    (:MIA           #(  0 142 151)   #(252  76   2)   #(255 255 255)   #(  0  87 120))
    (:NE            #(  0  34  68)   #(198  12  48)   #(176 183 188)   #(255 255 255))
    (:NYJ           #( 17  87  64)   #(255 255 255)   #(  0   0   0))
    (:BAL           #( 36  19  95)   #(  0   0   0)   #(154 118  17))
    (:CIN           #(  0   0   0)   #(251  79  20)   #(255 255 255))
    (:CLE           #( 49  29   0)   #(255  60   0)   #(255 255 255))
    (:PIT           #(  0   0   0)   #(255 182  18))
    (:HOU           #(  2  16  24)   #(235   0  40)   #(255 255 255)   #(  0 128 198))
    (:IND           #(  1  51 105)   #(255 255 255)   #(165 172 175)   #(  0   0   0))
    (:JAX           #(  0 103 120)   #(  0   0   0)   #(159 121  44))
    (:TEN           #(  0  34  68)   #( 75 146 219)   #(198  12  48)   #(156 172 175)   #(255 255 255))
    (:DEN           #(252  76   2)   #( 10  35  67)   #(255 255 255))
    (:KC            #(227  24  55)   #(255 182  18)   #(255 255 255))
    (:LV            #(165 172 175)   #(  0   0   0))
    (:LAC           #(  0 128 198)   #(255 194  14)   #(255 255 255))
    (:DAL           #(  0  34  68)   #(176 183 188)   #(  0  51 141)   #(173 217 206)   #(255 255 255))
    (:NYG           #( 11  34 101)   #(167  25  48)   #(255 255 255))
    (:PHI           #(  0  72  81)   #(162 170 173)   #(  0   0   0)   #(255 255 255))
    (:WAS           #( 90  20  20)   #(255 182  18)   #(255 255 255)   #(  0   0   0))
    (:CHI           #( 11  22  42)   #(230  65   0)   #(255 255 255))
    (:DET           #(  0 118 182)   #(176 183 188)   #(255 255 255)   #(  0   0   0))
    (:GB            #( 32  55  49)   #(255 182  18)   #(255 255 255))
    (:MIN           #( 79  38 131)   #(255 198  47)   #(255 255 255))
    (:ATL           #(  0   0   0)   #(167  25  48)   #(165 172 175)   #(255 255 255))
    (:CAR           #(  0   0   0)   #(  0 133 202)   #(191 192 191))
    (:NO            #(211 188 141)   #(  0   0   0)   #(255 255 255))
    (:TB            #(167  25  48)   #( 50  47  43)   #(220  68   5)   #(  0   0   0))
    (:ARI           #(151  35  63)   #(255 255 255)   #(  0   0   0)   #(209 209 211))
    (:LA            #(  0  53 148)   #(255 209   0))
    (:SF            #(170   0   0)   #(179 153  93)   #(255 255 255))
    (:SEA           #(  0  34  68)   #(105 190  40)   #(165 172 175))))

(defvar schedule #(
  ;;     TEAMS          DATE           TIME     AIRER

  ;; Week 1
  ( #( (:BAL . :KC )  #(2024  9  5)   #(10 20)  :nbc)
    #( (:GB  . :PHI)  #(2024  9  6)   #(10 15)  :peacock)
    #( (:PIT . :ATL)  #(2024  9  8)   #(13 00)  :fox)
    #( (:ARI . :BUF)  #(2024  9  8)   #(13 00)  :cbs)
    #( (:TEN . :CHI)  #(2024  9  8)   #(13 00)  :fox)
    #( (:NE  . :CIN)  #(2024  9  8)   #(13 00)  :cbs)
    #( (:HOU . :IND)  #(2024  9  8)   #(13 00)  :cbs)
    #( (:JAX . :MIA)  #(2024  9  8)   #(13 00)  :cbs)
    #( (:CAR . :NO )  #(2024  9  8)   #(13 00)  :fox)
    #( (:MIN . :NYG)  #(2024  9  8)   #(13 00)  :fox)
    #( (:LV  . :LAC)  #(2024  9  8)   #(16 05)  :cbs)
    #( (:DEN . :SEA)  #(2024  9  8)   #(16 05)  :cbs)
    #( (:DAL . :CLE)  #(2024  9  8)   #(16 25)  :fox)
    #( (:WAS . :TB )  #(2024  9  8)   #(16 25)  :fox)
    #( (:LA  . :DET)  #(2024  9  8)   #(20 20)  :nbc)
    #( (:NYJ . :SF )  #(2024  9  9)   #(20 15)  (:espn :abc)) )

  ;; Week 2
  ( #( (:BUF . :MIA)  #(2024  9 12)   #(20 15)  :prime)
    #( (:LV  . :BAL)  #(2024  9 15)   #(13 00)  :cbs)
    #( (:LAC . :CAR)  #(2024  9 15)   #(13 00)  :cbs)
    #( (:NO  . :DAL)  #(2024  9 15)   #(13 00)  :fox)
    #( (:TB  . :DET)  #(2024  9 15)   #(13 00)  :fox)
    #( (:IND . :GB )  #(2024  9 15)   #(13 00)  :fox)
    #( (:CLE . :JAX)  #(2024  9 15)   #(13 00)  :cbs)
    #( (:SF  . :MIN)  #(2024  9 15)   #(13 00)  :cbs)
    #( (:SEA . :NE )  #(2024  9 15)   #(13 00)  :fox)
    #( (:NYJ . :TEN)  #(2024  9 15)   #(13 00)  :cbs)
    #( (:NYG . :WAS)  #(2024  9 15)   #(13 00)  :fox)
    #( (:LA  . :ARI)  #(2024  9 15)   #(16 05)  :fox)
    #( (:PIT . :DEN)  #(2024  9 15)   #(16 25)  :cbs)
    #( (:CIN . :KC )  #(2024  9 15)   #(16 25)  :cbs)
    #( (:CHI . :HOU)  #(2024  9 15)   #(20 20)  :nbc)
    #( (:ATL . :PHI)  #(2024  9 16)   #(20 15)  :espn) )

  ;; Week 3
  ( #( (:NE  . :NYJ)  #(2024  9 19)   #(20 15)  :prime)
    #( (:NYG . :CLE)  #(2024  9 22)   #(13 00)  :fox)
    #( (:CHI . :IND)  #(2024  9 22)   #(13 00)  :cbs)
    #( (:HOU . :MIN)  #(2024  9 22)   #(13 00)  :cbs)
    #( (:PHI . :NO )  #(2024  9 22)   #(13 00)  :fox)
    #( (:LAC . :PIT)  #(2024  9 22)   #(13 00)  :cbs)
    #( (:DEN . :TB )  #(2024  9 22)   #(13 00)  :fox)
    #( (:GB  . :TEN)  #(2024  9 22)   #(13 00)  :fox)
    #( (:CAR . :LV )  #(2024  9 22)   #(13 05)  :cbs)
    #( (:MIA . :SEA)  #(2024  9 22)   #(13 05)  :cbs)
    #( (:DET . :ARI)  #(2024  9 22)   #(13 25)  :fox)
    #( (:BAL . :DAL)  #(2024  9 22)   #(13 25)  :fox)
    #( (:SF  . :LA )  #(2024  9 22)   #(13 25)  :fox)
    #( (:KC  . :ATL)  #(2024  9 22)   #(20 20)  :nbc)
    #( (:JAX . :BUF)  #(2024  9 23)   #(19 30)  :espn)
    #( (:WAS . :CIN)  #(2024  9 23)   #(20 15)  :abc) )

  ;; Week 4
  ( #( (:DAL . :NYG)  #(2024  9 26)   #(20 15)  :prime)
    #( (:NO  . :ATL)  #(2024  9 29)   #(13 00)  :fox)
    #( (:CIN . :CAR)  #(2024  9 29)   #(13 00)  :fox)
    #( (:LA  . :CHI)  #(2024  9 29)   #(13 00)  :fox)
    #( (:MIN . :GB )  #(2024  9 29)   #(13 00)  :cbs)
    #( (:JAX . :HOU)  #(2024  9 29)   #(13 00)  :cbs)
    #( (:PIT . :IND)  #(2024  9 29)   #(13 00)  :cbs)
    #( (:DEN . :NYJ)  #(2024  9 29)   #(13 00)  :cbs)
    #( (:PHI . :TB )  #(2024  9 29)   #(13 00)  :fox)
    #( (:WAS . :ARI)  #(2024  9 29)   #(16 05)  :fox)
    #( (:NE  . :SF )  #(2024  9 29)   #(16 05)  :fox)
    #( (:KC  . :LAC)  #(2024  9 29)   #(16 25)  :cbs)
    #( (:CLE . :LV )  #(2024  9 29)   #(16 25)  :cbs)
    #( (:BUF . :BAL)  #(2024  9 29)   #(20 20)  :nbc)
    #( (:TEN . :MIA)  #(2024  9 30)   #(19 30)  :espn)
    #( (:SEA . :DET)  #(2024  9 30)   #(20 15)  :abc) )

  ;; Week 5
  ( #( (:TB  . :ATL)  #(2024 10  3)   #(20 15)  :prime)
    #( (:NYJ . :MIN)  #(2024 10  6)   #( 9 30)  :nfl)
    #( (:CAR . :CHI)  #(2024 10  6)   #(13 00)  :fox)
    #( (:BAL . :CIN)  #(2024 10  6)   #(13 00)  :cbs)
    #( (:BUF . :HOU)  #(2024 10  6)   #(13 00)  :cbs)
    #( (:IND . :JAX)  #(2024 10  6)   #(13 00)  :cbs)
    #( (:MIA . :NE )  #(2024 10  6)   #(13 00)  :fox)
    #( (:CLE . :WAS)  #(2024 10  6)   #(13 00)  :fox)
    #( (:LV  . :DEN)  #(2024 10  6)   #(16 05)  :fox)
    #( (:ARI . :SF )  #(2024 10  6)   #(16 05)  :fox)
    #( (:GB  . :LA )  #(2024 10  6)   #(16 25)  :cbs)
    #( (:NYG . :SEA)  #(2024 10  6)   #(16 25)  :cbs)
    #( (:DAL . :PIT)  #(2024 10  6)   #(20 20)  :nbc)
    #( (:NO  . :KC )  #(2024 10  7)   #(20 20)  :espn) )

  ;; Week 6
  ( #( (:SF  . :SEA)  #(2024 10 10)   #(20 15)  :prime)
    #( (:JAX . :CHI)  #(2024 10 13)   #( 9 30)  :nfl)
    #( (:WAS . :BAL)  #(2024 10 13)   #(13 00)  :cbs)
    #( (:ARI . :GB )  #(2024 10 13)   #(13 00)  :fox)
    #( (:HOU . :NE )  #(2024 10 13)   #(13 00)  :cbs)
    #( (:TB  . :NO )  #(2024 10 13)   #(13 00)  :fox)
    #( (:CLE . :PHI)  #(2024 10 13)   #(13 00)  :fox)
    #( (:IND . :TEN)  #(2024 10 13)   #(13 00)  :cbs)
    #( (:LAC . :DEN)  #(2024 10 13)   #(16 05)  :cbs)
    #( (:PIT . :LV )  #(2024 10 13)   #(16 05)  :cbs)
    #( (:ATL . :CAR)  #(2024 10 13)   #(16 25)  :fox)
    #( (:DET . :DAL)  #(2024 10 13)   #(16 25)  :fox)
    #( (:CIN . :NYG)  #(2024 10 13)   #(20 20)  :nbc)
    #( (:BUF . :NYJ)  #(2024 10 14)   #(20 15)  :espn) )

  ;; Week 7
  ( #( (:DEN . :NO )  #(2024 10 17)   #(20 15)  :prime)
    #( (:NE  . :JAX)  #(2024 10 20)   #( 9 30)  :nfl)
    #( (:SEA . :ATL)  #(2024 10 20)   #(13 00)  :fox)
    #( (:TEN . :BUF)  #(2024 10 20)   #(13 00)  :cbs)
    #( (:CIN . :CLE)  #(2024 10 20)   #(13 00)  :cbs)
    #( (:HOU . :GB )  #(2024 10 20)   #(13 00)  :cbs)
    #( (:MIA . :IND)  #(2024 10 20)   #(13 00)  :fox)
    #( (:DET . :MIN)  #(2024 10 20)   #(13 00)  :fox)
    #( (:PHI . :NYG)  #(2024 10 20)   #(13 00)  :fox)
    #( (:LV  . :LA )  #(2024 10 20)   #(16 05)  :cbs)
    #( (:CAR . :WAS)  #(2024 10 20)   #(16 05)  :cbs)
    #( (:KC  . :SF )  #(2024 10 20)   #(16 25)  :fox)
    #( (:NYJ . :PIT)  #(2024 10 20)   #(16 25)  :nbc)
    #( (:BAL . :TB )  #(2024 10 21)   #(20 20)  :espn)
    #( (:LAC . :ARI)  #(2024 10 21)   #(21 00)  :espn+) )
    
  ;; Week 8
  ( #( (:MIN . :LA )  #(2024 10 24)   #(20 15)  :prime)
    #( (:BAL . :CLE)  #(2024 10 27)   #(13 00)  :cbs)
    #( (:TEN . :DET)  #(2024 10 27)   #(13 00)  :fox)
    #( (:IND . :HOU)  #(2024 10 27)   #(13 00)  :cbs)
    #( (:GB  . :JAX)  #(2024 10 27)   #(13 00)  :fox)
    #( (:ARI . :MIA)  #(2024 10 27)   #(13 00)  :fox)
    #( (:NYJ . :NE )  #(2024 10 27)   #(13 00)  :cbs)
    #( (:ATL . :TB )  #(2024 10 27)   #(13 00)  :fox)
    #( (:CHI . :WAS)  #(2024 10 27)   #(13 00)  :cbs)
    #( (:NO  . :LAC)  #(2024 10 27)   #(16 05)  :fox)
    #( (:NE  . :SEA)  #(2024 10 27)   #(16 05)  :fox)
    #( (:PHI . :CIN)  #(2024 10 27)   #(16 25)  :cbs)
    #( (:CAR . :DEN)  #(2024 10 27)   #(16 25)  :cbs)
    #( (:KC  . :LV )  #(2024 10 27)   #(16 25)  :cbs)
    #( (:DAL . :SF )  #(2024 10 27)   #(16 25)  :nbc)
    #( (:NYG . :PIT)  #(2024 10 28)   #(20 15)  (:espn :abc)) )

  ;; Week 9
  ( #( (:HOU . :NYJ)  #(2024 10 31)   #(20 15)  :prime)
    #( (:DAL . :ATL)  #(2024 11  3)   #(13 00)  :fox)
    #( (:DEN . :BAL)  #(2024 11  3)   #(13 00)  :cbs)
    #( (:MIA . :BUF)  #(2024 11  3)   #(13 00)  :cbs)
    #( (:NO  . :CAR)  #(2024 11  3)   #(13 00)  :cbs)
    #( (:LV  . :CIN)  #(2024 11  3)   #(13 00)  :fox)
    #( (:LAC . :CLE)  #(2024 11  3)   #(13 00)  :cbs)
    #( (:IND . :MIN)  #(2024 11  3)   #(13 00)  :cbs)
    #( (:WAS . :NYG)  #(2024 11  3)   #(13 00)  :fox)
    #( (:NE  . :TEN)  #(2024 11  3)   #(13 00)  :fox)
    #( (:CHI . :ARI)  #(2024 11  3)   #(16 05)  :cbs)
    #( (:DET . :GB )  #(2024 11  3)   #(16 25)  :fox)
    #( (:LA  . :SEA)  #(2024 11  3)   #(16 25)  :fox)
    #( (:JAX . :PHI)  #(2024 11  3)   #(20 20)  :nbc)
    #( (:TB  . :KC )  #(2024 11  4)   #(20 15)  :espn) )

  ;; Week 10
  ( #( (:CIN . :BAL)  #(2024 11  7)   #(20 15)  :prime)
    #( (:NYG . :CAR)  #(2024 11 10)   #( 9 30)  :nfl)
    #( (:NE  . :CHI)  #(2024 11 10)   #(13 00)  :fox)
    #( (:BUF . :IND)  #(2024 11 10)   #(13 00)  :cbs)
    #( (:MIN . :JAX)  #(2024 11 10)   #(13 00)  :fox)
    #( (:DEN . :KC )  #(2024 11 10)   #(13 00)  :cbs)
    #( (:ATL . :NO )  #(2024 11 10)   #(13 00)  :fox)
    #( (:SF  . :TB )  #(2024 11 10)   #(13 00)  :fox)
    #( (:PIT . :WAS)  #(2024 11 10)   #(13 00)  :cbs)
    #( (:TEN . :LAC)  #(2024 11 10)   #(16 05)  :fox)
    #( (:NYJ . :ARI)  #(2024 11 10)   #(16 25)  :cbs)
    #( (:PHI . :DAL)  #(2024 11 10)   #(16 25)  :cbs)
    #( (:DET . :HOU)  #(2024 11 10)   #(20 20)  :nbc)
    #( (:MIA . :LA )  #(2024 11 11)   #(20 15)  :espn) )
   
  ;; Week 11
  ( #( (:WAS . :PHI)  #(2024 11 14)   #(20 15)  :prime)
    #( (:GB  . :CHI)  #(2024 11 17)   #(13 00)  :fox)
    #( (:JAX . :DET)  #(2024 11 17)   #(13 00)  :fox)
    #( (:LV  . :MIA)  #(2024 11 17)   #(13 00)  :fox)
    #( (:LA  . :NE )  #(2024 11 17)   #(13 00)  :fox)
    #( (:CLE . :NO )  #(2024 11 17)   #(13 00)  :fox)
    #( (:BAL . :PIT)  #(2024 11 17)   #(13 00)  :fox)
    #( (:MIN . :TEN)  #(2024 11 17)   #(13 00)  :fox)
    #( (:ATL . :DEN)  #(2024 11 17)   #(16 05)  :fox)
    #( (:SEA . :SF )  #(2024 11 17)   #(16 05)  :fox)
    #( (:KC  . :BUF)  #(2024 11 17)   #(16 25)  :cbs)
    #( (:CIN . :LAC)  #(2024 11 17)   #(16 25)  :cbs)
    #( (:IND . :NYJ)  #(2024 11 17)   #(20 20)  :nbc)
    #( (:HOU . :DAL)  #(2024 11 18)   #(20 15)  :espn) )
   
  ;; Week 12
  ( #( (:PIT . :CLE)  #(2024 11 21)   #(20 15)  :prime)
    #( (:KC  . :CAR)  #(2024 11 24)   #(13 00)  :cbs)
    #( (:MIN . :CHI)  #(2024 11 24)   #(13 00)  :fox)
    #( (:TEN . :HOU)  #(2024 11 24)   #(13 00)  :cbs)
    #( (:DET . :IND)  #(2024 11 24)   #(13 00)  :fox)
    #( (:NE  . :MIA)  #(2024 11 24)   #(13 00)  :cbs)
    #( (:TB  . :NYG)  #(2024 11 24)   #(13 00)  :cbs)
    #( (:DAL . :WAS)  #(2024 11 24)   #(13 00)  :fox)
    #( (:DEN . :LV )  #(2024 11 24)   #(16 05)  :cbs)
    #( (:SF  . :GB )  #(2024 11 24)   #(16 25)  :fox)
    #( (:ARI . :SEA)  #(2024 11 24)   #(16 25)  :fox)
    #( (:PHI . :LA )  #(2024 11 24)   #(20 20)  :nbc)
    #( (:BAL . :LAC)  #(2024 11 25)   #(20 15)  :espn) )
   
  ;; Week 13
  ( #( (:CHI . :DET)  #(2024 11 28)   #(12 30)  :cbs) ;; Thanksgiving
    #( (:NYG . :DAL)  #(2024 11 28)   #(16 30)  :fox) ;; Thanksgiving
    #( (:MIA . :GB )  #(2024 11 28)   #(20 20)  :nbc) ;; Thanksgiving
    #( (:LV  . :KC )  #(2024 11 29)   #(15 00)  :prime)
    #( (:LAC . :ATL)  #(2024 12  1)   #(13 00)  :cbs)
    #( (:PIT . :CIN)  #(2024 12  1)   #(13 00)  :cbs)
    #( (:HOU . :JAX)  #(2024 12  1)   #(13 00)  :fox)
    #( (:ARI . :MIN)  #(2024 12  1)   #(13 00)  :fox)
    #( (:IND . :NE )  #(2024 12  1)   #(13 00)  :cbs)
    #( (:SEA . :NYJ)  #(2024 12  1)   #(13 00)  :fox)
    #( (:TEN . :WAS)  #(2024 12  1)   #(13 00)  :cbs)
    #( (:TB  . :CAR)  #(2024 12  1)   #(16 05)  :fox)
    #( (:LA  . :NO )  #(2024 12  1)   #(16 05)  :fox)
    #( (:PHI . :BAL)  #(2024 12  1)   #(16 25)  :cbs)
    #( (:SF  . :BUF)  #(2024 12  1)   #(20 20)  :nbc)
    #( (:CLE . :DEN)  #(2024 12  2)   #(20 15)  :espn) )

  ;; Week 14
  ( #( (:GB  . :DET)  #(2024 11  7)   #(20 15)  :prime)
    #( (:NYJ . :MIA)  #(2024 11 10)   #(13 00)  :cbs)
    #( (:ATL . :MIN)  #(2024 11 10)   #(13 00)  :fox)
    #( (:NO  . :NYG)  #(2024 11 10)   #(13 00)  :fox)
    #( (:CAR . :PHI)  #(2024 11 10)   #(13 00)  :fox)
    #( (:CLE . :PIT)  #(2024 11 10)   #(13 00)  :cbs)
    #( (:LV  . :TB )  #(2024 11 10)   #(13 00)  :cbs)
    #( (:JAX . :TEN)  #(2024 11 10)   #(13 00)  :cbs)
    #( (:SEA . :ARI)  #(2024 11 10)   #(16 05)  :cbs)
    #( (:BUF . :LA )  #(2024 11 10)   #(16 25)  :fox)
    #( (:CHI . :SF )  #(2024 11 10)   #(16 25)  :fox)
    #( (:LAC . :KC )  #(2024 11 10)   #(20 20)  :nbc)
    #( (:CIN . :DAL)  #(2024 11 11)   #(20 15)  (:espn :abc)) )

  ;; Week 15
  ( #( (:LA  . :SF )  #(2024 12 12)   #(20 15)  :prime)
    #( (:DAL . :CAR)  #(2024 12 15)   #(13 00)  :fox)
    #( (:KC  . :CLE)  #(2024 12 15)   #(13 00)  :cbs)
    #( (:MIA . :HOU)  #(2024 12 15)   #(13 00)  :cbs)
    #( (:NYJ . :JAX)  #(2024 12 15)   #(13 00)  :fox)
    #( (:WAS . :NO )  #(2024 12 15)   #(13 00)  :fox)
    #( (:BAL . :NYG)  #(2024 12 15)   #(13 00)  :cbs)
    #( (:CIN . :TEN)  #(2024 12 15)   #(13 00)  :fox)
    #( (:NE  . :ARI)  #(2024 12 15)   #(16 25)  :cbs)
    #( (:IND . :DEN)  #(2024 12 15)   #(16 25)  :cbs)
    #( (:BUF . :DET)  #(2024 12 15)   #(16 25)  :cbs)
    #( (:TB  . :LAC)  #(2024 12 15)   #(16 25)  :fox)
    #( (:PIT . :PHI)  #(2024 12 15)   #(16 25)  :fox)
    #( (:GB  . :SEA)  #(2024 12 15)   #(20 20)  :nbc)
    #( (:CHI . :MIN)  #(2024 12 16)   #(20 00)  :abc)
    #( (:ATL . :LV )  #(2024 12 16)   #(20 35)  :espn) )

  ;; Week 16
  ( #( (:CLE . :CIN)  #(2024 12 19)   #(20 15)  :prime)
    #( (:HOU . :KC )  #(2024 12 21)   #(13 00)  :nbc)
    #( (:PIT . :BAL)  #(2024 12 21)   #(16 30)  :fox)
    #( (:NYG . :ATL)  #(2024 12 22)   #(13 00)  :fox)
    #( (:NE  . :NE )  #(2024 12 22)   #(13 00)  :cbs)
    #( (:ARI . :CAR)  #(2024 12 22)   #(13 00)  :fox)
    #( (:DET . :CHI)  #(2024 12 22)   #(13 00)  :fox)
    #( (:TEN . :IND)  #(2024 12 22)   #(13 00)  :cbs)
    #( (:LA  . :NYJ)  #(2024 12 22)   #(13 00)  :cbs)
    #( (:PHI . :WAS)  #(2024 12 22)   #(13 00)  :fox)
    #( (:DEN . :LAC)  #(2024 12 22)   #(16 05)  :fox)
    #( (:MIN . :SEA)  #(2024 12 22)   #(16 05)  :fox)
    #( (:JAX . :LV )  #(2024 12 22)   #(16 25)  :cbs)
    #( (:SF  . :MIA)  #(2024 12 22)   #(16 25)  :cbs)
    #( (:TB  . :DAL)  #(2024 12 22)   #(20 20)  :nbc)
    #( (:NO  . :GB )  #(2024 12 23)   #(20 15)  :espn) )

  ;; Week 17
  ( #( (:KC  . :PIT)  #(2024 12 25)   #(13 05)  :netflix)
    #( (:BAL . :HOU)  #(2024 12 25)   #(16 30)  :netflix)
    #( (:SEA . :CHI)  #(2024 12 26)   #(20 15)  :prime)
    #( (:NYJ . :BUF)  #(2024 12 29)   #(13 00)  :cbs)
    #( (:TEN . :JAX)  #(2024 12 29)   #(13 00)  :cbs)
    #( (:GB  . :MIN)  #(2024 12 29)   #(13 00)  :fox)
    #( (:LV  . :NO )  #(2024 12 29)   #(13 00)  :fox)
    #( (:CAR . :TB )  #(2024 12 29)   #(13 00)  :cbs)
    #( (:DAL . :PHI)  #(2024 12 29)   #(16 25)  :fox)
    #( (:MIA . :CLE)  #(2024 12 29)   #(20 20)  :nbc)
    #( (:DET . :SF )  #(2024 12 30)   #(20 15)  :espn)
    #( (:DEN . :CIN)  nil             nil       nil)
    #( (:ARI . :LA )  nil             nil       nil)
    #( (:LAC . :NE )  nil             nil       nil)
    #( (:IND . :NYG)  nil             nil       nil)
    #( (:ATL . :WAS)  nil             nil       nil) )

  ;; Week 18
  ( #( (:SF  . :ARI)  nil             nil       nil)
    #( (:CAR . :ATL)  nil             nil       nil)
    #( (:CLE . :BAL)  nil             nil       nil)
    #( (:WAS . :DAL)  nil             nil       nil)
    #( (:KC  . :DEN)  nil             nil       nil)
    #( (:MIN . :DET)  nil             nil       nil)
    #( (:CHI . :GB )  nil             nil       nil)
    #( (:JAX . :IND)  nil             nil       nil)
    #( (:SEA . :LA )  nil             nil       nil)
    #( (:LAC . :LV )  nil             nil       nil)
    #( (:BUF . :NE )  nil             nil       nil)
    #( (:MIA . :NYJ)  nil             nil       nil)
    #( (:NYG . :PHI)  nil             nil       nil)
    #( (:CIN . :PIT)  nil             nil       nil)
    #( (:NO  . :TB )  nil             nil       nil)
    #( (:HOU . :TEN)  nil             nil       nil) )))

