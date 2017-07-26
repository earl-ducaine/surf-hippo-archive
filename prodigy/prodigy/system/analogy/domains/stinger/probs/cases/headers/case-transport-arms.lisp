
(setf result '(
   (time 0.7)
   (nodes 29)
   (exhaustedp nil)
   (solutionp t)
   (solution-length 6)
   (solution ((immovable-object monument) (pick-up researcher stinger1 airbase) (pick-up researcher luggage1 airbase) (put-in researcher stinger1 luggage1)
              (can-fly-domestic researcher usa airbase whitesands hartsfield atlanta) (fly researcher airbase whitesands hartsfield atlanta)
              (take-out researcher stinger1 luggage1)))))

(setf problem-solved 
   "/afs/cs.cmu.edu/user/centaur/Research/Prodigy/analogy/domains/stinger/probs/transport-arms")
(setf goal '((holding researcher stinger1) (in-city-p researcher atlanta)))

(setf case-objects '((usa country) (atlanta city) (hartsfield airport) (georgiatech location) (kinkos location) (postoffice location) (kingstavern location)
                     (boston city) (bostonport airport) (copyservice location) (washington city) (dulles airport) (statedept location) (mall location)
                     (whitesands city) (airbase airport) (uk country) (london city) (gatwick airport) (bigben location) (inverness city) (lochness location)
                     (greece country) (iraklion city) (tinyport airport) (forth location) (researcher person) (passport1 passport) (akr presentation)
                     (luggage1 luggage) (stinger1 stinger) (monument fixed)))

(setf insts-to-vars '(
   (usa . <country22>) 
   (atlanta . <city37>) 
   (hartsfield . <airport3>) 
   (georgiatech . <location99>) 
   (kinkos . <location2>) 
   (postoffice . <location37>) 
   (kingstavern . <location58>) 
   (boston . <city40>) 
   (bostonport . <airport36>) 
   (copyservice . <location98>) 
   (washington . <city16>) 
   (dulles . <airport19>) 
   (statedept . <location92>) 
   (mall . <location85>) 
   (whitesands . <city1>) 
   (airbase . <airport60>) 
   (uk . <country76>) 
   (london . <city37>) 
   (gatwick . <airport44>) 
   (bigben . <location30>) 
   (inverness . <city54>) 
   (lochness . <location24>) 
   (greece . <country54>) 
   (iraklion . <city43>) 
   (tinyport . <airport89>) 
   (forth . <location76>) 
   (researcher . <person85>) 
   (passport1 . <passport36>) 
   (akr . <presentation39>) 
   (luggage1 . <luggage75>) 
   (stinger1 . <stinger83>) 
   (monument . <fixed87>) 
))

(setf footprint-by-goal '(
   ((in-city-p researcher atlanta) (at-loc-p researcher airbase) (at-loc-o stinger1 airbase) (~ (immobile stinger1)) (nationality researcher usa)
    (in-city-l airbase whitesands) (in-city-p researcher whitesands) (in-country whitesands usa) (in-city-l hartsfield atlanta) (in-country atlanta usa)
    (~ (immobile luggage1)) (at-loc-o luggage1 airbase))
   ((holding researcher stinger1) (at-loc-p researcher airbase) (at-loc-o stinger1 airbase) (~ (immobile stinger1)) (~ (immobile luggage1))
    (at-loc-o luggage1 airbase))))