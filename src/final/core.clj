(ns final.core
  (:gen-class))

(def objects '(whiskey-bottle bucket frog chain))

(def world {
   'living-room '("you are in the living room of a wizards house. there is a wizard snoring loudly on the couch. "
                  (west door garden)
                  (upstairs stairway attic))
   'garden '("you are in a beautiful garden. there is a well in front of you. "
             (east door living-room))
   'attic '("you are in the attic of the wizards house. there is a giant welding torch in the corner. "
            (downstairs stairway living-room))
   })

(def obj-locations {'whiskey-bottle 'living-room
                    'bucket 'living-room
                    'chain 'garden
                    'frog 'garden
  })

(def picked-up {'whiskey-bottle 0
                'bucket 0
                'chain 0
                'frog 0
  })

(def location 'living-room)

(defn describe-loc [loc loc-map]
	(str (first (loc loc-map))))

(defn describe-path [path]
  ;second element in path is mode of transport, ex. door or staircase
  ;first element is direction
  ;defined in world map
  (str "there is a " (str (second path)) " going " (str (first path)) " from here. "))

(defn describe-paths [location world]
  ;(get world location) need elements after first one, path(s), using rest
  ;rest gets everything after first element
  ;map applies describe-path to all elements of list
  (apply str (map describe-path (rest (get world location)))))

;checks locations map for key value pair object-obj location
(defn is-at? [object object-location locations]
  (= (object locations) object-location))

(defn describe-floor [obj-location objects all-locations]
  ;apply string to return of map
  ;map applies anonymous function to return of filter
  ;filter returns only objects that is-at? current location
  ;can be more than one string
  (apply str (map (fn [x] 
                    (str "you see a " (str x) " on the floor. "))
                (filter (fn [x] (is-at? x obj-location all-locations))
                      objects))))

(defn look []
  ;concatenate return strings of 3 functions
  (str (describe-loc location world)
    (describe-paths location world)
    (describe-floor location objects obj-locations)))


(defn walk-direction [direction]
  ;assign next to first location in world that has direction matching given direction
  ;filter returns all locations matching given direction
  (let [next (first (filter (fn[x] (= direction (first x)))
                                    (rest (location world))))]
        (cond next
              ;if next not nil
              ;redefine location to next, where person is moving to
              ;then execute look function
              (do
                (def location (nth next 2))
                (look))
          :else "you can't go that way.")))

;easier way to call functions
;telling it to put quote in front of passed argument
;quote = don't evaluate, return data structure
;saying it's not a function call
(defmacro defspel [& other] `(defmacro ~@other))

(defspel walk [direction]
  `(walk-direction '~direction))

(defn pickup-object [object]
  (cond (is-at? object location obj-locations)
            (do
              ;redefine obj-locations with key-value pair (object 'body) added
              (def obj-locations (assoc obj-locations object 'body))
              ;increase count associated with object picked up
              (def picked-up (assoc picked-up object (inc (get picked-up object))))
              (str "you are now carrying the " (str object) "."))
        ;if location of object being picked up is body
        (is-at? object 'body obj-locations)
            (str "you're already carrying the " (str object) ".")
        :else "you can't get that."))

(defspel pickup [object]
  `(pickup-object '~object))

;print number of times item picked up using picked-up map
(defn get-count [object]
  (str "you've picked up the " (str object) " " (str (get picked-up object)) " time(s)."))

(defn inventory [] 
  ;filter objects in obj-locations based on where they are
  ;return objects where location = body
  (filter (fn[x] (is-at? x 'body obj-locations))
          objects))

(defn is-holding? [object]
  (cond
    ;some #{object} returns value that is in inventory
    ;return false if nothing returns
    (nil? (some #{object} (inventory)))
    false
    :else true))

;remove object from inventory (body)
;leave in current location, update location in obj-locations
(defn drop-object [object]
  (cond (is-holding? object)
    (do
      (def obj-locations (assoc obj-locations object location))
      (str "you dropped the " (str object) " in the " (str location) "."))
    :else (str "you are not holding the " (str object) ".")))

;prints where all 4 objects are currently located
;updated locations if you have moved any from original positions
;different sentence structure for objects being held
(defn show-locations [objects]
  (apply str (map (fn [x] 
                    ;if it isn't being held
                    (cond (not (is-holding? x))
                      (str "the " (str x) " is in the " (str (get obj-locations x)) ". ")
                      :else (str "you are holding the " (str x) ". "))
                    )
                objects)))

(def chain-welded false)
(def bucket-filled false)

(defspel game-action [command subj obj place & rest]
  `(defspel ~command [subject# object#]
    `(cond
        (and (= location '~'~place)
          (= '~subject# '~'~subj)
          (= '~object# '~'~obj)
          (is-holding? '~'~subj))
        ~@'~rest
    :else (str "you cannot " (str 'command) " like that."))))

(game-action weld chain bucket attic
  (cond
    ;game-action macro only checks is-holding? for one object
    ;need to confirm is-holding? chain AND bucket
    (and (is-holding? 'bucket)
      (def chain-welded true))
      "the chain is now securely welded to the bucket."
    :else "you don't have the bucket."))

(game-action dunk bucket well garden
  (cond 
    ;game-action macro checks location, subject, object, is-holding
    ;only need to redefine bool and return string
    chain-welded 
        (do 
          (def bucket-filled true)
          "the bucket is now filled with water.")
    :else "the water level is too low to reach."))

(game-action splash bucket wizard living-room
  (cond
    ;bucket-filled = additional condition from game-action that must be met
    (not bucket-filled)
    "the bucket has nothing in it"

    (is-holding? 'frog)
    "the wizard awakens and sees that you stole his frog. he is so upset he banishes you to the netherworlds- you lose! the end."

    :else "the wizard awakens from his slumber and greets you warmly. he hands you the magic low-carb donut- you win! the end."))


(defn -main []
  (println (look))
  (newline)
  (println "PICKUP: " (pickup bucket))
  (newline)
  (println "WALK: " (walk west))
  (newline)
  (println "PICKUP: " (pickup frog))
  (newline)
  (println "PICKUP: " (pickup chain))
  (newline)
  (println "WALK: " (walk east))
  (newline)
  (println "DROP: " (drop-object 'frog))
  (newline)
  ;(println "PICKUP: " (pickup frog))
  (newline)
  (println "WALK: " (walk east))
  (newline)
  (println "WALK: " (walk upstairs))
  (newline)
  (println "WELD: " (weld chain bucket))
  (newline)
  (println "WALK: " (walk downstairs))
  (newline)
  (println "WALK: " (walk west))
  (newline)
  (println "DUNK: " (dunk bucket well))
  (newline)
  (println "COUNT: " (get-count 'frog))
  (newline)
  (println "WALK: " (walk east))
  (newline)
  (println (show-locations objects))
  (newline)
  (println (splash bucket wizard))
  )

