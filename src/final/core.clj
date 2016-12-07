(ns final.core)

(def objects '(whiskey-bottle bucket frog chain))

(def world {
   'living-room '("you are in the living room of a wizards house. there is a wizard snoring loudly on the couch. "
                  (west door garden)
                  (upstairs stairway attic))
   'garden '("you are in a beautiful garden. there is a well in front of you. "
             (east door living-room))
   'attic '("you are in the attic of the wizards house. there is a giant welding torch in the corner. "
            (downstairs stairway living-room))})

(def obj-locations {'whiskey-bottle 'living-room
                    'bucket 'living-room
                    'chain 'garden
                    'frog 'garden})

(def location 'living-room)

(defn describe-loc [loc loc-map]
	(str (first (loc loc-map))))

(defn describe-path [path]
  (str "there is a " (str (second path)) " going " (str (first path)) " from here. "))

(defn describe-paths [location world]
  ;(get world location) need elements after first one, path(s), using rest
  ;rest gets everything after first element
  ;map applies describe-path to all elements of list
  (apply str (map describe-path (rest (get world location)))))

(defn is-at? [object object-location locations]
  (= (object locations) object-location))

(defn describe-floor [obj-location objects all-locations]
  (apply str (map (fn [x] 
                    (str "you see a " (str x) " on the floor. "))
                (filter (fn [x] (is-at? x obj-location all-locations))
                      objects))))

(defn look []
  (str (describe-loc location world)
    (describe-paths location world)
    (describe-floor location objects obj-locations)))


(defn walk-direction [direction]
  (let [next (first (filter (fn[x] (= direction (first x)))
                                    (rest (location world))))]
        (cond next
              (do 
                (def location (nth next 2))
                (look))
          :else (str "you can't go that way."))))

(defmacro defspel [& rest] `(defmacro ~@rest))

(defspel walk [direction]
  `(walk-direction '~direction))

(defn pickup-object [object]
  (cond (is-at? object location obj-locations)
            (do
              ;return obj-locations with key-value pair (object 'body) added
              (def obj-locations (assoc obj-locations object 'body))
              (str "you are now carrying the " (str object) "."))
        (is-at? object 'body obj-locations)
            (str "you're already carrying the " (str object) ".")
        :else "you can't get that."))

(defspel pickup [object]
  `(pickup-object '~object))

(defn inventory [] 
  (filter (fn[x] (is-at? x 'body obj-locations))
          objects))

(defn is-holding? [object]
  (cond
    (nil? (some #{object} (inventory)))
    false
    :else true))

(defn drop-object [object]
  (cond (is-holding? object)
    (do
      (def obj-locations (assoc obj-locations object location))
      (str "you dropped the " (str object) " in the " (str location) "."))
    :else (str "you are not holding the " (str object) ".")))

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
    :else (str "I can't " (str 'command) " like that."))))

(game-action weld chain bucket attic
  (cond
    (and (is-holding? 'bucket)
      (def chain-welded true))
    (str "the chain is now securely welded to the bucket.")

    :else (str "you don't have the bucket.")))

(game-action dunk bucket well garden
  (cond 
    chain-welded 
        (do 
          (def bucket-filled true)
          (str "the bucket is now filled with water."))
    :else (str "the water level is too low to reach.")))

(game-action splash bucket wizard living-room
  (cond
    (not bucket-filled)
    (str "the bucket has nothing in it")

    (is-holding? 'frog)
    (str "the wizard awakens and sees that you stole his frog. he is so upset he banishes you to the netherworlds- you lose! the end.")

    :else (str "the wizard awakens from his slumber and greets you warmly. he hands you the magic low-carb donut- you win! the end.")))


(defn -main []
  (println (look))
  (println)
  (println "PICKUP: " (pickup bucket))
  (println)
  (println "WALK: " (walk west))
  (println)
  (println "PICKUP: " (pickup frog))
  (println)
  (println "PICKUP: " (pickup chain))
  (println)
  (println "DROP: " (drop-object 'frog))
  (println)
  (println "WALK: " (walk east))
  (println)
  (println "WALK: " (walk upstairs))
  (println)
  (println "WELD: " (weld chain bucket))
  (println)
  (println "WALK: " (walk downstairs))
  (println)
  (println "WALK: " (walk west))
  (println)
  (println "DUNK: " (dunk bucket well))
  (println)
  (println "WALK: " (walk east))
  (println)
  (println (splash bucket wizard))
  )



