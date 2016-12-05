(ns final.core)

(def objects '(whiskey-bottle bucket frog chain))

(def world {
   'living-room '((you are in the living room
                   of a wizards house. there is a wizard
                   snoring loudly on the couch.)
                  (west door garden)
                  (upstairs stairway attic))
   'garden '((you are in a beautiful garden.
              there is a well in front of you.)
             (east door living-room))
   'attic '((you are in the attic of the
             wizards house. there is a giant
             welding torch in the corner.)
            (downstairs stairway living-room))})

(def obj-locations {'whiskey-bottle 'living-room
        'bucket 'living-room
        'chain 'garden
        'frog 'garden})

(def location 'living-room)

(defn describe-loc [loc loc-map]
	(first (loc loc-map)))

(defn describe-path [path]
  (str "there is a " (str (second path)) " going " (str (first path)) " from here. "))

(defn spel-print [list] (map (fn [x] (symbol (name x))) list))

(defn describe-paths [location world]
  ;(get world location) need elements after first one, path(s), using rest
  ;rest gets everything after first element
  ;map applies describe-path to all elements of list
  (apply str (map describe-path (rest (get world location)))))

(defn is-at? [object object-location locations]
  (= (object locations) object-location))

(defn describe-floor [obj-location objects all-locations]
  (apply concat (map (fn [x] 
                    (spel-print `(you see a ~x on the floor.)))
                (filter (fn [x] (is-at? x obj-location all-locations))
                      objects))))

(defn look []
  (spel-print (concat (describe-loc location world)
    (describe-paths location world)
    (describe-floor location objects obj-locations))))


(defn walk-direction [direction]
  (let [next (first (filter (fn[x] (= direction (first x)))
                                    (rest (location world))))]
        (cond next
              (do 
                (def location (nth next 2))
                (look))
          :else (spel-print `(you can't go that way))))
  )

(defmacro defspel [& rest] `(defmacro ~@rest))

(defspel walk [direction]
  `(walk-direction '~direction))

(defn pickup-object [object]
  (cond (is-at? object location obj-locations)
            (do
              ;return obj-locations with key-value pair (object 'body) added
              (def obj-locations (assoc obj-locations object 'body))
              (spel-print `(you are now carrying the ~object)))
        (is-at? object 'body obj-locations)
            (spel-print `(you're already carrying the ~object))
        :else (spel-print `(you can't get that))))

(defn drop-object [object]
  (cond (is-holding? object)
    (do
      (def obj-locations (assoc obj-locations object location))
      (spel-print `(you dropped the ~object in the ~location)))
    :else "you are not holding the object"+'object))

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

    :else (spel-print '(I can't ~'~command like that)))))

(game-action weld chain bucket attic
  (cond
    (and (is-holding? 'bucket)
      (def chain-welded true))
    (spel-print '(the chain is now securely welded to the bucket))

    :else (spel-print '(you don't have the bucket))))

(game-action dunk bucket well garden
  (cond 
    chain-welded 
        (do 
          (def bucket-filled true)
          (spel-print '(the bucket is now filled with water)))
    :else (spel-print '(the water level is too low to reach))))

(game-action splash bucket wizard living-room
  (cond
    (not bucket-filled)
    (spel-print '(the bucket has nothing in it))

    (is-holding? 'frog)
    '(the wizard awakens and sees that you stole his frog. 
                  he is so upset he banishes you to the 
                  netherworlds- you lose! the end.)
    :else '(the wizard awakens from his slumber and greets you warmly. 
                  he hands you the magic low-carb donut- you win! the end.)))


(defn -main [])



