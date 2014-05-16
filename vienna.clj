;; This code was worked out in a group to start solving a small
;; portion of a Viennese maze, specifically the 4 top left nodes (a, b, e, f)
;; of the square maze at https://i.imgur.com/j2gWurM.jpg
;;
;; The code below can get the bicyclist home... ignoring important constraints.
;; It lacks error handling, name spacing, and pull requests to add all that.
;;
;; Blog post for more details on a Viennese Maze: http://zulko.github.io/blog/2014/04/27/viennese-mazes-what-they-are/

;; it's a map! it's a function!
(def next-light
  {:g :y
   :y :r
   :r :g})

(def world
  {:location :a
   :nodes    {#{:a :b} :y
              #{:a :e} :g
              #{:b :f} :g
              #{:f :e} :g}
   :home     :f})

(defn map-vals
  [m f]
  (reduce-kv (fn [a k v] (assoc a k (f v))) {} m))

(defn move-to [world new-location]
  (-> world
      (assoc
          :last-location (:location world)
                         :location new-location)
      (update-in
        [:nodes]
        (fn [nodes] (map-vals nodes next-light)))))

(defn adjacent-streets
  "Returns the set of streets adjacent to a given location in the world."
  [world location]
  (filter location (keys (:nodes world))))

(defn not-red?
  "Returns true iff a given street does not currently have a red light."
  [world street]
  (not= :r (get-in world [:nodes street])))

(defn candidate-streets
  "Returns the set of streets that it is legal ride onto given the current location.
  (These are the adjacent streets without red lights that don't return to previous location.)"
  [world]
  (let [non-red-streets (filter (partial not-red? world) (adjacent-streets world (:location world)))]
    (if (:last-location world)
      (filter #(not (contains? % (:last-location world))) non-red-streets)
      non-red-streets)))

(defn take-street
  "Takes a street, moving the rider to a new location via that street. Returns the new
  world state."
  [world street]
  (let [new-location (first (filter #(not= % (:location world)) street))]
    (move-to world new-location)))

(defn homeward?
  [world street]
  (when (contains? street (:home world))
    street))

(defn choose-street
  "Chooses a street to take."
  [world streets]
  (if-let [homeward (some (partial homeward? world) streets)]
    homeward
    (first streets)))                                       ; We simply choose the first street

(defn not-home?
  "Returns true iff the rider is in the home location in the world."
  [world]
  (not= (:location world) (:home world)))

(defn step-forward
  "Takes the world forward one iteration step. The set of candidate streets are identified,
  one of those streets is chosen, and then taken. After the home location is reached, nil
  will be returned."
  [world]
  (when (not-home? world)
    (take-street world (choose-street world (candidate-streets world)))))

(defn solution
  "Takes the inital state of the world and iterates to the solution."
  [world]
  (take-while identity (iterate step-forward world)))
