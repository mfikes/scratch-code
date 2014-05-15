;; This code was worked out in a group to start solving a small
;; portion of a Viennese maze, specifically the 4 top left nodes (a, b, e, f)
;; of https://i.imgur.com/j2gWurM.jpg
;;
;; The code below can get the bicyclist home... ignoring important constraints.
;; It lacks error handling, name spacing, and pull requests to add all that.

;; it's a map! it's a function!
(def next-light
  {:g :y
   :y :r
   :r :g})

(def world
  {:location :a
   :nodes {#{:a :b} :y
           #{:a :e} :g
           #{:b :f} :g
           #{:f :e} :g}})

(defn map-vals
  [m f]
  (reduce-kv (fn [a k v] (assoc a k (f v))) {} m))

(defn move-to [world new-location]
  (-> world
      (assoc
          :last-location (:location world)
          :location       new-location)
      (update-in
       [:nodes]
       (fn [nodes] (map-vals nodes next-light)))))
