(ns the-divine-cheese-code.visualization.svg
  (:require [clojure.string :as s])
  (:refer-clojure :exclude [min max]))

(defn latlng->point
  "Convert lat/lng map to comma-separated string"
  [latlng]
  (str (:lng latlng) "," (:lat latlng)))

(defn points
  "Convert seq of lat/long points to comma-separated string"
  [locs]
  (s/join " " (map latlng->point locs)))

(defn line
  "Generate an SVG line representation of a set of points"
  [points]
  (str "<polyline points=\"" points "\" />"))

;; This is really difficult to reason about without seeing an example:
;;
;;     (def maps [{:x 0 :y 1}, {:x 1 :y 2}, {:x 1 :y 4}])
;;     
;;     (defn min [a b] (if (< a b) a b)) ; return the smaller value
;;     (def min-comparator comparator-over-maps min [:x :y])
;;     (min-comparator maps)
;;     ;;=> {:x 0 :y 1}
;;     
;;     (defn max [a b] (if (> a b) a b)) ; return the larger value
;;     (def max-comparator comparator-over-maps max [:x :y])
;;     (max-comparator maps)
;;     ;;=> {:x 1 :y 4}
;;
;; This example doesn't even work, since comparison-fn needs to accept
;; a variadic argument (it's expected to process _all_ values in
;; a single call), but it illustrates the high-level concept.
(defn comparator-over-maps
  "Given a comparison function and a seq of keys, return a function
   that accepts a seq of maps and returns a map with each key present
   in the original seq of keys, where the value is the result of
   applying the comparison function to a seq of every value present
   for that key in the seq of maps."
  [comparison-fn ks]
  (fn [maps]
    (zipmap ks
            (map (fn [k] (apply comparison-fn (map k maps)))
                 ks))))

(def min (comparator-over-maps clojure.core/min [:lat :lng]))

(def max (comparator-over-maps clojure.core/max [:lat :lng]))

(defn translate-to-00
  "Determine the lowest point (both lat and lng) and translate
   all locations so that that point is treated as the origin (0, 0)."
  [locs]
  (let [mincoords (min locs)]
    (map #(merge-with - % mincoords) locs)))

(defn scale
  "Determine the highest point (furthest from the origin, both
   lat and lng) and scale all locations so that that the highest
   lat = height and highest lng = width."
  [width height locs]
  (let [maxcoords (max locs)
        ratio {:lat (/ height (:lat maxcoords))
               :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locs)))

(defn normalize
  "Normalize the given locations by adjusting the origin to (0, 0)
   and scaling all points within (0, 0) and (width, height)."
  [width height locs]
  (->> locs
       translate-to-00
       (scale width height)))

(defn xml
  "SVG template that flips the coordinate system to accomodate
  the difference between SVG coord space and cartesian"
  [width height locs]
  (str "<svg height=\"" height "\" width=\"" width "\">"
       ;; These two <g> tags flip the coordinate system
       "<g transform=\"translate(0," height ")\">"
       "<g transform=\"scale(1,-1)\">"
       ;; ---------------------------------------------
       (->> locs
            (normalize width height)
            points
            line)
       ;; ---------------------------------------------
       "</g>"
       "</g>"
       "</svg>"))
