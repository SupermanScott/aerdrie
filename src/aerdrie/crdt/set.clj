(ns aerdrie.crdt.set
  (:use clojure.set))
(defrecord set-member [value timestamp])
(defrecord lww-set [added removed])

(defn create-lww-set
  "Creates a new set"
  []
  (atom (->lww-set #{} #{})))

(defn lookup-lww-set
  "Returns non-nil if the value is in the set"
  [set value]
  (let [set-value @set
        added (some #(when (= (:value %) value) %) (:added set-value))
        removed (some #(when (= (:value %) value) %) (:removed set-value))]
    (cond
     (not added) false
     (not removed) true
     (< (:timestamp removed) (:timestamp added)) true
     :else false
     )))

(defn add-lww-set
  "Add the value to the set"
  [set value]
  (let [member (->set-member value (System/currentTimeMillis))]
    (swap! set #(assoc % :added (conj (:added %) member)))))

(defn remove-lww-set
  "Remove the value from the set"
  [set value]
  (when (lookup-lww-set set value)
    (let [member (->set-member value (System/currentTimeMillis))]
      (swap! set #(assoc % :removed (conj (:removed %) member))))))

(defn merge-lww-set
  "Merges multiple versions of the set into one atom"
  [& sets]
  (let [added (apply union (map #(:added @%) sets))
        removed (apply union (map #(:removed @%) sets))]
    (atom (->lww-set added removed))))
