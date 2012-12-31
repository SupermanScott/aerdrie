(ns aerdrie.crdt.set
  (:use clojure.set))
(defprotocol CRDTSet
  (lookup [this value])
  (add-value [this value])
  (remove-value [this value])
)

(defrecord set-member [value timestamp])
(defrecord lww-set [added removed]
  CRDTSet
  (lookup [this value]
    (let [added (some #(when (= (:value %) value) %) (:added this))
          removed (some #(when (= (:value %) value) %) (:removed this))]
          (cond
           (not added) false
           (not removed) true
           (< (:timestamp removed) (:timestamp added)) true
           :else false
           )))

  (add-value [this value]
    (let [member (->set-member value (System/currentTimeMillis))]
      (assoc this :added (conj (:added this) member))))

  (remove-value [this value]
    (if (.lookup this value)
      (let [member (->set-member value (System/currentTimeMillis))]
        (assoc this :removed (conj (:removed this) member)))
      this)))

(defn create-lww-set
  "Creates a new set"
  []
  (atom (->lww-set #{} #{})))

(defn lookup-lww-set
  "Returns non-nil if the value is in the set"
  [set value]
  (let [set-value @set]
    (.lookup set-value value)))

(defn add-lww-set
  "Add the value to the set"
  [set value]
  (swap! set #(.add-value % value)))

(defn remove-lww-set
  "Remove the value from the set"
  [set value]
  (swap! set #(.remove-value % value)))

(defn merge-lww-set
  "Merges multiple versions of the set into one atom"
  [& sets]
  (let [added (apply union (map #(:added @%) sets))
        removed (apply union (map #(:removed @%) sets))]
    (atom (->lww-set added removed))))
