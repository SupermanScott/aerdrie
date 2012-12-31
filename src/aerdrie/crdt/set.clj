(ns aerdrie.crdt.set
  (:use clojure.set))
(defprotocol CRDTSet
  (lookup [this value])
  (remove-value [this value])
)

(defprotocol LWWSet
    (add-value [this value]))

(defprotocol SortedSet
  (add-sorted-value [this value score]))

(defrecord set-member [value timestamp])
(defrecord lww-set [added removed]
  CRDTSet
  (lookup [this value]
    (let [query #(= value (:value %))
          matching-add (filter query (:added this))
          matching-remove (filter query (:removed this))
          added (first (sort-by :timestamp > matching-add))
          removed (first (sort-by :timestamp > matching-remove))]
          (cond
           (not added) nil
           (not removed) true
           (< (:timestamp removed) (:timestamp added)) true
           :else nil
           )))
  (remove-value [this value]
    (if (.lookup this value)
      (let [member (->set-member value (System/currentTimeMillis))]
        (assoc this :removed (conj (:removed this) member)))
      this))
  LWWSet
  (add-value [this value]
    (let [member (->set-member value (System/currentTimeMillis))]
      (assoc this :added (conj (:added this) member)))))

(defn create-lww-set
  "Creates a new set"
  []
  (atom (->lww-set #{} #{})))

(defrecord sorted-set-member [value timestamp score])

(defn <-score
  "Compares two sorted-set-members and takes smallest"
  [x y]
  (let [c (compare (:score x) (:score y))]
    (if (not= c 0)
      c
      (compare (:timestamp x) (:timestamp y)))))

(defrecord lww-sorted-set [added removed]
  CRDTSet
  (lookup [this value]
    (let [added (some #(when (= (:value %) value) %) (:added this))
          removed (some #(when (= (:value %) value) %) (:removed this))]
          (cond
           (not added) nil
           (not removed) (:score added)
           (< (:timestamp removed) (:timestamp added)) (:score added)
           :else nil
           )))
  (remove-value [this value]
    (if (.lookup this value)
      (let [member (->set-member value (System/currentTimeMillis))]
        (assoc this :removed (conj (:removed this) member)))
      this))
  SortedSet
  (add-sorted-value [this value score]
    (when (.lookup this value)
      (.remove-value this value))
    (let [member (->sorted-set-member value (System/currentTimeMillis) score)
          all-members (conj (:added this) member)
          sorted-members (apply sorted-set-by <-score all-members)]
      (assoc this :added sorted-members)
      )))

(defn create-sorted-set
  "Starts a new sorted set"
  []
  (atom (->lww-sorted-set #{} #{})))

(defn lookup-set
  "Returns non-nil if the value is in the set"
  [set value]
  (let [set-value @set]
    (.lookup set-value value)))

(defn add-set
  "Add the value to the set"
  [set value]
  (swap! set #(.add-value % value)))

(defn add-sorted-set
  "Add the value to the sorted set"
  [set value score]
  (swap! set #(.add-sorted-value % value score)))

(defn remove-set
  "Remove the value from the set"
  [set value]
  (swap! set #(.remove-value % value)))

(defn merge-set
  "Merges multiple versions of the set into one atom"
  [& sets]
  (let [added (apply union (map #(:added @%) sets))
        removed (apply union (map #(:removed @%) sets))]
    (atom (->lww-set added removed))))
