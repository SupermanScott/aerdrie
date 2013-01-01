(ns aerdrie.crdt.set
  (:use clojure.set))
(defprotocol CRDTSet
  (lookup [this member-id])
  (remove-value [this member-id])
  (realized-value [this])
  (add-value [this member-id value])
)

(defrecord set-member [member-id timestamp value])
(defrecord lww-set [added removed]
  CRDTSet
  (lookup [this member-id]
    (let [query #(= member-id (:member-id %))
          matching-add (filter query (:added this))
          matching-remove (filter query (:removed this))
          added (first (sort-by :timestamp > matching-add))
          removed (first (sort-by :timestamp > matching-remove))]
          (cond
           (not added) nil
           (not removed) added
           (< (:timestamp removed) (:timestamp added)) added
           :else nil
           )))
  (remove-value [this member-id]
    (if (.lookup this member-id)
      (let [member (->set-member member-id (System/currentTimeMillis) true)]
        (assoc this :removed (conj (:removed this) member)))
      this))
  (realized-value [this]
    (filter #(= (.lookup this (:member-id %)) %) (:added this)))
  (add-value [this member-id value]
    (let [member (->set-member member-id (System/currentTimeMillis) value)]
      (assoc this :added (conj (:added this) member)))))

(defn create-lww-set
  "Creates a new set"
  []
  (atom (->lww-set #{} #{})))

(defn <-score
  "Compares two set-members and takes smallest"
  [x y]
  (let [c (compare (:value x) (:value y))]
    (if (not= c 0)
      c
      (compare (:timestamp x) (:timestamp y)))))

(defrecord lww-sorted-set [added removed]
  CRDTSet
  (lookup [this member-id]
    (let [query #(= member-id (:member-id %))
          matching-add (filter query (:added this))
          matching-remove (filter query (:removed this))
          added (first (sort-by :timestamp > matching-add))
          removed (first (sort-by :timestamp > matching-remove))]
          (cond
           (not added) nil
           (not removed) added
           (< (:timestamp removed) (:timestamp added)) added
           :else nil
           )))
  (remove-value [this member-id]
    (if (.lookup this member-id)
      (let [member (->set-member member-id (System/currentTimeMillis) true)]
        (assoc this :removed (conj (:removed this) member)))
      this))
  (realized-value [this]
    (filter #(= (.lookup this (:member-id %)) %) (:added this)))
  (add-value [this member-id score]
    (when (.lookup this member-id)
      (.remove-value this member-id))
    (let [member (->set-member member-id (System/currentTimeMillis) score)
          all-members (conj (:added this) member)
          sorted-members (apply sorted-set-by <-score all-members)]
      (assoc this :added sorted-members)
      )))

(defn create-sorted-set
  "Starts a new sorted set"
  []
  (atom (->lww-sorted-set #{} #{})))

(defn lookup-set
  "Returns non-nil if the member-id is in the set"
  [set member-id]
  (let [set-value @set]
    (.lookup set-value member-id)))

(defn add-set
  "Add the member-id to the set"
  ([set member-id]
     (add-set set member-id true))
  ([set member-id value]
     (swap! set #(.add-value % member-id value))))

(defn remove-set
  "Remove the member-id from the set"
  [set member-id]
  (swap! set #(.remove-value % member-id)))

(defn realized-set-value
  "Return the full realized value of the set as a lazy sequence"
  [set]
  (.realized-value @set))

(defn merge-set
  "Merges multiple versions of the set into one atom"
  [& sets]
  (let [added (apply union (map #(:added @%) sets))
        removed (apply union (map #(:removed @%) sets))]
    (atom (->lww-set added removed))))
