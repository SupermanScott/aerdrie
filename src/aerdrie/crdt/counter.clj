(ns aerdrie.crdt.counter)
(defrecord pn-counter [postive negative])

(defn create-pn-counter
  "Creates a new pn counter"
  []
  (atom (->pn-counter 0 0)))

(defn inc-pn-counter
  "Increments the counter atom"
  [counter]
  (swap! counter #(assoc % :postive (inc (:postive %)))))

(defn dec-pn-counter
  "Decrements the counter atom"
  [counter]
  (swap! counter #(assoc % :negative (inc (:negative %)))))

(defn value-pn-counter
  "Returns the value of counter atom"
  [counter]
  (let [c @counter]
    (- (:postive c) (:negative c))))

(defn merge-pn-counters
  "Merges the counters and produces a new counter atom"
  [& counters]
  (let [postive (apply max (map #(:postive @%) counters))
        negative (apply max (map #(:negative @%) counters))]
    (atom (->pn-counter postive negative))))