(ns aerdrie.counter-test
  (:use clojure.test
        aerdrie.crdt.counter))

(deftest inc-test
  (testing "Proper increments"
    (let [c (create-pn-counter)]
      (inc-pn-counter c)
      (is (= 1 (value-pn-counter c)))
      (dec-pn-counter c)
      (is (= 0 (value-pn-counter c)))
      (inc-pn-counter c)
      (is (= 1 (value-pn-counter c)))
      (inc-pn-counter c)
      (is (= 2 (value-pn-counter c)))
      (dec-pn-counter c)
      (is (= 1 (value-pn-counter c)))
      )))

(deftest merge-counter-test
  (testing "Merging counters together"
    (let [c (create-pn-counter)
          d (create-pn-counter)
          g (create-pn-counter)]
      (is (= 0 (value-pn-counter (merge-pn-counters c d g))))
      (inc-pn-counter c)
      (is (= 1 (value-pn-counter (merge-pn-counters c d g))))
      (inc-pn-counter d)
      (is (= 1 (value-pn-counter (merge-pn-counters c d g))))
      (inc-pn-counter d)
      (is (= 2 (value-pn-counter (merge-pn-counters c d g))))
      (dec-pn-counter g)
      (is (= 1 (value-pn-counter (merge-pn-counters c d g))))
      )))