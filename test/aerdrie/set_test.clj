(ns aerdrie.set-test
  (:use clojure.test
        aerdrie.crdt.set
        clojure.set))

(deftest multiple-add
  (testing "Adding same member multiple times"
    (let [s (create-lww-set)]
      (add-set s 1)
      (is (seq (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (add-set s 1)
      (is (seq (lookup-set s 1)))
      )))

(deftest multiple-add-sort
  (testing "Adding same member multiple times with different scores")
  (let [s (create-sorted-set)]
    (add-set s 1 1.0)
    (is (= 1.0 (:value (lookup-set s 1))))
    (add-set s 1 2.0)
    (is (= 2.0 (:value (lookup-set s 1))))
    (remove-set s 1)
    (is (nil? (lookup-set s 1)))
    (add-set s 1 3.0)
    (is (= 3.0 (:value (lookup-set s 1))))
    ))

(deftest set-operations
  (testing "Set operations of removing and adding"
    (let [s (create-lww-set)]
      (is (nil? (lookup-set s 1)))
      (add-set s 1)
      (is (seq (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (add-set s 2)
      (is (nil? (lookup-set s 1)))
      )))

(deftest set-realize
  (testing "Checking realized value of the set"
    (let [s (create-lww-set)]
      (add-set s "a")
      (is (seq (filter #(= "a" (:member-id %)) (realized-set-value s))))
      (remove-set s "a")
      (is (empty? (realized-set-value s)))
      )))

(deftest merge-set-test
  (testing "Merging sets together"
    (let [c (create-lww-set)
          d (create-lww-set)
          g (create-lww-set)]
      (is (nil? (lookup-set (merge-set c d g) 1)))
      (add-set c 1)
      (is (seq (lookup-set (merge-set c d g) 1)))
      (add-set d 2)
      (is (seq (lookup-set (merge-set c d g) 1)))
      (is (seq (lookup-set (merge-set c d g) 2)))
      (remove-set c 1)
      (is (nil? (lookup-set (merge-set c d g) 1)))
      (is (seq (lookup-set (merge-set c d g) 2)))
      (add-set g 2)
      (remove-set g 2)
      (is (nil? (lookup-set (merge-set c d g) 2)))
      )))

(deftest sorted-set-operations
  (testing "Sorted Set operations of removing and adding"
    (let [s (create-sorted-set)]
      (is (nil? (lookup-set s 1)))
      (add-set s 1 2.0)
      (is (= 2.0 (:value (lookup-set s 1))))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (add-set s 2 1.0)
      (is (nil? (lookup-set s 1)))
      )))

(deftest sorted-realize
  (testing "Checking the realized value of the sorted set"
    (let [s (create-sorted-set)]
      (add-set s "a" 1.0)
      (add-set s "b" 2.0)
      (is (= "b" (:member-id (last (realized-set-value s)))))
      (add-set s "a" 3.0)
      (is (= "a" (:member-id (last (realized-set-value s)))))
      (remove-set s "a")
      (is (= "b" (:member-id (last (realized-set-value s)))))
      )))

(deftest sync-merge-values
  (testing "Ensuring the sync merge returns the proper set"
    (let [s (create-lww-set)
          g (create-lww-set)]
      (add-set s "a")
      (add-set s "b")
      (add-set g "b")
      (add-set g "a")
      (remove-set s "a")
      (remove-set s "b")
      (remove-set g "b")

      (is (empty? (intersection
                   (:added (deref (sync-merge-set s g))))))
      (add-set g "c")
      (is (not (empty? (intersection
                        (:added (deref (sync-merge-set s g)))))))
      )))