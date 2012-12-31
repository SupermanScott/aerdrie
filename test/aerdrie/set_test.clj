(ns aerdrie.set-test
  (:use clojure.test
        aerdrie.crdt.set))

(deftest multiple-add
  (testing "Adding same member multiple times"
    (let [s (create-lww-set)]
      (add-set s 1)
      (is (true? (lookup-set s 1)))
      (remove-set s 1)
      (is (false? (lookup-set s 1)))
      (add-set s 1)
      (is (true? (lookup-set s 1)))
      )))

(deftest set-operations
  (testing "Set operations of removing and adding"
    (let [s (create-lww-set)]
      (is (nil? (lookup-set s 1)))
      (add-set s 1)
      (is (true? (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (add-set s 2)
      (is (nil? (lookup-set s 1)))
      )))

(deftest merge-set-test
  (testing "Merging sets together"
    (let [c (create-lww-set)
          d (create-lww-set)
          g (create-lww-set)]
      (is (nil? (lookup-set (merge-set c d g) 1)))
      (add-set c 1)
      (is (true? (lookup-set (merge-set c d g) 1)))
      (add-set d 2)
      (is (true? (lookup-set (merge-set c d g) 1)))
      (is (true? (lookup-set (merge-set c d g) 2)))
      (remove-set c 1)
      (is (nil? (lookup-set (merge-set c d g) 1)))
      (is (true? (lookup-set (merge-set c d g) 2)))
      (add-set g 2)
      (remove-set g 2)
      (is (nil? (lookup-set (merge-set c d g) 2)))
      )))

(deftest sorted-set-operations
  (testing "Sorted Set operations of removing and adding"
    (let [s (create-sorted-set)]
      (is (nil? (lookup-set s 1)))
      (add-sorted-set s 1 2.0)
      (is (= 2.0 (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (remove-set s 1)
      (is (nil? (lookup-set s 1)))
      (add-sorted-set s 2 1.0)
      (is (nil? (lookup-set s 1)))
      )))