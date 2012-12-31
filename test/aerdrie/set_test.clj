(ns aerdrie.set-test
  (:use clojure.test
        aerdrie.crdt.set))

(deftest set-operations
  (testing "Set operations of removing and adding"
    (let [s (create-lww-set)]
      (is (false? (lookup-lww-set s 1)))
      (add-lww-set s 1)
      (is (true? (lookup-lww-set s 1)))
      (remove-lww-set s 1)
      (is (false? (lookup-lww-set s 1)))
      (remove-lww-set s 1)
      (is (false? (lookup-lww-set s 1)))
      (add-lww-set s 2)
      (is (false? (lookup-lww-set s 1)))
      )))

(deftest merge-set-test
  (testing "Merging sets together"
    (let [c (create-lww-set)
          d (create-lww-set)
          g (create-lww-set)]
      (is (false? (lookup-lww-set (merge-lww-set c d g) 1)))
      (add-lww-set c 1)
      (is (true? (lookup-lww-set (merge-lww-set c d g) 1)))
      (add-lww-set d 2)
      (is (true? (lookup-lww-set (merge-lww-set c d g) 1)))
      (is (true? (lookup-lww-set (merge-lww-set c d g) 2)))
      (remove-lww-set c 1)
      (is (false? (lookup-lww-set (merge-lww-set c d g) 1)))
      (is (true? (lookup-lww-set (merge-lww-set c d g) 2)))
      (add-lww-set g 2)
      (remove-lww-set g 2)
      (is (false? (lookup-lww-set (merge-lww-set c d g) 2)))
      )))