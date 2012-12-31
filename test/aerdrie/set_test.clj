(ns aerdrie.set-test
  (:use clojure.test
        aerdrie.crdt.set))

(deftest set-operations
  (testing "Set operations of removing and adding"
    (let [s (create-lww-set)]
      (is (false? (lookup-set s 1)))
      (add-set s 1)
      (is (true? (lookup-set s 1)))
      (remove-set s 1)
      (is (false? (lookup-set s 1)))
      (remove-set s 1)
      (is (false? (lookup-set s 1)))
      (add-set s 2)
      (is (false? (lookup-set s 1)))
      )))

(deftest merge-set-test
  (testing "Merging sets together"
    (let [c (create-lww-set)
          d (create-lww-set)
          g (create-lww-set)]
      (is (false? (lookup-set (merge-set c d g) 1)))
      (add-set c 1)
      (is (true? (lookup-set (merge-set c d g) 1)))
      (add-set d 2)
      (is (true? (lookup-set (merge-set c d g) 1)))
      (is (true? (lookup-set (merge-set c d g) 2)))
      (remove-set c 1)
      (is (false? (lookup-set (merge-set c d g) 1)))
      (is (true? (lookup-set (merge-set c d g) 2)))
      (add-set g 2)
      (remove-set g 2)
      (is (false? (lookup-set (merge-set c d g) 2)))
      )))