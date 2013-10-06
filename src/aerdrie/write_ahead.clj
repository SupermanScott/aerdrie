(ns aerdrie.write-ahead
  "Functions for recording an operation to write-ahead log"
  (:require [taoensso.nippy]
            [clojure.java.io])
  (:import [java.util UUID]
           [java.io DataOutputStream DataInputStream]))

(defrecord transaction [transaction-id key object-type method args])

(def transaction-table (ref {}))
(def transaction-file (agent (DataOutputStream.
                              (clojure.java.io/output-stream
                               "/tmp/trans2.log" :append true))))

(defn read-transaction-file
  "Read in all the transactions from the transaction-file"
  [file-name]
  (with-open [input-stream (DataInputStream.
                            (clojure.java.io/input-stream
                             file-name))]
    (taoensso.nippy/thaw-from-stream! input-stream)))

(defn write-out-trans
  "For use in send-off function to write the transaction to disk"
  [current-file trans]
  (let [bytes (taoensso.nippy/freeze-to-bytes trans)
        len (count bytes)]
    (.write current-file bytes 0 len)
    (.flush current-file))
  (println "Wrote transaction")
  current-file)

(defn add-to-set-transaction
  "Creates a transaction record and records to log"
  [key & arguments]
  (let [trans (->transaction (str (java.util.UUID/randomUUID))
                             key :set :add-set 'arguments)
        ]
    (dosync
     (alter transaction-table assoc key trans)
     (send-off transaction-file write-out-trans trans)
     ))
  (await-for 1000 transaction-file))
