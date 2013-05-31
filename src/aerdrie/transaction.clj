(ns aerdrie.transaction
  :require [taoensso.nippy]
            [clojure.java.io]
  :import [java.util.UUID])

(defrecord transaction [transaction-id key object-type method args])

(def transaction-table (ref {}))
(def transaction-file (agent (DataOutputStream. (clojure.java.io/output-stream
                                                 "/tmp/trans2.log" :append true))))

(defn write-out-trans
  [current-file trans]
  (let [bytes (taoensso.nippy/freeze-to-bytes trans)
        len (count bytes)]
    (.write current-file bytes 0 len))
  current-file)

(defn add-to-set-transaction
  "Creates a transaction record and records to log"
  [key &arguments]
  (let [trans (->transaction (str (java.util.UUID/randomUUID))
                             key :set :add-set 'arguments)
        ]
    (dosync
     (alter transaction-table assoc key trans)
     (send-off transaction-file write-out-trans trans)
     )))