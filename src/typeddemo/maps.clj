(ns typeddemo.maps
  (:require [clojure.core.typed :as t]))

;; There are 2 types of maps in core.typed
;; either maps similar to Java, with TKey, TValue or heterogenous maps.
;; Heterogenous maps store information about specific keys and values.
;; For HMaps, there is also a shorthand syntax.

;; First a simple type for a map<Keyword, Number>
(t/defalias key-num (t/Map t/Kw t/Num) )

;; define and typecheck a var using our Type.
(t/def cache :- key-num {:key 22})

(t/defn put [c :- key-num, k :- t/Kw, val :- t/Num] :- key-num
  (assoc c k val))

(t/def new-cache (put cache :qux 100))

;; fails to typecheck
;;(t/def bad-cache (put cache "qux" 200))

;; The next example is using HMaps. Those simply maps with particular
;; information about keys. Keys can be mandatory or optional and have
;; an associated type. 

(t/defalias User (t/HMap :mandatory {:name t/Str :email t/Str}
                         :optional {:age t/Num}))


;; a short helper fn for our casts.
(t/defn to-str [x :- t/Any] :- t/Str
  (assert (string? x))
  x)

(t/defn to-num [x :- t/Any] :- t/Num
  (assert (number? x))
  x)

;; from any map to a User. Obviously here you can have runtime exceptions
(t/defn create-user [obj :- (t/Map t/Kw t/Any)] :- User
  (let [{:keys [name email age]} obj]
    {:name (-> name to-str)
     :email (-> email to-str)
     :age (-> age to-num)}))

;; Now we try an annotated multimethod to update the user.
;; it receives the user and an event and returns a new user.
;; The event is a map that has at least a :type key with 2 possible values,
;; and a generic map.
;; When you went though the previous examples (repl.clj, functions.clj, errors.clj)
;; now this starts to look really easy compared to it.
(t/ann update-user [User, (t/U
                           '{:type (t/U (t/Val :change-mail)
                                        (t/Val :change-name))}
                           (t/Map t/Kw t/Any)) -> User])

(defmulti update-user (fn [_ x] (:type x)))

(defmethod update-user :change-mail [user event]
  (assoc user :email (to-str (:email event))))

(defmethod update-user :change-name [user event]
  (assoc user :name  (to-str (:name event))))

;; you can verify that the fn is type checked, just uncommenet this example
;; (defmethod update-user :change-name [user event]
;;   (assoc user :name  100))


;; This example uses the short form of an HMap: '{TKey TVal, TKey2 TVal2, ...}
(t/defn sum-some-keys 
  [x :- '{:a t/Num :b t/Num :c t/Num }] :- t/Num
  (+ (:a x) (:b x) (:c x)) )

(comment
  "missing keyword, should not typecheck."
  (defn foo []
    (sum-some-keys {:a 1 :b 2 })))

