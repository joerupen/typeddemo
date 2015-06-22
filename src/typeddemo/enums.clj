(ns typeddemo.enums
  (:require [clojure.core.typed :as t]))


;; I am not sure what the purpose is for defining types such as (t/Val 4)
;; This example shows a potential scenario for using it with enums.




(t/defalias week-days "a day of the week"
  (t/U (t/Val :mon) (t/Val :tue) (t/Val :wed) 
       (t/Val :thu) (t/Val :fri) ))

(t/defalias party-days "days where we can party."
  (t/U  (t/Val :fri) (t/Val :sat)))

(t/defalias week-end-days "days where we can party."
  (t/U  (t/Val :sat) (t/Val :sun)))

(t/defalias all-days "all days of the week"
  (t/U week-days party-days week-end-days))

(t/defalias party-days-of-the-week "days in the week where we can party"
  (t/I week-days party-days))

(t/defalias party-days-of-the-week2 "days in the week where we can party"
  (t/Difference week-days party-days))

;; monday is a week day.
(t/def mon :- week-days :mon)

;; sunday is not week day
;; (t/def sun :- week-days :sun)

;; but sunday is part of all days.
(t/def sun :- all-days :sun)

;; qux is not a day
;;(t/def qux :- all-days :qux)

;; saturday is not a week day
;;(t/def sat :- week-days :sat)

;; sunday is not party day.
;;(t/def sun :- party-days :sun)

;; saturday is a party day, but not of the week, so this should fail.
;; sorry, don't really know why this doesn't fail. maybe a bug in core.typed
;; or it's my wrong conclusion of how intersetion t/I works.
;; so if you know why this succeeds, please let me know ;)
(t/def party-day :- party-days-of-the-week :sat)

;; same here. this one should not fail, but it does. Not sure why t/Difference
;; does not work here.
;;(t/def party-day :- party-days-of-the-week2 :fri)








