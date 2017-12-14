(ns toolbelt.core-test
  (:require [clojure.test :refer :all]
            [toolbelt.core :refer :all]))

(deftest transform-when-key-exists-test
  (let [m1 {:a 1 :b 2}
        m2 {:a 1 :b {:c 2}}]
    (testing "Can perform simple transforms."
      (is (= {:a 2 :b 3} (transform-when-key-exists m1 {:a inc :b inc}))
          "Can transform all keys.")
      (is (= {:a 2 :b 2} (transform-when-key-exists m1 {:a inc :c inc}))
          "Transforms of non-existent keys have no effect."))
    (testing "Can transform values of nested maps."
      (is (= {:a 2 :b {:c 3}} (transform-when-key-exists m2 {:a inc :b {:c inc}}))))))


(deftest str->int-test
  (testing "Can convert strings to integers."
    (is (= 2 (str->int "2")))
    (is (= 0 (str->int "0")))
    (is (= 2 (str->int "a2")) "preceding letters are skipped")
    (is (= 2 (str->int "2a")) "proceding letters are skipped")
    (is (= -2 (str->int "-2")) "negative numbers can be converted"))

  (testing "Numeric arguments aren't modified."
    (is (= 2 (str->int 2)))
    (is (= 2.2 (str->int 2.2))))

  (testing "Empty strings produce `nil`."
    (is (nil? (str->int ""))))

  (testing "Non-numeric, non-string arguments throw"
    (is (thrown? clojure.lang.ExceptionInfo (str->int [])))
    (is (thrown? clojure.lang.ExceptionInfo (str->int {})))
    (is (thrown? clojure.lang.ExceptionInfo (str->int :hello))))

  (testing "Maps can have their string-keys converted to integers."
    (is (= {:a 1} (str->int {:a "1"} :a)))))


(deftest assoc-some-test
  (is (= {:a 1} (assoc-some {} :a 1)))
  (is (= {:a true} (assoc-some {} :a true)))
  (is (= {:a false} (assoc-some {} :a false)))
  (is (= {} (assoc-some {} :a nil)))
  (is (= {:a 1} (assoc-some {} :a 1 :b nil))))


(deftest assoc-when-test
  (is (= {:a 1} (assoc-when {} :a 1)))
  (is (= {:a true} (assoc-when {} :a true)))
  (is (= {} (assoc-when {} :a false)))
  (is (= {} (assoc-when {} :a nil)))
  (is (= {:a 1} (assoc-when {} :a 1 :b nil))))


(deftest conj-when-test
  (is (= [:a] (conj-when [] :a)))
  (is (= [:a :b] (conj-when [] :a :b)))
  (is (= [:a :b] (conj-when [] :a :b nil)))
  (is (= [:a :b] (conj-when [] :a :b false))))


(deftest dissoc-in-test
  (is (nil? (dissoc-in nil [:a :b])))
  (is (= {:a 1} (dissoc-in {:a 1 :b 2} [:b])))
  (is (= {:a 1} (dissoc-in {:a 1 :b {:c {:d 2}}} [:b])))
  (is (= {:a 1} (dissoc-in {:a 1 :b {:c {:d 2}}} [:b :c :d])))
  (is (= {:a 1 :b {:c {:e 3}}} (dissoc-in {:a 1 :b {:c {:d 2 :e 3}}} [:b :c :d]))))


(deftest dissoc-when-test
  (is (= {:a 1} (dissoc-when {:a 1} :b)))
  (is (= {:a 1} (dissoc-when {:a 1 :b false} :b)))
  (is (= {:a 1} (dissoc-when {:a 1 :b nil} :b)))
  (is (= {:a 1} (dissoc-when {:a 1 :b {:c false}} [:b :c])))
  (is (= {:a 1} (dissoc-when {:a 1 :b [0 2 4 5]} :b (partial every? even?)))))


(deftest update-in-when-test
  (is (= {:a 2} (update-in-when {:a 1} [:a] inc)))
  (is (= {:a 2} (update-in-when {:a 2} [:b] inc)))
  (is (= {:a 2} (update-in-when {:a 2} [:b :c] inc)))
  (is (= {:a 2 :b true} (update-in-when {:a 2 :b false} [:b] not)))
  (is (= {:a 2 :b true} (update-in-when {:a 2 :b nil} [:b] not))))


(deftest update-in-some-test
  (is (= {:a 2} (update-in-some {:a 1} [:a] inc)))
  (is (= {:a 2} (update-in-some {:a 2} [:b] inc)))
  (is (= {:a 2 :b true} (update-in-some {:a 2 :b false} [:b] not)))
  (is (= {:a 2 :b nil} (update-in-some {:a 2 :b nil} [:b] not))))


(deftest find-by-test
  (is (= 1 (find-by odd? (range 3))))
  (is (= :c (find-by (partial = :c) [:a :b :c])))
  (is (nil? (find-by (partial = :c) [:a :b]))))


(deftest strip-namespaces-test
  (is (= {:a 1 :b 2} (strip-namespaces {:a 1 :b 2})))
  (is (= {:a 1 :b 2} (strip-namespaces {:foo/a 1 :bar/b 2})))
  (is (= {:a 1 :b {:c 3}} (strip-namespaces {:foo/a 1 :bar/b {:baz/c 3}}))))


(deftest remove-at-test
  (is (= [0 1 3] (remove-at (vec (range 4)) 2)))
  (is (thrown? java.lang.IndexOutOfBoundsException (remove-at [0 1 2] 3))))


(deftest distinct-by-test
  (is (= '(0 1 2) (distinct-by #(* % %) [0 1 2 2 1]))))


(deftest round-test
  (is (= 1 (round 1.4)))
  (is (= 2 (round 1.5)))
  (is (= 1.1 (round 1.1 1)))
  (is (= 1.17 (round 1.167 2))))


(deftest throwable-test
  (is (throwable? (ex-info "Test" {}))))
