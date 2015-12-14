(ns schema.core-test
  (:import clojure.lang.LazySeq)
  (:require [clojure.test :refer :all]
            [schema.core :refer :all]))

;; clojure specific tests

(deftest anything-sequential-that-is-not-a-vector-is-a-list
  ;; NOTE use `() not '() for literal lists because it resolves symbols in place.
  (doseq [schema [(concat [Long] [String]) '(Long String) `(Long String)]]
    (is (= [1 "2"] (validate schema [1 "2"])))
    (is (thrown? AssertionError (validate schema [1 2])))
    (is (thrown? AssertionError (validate schema ["1" 2])))))

(deftest recursive-types
  (def schema {String [:U Long String #'schema]})
  (is (= {"a" {"number" 1}} (validate schema {"a" {"number" 1}})))
  (is (thrown? AssertionError (validate schema {"a" {"number" :not-valid}})))
  (is (thrown? AssertionError (validate schema {"a" :not-valid}))))

(deftest test-defnv
  (defnv f
    [String -> Long]
    [x]
    (if (= "0" x)
      0
      :not-a-long))
  (is (= 0 (f "0")))
  (is (thrown? AssertionError (f :not-a-string)))
  (is (thrown? AssertionError (f "not 0"))))

(deftest test-defnv-requires-arrow
  (is (thrown? AssertionError (eval '(schema.core/defnv f
                                       [nil nil]
                                       [])))))

(deftest test-defnv-multiple-return-schemas
  (is (thrown? AssertionError (eval '(schema.core/defnv f
                                       [-> nil nil]
                                       [])))))

(deftest test-defnv-no-args
  (defnv f
    [-> Number]
    []
    1)
  (is (= 1 (f))))

(deftest test-defnv-lazy-seq
  (defnv f
    [String -> [Number]]
    [_]
    (concat [0] [:not-a-number]))
  (let [xs (f "")]
    (is (instance? LazySeq xs))
    (is (= 0 (first xs)))
    (is (thrown? AssertionError (second xs)))))

(deftest eager-and-lazy-seq-validation
  (let [schema [Long]]
    (testing "eager validation"
      (let [xs (validate schema [1 2])]
        (is (= [1 2] xs))
        (is (not (instance? LazySeq xs)))
        (is (thrown? AssertionError (validate schema [1 2.0])))))
    (testing "lazy validation"
      (let [xs (validate schema (concat [1] [2.0]))]
        (is (instance? LazySeq xs))
        (is (= 1 (first xs)))
        (is (thrown? AssertionError (second xs)))))))

;; common tests between python and clojure

(deftest nil-as-schema
  (let [schema {String nil}]
    (is (= {"a" nil} (validate schema {"a" nil})))))

(deftest map-behavior-key-ordering
  (let [schema {Long Double}]
    (is (= {1 1.1} (validate schema {1 1.1})))
    (is (= {1 1.1} (validate schema {1.1 1 1 1.1})))
    (is (= {1 1.1} (validate schema {1 1.1 1.1 1})))))

(deftest false-as-schema
  (let [schema {String false}]
    (is (= {"a" false} (validate schema {"a" false})))))

(deftest forward-and-backward-compatability
  (testing "new schema old data"
    (let [schema {"a" Long "b" [:O Long 2]}]
      (is (= {"a" 1 "b" 2} (validate schema {"a" 1})))))
  (testing "old schema new data"
    (let [schema {"a" Long}]
      (is (= {"a" 1} (validate schema {"a" 1 "b" 2})))))
  (testing "old schema new data throws with exact-match"
    (let [schema {"a" Long}]
      (is (thrown? AssertionError (validate schema {"a" 1 "b" 2} :exact-match true))))))

(deftest exact-match
  (let [schema {"a" 1}]
    (is (thrown? AssertionError (validate schema {"a" 1 "b" 2} :exact-match true)))))

(deftest missing-keys-in-value-are-never-allowed
  (let [schema {"a" Long "b" Long}]
    (is (thrown? AssertionError (validate schema {"a" 1} :exact-match true)))
    (is (thrown? AssertionError (validate schema {"a" 1})))))

(deftest type-schemas-pass-value-through
  (let [schema Object
        x (Object.)]
    (assert (identical? x (validate schema x)))))

#_(deftest deferred
    (is (= 1 0)))

#_(deftest deferred-fail
    (is (= 1 0)))

(deftest union
  (let [schema [:U String nil]]
    (is (= "foo" (validate schema "foo")))
    (is (= nil (validate schema nil)))
    (is (thrown? AssertionError (validate schema true)))))

(deftest union-empty
  (let [schema [:U]]
    (is (thrown? AssertionError (validate schema true)))))

(deftest intersection
  (let [schema [:I String #(-> % count (> 2))]]
    (is (= "foo" (validate schema "foo")))
    (is (thrown? AssertionError (validate schema [])))
    (is (thrown? AssertionError (validate schema "")))))

(deftest intersection-empty
  (let [schema [:I]]
    (is (thrown? AssertionError (validate schema true)))))

(deftest union-applied-in-order
  (let [schema [:U
                {"name" [:O String "bob"]}
                {"name" Long}]]
    (is (= {"name" "bob"} (validate schema {})))
    (is (= {"name" 123} (validate schema {"name" 123})))
    (is (thrown? AssertionError (validate schema {"name" 1.0})))))

(deftest intersection-applied-in-order
  (let [schema [:I
                {"name" [:O :Any "bob"]}
                {"name" Long}]]
    (is (= {"name" 123} (validate schema {"name" 123})))
    (is (thrown? AssertionError (validate schema {})))
    (is (thrown? AssertionError (validate schema {"name" "not-a-long"})))))

(deftest test-fn?
  (let [schema {String fn?}
        f #()]
    (is (identical? f (get (validate schema {"fn" f}) "fn")))
    (is (thrown? AssertionError (validate schema {"fn" "not-a-fn"})))))
