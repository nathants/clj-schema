(ns schema.core-test
  (:import clojure.lang.LazySeq)
  (:require [clojure.test :refer :all]
            [schema.core :refer :all]))

;; clojure specific tests

(deftest resolve-var-keys-in-defnv-maps
  (defnv f
    [`(String {keyword? String}) -> Integer]
    [x]
    (count x))
  (is (= 2 (f ["foo" {:foo "bar"}]))))

(deftest symbols-as-map-keys-and-vals-are-resolved
  (= [{"1" 2}] (validate `({String Long}) [{"1" 2}])))

(deftest anything-sequential-that-is-not-a-vector-is-a-list
  ;; NOTE use `() not '() for literal lists because it resolves symbols in place.
  (doseq [schema [(concat [Long] [String]) '(Long String) `(Long String)]]
    (is (= [1 "2"] (validate schema [1 "2"])))
    (is (thrown? AssertionError (validate schema [1 2])))
    (is (thrown? AssertionError (validate schema ["1" 2])))))

(deftest value-vars
  (def v 123)
  (is (= 123 (validate Long #'v))))

(deftest recursive-types
  (def schema {String [:U Long String #'schema]})
  (is (= {"a" {"number" 1}} (validate schema {"a" {"number" 1}})))
  (is (thrown? AssertionError (validate schema {"a" {"number" :not-valid}})))
  (is (thrown? AssertionError (validate schema {"a" :not-valid}))))

(deftest test-fn
  (let [f (fnv
            [Long -> Long]
            [x]
            x)]
    (is (= 0 (f 0)))
    (is (thrown? AssertionError (f :not-long)))))

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
  (is (= 1 (f)))
  (is (thrown? AssertionError (f 1))))

(deftest test-defnv-unpacking
  (defnv f
    [`(String Long) -> String]
    [[a b]]
    (str a b))
  (is (= "a0" (f ["a" 0]))))

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

(deftest set-schema
  ;; TODO add to py-schema
  (let [schema #{Long}]
    (is (= #{1 2} (validate schema #{1 2})))
    (is (thrown? AssertionError (validate schema #{1 "2"}))))
  (let [bad-schema #{Long String}]
    (is (thrown? AssertionError (validate bad-schema #{1 "2"})))))

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

(deftest union-as-key
  (let [schema {[:U "a" "b"] Long}]
    (is (= {"a" 1} (validate schema {"a" 1})))
    (is (= {"b" 2} (validate schema {"b" 2})))
    (is (= {} (validate schema {"c" 3})))
    (is (thrown? AssertionError (validate schema {"c" 3} :exact-match true)))))

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

(deftest predicate
  (let [schema {String fn?}
        f #()]
    (is (identical? f (get (validate schema {"fn" f}) "fn")))
    (is (thrown? AssertionError (validate schema {"fn" "not-a-fn"})))))

(deftest predicate-keys-are-optional
  (let [schema {string? String}]
    (is (= {"a" "b"} (validate schema {"a" "b"})))
    (is (= {} (validate schema {:not-a-str :value-to-drop})))))

(deftest empty-maps
  (is (= {} (validate {} {}))))

(deftest type-keys-are-optional
  (is (= {} (validate {String String} {}))))

(deftest empty-map-exact-match
  (is (thrown? AssertionError (validate {} {:not :empty} :exact-match true))))

(deftest partial-comparisons-for-testing
  (let [schema {:blah String
                :data [{String String}]}
        data {:blah "foobar"
              :data [{"a" "b"}
                     {"c" "d"}
                     ;; ...
                     ;; pretend 'data' is something too large to specify as a value literal in a test
                     ]}]
    (is (validate schema data))
    (is (thrown? AssertionError (validate schema {:blah "foobar"
                                                  :data [{"a" 1}]})))))

(deftest any-as-key
  (let [schema {:Any Long}]
    (is (= {:1 2} (validate schema {:1 2})))))

(deftest any-as-tuple
  (let [schema `(:Any :Any)]
    (is (= [1 "2"] (validate schema [1 "2"])))
    (is (thrown? AssertionError (validate schema [1 2 3])))))

(deftest any-as-vector
  (let [schema [:Any]]
    (is (= [1 2 3] (validate schema [1 2 3])))
    (is (= [1 "2" 3.0] (validate schema [1 "2" 3.0])))))

(deftest any-as-value
  (let [schema {keyword? :Any}]
    (is (= {:a :apple} (validate schema {:a :apple})))
    (is (= {:b 123} (validate schema {:b 123})))))

(deftest any-as-value-exact-match
  (let [schema {keyword? :Any}]
    (is (= {:fruit :apple} (validate schema {:fruit :apple})))
    (is (= {} (validate schema {1 :apple})))
    (is (thrown? AssertionError (validate schema {1 :apple} :exact-match true)))))

(deftest required-value-to-type
  (let [schema {:a "apple"
                :b String}]
    (is (= {:a "apple" :b "banana"}
           (validate schema {:a "apple" :b "banana"})))
    (is (thrown? AssertionError (validate schema {:a "apple"})))
    (is (thrown? AssertionError (validate schema {:a "apple" :b 1})))))

(deftest required-value-to-value
  (let [schema {:a "apple"
                :b "banana"}]
    (is (= {:a "apple" :b "banana"} (validate schema {:a "apple" :b "banana"})))
    (is (thrown? AssertionError (validate schema {:a "apple"})))))

(deftest type-to-value
  (let [schema {String "apple"}]
    (is (= {"a" "apple"} (validate schema {"a" "apple"})))
    (is (thrown? AssertionError (validate schema {"a" "notapple"})))))

(deftest nested-optional
  (let [schema {:a {:b [:O String "default"]}}]
    (is (= {:a {:b "default"}} (validate schema {:a {}})))
    (is (= {:a {:b "value"}} (validate schema {:a {:b "value"}})))
    (is (thrown? AssertionError (validate schema {:a {:b 123}}))))
  (let [schema [{:name [:O String "bob"]}]]
    (is (= [{:name "bob"}] (validate schema [{}])))
    (is (= [{:name "joe"}] (validate schema [{:name "joe"}])))))

(deftest optional
  (let [schema {:a "apple"
                :b [:O String "banana"]}]
    (is (= {:a "apple" :b "banana"} (validate schema {:a "apple"})))
    (is (= {:a "apple" :b "bar"} (validate schema {:a "apple" :b "bar"})))
    (is (thrown? AssertionError (validate schema {:a "apple" :b 1.0})))))

(deftest value-schema
  (let [schema 1]
    (is (= 1 (validate schema 1)))
    (is (thrown? AssertionError (validate schema 2)))))

(deftest single-type-schema
  (let [schema Long]
    (is (= 1 (validate schema 1)))
    (is (thrown? AssertionError (validate schema "2")))))

(deftest iterable-length-n
  (let [schema [Long]]
    (is (= [1 2] (validate schema [1 2])))
    (is (thrown? AssertionError (validate schema [1 "2"])))))

(deftest iterable-fixed-length
  (let [schema `(Double Long)]
    (is (= [1.0 1] (validate schema [1.0 1])))
    (is (thrown? AssertionError (validate schema [1.0 "1"])))))

(deftest nested-type-to-type
  (let [schema {String {String Long}}]
    (is (= {"1" {"1" 1}} (validate schema {"1" {"1" 1}})))
    (is (thrown? AssertionError (validate schema {"1" nil})))
    (is (thrown? AssertionError (validate schema {"1" {"1" nil}})))))

(deftest val-to-val-and-type-to-type
  (let [schema {:a "apple"
                String Double}]
    (is (= {:a "apple" "1" 1.1} (validate schema {:a "apple" "1" 1.1})))
    (is (= {:a "apple"} (validate schema {:a "apple"})))
    (is (thrown? AssertionError (validate schema {:a "applebees"})))))

(deftest type-to-type
  (let [schema {String Long}]
    (is (= {"1" 1} (validate schema {"1" 1})))
    (is (= {} (validate schema {})))))

(deftest value-to-type
  (let [schema {"foo" Long}]
    (is (= {"foo" 1} (validate schema {"foo" 1})))
    (is (thrown? AssertionError (validate schema {"foo" "bar"})))))

(deftest value-to-value
  (let [schema {"foo" "bar"}]
    (is (= {"foo" "bar"} (validate schema {"foo" "bar"})))
    (is (thrown? AssertionError (validate schema {"foo" 1})))))

(deftest predicate-schema
  (let [schema {"foo" #(and (integer? %) (pos? %))}]
    (is (= {"foo" 1} (validate schema {"foo" 1})))
    (is (thrown? AssertionError (validate schema {"foo" 0})))))

(deftest nested-predicate-schema
  (let [schema {"foo" {"bar" #(and (integer? %) (pos? %))}}]
    (is (= {"foo" {"bar" 1}} (validate schema {"foo" {"bar" 1}})))
    (is (thrown? AssertionError (validate schema {"foo" {"bar" 0}})))))

(deftest iterable-length-n-must-be-length-one
  (let [schema [Long Long]]
    (is (thrown? AssertionError (validate schema [1])))))

(deftest nested-iterables
  (let [schema [[String]]]
    (is (= [["1"] ["2"]] (validate schema [["1"] ["2"]])))
    (is (thrown? AssertionError (validate schema [["1"] [2]])))))

(deftest many-keys
  (let [schema {String Long}]
    (is (= {"1" 2 "3" 4} (validate schema {"1" 2 "3" 4})))
    (is (thrown? AssertionError (validate schema {"1" 2 "3" 4.0})))))

(deftest value-matches-are-higher-precidence-than-type-matches
  (let [schema {String Long
                "foo" "bar"}]
    (is (= {"1" 2 "foo" "bar"} (validate schema {"1" 2 "foo" "bar"})))
    (is (thrown? AssertionError (validate schema {"1" 2 "foo" 2})))))

(deftest complex-types
  (let [schema {:name `(String String)
                :age #(and (integer? %) (pos? %))
                :friends [`(String String)]
                :events [{:what String
                          :when Double
                          :where `(Long Long)}]}
        data {:name ["jane", "doe"]
              :age 99
              :friends [["dave" "g"]
                        ["tom" "p"]]
              :events [{:what "party"
                        :when 123.11
                        :where [65 73]}
                       {:what "shopping"
                        :when 145.22
                        :where [77 44]}]}]
    (is (= data (validate schema data)))
    (is (thrown? AssertionError (validate schema (merge data {:name 123}))))
    (is (thrown? AssertionError (validate schema (merge data {:events [nil]}))))
    (is (thrown? AssertionError (validate schema (merge data {:events (concat (:events data) [nil])}))))
    (is (thrown? AssertionError (validate schema (merge data {:events [{:what "shopping"
                                                                        :when 123.11
                                                                        :where [0 0 0]}]}))))))
