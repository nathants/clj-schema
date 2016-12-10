(ns thinking-about-spec
  (:require [clojure.spec :as spec]
            [schema.core :as schema]))

;; a schema for some complex data
(def schema {:name `(String String)
             :age #(and (integer? %) (pos? %))
             :friends [`(String String)]
             :events [{:what String
                       :when Double
                       :where `(Long Long)}]})

;; a spec, semantically identical
(spec/def ::name (spec/tuple string? string?))
(spec/def ::age (spec/and integer? pos?))
(spec/def ::friends (spec/* ::name))
(spec/def ::what string?)
(spec/def ::when float?)
(spec/def ::where (spec/tuple integer? integer?))
(spec/def ::event (spec/keys :req-un [::what ::when ::where]))
(spec/def ::events (spec/* ::event))
(spec/def ::schema (spec/keys :req-un [::name ::age ::friends ::events]))

;; some data that fits the spec/schema
(def data {:name ["jane", "doe"]
           :age 99
           :friends [["dave" "g"]
                     ["tom" "p"]]
           :asdf 123
           :events [{:what "party"
                     :when 123.11
                     :where [65 73]}
                    {:what "shopping"
                     :when 145.22
                     :where [77 44]}]})

;; compare error reporting, and conformed data
(comment
  (>pprint (schema/validate schema data))
  (>pprint (spec/conform ::schema data))

  (schema/validate schema (merge data {:name 123}))
  (spec/explain ::schema  (merge data {:name 123}))

  (schema/validate schema (merge data {:events [nil]}))
  (spec/explain ::schema  (merge data {:events [nil]}))

  (schema/validate schema (merge data {:events (concat (:events data) [nil])}))
  (spec/explain ::schema  (merge data {:events (concat (:events data) [nil])}))

  (schema/validate schema (merge data {:events [{:what "shopping" :when 123.11 :where [0 0 0]}]}))
  ;; AssertionError
  ;; schema: clojure.lang.PersistentArrayMap
  ;;         {:name (java.lang.String java.lang.String),
  ;;          :age :exp.core/fn,
  ;;          :friends [(java.lang.String java.lang.String)],
  ;;          :events
  ;;          [{:what java.lang.String,
  ;;            :when java.lang.Double,
  ;;            :where (java.lang.Long java.lang.Long)}]}
  ;; value:  clojure.lang.PersistentArrayMap
  ;;         {:name ["jane" "doe"],
  ;;          :age 99,
  ;;          :friends [["dave" "g"] ["tom" "p"]],
  ;;          :asdf 123,
  ;;          :events [{:what "shopping", :when 123.11, :where [0 0 0]}]}
  ;;
  ;; checking key: :events
  ;;
  ;; schema: clojure.lang.PersistentVector
  ;;         [{:what java.lang.String,
  ;;           :when java.lang.Double,
  ;;           :where (java.lang.Long java.lang.Long)}]
  ;; value:  clojure.lang.PersistentVector
  ;;         [{:what "shopping", :when 123.11, :where [0 0 0]}]
  ;;
  ;; schema: clojure.lang.PersistentArrayMap
  ;;         {:what java.lang.String,
  ;;          :when java.lang.Double,
  ;;          :where (java.lang.Long java.lang.Long)}
  ;; value:  clojure.lang.PersistentArrayMap
  ;;         {:what "shopping", :when 123.11, :where [0 0 0]}
  ;;
  ;; checking key: :where
  ;;
  ;; schema: clojure.lang.Cons
  ;;         (java.lang.Long java.lang.Long)
  ;; value:  clojure.lang.PersistentVector
  ;;         [0 0 0]
  ;; Assert failed:
  ;; value mismatched length of schema, 3 != 2
  ;; (= (count schema) (count value))  schema.core/-validate (core.clj:156)

  (spec/explain ::schema (merge data {:events [{:what "shopping" :when 123.11 :where [0 0 0]}]}))
  ;; In: [:events 0 :where] val: [0 0 0] fails spec: :exp.core/where at: [:events :where] predicate: (= (count %) 2)

  )

;; TODO open questions.
;; is spec more performant than schema?
;; can we build an alternate explain that does recursive, pretty printed, explanations the way schema does?
;; can we build a fn/macro to that translates schemas to specs, so we can retain a schema dsl while using spec under the hood?
;; are schemas easier to read/understand than specs?
;; do we need/want the strictly greater set of features, and associated complexity, that spec provides?

(comment
  (defn|defmacro schema->spec
    [schema]
    (cond
      (map? schema)    (do ...)
      (vector? schema) (do ...)
      (list? schema)   (do ...)
      (class? schema)  (do ...)
      :else            (do ...))))
