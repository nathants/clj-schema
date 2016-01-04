(ns schema.core
  ;; TODO drop pprint. it uses STM and that is very bad for perf.
  ;; https://github.com/brandonbloom/fipp
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s])
  (:import clojure.lang.LazySeq
           clojure.lang.Cons))

(def *disable-update-exception* (System/getenv "DISABLE_UPDATE_EXCEPTION"))
(def *disable-schema* (doto (System/getenv "DISABLE_SCHEMA") (->> boolean (println "schemas disabled:"))))

;; TODO write backwards compatability checker for two schema values.
(defn compatible [old new] false)

;; TODO have functions that translate to and from json mode. sets and
;; lists become arrays, types are converted to a namespaced keyword,
;; etc.
(defn freeze [x] x)
(defn thaw [x] x)

;; TODO probably more efficient to pass down the context, rather than
;; storing it in a stack of try/catches.
(defmacro with-update-exception
  [cls msg body]
  (if *disable-update-exception*
    body
    `(try
       ~body
       (catch ~cls ex#
         (throw (new ~cls (str (.getMessage ex#) "\n" ~msg "\n")))))))

(defn indent
  [n x]
  (->> x
    s/split-lines
    (map #(str (apply str (repeat n " ")) %))
    (s/join "\n")))

(defn pretty-class
  [c]
  (second (s/split (str c) #" ")))

(defn helpful-message
  [schema value]
  (str "schema: "
       (pretty-class (type schema))
       "\n"
       (indent 8 (with-out-str (pprint schema)))
       "\nvalue:  "
       (pretty-class (type value))
       "\n"
       (indent 8 (with-out-str (pprint value)))))

(defn error-message
  [& args]
  (str "\n" (s/join " " (map str args)) "\n"))

(def -schema-commands
  #{:U ;; union
    :I ;; intersection
    :O ;; optional
    :fn})

(defn -resolve-symbols
  [x]
  (cond
    (map? x) (reduce-kv #(assoc %1 (-resolve-symbols %2) (-resolve-symbols %3)) {} x)
    (symbol? x) (resolve x)
    :else x))

(defn -validate
  [schema value & {:keys [exact-match]}]
  (with-update-exception AssertionError (helpful-message schema value)
    ;; TODO type based dispatch better than long conditionals? possible?
    ;; TODO flatten this into un-nested conditionals? perf hit?
    ;; TODO rewrite py-schema in cljc. use macros. do compile time
    ;; transformations so that type lookup is used instead of long chains
    ;; of conditionals. perf boost?
    (let [[schema value] (map -resolve-symbols [schema value])]
      (cond
        ;; TODO suppport validating a manifold deferred
        (set? schema)
        (do (assert (set? value) (error-message value "is not a set"))
            (assert (= (count schema) 1) (error-message "set schemas represent homogenous sets and must (= 1 (count schema))"))
            (set (map #(-validate (first schema) %) value)))
        (map? schema)
        (do (assert (map? value) (error-message value "is not a map"))
            ;; if schema keys are all types, and _value is empty, return. ie, type keys are optional, so {} is a valid {int: int}
            (if (and (= {} value)
                     (= #{Class} (set (map type schema))))
              {}
              ;; TODO is there an fp way to do this that is *more* elegant than current implementation?
              (let [-value (volatile! {})]
                ;; check for items in value that dont satisfy schema, dropping unknown keys unless exact_match=true
                (doseq [[k v] value]
                  (with-update-exception AssertionError (str "checking key: " k)
                    (let [value-match (contains? schema k)
                          type-match (some #{(type k)} (filterv class? (keys schema))) ;; TODO sort this filterv for consistent results?
                          predicate-match (some #(if (% k) %) (filterv fn? (keys schema))) ;; TODO sort this filterv for consistent results?
                          any-match (contains? schema :Any)]
                      (cond
                        (or value-match type-match predicate-match any-match)
                        (let [-schema (get schema (cond value-match k
                                                        type-match (type k)
                                                        predicate-match predicate-match
                                                        any-match :Any))]
                          (vswap! -value assoc k (-validate -schema v)))
                        :else
                        (assert (not exact-match) (error-message k "does not match schema keys"))))))
                ;; check for items in schema missing in value, filling in optional value
                (doseq [[k v] schema]
                  (with-update-exception AssertionError (str "checking key: " k)
                    (when (not (contains? @-value k))
                      (cond
                        (and (sequential? v)
                             (= :O (first v)))
                        (do (assert (= 3 (count v)) (error-message ":O schema should be [:O, schema, default-value]"))
                            (vswap! -value assoc k (apply -validate (rest v))))
                        (not (or (= :Any k)
                                 (class? k)
                                 (fn? k)))
                        (throw (AssertionError. (error-message "missing required key:" k)))))))
                @-value)))
        (= schema :Any) value
        (sequential? schema)
        (do (assert (or (sequential? value) (get -schema-commands (first schema))) (error-message value "is not sequential"))
            (cond
              (get -schema-commands (first schema))
              (condp = (first schema)
                :O
                (do (assert (= 3 (count schema)) (error-message ":O schema should be [:O, schema, default-value]"))
                    (-validate (second schema) value))
                :U ;; TODO is there an fp way to do this that is *more* elegant than current implementation?
                (let [_ (assert (> (count schema) 1) (error-message ":U types cannot be empty"))
                      -value (volatile! value)
                      tracebacks (volatile! [])]
                  (doseq [-schema (rest schema)]
                    (try
                      (vswap! -value #(-validate -schema %))
                      (catch AssertionError ex
                        (vswap! tracebacks conj (.getMessage ex)))))
                  (if (= (count (rest schema)) (count @tracebacks))
                    (throw (AssertionError. (error-message "did not match *any* of :U\n" (apply str (interpose "\n" @tracebacks)))))
                    @-value))
                :I ;; TODO is there an fp way to do this that is *more* elegant than current implementation?
                (let [_ (assert (> (count schema) 1) (error-message ":I types cannot be empty"))
                      -value (volatile! value)
                      tracebacks (volatile! [])]
                  (doseq [-schema (rest schema)]
                    (try
                      (vswap! -value #(-validate -schema %))
                      (catch AssertionError ex
                        (vswap! tracebacks conj (.getMessage ex)))))
                  (if (seq @tracebacks)
                    (throw (AssertionError. (error-message "did not match *all* of :I\n" (apply str (interpose "\n" @tracebacks)))))
                    @-value))
                :fn
                (assert false "TODO implement me. how does this differ from python?"))
              (vector? schema)
              (do (assert (= (count schema) 1) (error-message "vector schemas represent homogenous seqs and must (= 1 (count schema))"))
                  (mapv #(-validate (first schema) %) value))
              :else
              (do (assert (= (count schema) (count value)) (error-message "value mismatched length of schema," (count value) "!=" (count schema)))
                  (mapv -validate schema value))))
        (class? schema)
        (do (assert (instance? schema value) (error-message value "is not a" (pretty-class schema)))
            value)
        (fn? schema)
        (do (assert (schema value) (error-message "failed predicate schema"))
            value)
        (var? schema)
        (-validate (var-get schema) value)
        :else
        (do (assert (= schema value) (error-message value "does not equal" schema))
            value)))))

(defmacro validate
  [schema value & {:keys [exact-match]}]
  ;; TODO note about tuples. use "`", but only once. ie dont nest "`" or bad results.
  ;; TODO core.async chan, manifold defered
  (if *disable-schema*
    value
    `(let [value# ~value
           schema# ~schema
           exact-match# ~exact-match]
       (if (and (vector? schema#)
                (= (count schema#) 1)
                (instance? LazySeq value#)
                (not (realized? value#)))
         (map #(-validate (first schema#) % :exact-match exact-match#) value#)
         (-validate schema# value# :exact-match exact-match#)))))

(defmacro defnv
  "define validated function.
  usage: (defnv plus
           [Long Long -> Long]
           [x y]
           (+ x y))"
  ;; TODO support generics? [[:T] -> [:T]]
  ;; TODO completely ignore & {:keys [blah]}
  ;; TODO add support for arity overloading
  ;; TODO variadic sig support
  ;; (deftest test-defnv-variadic
  ;;   (defnv f
  ;;     [& String -> String]
  ;;     [& xs]
  ;;     (apply str xs))
  ;;   (is (= "12" (f "1" "2" "3"))))
  [& forms]
  (let [[name doc sig args & body]
        (if (string? (second forms))
          forms
          (apply vector (first forms) "" (drop 1 forms)))
        _ (assert (->> sig (filter #{'->}) count (= 1)) (error-message "sig should have '-> in it, not:" sig))
        _ (assert (->> sig (partition-by #{'->}) second count (= 1)) (error-message "sig should have only one schema to the right of '->, not:" sig))
        [arg-schema _ [return-schema]] (if (= '-> (first sig))
                                         [() nil [(last sig)]]
                                         (partition-by #{'->} sig))]
    (if *disable-schema*
      `(defn ~name
         ~doc
         ~args
         ~@body)
      `(defn ~name
         {:doc ~doc :arglists '(~args)}
         [& args#]
         (assert (= (count args#) (count '~args)) (str "unknown arity: " (count args#) " for fn: " ~name))
         (let [~args (for [[i# a# s#] (map vector (range) (list ~@arg-schema) (or args# ()))]
                       (with-update-exception AssertionError (str "\nschema check failed for args to fn: " ~name ", for pos arg: " i#)
                         (validate a# s#)))
               res# (do ~@body)]
           (with-update-exception AssertionError (str "\nschema check failed for return value from fn: " ~name)
             (validate ~return-schema res#)))))))
