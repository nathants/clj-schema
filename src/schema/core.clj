(ns schema.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s])
  (:import clojure.lang.LazySeq
           clojure.lang.Cons))

;; TODO add fnv and fnv! to complement defnv and defnv!

;; TODO type hints break defnv

;; TODO
;;
;; drop pprint. it uses STM and that is very bad for perf.
;; https://github.com/brandonbloom/fipp


;; TODO
;;
;; schema. having optional values could cause failures when schema is
;; disabled. ie got nil instead of optional val. change -validate to
;; validate! and make it fill optionals by default, but not the
;; validate macro, which can be disabled. same for defnv!.


(def *disable-update-exception* false)
(def *disable-schema* (doto (not (System/getenv "ENABLE_SCHEMA"))
                        (->> (println "disable schema:"))))

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
  [cls msg & forms]
  (let [after (= [:after true] (take 2 forms))
        forms (if after (drop 2 forms) forms)
        ex (gensym)
        string (if after
                 `(str "\n" (.getMessage ~ex) "\n" ~msg "\n")
                 `(str "\n" ~msg "\n" (.getMessage ~ex)))]
    (assert (= 1 (count forms)))
    (if *disable-update-exception*
      (first forms)
      `(try
         ~@forms
         (catch ~cls ~ex
           (throw (new ~cls ~string)))))))

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
  (str "\n" (s/join " " (map str args))))

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

(defn -sort-any
  "sort anything by grouping types and sorting by type name before
  sorting values by type group"
  [xs]
  (->> xs
    (group-by type)
    (sort-by (comp str first))
    (map (comp sort val))
    (apply concat)))

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
        (var? schema)
        (-validate (var-get schema) value)
        (var? value)
        (-validate schema (var-get value))
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
                    (if-let [schema-k (->> (keys schema)
                                        -sort-any ;; map key should be in a stable ordering
                                        (sort-by #(not= % k)) ;; value matched keys are higher precedence
                                        (map #(try
                                                (-validate % k)
                                                %
                                                (catch AssertionError _
                                                  ::fail)))
                                        (remove #{::fail})
                                        first)]
                      (vswap! -value assoc k (-validate (get schema schema-k) v))
                      (assert (not exact-match) (error-message k "does not match schema keys")))))
                ;; check for items in schema missing in value, filling in optional value
                (doseq [[k v] schema]
                  (let [k (if (var? k) (var-get k) k)
                        v (if (var? v) (var-get v) v)]
                    (with-update-exception AssertionError (str "checking key: " k)
                      (when (not (contains? @-value k))
                        (cond
                          (and (sequential? v)
                               (= :O (first v)))
                          (do (assert (= 3 (count v)) (error-message ":O schema should be [:O, schema, default-value]"))
                              (vswap! -value assoc k (apply -validate (rest v))))
                          (not (or (and (sequential? k) (-schema-commands (first k)))
                                   (= :Any k)
                                   (class? k)
                                   (fn? k)))
                          (throw (AssertionError. (error-message "missing required key:" k))))))))
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
        :else
        (do (assert (= schema value) (error-message value "does not equal" schema))
            value)))))

(defmacro validate
  [schema value & {:keys [exact-match]}]
  ;; TODO note about tuples. use "`", but only once. ie dont nest "`" or bad results.
  ;; TODO core.async chan, manifold defered
  (if *disable-schema*
    value
    `(if *disable-schema*
       ~value
       (let [value# ~value
             schema# ~schema
             exact-match# ~exact-match]
         (if (and (vector? schema#)
                  (= (count schema#) 1)
                  (instance? LazySeq value#)
                  (not (realized? value#)))
           (map #(-validate (first schema#) % :exact-match exact-match#) value#)
           (-validate schema# value# :exact-match exact-match#))))))

(defmacro defnv
  "define validated function.
  usage: (defnv plus
           [Long Long -> Long]
           [x y]
           (+ x y))"
  ;; TODO support generics? [[:T] -> [:T]]
  ;; TODO completely ignore & {:keys [blah]}
  ;; TODO add support for arity overloading
  ;; TODO multiple arities
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
                       (with-update-exception AssertionError (str "\nschema check failed for args to fn: " ~name ", for pos arg: " i#) :after true
                         (validate a# s#)))
               res# (do ~@body)]
           (with-update-exception AssertionError (str "\nschema check failed for return value from fn: " ~name) :after true
             (validate ~return-schema res#)))))))

(defmacro fnv
  "define validated anonymous function.
  usage: (fnv
           [Long Long -> Long]
           [x y]
           (+ x y))"
  [sig args & forms]
  `(var-get
    (defnv ~(gensym)
      ~sig
      ~args
      ~@forms)))
