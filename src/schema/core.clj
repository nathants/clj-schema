(ns schema.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [clojure.set :as set])
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
;; validate macro, which can be disabled. same for defnv!. perhaps add
;; logged warnings to defnv and validate calls that use optional
;; values.


(def *disable-update-exception*
  "there is zero cost in the happy path, ie no exceptions
  thrown. however, this is significant cost in the sad path. this cost
  is paid to get very helpful messages about what parts of the schema
  mismatche the value. likely you will want these messages for dev,
  but never for prod."
  (doto (boolean (System/getenv "DISABLE_HELPFUL_SCHEMA_ERRORS"))
    (->> (println "disable helpful schema errors:"))))

(def *disable-schema*
  "schema validation is costly. by default, schemas are disabled. you
  likely want them for dev, but not for prod. however, they may be
  useful in prod, if validation is always required."
  (doto (not (System/getenv "ENABLE_SCHEMA"))
    (->> (println "disable schema:"))))

(defn compatible
  "you can only add new keys to schemas, and their values must be
  optional. you can never change existing keys. schema validation
  ignores unknown keys, so as long as this compatability is
  maintained, old code can use new data (ignores keys unknown in the
  past), and new code can use old data (all new keys are optional and
  have default values to fill in the old data)."
  [old new]
  (assert (map? old))
  (assert (map? new))
  (let [ks-old (set (keys old))
        ks-new (set (keys new))
        ks-added (set/difference ks-new ks-old)]
    (assert (set/subset? ks-old ks-new) (str "new schema is missing keys from old schema: " (set/difference ks-old ks-new)))
    (doseq [[k v] old]
      (let [v2 (get new k)]
        (if (map? v)
          (do (assert (map? v2))
              (compatible v v2))
          (assert (= v v2) (str "new schema changes existing value for key " k ", " v " != " v2)))))
    (doseq [k ks-added]
      (let [v (get new k)]
        (assert (vector? v) v)
        (assert (= 3 (count v) ) v)
        (assert (= :O (first v)) v)))))


;; TODO have functions that translate to and from json. sets and
;; lists become arrays, types/fns are converted to a namespaced keyword,
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
         ~(first forms)
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
    ;; TODO type based dispatch better than long conditionals? possible? perf hit?
    ;; TODO flatten this into un-nested conditionals? perf hit?
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
  ;; TODO add support for arity overloading
  ;; TODO multiple arities
  [& forms]
  (let [[name doc sig args & body]
        (if (string? (second forms))
          forms
          (apply vector (first forms) "" (drop 1 forms)))
        _ (assert (->> sig (filter #{'->}) count (= 1)) (error-message "sig should have '-> in it, not:" sig))
        _ (assert (->> sig (partition-by #{'->}) second count (= 1)) (error-message "sig should have only one schema to the right of '->, not:" sig))
        [pos-schema _ [return-schema]] (if (= '-> (first sig))
                                         [() nil [(last sig)]]
                                         (partition-by #{'->} sig))
        pos-schema (if (= '& (first pos-schema))
                     (concat [nil] pos-schema)
                     pos-schema)
        [pos-schema _ [variadic-schema]] (concat (partition-by #(= '& %) pos-schema) [nil nil])
        _ (assert (or (nil? variadic-schema) (vector? variadic-schema) (map? variadic-schema)) (error-message "variadic schema should be vector or map, not:" variadic-schema))
        n-args (count (remove nil? pos-schema))]
    (if *disable-schema*
      `(defn ~name
         ~doc
         ~args
         ~@body)
      `(defn ~name
         {:doc ~doc :arglists '(~args)}
         [& args#]
         (assert (or (= (count args#) (count '~args))
                     (and (some #{'&} '~args)
                          (>= (count args#) (count (take-while #(not= '& %) '~args)))))
                 (str "unknown arity: " (count args#) " for fn: " ~name))
         (let [[pos-args# variadic-args#] (split-at ~n-args (or args# ()))
               validated-args# (doall
                                (concat
                                 (for [[i# s# a#] (map vector (range) (list ~@pos-schema) pos-args#)]
                                   (with-update-exception AssertionError (str "\nschema check failed for args to fn: " ~name ", for argument index: " i#) :after true
                                     (validate s# a#)))
                                 (when (and ~variadic-schema variadic-args#)
                                   (with-update-exception AssertionError (str "\nschema check failed for variadic args to fn: " ~name) :after true
                                     (if (map? ~variadic-schema)
                                       (apply concat (seq (validate ~variadic-schema (apply hash-map  variadic-args#))))
                                       (validate ~variadic-schema variadic-args#))))))
               ~args validated-args#
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
