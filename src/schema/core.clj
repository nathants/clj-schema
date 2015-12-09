(ns schema.core
  (:import clojure.lang.LazySeq
           clojure.lang.Cons))

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
  (if (System/getenv "DISABLE_UPDATE_EXCEPTION")
    body
    `(try
       ~body
       (catch ~cls ex#
         (throw (new ~cls (str (.getMessage ex#) "\n" ~msg) ex#))))))

(defn helpful-message
  [schema value]
  (str "\n\nschema: " schema "\nvalue: " value))

(defn error-message
  [schema value & [msg extra-msg]]
  (str value " <" (type value) "> " (or msg "does not match schema") ": " schema " <" (type schema) ">" (when extra-msg (str "\n" extra-msg))))

;; TODO namespaced keywords? susinct usage? capitalization?
(def -schema-commands
  #{:U ;; union
    :I ;; intersection
    :O ;; optional
    :fn})

(defn -validate
  [schema value & {:keys [exact-match]}]
  (with-update-exception AssertionError (helpful-message schema value)
    ;; TODO type based dispatch better than long conditionals? possible?
    ;; TODO flatten this into un-nested conditionals? perf hit?
    ;; TODO rewrite py-schema in cljc. use macros. do compile time
    ;; transformations so that type lookup is used instead of long chains
    ;; of conditionals. perf boost?
    (let [[schema value] (map #(if (symbol? %) (resolve %) %) [schema value])]
      (cond
        ;; TODO suppport validating a manifold deferred
        (map? schema)
        (do (assert (map? value) (error-message schema value))
            ;; if schema keys are all types, and _value is empty, return. ie, type keys are optional, so {} is a valid {int: int}
            (if (and (= {} value)
                     (= #{Class} (set (map type schema))))
              {}
              ;; TODO is there an fp way to do this that is *more* elegant than current implementation?
              (let [-value (volatile! {})]
                ;; check for items in value that dont satisfy schema, dropping unknown keys unless exact_match=true
                (doseq [[k v] value]
                  (let [value-match (contains? schema k)
                        type-match (some #{(type k)} (filterv class? (keys schema)))
                        any-match (contains? schema :Any)]
                    (cond
                      (or value-match type-match any-match)
                      (let [-schema (get schema (cond value-match k type-match (type k) any-match :Any))]
                        (vswap! -value assoc k (-validate -schema v)))
                      exact-match
                      (throw (AssertionError. (error-message schema value "does not match schema keys")))
                      :else
                      nil)))
                ;; check for items in schema missing in value, filling in optional value
                (doseq [[k v] schema]
                  (when (not (contains? @-value k))
                    (cond
                      (and (sequential? v)
                           (= :O (first v)))
                      (do (assert (= 3 (count v)) (str ":O schema should be [:O, schema, default-value], not: " v))
                          (vswap! -value assoc k (apply -validate (rest v))))
                      (not (class? k))
                      (throw (AssertionError. (error-message schema value "is missing required key"))))))
                @-value)))
        (= schema :Any) value
        (sequential? schema)
        (do (assert (or (sequential? value) (get -schema-commands (first schema))) (error-message schema value "is not sequential"))
            (cond
              (get -schema-commands (first schema))
              (condp = (first schema)
                :O
                (do (assert (= 3 (count schema)) (str ":O schema should be [:O, schema, default-value], not: " schema))
                    (-validate (second schema) value))
                :U ;; TODO is there an fp way to do this that is *more* elegant than current implementation?
                (let [_ (assert (> (count schema) 1) (str "union types cannot be empty: " schema))
                      -value (volatile! value)
                      tracebacks (volatile! [])]
                  (doseq [-schema (rest schema)]
                    (try
                      (vswap! -value #(-validate -schema %))
                      (catch AssertionError ex
                        (vswap! tracebacks conj (.getMessage ex)))))
                  (if (= (count (rest schema)) (count @tracebacks))
                    (throw (AssertionError. (error-message schema value "did not match *any* of" (apply str (interpose "\n" @tracebacks)))))
                    @-value))
                :I ;; TODO is there an fp way to do this that is *more* elegant than current implementation?
                (let [_ (assert (> (count schema) 1) (str "intersection types cannot be empty: " schema))
                      -value (volatile! value)
                      tracebacks (volatile! [])]
                  (doseq [-schema (rest schema)]
                    (try
                      (vswap! -value #(-validate -schema %))
                      (catch AssertionError ex
                        (vswap! tracebacks conj (.getMessage ex)))))
                  (if (seq @tracebacks)
                    (throw (AssertionError. (error-message schema value "did not match *all* of" (apply str (interpose "\n" @tracebacks)))))
                    @-value))
                :fn
                (assert false "TODO implement me. how does this differ from python?"))
              (vector? schema)
              (do (assert (= (count schema) 1) (str "vector schemas represent homogenous seqs and must contain a single schema: " schema))
                  (mapv #(-validate (first schema) %) value))
              :else
              (do (assert (= (count schema) (count value)) (error-message schema value "mismatched length of schema"))
                  (mapv -validate schema value))))
        (class? schema)
        (do (assert (instance? schema value) (error-message schema value "is not a"))
            value)
        (fn? schema)
        (do (assert (schema value) (error-message schema value "failed fn schema"))
            value)
        (var? schema)
        (-validate (var-get schema) value)
        :else
        (do (assert (= schema value) (error-message schema value "does not equal"))
            value)))))

(defmacro validate
  [schema value & {:keys [exact-match]}]
  (if (System/getenv "DISABLE_SCHEMA")
    value
    `(if (and (vector? ~schema)
              (= (count ~schema) 1)
              (instance? LazySeq ~value)
              (not (realized? ~value)))
       (map #(-validate (first ~schema) % :exact-match ~exact-match) ~value)
       (-validate ~schema ~value :exact-match ~exact-match))))

(defmacro defnv
  "define validated function.
  usage: (defnv plus
           [Long Long -> Long]
           [x y]
           (+ x y))"
  [& forms]
  (let [[name doc sig args & body]
        (if (string? (second forms))
          forms
          (apply vector (first forms) "" (drop 1 forms)))
        _ (assert (->> sig (filter #{'->}) count (= 1)) (str "sig should have '-> in it, not: " sig))
        [arg-schema _ [return-schema]] (partition-by #{'->} sig)]
    `(defn ~name
       ~doc
       [& args#]
       (let [~args (with-update-exception AssertionError (str "schema check failed for args to fn: " ~name)
                     (validate (list ~@arg-schema) args#))]
         (with-update-exception AssertionError (str "schema check failed for return value from fn: " ~name)
           (validate ~return-schema (do ~@body)))))))
