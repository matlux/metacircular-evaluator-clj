(ns clj-eval.core
  (:require [clojure.core.match :refer [match]]
            [clojure.set :refer [union]]
            [alex-and-georges.debug-repl :refer [debug-repl]]
            [clojure.tools.trace :refer [trace deftrace]]))

(defn atom? [x]
  (or (not (seq? x))
      (empty? x)))

(defn null? [x]
  (and (seq? x)
       (empty? x)))

(defn boolean? [x]
  (= (type x) java.lang.Boolean))

(defn pair
  [xs ys]
  {:pre [(= (count xs) (count ys))]}
  (into {} (map vector xs ys)))

(defn pair? [sx]
  (= (count sx) 2))

; Some aliases.
(def append concat)

(defn third [x]
  (second (rest x)))
(defn fourth [x]
  (second (rest (rest x))))

(defn error [& msg] (throw (Exception. (apply str msg))))
(defn last-exp? [exps] (= (count exps) 1))


;; Y combinator
(defn Y [m] ;;  λf
  ((fn [x] ;; λx
     (x x)) ;; f(x x)
    (fn [x] ;; λx
      (m (fn [& arg]
           (apply (x x) arg))))))

(defmacro recdef-old [namefn lambda]
  (list 'def namefn (list 'Y (list 'fn [namefn] lambda))))

(defmacro defrec [namefn lambda]
  `(def ~namefn (Y (fn [~namefn] ~lambda))))

(declare let->lambda)
(defn unbound-symbols [form]
  (match form
         (['fn params body] :seq) (apply disj (unbound-symbols body) params)
         (['def name body] :seq) (unbound-symbols body)
         (['if pred then else] :seq) (union (unbound-symbols pred) (unbound-symbols then) (unbound-symbols else))
         (['let bindings body] :seq) (unbound-symbols (let->lambda ['let bindings body]))
         (['let1 [sym binding] body] :seq) (union (unbound-symbols binding) (disj (unbound-symbols body) sym))
         (['quote _] :seq) #{}
         ([f & exprs] :seq) (apply union (unbound-symbols f) (map unbound-symbols exprs))
         (coll :guard coll?) (apply union (map unbound-symbols coll))
         (sym :guard symbol?) #{sym}
         _ #{}))


(declare self-evaluating? variable? lookup-var quoted? text-of-quotation assignment? eval-assignment definition? eval-definition
         if? eval-if lambda? make-procedure lambda-parameters lambda-body
         begin? eval-sequence begin-actions cond? cond->if application? l-apply operator operands list-of-values error
         primitive-procedure? apply-primitive-procedure no-operands? first-operand rest-operands
         compound-procedure? eval-sequence procedure-parameters procedure-environment procedure-body
         extend-environment if-predicate if-consequent if-alternative tagged-list? lambda?
         let? macro-expansion? l-macroexpand macro?)

(comment

(def exp '(list (l-quote def) namefn (list (l-quote Y) (list (l-quote fn) (vector namefn) lambda))))

(def exp '(defrec curried-foldl (fn [f] (fn [val] (fn [coll] (if (= (count coll) 1) (f val (first coll)) (((curried-foldl f) (f val (first coll))) (rest coll))))))))
(def exp '(def x 43))
(def exp 'list)
(def k identity)
(l-eval (operator exp) env k)
(trampoline (l-eval exp env k))

  )

(defn l-eval [exp env k]
  ;;(println "l-eval " exp)
  #(let [ktest (fn [r] (println "res=" r) (k r))] (cond (self-evaluating? exp) (k exp)
          (variable? exp) (lookup-var exp env k)
          (quoted? exp) (k (text-of-quotation exp))
          (assignment? exp) (k (eval-assignment exp env))
          (definition? exp) (eval-definition exp env k)
          (let? exp) (l-eval (let->lambda exp) env k)
          (if? exp) (eval-if exp env k)
          (lambda? exp)
          (k (make-procedure (lambda-parameters exp)
                             (lambda-body exp)
                             env))
          (begin? exp)
          (eval-sequence (begin-actions exp) env k)
          (cond? exp) (l-eval (cond->if exp) env k)
          (macro-expansion? exp) (l-eval (second exp) env (fn [r]
                                                            (k (l-macroexpand r env))))
          (macro? exp env) (l-eval (trampoline (l-macroexpand exp env)) env k)
          (application? exp)
          (l-eval (operator exp) env
                  (fn [op-r] (list-of-values (operands exp) env
                                            (fn [lv-r] (l-apply op-r lv-r env k)))))
          ;;(l-apply (l-eval (operator exp) env)
          ;;         (list-of-values (operands exp) env) env k)
          :else
          (error "Unknown expression type -- EVAL" exp))))

(defn l-apply [procedure args global-env k] ;; needed to add the env for recursivity to work so symbol lookup could fall back onto global env. But that's not great because that means recursive function lookup is dynamic (rather than lexical) binding
  (cond (primitive-procedure? procedure) (k (apply-primitive-procedure procedure args))
        (compound-procedure? procedure)
        (eval-sequence
         (procedure-body procedure)
         (merge global-env (extend-environment
           (procedure-parameters procedure)
           args
           (procedure-environment procedure))) k)
        :else (error "Unknown procedure type -- APPLY" procedure)))

(defn text-of-quotation [txt]
  (second txt))

(defn list-of-values-direct [exps env]
  (if (no-operands? exps)
      '()
      (cons (l-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defn list-of-values [exps env k]
  (if (no-operands? exps)
      #(k '())
      #(l-eval (first-operand exps) env
               (fn [first-op] (list-of-values (rest-operands exps) env
                                             (fn [rest-op] (k (cons first-op rest-op))))))
      ))

(defn extend-environment [procedure-parameters
                          args
                          procedure-environment]
  (merge procedure-environment (pair procedure-parameters args )))

  (defn define-variable! [var val env]
    (assoc env var val))

(defn eval-if-direct [exp env k]
  (if (true? (l-eval (if-predicate exp) env))
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

(defn eval-if [exp env k]
  (l-eval (if-predicate exp) env
          (fn [r] (if r
                   (l-eval (if-consequent exp) env k)
                   (l-eval (if-alternative exp) env k)))))

(defn if? [exp] (tagged-list? exp 'if))
(defn if-predicate [exp] (second exp))
(defn if-consequent [exp] (third exp))
(defn if-alternative [exp]
  (if (not (null? (-> exp rest rest rest)))
      (fourth exp) nil))

(defn eval-sequence [exps env k]
  (cond (last-exp? exps) (l-eval (first exps) env k)
        :else (do (l-eval (first exps) env (fn [r] (eval-sequence (rest exps) env k)))
                  )))
(defn macro? [exp env]
  ;;(println "macro? " exp)
  (and (not (list? (operator exp))) (tagged-list? (lookup-var (operator exp) env identity) 'macro)
       (not (tagged-list? (operator exp) 'macro))))

(defn macro-expansion? [exp]
  (tagged-list? exp 'macroexpand))

(defn l-macroexpand [exp env]
  (let [[_ params body] (lookup-var (operator exp) env identity)
        proc-with-params (cons (make-procedure params body env) (operands exp))]
    (l-apply (operator proc-with-params)
             (operands proc-with-params) env identity)))


(defn let? [exp] (or (tagged-list? exp 'let) (tagged-list? exp 'let1)))

  ;; (let1 [a b] c) -> ((fn [a] c) b)
(defn let1->lambda [bindings body]
  (let [[sym value] bindings]
    (if (empty? bindings)
      body
      (list (list 'fn [sym] body) value))))

  ;; (let [a b c d] (+ a b c d) -> (let1 [a b] (let1 [c d] (+ a b c d)))
(defn bindings->let1 [bindings body]
  (let [[sym value & r] bindings]
    (if (empty? bindings)
      body
      (list 'let1 [sym value] (bindings->let1 r body)))))

(defn let->lambda [exp]
  (let [[_ bindings body] exp
        nb (count bindings)]
      (cond (odd? nb) (error "let should have an even number of bindings")
            (not (vector? bindings)) (error "let bindings should be a vector")
            (tagged-list? exp 'let1) (let1->lambda bindings body)
            :default (bindings->let1 bindings body)))

    )



(defn primitive-implementation [proc] (second proc))
(def primitive-procedures
  {'+ +
   '- -
   'cons cons
   'count count
   'first first
   'rest rest
   'range range
   'empty? empty?
   '= =
   'println println
   'list    list
   'vector  vector
   })
(defn primitive-procedure-names []
  (keys primitive-procedures))

(defn primitive-procedure-objects []
  (map (fn [proc] (list 'primitive (second proc))) primitive-procedures))

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(defn apply-primitive-procedure [proc args]
  (apply
   (primitive-implementation proc) args))

(defn make-procedure [parameters body env]
  (let [usyms (unbound-symbols (list 'fn parameters body))]
    (list 'procedure parameters body (select-keys env usyms))))

(defn compound-procedure? [p]
  (tagged-list? p 'procedure))
(defn procedure-parameters [p] (second p))
(defn procedure-body [p] (third p))
(defn procedure-environment [p] (fourth p))

(defn self-evaluating? [exp]
  (cond (number? exp) true
        (string? exp) true
        (boolean? exp) true
        (nil? exp) true
        (and (seq? exp) (empty? exp)) true
        (= 'procedure (and (seq? exp) (first exp))) true
        :else false))

(defn variable? [exp] (symbol? exp))

(defn lookup-var [var env k]
  (let [item (env var)]
    (cond ;(tagged-list? item 'fn) (l-eval item env k)
          (nil? item) (k (error var " is not a valid symbol"))
          :else (k item))))

(defn quoted? [exp]
  (tagged-list? exp 'l-quote))

(defn tagged-list? [exp tag]
  (= (and (seq? exp) (first exp)) tag))

(defn assignment? [exp]
  (tagged-list? exp 'set!))
(defn assignment-variable [exp] (second exp))
(defn assignment-value [exp] (first (rest (rest exp))))

(defn make-lambda [parameters body]
  (cons 'fn (cons parameters body)))

(defn definition? [exp]
  (tagged-list? exp 'def))

(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (third exp)))
(defn definition-value [exp]
  (if (symbol? (second exp))
    (third exp)
    (make-lambda (rest (second exp))
                 (rest (rest exp)))))

(defn eval-definition [exp env k]
  (l-eval (definition-value exp) env
          (fn [r]
            (k (list 'updated-env (define-variable! (definition-variable exp) r
                                  env))))))

(defn lambda? [exp] (tagged-list? exp 'fn))
(defn lambda-parameters [exp] (second exp))
(defn lambda-body [exp] (rest (rest exp)))

(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))
(defn no-operands? [ops] (empty? ops))
(defn first-operand [ops] (first ops))
(defn rest-operands [ops] (rest ops))

(defn application? [exp] (not (nil? exp)))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn begin? [exp] (tagged-list? exp 'do))
(defn begin-actions [exp] (rest exp))
(defn last-exp? [seq] (null? (rest seq)))
(defn first-exp [seq] (first seq))
(defn rest-exps [seq] (rest seq))

(defn make-begin [seq] (cons 'do seq))

(defn sequence->exp [seq]
  (cond (null? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn cond? [exp] (tagged-list? exp 'cond))
(defn cond-clauses [exp] (rest exp))
(defn cond-predicate [clause] (first clause))
(defn cond-else-clause? [clause]
  (= (cond-predicate clause) :else))

(defn cond-actions [clause] (rest clause))

(defn expand-clauses [clauses]
  (if (null? clauses)
    'false ; no else clause
    (let [first (first clauses)
          rest (rest clauses)]
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if
         (cond-predicate first)
         (sequence->exp (cond-actions first))
         (expand-clauses rest))))))
(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))



(defn l-eval-root [exp env]
  (trampoline #(l-eval exp env identity)))

(defn load-expr [[_ env] exp]
  (let [output (try
                 (l-eval-root exp env)
                 (catch Exception e (str (.printStackTrace e) (.getMessage e))))]
    ;;(println output)
    (if (tagged-list? output 'updated-env)
      ['ok (second output)]
      [output env])
    ))

(defn load [^String script-content env]
  (let [script (read-string (str "[" script-content "]"))]
    (reduce load-expr [nil env] script)))

  (def env
    (second (load "
(def f (fn [a] (+ 1 a)))
(def g (fn [a b] (+ a b)))

(def Y (fn [m]
         ((fn [x]
            (x x))
           (fn [x]
             (m (fn [arg]
                  ((x x) arg)))))))

(defrec curried-map (fn [f]
                  (fn [coll]
                    (if (empty? coll)
                      ()
                      (cons (f (first coll)) ((curried-map f) (rest coll)))))))
(def map (fn [f coll]
                    ((curried-map f) coll)))
(def map-cps (fn [f coll k]
  (if (empty? coll)
    (k ())
    (map-cps f (rest coll) (fn [r]
                             (k (cons (f (first coll)) r))) ))))

(defrec curried-filter (fn [f]
                        (fn [coll]
                          (if (empty? coll)
                            ()
                            (if (f (first coll))
                              (cons (first coll) ((curried-filter f) (rest coll)))
                              ((curried-filter f) (rest coll)))))))
(def filter (fn [f coll]
           ((curried-filter f) coll)))
(defrec curried-foldl (fn [f]
                        (fn [val]
                          (fn [coll]
                            (if (= (count coll) 1)
                              (f val (first coll))
                              (((curried-foldl f) (f val (first coll))) (rest coll)))))))

(def foldl (fn [f val coll]
             (((curried-foldl f) val) coll)))

(def identity (fn [x] x))

(def x 1)
(def y 2)

                               " (extend-environment (primitive-procedure-names)
                                                     (primitive-procedure-objects)
                                                     (read-string "{
  defrec (macro [namefn lambda]
                ((list (l-quote def) namefn (list (l-quote Y) (list (l-quote fn) (vector namefn) lambda)))))

  debug  (macro [body] ((list (l-quote do) (list (l-quote println) \"hellow\") body)))
}")))))

(defn read-form [prefix]
  (let [new-line (str prefix (read-line))]
    (try
      (read-string new-line)
      (catch RuntimeException e
        (if (= (.getMessage e) "EOF while reading")
          (read-form new-line)
          (throw e))))))

(defn repl-loop [env]
    (print "REPL> ")
    (flush)
    (let [output (try
                   (let [form (read-form "")]
                     (l-eval-root form env))
                   (catch Exception e (str (.printStackTrace e) (.getMessage e)))
                   (catch StackOverflowError e (str (.printStackTrace e) (.getMessage e))))]
      (println output)
      (if (tagged-list? output 'updated-env)
        (recur (second output))
        (recur env))
      ))

  (defn -main []
    (repl-loop env))
