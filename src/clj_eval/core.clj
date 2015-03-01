(ns clj-eval.core
  (:require [clojure.core.match :refer [match]]))

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

(declare self-evaluating? variable? lookup-var quoted? text-of-quotation assignment? eval-assignment definition? eval-definition
         if? eval-if lambda? make-procedure lambda-parameters lambda-body
         begin? eval-sequence begin-actions cond? cond->if application? l-apply operator operands list-of-values error
         primitive-procedure? apply-primitive-procedure no-operands? first-operand rest-operands
         compound-procedure? eval-sequence procedure-parameters procedure-environment procedure-body
         extend-environment if-predicate if-consequent if-alternative tagged-list? lambda?
         let? let->lambda)

(comment

(def exp '(def x 43))
  )

(defn l-eval [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-var exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (let? exp) (l-eval (let->lambda exp) env)
        (if? exp) (eval-if exp env)
        (lambda? exp)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env)
        (begin? exp)
        (eval-sequence (begin-actions exp) env)
        (cond? exp) (l-eval (cond->if exp) env)
        (application? exp)
        (l-apply (l-eval (operator exp) env)
                 (list-of-values (operands exp) env) env)
        :else
        (error "Unknown expression type -- EVAL" exp)))

(defn l-apply [procedure args global-env] ;; needed to add the env for recursivity to work so symbol lookup could fall back onto global env. But that's not great because that means recursive function lookup is dynamic (rather than lexical) binding
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure args)
        (compound-procedure? procedure)
        (eval-sequence
         (procedure-body procedure)
         (merge global-env (extend-environment
           (procedure-parameters procedure)
           args
           (procedure-environment procedure))))
        :else (error "Unknown procedure type -- APPLY" procedure)))

(defn text-of-quotation [txt]
  (second txt))

(defn list-of-values [exps env]
  (if (no-operands? exps)
      '()
      (cons (l-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defn extend-environment [procedure-parameters
                          args
                          procedure-environment]
  (merge procedure-environment (pair procedure-parameters args )))

(defn define-variable! [var val env]
  (assoc env var val))

(defn eval-if [exp env]
  (if (true? (l-eval (if-predicate exp) env))
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

(defn if? [exp] (tagged-list? exp 'if))
(defn if-predicate [exp] (second exp))
(defn if-consequent [exp] (third exp))
(defn if-alternative [exp]
  (if (not (null? (-> exp rest rest rest)))
      (fourth exp) nil))

(defn eval-sequence [exps env]
  (cond (last-exp? exps) (l-eval (first exps) env)
        :else (do (l-eval (first exps) env)
                  (eval-sequence (rest exps) env))))

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

(comment

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
   'list list
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
  (list 'procedure parameters body env))

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

(defn lookup-var [var env]
  (let [item (env var)]
    (cond (tagged-list? item 'fn) (l-eval item env)
          (nil? item) (error var " is not a valid symbol")
          :else item)))

(defn quoted? [exp]
  (tagged-list? exp 'l-quote))

(defn tagged-list? [exp tag]
  (= (and (seq? exp) (first exp)) tag))

(comment (tagged-list? 'y 'fn)
         ( lookup-var exp env)
         (def var exp)
         (def item 2)
         (def exp 2))

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

(defn eval-definition [exp env]
  (list 'updated-env (define-variable! (definition-variable exp)
                       (l-eval (definition-value exp) env)
     env)))

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

(def  env
  (extend-environment (primitive-procedure-names)
                      (primitive-procedure-objects)
                      (read-string "{
f (fn [a] (+ 1 a))
g (fn [a b] (+ a b))
map (fn [f coll]
             (if (empty? coll)
               ()
               (cons (f (first coll)) (map f (rest coll)))))
filter (fn [f coll]
             (if (empty? coll)
               ()
               (if (f (first coll))
                 (cons (first coll) (filter f (rest coll)))
                 (filter f (rest coll)))))
foldl (fn [f val coll]
                (if (= (count coll) 1)
                  (f val (first coll))
                  (foldl f (f val (first coll)) (rest coll) )))
x 1
y 2
}")))

(defn load-expr [[_ env] exp]
  (let [output (try
                 (l-eval exp env)
                 (catch Exception e (str (.printStackTrace e) (.getMessage e))))]
    ;;(println output)
    (if (tagged-list? output 'updated-env)
      ['ok (second output)]
      [output env])
    ))

(defn load [^String script-content env]
  (let [script (read-string (str "[" script-content "]"))]
    (reduce load-expr [nil env] script)))

(defn repl-loop [env]
  (print "REPL> ")
  (flush)
  (let [line (read-line)
        output (try
                 (l-eval (read-string line) env)
                 (catch Exception e (str (.printStackTrace e) (.getMessage e)))
                 (catch StackOverflowError e (str (.printStackTrace e) (.getMessage e))))]
    (println output)
    (if (tagged-list? output 'updated-env)
      (recur (second output))
      (recur env))
    ))

(defn -main []
  (repl-loop env))
