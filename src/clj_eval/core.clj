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


;; Y combinator
(defn Y [m] ;;  λf
  ((fn [x] ;; λx
     (x x)) ;; f(x x)
    (fn [x] ;; λx
      (m (fn [arg]
           ((x x) arg))))))

(defmacro recdef-old [namefn lambda]
  (list 'def namefn (list 'Y (list 'fn [namefn] lambda))))

(defmacro defrec [namefn lambda]
  `(def ~namefn (Y (fn [~namefn] ~lambda))))




(declare self-evaluating? variable? lookup-var quoted? text-of-quotation assignment? eval-assignment definition? eval-definition
         if? eval-if lambda? make-procedure lambda-parameters lambda-body
         begin? eval-sequence begin-actions cond? cond->if application? l-apply operator operands list-of-values error
         primitive-procedure? apply-primitive-procedure no-operands? first-operand rest-operands
         compound-procedure? eval-sequence procedure-parameters procedure-environment procedure-body
         extend-environment if-predicate if-consequent if-alternative tagged-list? lambda?
         let? let->lambda macro? l-macroexpand macro-expansion?)

(comment

  (def Y
    (fn [m]
      ((fn [x]
         (x x))
        (fn [x]
          (m (fn [arg]
               ((x x) arg)))))))


  (defrec factorial
          (fn [n]
            (if (= n 0)
              1
              (* n (factorial (- n 1))))))

  (def factorial
    (fn [n]
      (if (= n 0)
        1
        (* n (factorial (- n 1))))))

  (def almost-factorial
    (fn [f]
      (fn [n]
        (if (= n 0)
          1
          (* n (f (- n 1)))))))


  (macroexpand '(defrec factorial
                        (fn [n]
                          (if (= n 0)
                            1
                            (* n (factorial (- n 1)))))))

  (defmacro defrec [namefn lambda]
    (list 'def namefn (list 'Y (list 'fn [namefn] lambda))))

    (def exp '(def Y (fn [m]
                       ((fn [x]
                          (x x))
                         (fn [x]
                           (m (fn [arg]
                                ((x x) arg))))))))
  ;; ((fn [m] ((fn [x] (x x)) (fn [x] (m (fn [arg] ((x x) arg))))))

    (factorial 5)
    ((Y almost-factorial) 5)

    (macroexpand '(recdef fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))))

    (def fib-definition '(Y (fn [fib] (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))))
    (macroexpand '(devrec foo (fn [x] (if (= x 99) x (foo (+ x 1))))))

    (let [[_ new-env] (l-eval fib-definition env)]
      (l-eval '(fib 17) new-env))

    (def exp '(defrec foo (fn [x] (if (= x 99) x (foo (+ x 1))))))
    (def exp '(defrec foo (fn [x] (if (= x 99) x (foo (+ x 1))))))
    (def exp '(def foo (Y (fn [foo] (fn [x] (if (= x 99) x (foo (+ x 1))))))))
    (def exp '(macroexpand (l-quote (defrec foo (fn [x] (if (= x 99) x (foo (+ x 1))))))))
    (l-eval '(defrec foo (fn [x] (if (= x 99) x (foo (+ x 1))))) env)

  ()
    '(def foo
       (Y (fn [foo]
            (procedure [x] ((if (= x 99) x (foo (+ x 1))))))))

    (def exp '((procedure [namefn lambda] (list (quote def) namefn (list (quote Y) (list (quote fn) [namefn] lambda))))))

    (def exp '((procedure [namefn lambda] (list (l-quote def) namefn (list (l-quote Y) (list (l-quote fn) [namefn] lambda))) env)
                (l-quote foo) (l-quote (fn [x] (if (= x 99) x (foo (+ x 1)))))))

    (l-eval '((procedure [body] ((l-quote do) (println "hellow") body) {}) (+ 1 2)), env)

    (list 'do (list (quote println) "hellow") (+ 1 2))

    (def exp '(debug (+ 1 2)))
    (def exp '(macroexpand (l-quote (debug (+ 1 2)))))

    ;; target
    (def exp (list (list 'procedure ['body] (list (list 'list (list 'l-quote 'do) (list 'list (list 'l-quote 'println) "hellow") 'body)) env) '(+ 1 2)))


    (def exp (list (list 'procedure ['a] (list ('+ 1 'a)) env) 2))

    (let [[[proc params body env] vals] (l-eval exp env)]
      (list (list proc params body 'env) vals))
  (l-eval exp env)
  (def env (second (l-eval exp env)))
  (l-eval '(foo 4) env)
  {Y (procedure [m] (((fn [x] (x x)) (fn [x] (m (fn [arg] ((x x) arg)))))) {})}

    (l-apply (operator (l-eval exp env))
             (operands (l-eval exp env)) env)
    (l-apply (operator exp)
             (operands exp) env)
    (def exp '(do (+ 1 2)))

    (def exp '(def x 43))
    (def exp '(fn [a] (+ 1 a)))
    (l-eval ((fn [x] (+ 1 x) 2)) env)

  (l-eval '(+ 1 2) env)
    (def exp-old exp)

    (def exp (first exps))
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
          (macro-expansion? exp) (l-macroexpand (l-eval (second exp) env) env)
          (macro? exp env) (l-eval (l-macroexpand exp env) env)
          (application? exp)
          (l-apply (l-eval (operator exp) env)
                   (list-of-values (operands exp) env) env)
          :else
          (error "Unknown expression type -- EVAL" exp)))

  (comment

    (def args '(foo (fn [x] (if (= x 99) x (foo (+ x 1))))))
    (def procedure (list 'procedure ['namefn 'lambda] '((list (l-quote def) namefn (list (l-quote Y) (list (l-quote fn) [namefn] lambda)))) env))
    ((list (l-quote def) namefn (list (l-quote Y) (list (l-quote fn) [namefn] lambda))))

    (def exps (procedure-body procedure))
    (def env (extend-environment
               (procedure-parameters procedure)
               args
               (procedure-environment procedure)))
    )

  (defn l-apply [procedure args global-env]                 ;; needed to add the env for recursivity to work so symbol lookup could fall back onto global env. But that's not great because that means recursive function lookup is dynamic (rather than lexical) binding
    (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure args)
          (compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              args
              (procedure-environment procedure)))
          :else (error "Unknown procedure type -- APPLY (" procedure " " args ")")))

  (defn text-of-quotation [txt]
    (second txt))

  (comment
    (def exps (operands exp))
    (def exps (rest-operands exps))

    (def exp (first-operand exps))
    )

  (defn list-of-values [exps env]
    (if (no-operands? exps)
      '()
      (cons (l-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

  (defn extend-environment [procedure-parameters
                            args
                            procedure-environment]
    (merge procedure-environment (pair procedure-parameters args)))

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

  (defn macro? [exp env]
    (and (not (list? (operator exp))) (tagged-list? (lookup-var (operator exp) env) 'macro)
         (not (tagged-list? (operator exp) 'macro))))

  (defn macro-expansion? [exp]
    (tagged-list? exp 'macroexpand))


  (comment

    (def procedure (let [[_ params body] (lookup-var (operator exp) env)
                         proc-with-params (cons (make-procedure params body env) (operands exp))]
                     (operator proc-with-params)))

    (def args (let [[_ params body] (lookup-var (operator exp) env)
                    proc-with-params (cons (make-procedure params body env) (operands exp))]
                (operands proc-with-params)))

    )

  (defn l-macroexpand [exp env]
    (let [[_ params body] (lookup-var (operator exp) env)
          proc-with-params (cons (make-procedure params body env) (operands exp))]
      (l-apply (operator proc-with-params)
               (operands proc-with-params) env)))



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
    {'+       +
     '-       -
     'cons    cons
     'count count
     'first first
     'rest rest
     'range range
     'empty? empty?
     '=       =
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
      (cond                                                 ;;(tagged-list? item 'fn) (l-eval item env)
        (nil? item) (error var " is not a valid symbol")
        :else item)))

  (defn quoted? [exp]
    (tagged-list? exp 'l-quote))

  (defn tagged-list? [exp tag]
    (= (and (seq? exp) (first exp)) tag))

  (comment (tagged-list? 'y 'fn)
           (lookup-var exp env)
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
                                         (l-eval (definition-value exp) (dissoc env (definition-variable exp)))
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
      'false                                                ; no else clause
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

(defrec map (fn [f coll]
           (if (empty? coll)
             ()
             (cons (f (first coll)) (map f (rest coll))))))

(defrec filter (fn [f coll]
              (if (empty? coll)
                ()
                (if (f (first coll))
                  (cons (first coll) (filter f (rest coll)))
                  (filter f (rest coll))))))

(defrec foldl (fn [f val coll]
             (if (= (count coll) 1)
               (f val (first coll))
               (foldl f (f val (first coll)) (rest coll)))))

(def x 1)
(def y 2)

                               " (extend-environment (primitive-procedure-names)
                                                     (primitive-procedure-objects)
                                                     (read-string "{
  defrec (macro [namefn lambda]
                ((list (l-quote def) namefn (list (l-quote Y) (list (l-quote fn) (vector namefn) lambda)))))

  debug  (macro [body] ((list (l-quote do) (list (l-quote println) \"hellow\") body)))
}")))))



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


(comment
Y  (fn [m]
    ((fn [x]
       (x x))
      (fn [x]
        (m (fn [arg]
             ((x x) arg))))))

  )





