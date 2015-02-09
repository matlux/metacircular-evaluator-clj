(ns clj-eval.core)

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

(comment
  (pair '(x y z) '(1 2 3))
  )

(defn pair? [sx]
  (= (count sx) 2))

; Some aliases.
(def append concat)

(defn third [x]
  (second (rest x)))
(defn fourth [x]
  (second (rest (rest x))))

(defn error [& msg] (apply println msg))
(defn last-exp? [exps] (= (count exps) 1))


(comment

  (def exp '+)
  (def exp 'y)
  (l-eval exp env)
  (def exp '(+ 1 1))

  )


(declare self-evaluating? variable? lookup-var quoted? text-of-quotation assignment? eval-assignment definition? eval-definition
         if? eval-if lambda? make-procedure lambda-parameters lambda-body
         begin? eval-sequence begin-actions cond? cond->if application? l-apply operator operands list-of-values error
         primitive-procedure? apply-primitive-procedure no-operands? first-operand rest-operands
         compound-procedure? eval-sequence procedure-parameters procedure-environment procedure-body
         extend-environment if-predicate if-consequent if-alternative tagged-list? lambda?)

(defn l-eval [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-var exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
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
                 (list-of-values (operands exp) env))
        :else
        (error "Unknown expression type -- EVAL" exp)))

(comment
  (def procedure (list 'primitive +))
  (def args '(1 1))
  )

(defn l-apply [procedure args]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure args)
        (compound-procedure? procedure)
        (eval-sequence
         (procedure-body procedure)
         (extend-environment
          (procedure-parameters procedure)
          args
          (procedure-environment procedure)))
        :else (error "Unknown procedure type -- APPLY" procedure)))

(comment
  (list-of-values '(2 (l-quote 2 3)) env)
  (def exps '(x))
  (def exp 'x)
  (def exps '((+ 2 x)))
  (eval-sequence '(+ 3 y) env)
  (l-eval '(+ 3 y) env)
  (def exp '(cons 1 '(2 3)))

  (if-consequent '(if true "a" "b"))
  (if-alternative '(if true "a" "b"))
  (if-consequent '(if true "a" "b"))

  )

(defn text-of-quotation [txt]
  (do (println txt) (rest txt)))

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

(defn primitive-implementation [proc] (second proc))
(def primitive-procedures
  {'+ +
   '- -
   'cons cons
   '= =
   'println println
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
        (= 'procedure (and (seq? exp) (first exp))) true
        :else false))
(comment
(def proc procedure)
  (boolean? false)
  (type (type true))

(def exp '(procedure [x] ((+ 2 x)) {f (fn [a b] (+ a b)), g (fn [a b] (- a b)), x 1, y 2, z 2}))
  )

(defn variable? [exp] (symbol? exp))

(defn lookup-var [var env]
  (let [item (env var)]
    (if (tagged-list? item 'fn)
      (l-eval item env)
      item)))

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

(defn definition? [exp]
  (tagged-list? exp 'defn))

(comment
  (lambda? '(fn [x] (+ 2 x)))
   (primitive-procedure-names)
   (primitive-procedure-objects)
   (primitive-implementation '(primitive sadfsafs) (1 2))
   env
  )

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

(def ^:private env
  (extend-environment (primitive-procedure-names)
                      (primitive-procedure-objects)
                      (read-string "{
f (fn [a] (+ 1 a))
g (fn [a b] (+ a b))
x 1
y 2
z 2
}")))

(comment


  (lookup-var 'y env)
  (quoted? '(quote (1 2 3) ))
  (pair? '( 1 2))

  (def exp '(cond (true 2)
          (:else 4)))

  (def exp '(cond (false 2)
                  (:else 4)))

  (def exp '(cond ((= x 2) 42)
                  ((= x 3) -1)
                  ((= x 4) -2)
          (:else 4)))

  )
