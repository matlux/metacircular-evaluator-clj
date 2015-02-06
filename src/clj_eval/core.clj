(ns clj-eval.core)

(defn atom? [x]
  (or (not (seq? x))
      (empty? x)))

(defn null? [x]
  (and (seq? x)
       (empty? x)))

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
 (def program (read-string "1"))
  (def program1 (read-string "y"))
  (def program2 (read-string "(+ 1 2)"))
  (def exp (read-string "((fn [x] (+ 2 x)) 3)"))
  (def exp (read-string "(fn [x] (+ 2 x))"))

  (def exp program2)
  (def exp '(procedure [x] ((+ 2 x)) {f (fn [a b] (+ a b)), g (fn [a b] (- a b)), x 1, y 2, z 2}))
  (def exp '((procedure [x] ((+ 2 x)) {f (fn [a b] (+ a b)), g (fn [a b] (- a b)), x 1, y 2, z 2}) 1))
  (def exp '+)
  (def exp 'y)
  (l-eval exp env)
  (def exp '(+ 1 1))
  (def exp '(+ 1 y))
  (l-eval (read-string "(+ 2 y)") env)
  (l-eval (read-string "(+ (+ 2 y) 4)") env)
  (l-eval (read-string "((fn [x] (+ 2 x)) 3)") env)
  (l-eval 'f env)
  (l-eval '(f 2) env)
  (l-eval '((fn [x] (+ 1 x)) 42) env)
  (l-eval '(g 3 2) env)
  (l-eval (read-string "(f 2)") env)
  (l-eval (read-string "(f 2)") env)
  (l-eval (read-string "(fn [x] (+ 2 x))") env)
  (l-eval '(procedure [x] ((+ 2 x)) {f (fn [a b] (+ a b)), g (fn [a b] (- a b)), x 1, y 2, z 2}) env)
  (def procedure '(procedure [x] ((+ 2 x)) {f (fn [a b] (+ a b)), g (fn [a b] (- a b)), x 1, y 2, z 2}))
  (operator program2)
  (operands program2)

  (def exp '(f 2))
  (def procedure (l-eval (operator exp) env))
  (def args (list-of-values (operands exp) env))

  (merge (pair '(x y z) '(1 2 3) ) {'a 7})

  (def env '{z 2, y 2, g (fn [a b] (- a b)), f (fn [a b] (+ a b)), x 1})
  (def exps '((+ 2 x)))

  (l-eval '(l-quote "a" "b") env)
  (def exp '(l-quote "a" "b"))
  (l-eval exp env)
  (l-eval '+ env)
  (l-eval '(+ 2 x) env)
  (def exp (first exps))
  (l-eval (first exps) env)
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
        ;;(if? exp) (eval-if exp env)
        (lambda? exp)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env)
        ;;(begin? exp)
        ;;(eval-sequence (begin-actions exp) env)
        ;;(cond? exp) (l-eval (cond->if exp) env)
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
  )

(defn text-of-quotation [txt]
  (do (println txt) (rest txt)))

(text-of-quotation '(l-quote 1 2))

(defn list-of-values [exps env]
  (if (no-operands? exps)
      '()
      (cons (l-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defn extend-environment [procedure-parameters
                          args
                          procedure-environment]
  (merge procedure-environment (pair procedure-parameters args )))


(defn eval-if [exp env]
  (if (true? (l-eval (if-predicate exp) env))
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
  (cond (last-exp? exps) (l-eval (first exps) env)
        :else (do (l-eval (first exps) env)
                  (eval-sequence (rest exps) env))))

(defn primitive-implementation [proc] (second proc))
(def primitive-procedures
  {'+ +
   '- -
   'cons cons
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
        (= 'procedure (and (seq? exp) (first exp))) true
        :else false))
(comment
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

  )
