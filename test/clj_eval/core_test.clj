(ns clj-eval.core-test
  (:require [clojure.test :refer :all]
            [clj-eval.core :refer :all]))


(deftest primitives
  (testing "Evaluation of primitives should work"
    (is (= (l-eval 42 env) 42))
    (is (= (l-eval true env) true))
    (is (= (l-eval "abc" env) "abc"))
    (is (= (l-eval () env) ()))
    (is (= (l-eval '(procedure _ _ _) env) '(procedure _ _ _)))))

(deftest variables
  (testing "Evaluation of variables should work"
    (is (= (l-eval 'x env) 1))
    (is (= (l-eval 'y env) 2))
    (is (= (try (l-eval 'do-not-exist env)
                (catch Exception e (.getMessage e))) "do-not-exist is not a valid symbol"))
    (is (= (drop-last (l-eval 'f env)) '(procedure [a] ((+ 1 a)))))))

(deftest special-foms
  (testing "Evaluation of special forms should work"
    (is (= (l-eval '(l-quote (5 6)) env) '(5 6)))
    (is (= (l-eval '(if true 5 6) env) 5))
    (is (= (l-eval '(if false 5 6) env) 6))

    ))

(deftest let-test
  (testing "Evaluation of let blocks should work"
    (is (= (l-eval '(let [] 3) env) 3))
    (is (= (l-eval '(let [a 3] a) env) 3))
    (is (= (l-eval '(let [a 3 b 4] (+ a b)) env) 7))
    (is (= (l-eval '(let [a (+ 1 2) b (+ a 1)] (+ a b)) env) 7))
    (is (= (l-eval '(let [a (+ 1 2) b (+ a 1) c (+ a b)] (+ a b c)) env) 14))
    (is (= (l-eval '(let [f (fn [a b c] (+ a b c)) a (+ 1 2) b (+ a 1) c (+ a b)] (f a b c)) env) 14))
    (is (= (l-eval '(let [f (fn [x y z] (+ x y z)) a (+ 1 2) b (+ a 1) c (+ a b)] (f a b c)) env) 14))

    ))

(deftest definitions
  (testing "Evaluation of variables should work"
    (is (= (l-eval '(def a 42) env) (list 'updated-env (merge env {'a 42}))))

    ))

(deftest expressions
  (testing "Evaluation of expressions should work"
    (is (= (l-eval '(+ 1 x) env) 2))
    (is (= (l-eval '(+ x y) env) 3))
    (is (= (l-eval '(+ (+ x y) 1) env) 4))
    (is (= (l-eval '(- (+ x y 3) 1) env) 5))
    (is (= (l-eval '(+ (+ 2 3) 1) env) 6))
    (is (= (l-eval (list 'cons 1 (list 'l-quote '(2 3))) env) '(1 2 3)))))

(deftest lambdas
  (testing "Evaluation of anonymous functions should work"
    (is (= (l-eval '((fn [x] (+ 1 x)) 42) env) 43))
    (is (= (l-eval '((fn [x y] (+ x y)) 42 42) env) 84))
    (is (= (l-eval '((fn [x y] (+ ((fn [] (+ x 1))) y)) 42 42) env) 85))
    (is (= (l-eval '((fn [x y] (+ ((fn [b] (+ x b)) 3) y)) 42 42) env) 87))
    (is (= (l-eval '((fn [x y] (+ ((fn [y] (+ x y)) 3) y)) 42 42) env) 87))))


(deftest functions
  (testing "Calls to functions should work"
    (is (= (l-eval '(f 2) env) 3))
    (is (= (l-eval '(g 2 3) env) 5))
    (is (= (l-eval '(f (g 2 3)) env) 6))))

(deftest std-functions
  (testing "Eval standard functions should work"
    (is (= (l-eval (list 'map '(fn [x] (+ 1 x)) '(l-quote (1 2 3 ))) env) '(2 3 4)))
    (is (= (l-eval '(map (fn [x] (+ 1 x)) (l-quote (1 2 3 ))) env) '(2 3 4)))
    (is (= (l-eval '(filter (fn [x] (= 2 x)) (l-quote (1 2 3 ))) env) '(2)))
    (is (= (l-eval '(foldl + 0 (range 99)) env) 4851))
    ))


(deftest cond-test
  (testing "Calls to cond should work"
    (is (= (l-eval '(cond (true 2)
                          (:else 4)) env) 2))

    (is (= (l-eval '(cond (false 2)
                          (:else 4)) env) 4))
    ))

(deftest begin
  (testing "Calls to do should work"
    (is (= (l-eval '(do (println "testing side effect")
                          (+ 2 2)) env) 4))


))

(deftest standard-functions
  (testing "Calls to functions should work"
    (is (= (l-eval '(cons 1 (cons 2 ())) env) '(1 2)))
    (is (= (l-eval '(= 1 2) env) false))
    (is (= (l-eval '(= 1 1) env) true))
    (is (= (l-eval '(= 1 2) env) false))
    (is (= (l-eval '(count  (l-quote (1 2))) env) 2))
    (is (= (l-eval '(empty?  (l-quote (1 2))) env) false))

    ))

(deftest test-load
  (testing "loading a script"
    (is (= (first (load "
(def a 42)
a
" env)) 42))))

(deftest test-recursive-fction
  (testing "reloading recurcive fction and making sure they work"
    (is (= (first (load "
 (defrec foo (fn [x] (if (= x 99) x (foo (+ x 1)))))
 (def res (foo 4))
 (defrec foo (fn [x] (if (= x 42) x (foo (+ x 1)))))
 (list res (foo 4))
" env)) '(99 42)))))

(deftest test-lexical-binding
  (testing "loading a script"
    (is (= (first (load "
(def x 42)
(def f (fn [] x))
(f)
((fn [x] (f)) 7)
" {})) 42))
    (is (= (first (load "
(def x 42)
(def f (fn [] x))
(f)
((fn [x] (f)) 7)
x
" {})) 42))
     (is (= (first (load "
(def w 0)
(def foo (fn [] w))
(let [w 42]
   (foo))
" env)) 0))

    ;; this is a bug, this should be equal
    (is (not= (first (load "
(def foo (fn [x] (list x w)))
(def bar (fn [w] (foo 1991)))
(def w 0)
(list (bar 100) (foo 3))
" env)) '((1991 0) (3 0))))

    ))

;; currently the evaluator is dynamic binding
(comment

  (deftest test-dynamic-binding
   (testing "loading a script"
     (is (= (first (load "

(def foo (fn [] w))
(let [w 42]
   (foo))
" env)) 42))
     (is (= (first (load "
(def foo (fn [x] (list x w)))
(def bar (fn [w] (foo 1991)))
(def w 0)
(list (bar 100) (foo 3))
" env)) '((1991 100) (3 0)))))))

(deftest test-dynamic-binding-bug
  (testing "a bug"
      (is (= (first (load "
(def foo (fn [x] (list x w)))
(def bar (fn [w] (foo 1991)))
(def w 0)
(list (bar 100) (foo 3))
" (assoc env 'w 2))) '((1991 2) (3 2)))) ))

(def  fib-definition-old '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))))
(def fib-definition '(def fib2 (Y (fn [fib] (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))))))

(deftest definition-recurcivity-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval fib-definition env)]
             (l-eval '(fib2 17) new-env)) 1597))))

(deftest StackOverflowError-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval fib-definition env)]
             (first (l-eval '(map fib2 (map (fn [_] 5) (range 500))) new-env))) 5))

    (is (= (try (let [[_ new-env] (l-eval fib-definition env)]
                  (l-eval '(map fib2 (map (fn [_] 5) (range 3000))) new-env))
                (catch StackOverflowError e (.toString e))) "java.lang.StackOverflowError"))
    ))


(comment
  (let [[_ new-env] (l-eval '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) env)]
    new-env)
  )
