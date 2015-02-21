(ns clj-eval.core-test
  (:require [clojure.test :refer :all]
            [clj-eval.core :refer :all]))


(deftest primitives
  (testing "Evaluation of primitives should work"
    (is (= (l-eval-root 42 env) 42))
    (is (= (l-eval-root true env) true))
    (is (= (l-eval-root "abc" env) "abc"))
    (is (= (l-eval-root () env) ()))
    (is (= (l-eval-root '(procedure _ _ _) env) '(procedure _ _ _)))))

(deftest variables
  (testing "Evaluation of variables should work"
    (is (= (l-eval-root 'x env) 1))
    (is (= (l-eval-root 'y env) 2))
    (is (= (try (l-eval-root 'do-not-exist env)
                (catch Exception e (.getMessage e))) "do-not-exist is not a valid symbol"))
    (is (= (drop-last (l-eval-root 'f env)) '(procedure [a] ((+ 1 a)))))))

(deftest special-foms
  (testing "Evaluation of variables should work"
    (is (= (l-eval-root '(l-quote (5 6)) env) '(5 6)))
    (is (= (l-eval-root '(if true 5 6) env) 5))
    (is (= (l-eval-root '(if false 5 6) env) 6))

    ))

(deftest definitions
  (testing "Evaluation of variables should work"
    (is (= (l-eval-root '(def a 42) env) (list 'updated-env (merge env {'a 42}))))

    ))

(deftest expressions
  (testing "Evaluation of expressions should work"
    (is (= (l-eval-root '(+ 1 x) env) 2))
    (is (= (l-eval-root '(+ x y) env) 3))
    (is (= (l-eval-root '(+ (+ x y) 1) env) 4))
    (is (= (l-eval-root '(- (+ x y 3) 1) env) 5))
    (is (= (l-eval-root '(+ (+ 2 3) 1) env) 6))
    (is (= (l-eval-root (list 'cons 1 (list 'l-quote '(2 3))) env) '(1 2 3)))))

(deftest lambdas
  (testing "Evaluation of anonymous functions should work"
    (is (= (l-eval-root '((fn [x] (+ 1 x)) 42) env) 43))
    (is (= (l-eval-root '((fn [x y] (+ x y)) 42 42) env) 84))
    (is (= (l-eval-root '((fn [x y] (+ ((fn [] (+ x 1))) y)) 42 42) env) 85))
    (is (= (l-eval-root '((fn [x y] (+ ((fn [b] (+ x b)) 3) y)) 42 42) env) 87))
    (is (= (l-eval-root '((fn [x y] (+ ((fn [y] (+ x y)) 3) y)) 42 42) env) 87))))


(deftest functions
  (testing "Calls to functions should work"
    (is (= (l-eval-root '(f 2) env) 3))
    (is (= (l-eval-root '(g 2 3) env) 5))
    (is (= (l-eval-root '(f (g 2 3)) env) 6))))

(deftest std-functions
  (testing "Eval standard functions should work"
    (is (= (l-eval-root (list 'map '(fn [x] (+ 1 x)) '(l-quote (1 2 3 ))) env) '(2 3 4)))
    (is (= (l-eval-root '(map (fn [x] (+ 1 x)) (l-quote (1 2 3 ))) env) '(2 3 4)))
    (is (= (l-eval-root '(filter (fn [x] (= 2 x)) (l-quote (1 2 3 ))) env) '(2)))
    (is (= (l-eval-root '(foldl + 0 (range 9)) env) 36))
    (is (= (l-eval-root '(foldl + 0 (range 99)) env) 4851))
    ))


(deftest cond-test
  (testing "Calls to cond should work"
    (is (= (l-eval-root '(cond (true 2)
                          (:else 4)) env) 2))

    (is (= (l-eval-root '(cond (false 2)
                          (:else 4)) env) 4))
    ))

(deftest begin
  (testing "Calls to do should work"
    (is (= (l-eval-root '(do (println "testing side effect")
                          (+ 2 2)) env) 4))


))

(deftest standard-functions
  (testing "Calls to functions should work"
    (is (= (l-eval-root '(cons 1 (cons 2 ())) env) '(1 2)))
    (is (= (l-eval-root '(= 1 2) env) false))
    (is (= (l-eval-root '(= 1 1) env) true))
    (is (= (l-eval-root '(= 1 2) env) false))
    (is (= (l-eval-root '(count  (l-quote (1 2))) env) 2))
    (is (= (l-eval-root '(empty?  (l-quote (1 2))) env) false))

    ))

(deftest test-load
  (testing "loading a script"
    (is (= (first (load "
(def a 42)
a
" env)) 42))))

(deftest test-lexical-binding
  (testing "loading a script"
    (is (= (first (load "
(def x 42)
(def f (fn [] x))
(f)
((fn [x] (f)) 7)
" env)) 42))
    (is (= (first (load "
(def x 42)
(def f (fn [] x))
(f)
((fn [x] (f)) 7)
x
" env)) 42))))

(def  fib-definition '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))))

(deftest definition-recurcivity-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (l-eval-root '(fib 7) new-env)) 13))
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (l-eval-root '(fib 17) new-env)) 1597))))

(deftest StackOverflowError-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (first (l-eval-root '(map fib (map (fn [_] 5) (range 2))) new-env))) 5))
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (first (l-eval-root '(map fib (map (fn [_] 5) (range 500))) new-env))) 5))

    (is (= (try (let [[_ new-env] (l-eval-root fib-definition env)]
                  (l-eval-root '(map fib (map (fn [_] 5) (range 3000))) new-env))
                (catch StackOverflowError e (.toString e))) "java.lang.StackOverflowError"))
    ))


(comment
  (let [[_ new-env] (l-eval-root '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) env)]
    new-env)
  )
