(ns clj-eval.core-test
  (:require [clojure.test :refer :all]
            [clj-eval.core :refer :all]))


(deftest primitives
  (testing "Evaluation of primitives should work"
    (is (= (l-eval 42 env identity) 42))
    (is (= (l-eval true env identity) true))
    (is (= (l-eval "abc" env identity) "abc"))
    (is (= (l-eval () env identity) ()))
    (is (= (l-eval '(procedure _ _ _) env identity) '(procedure _ _ _)))))

(deftest variables
  (testing "Evaluation of variables should work"
    (is (= (l-eval 'x env identity) 1))
    (is (= (l-eval 'y env identity) 2))
    (is (= (try (l-eval 'do-not-exist env identity)
                (catch Exception e (.getMessage e))) "do-not-exist is not a valid symbol"))
    (is (= (drop-last (l-eval 'f env identity)) '(procedure [a] ((+ 1 a)))))))

(deftest special-foms
  (testing "Evaluation of variables should work"
    (is (= (l-eval '(l-quote (5 6)) env identity) '(5 6)))
    (is (= (l-eval '(if true 5 6) env identity) 5))
    (is (= (l-eval '(if false 5 6) env identity) 6))

    ))

(deftest definitions
  (testing "Evaluation of variables should work"
    (is (= (l-eval '(def a 42) env identity) (list 'updated-env (merge env {'a 42}))))

    ))

(deftest expressions
  (testing "Evaluation of expressions should work"
    (is (= (l-eval '(+ 1 x) env identity) 2))
    (is (= (l-eval '(+ x y) env identity) 3))
    (is (= (l-eval '(+ (+ x y) 1) env identity) 4))
    (is (= (l-eval '(- (+ x y 3) 1) env identity) 5))
    (is (= (l-eval '(+ (+ 2 3) 1) env identity) 6))
    (is (= (l-eval (list 'cons 1 (list 'l-quote '(2 3))) env identity) '(1 2 3)))))

(deftest lambdas
  (testing "Evaluation of anonymous functions should work"
    (is (= (l-eval '((fn [x] (+ 1 x)) 42) env identity) 43))
    (is (= (l-eval '((fn [x y] (+ x y)) 42 42) env identity) 84))
    (is (= (l-eval '((fn [x y] (+ ((fn [] (+ x 1))) y)) 42 42) env identity) 85))
    (is (= (l-eval '((fn [x y] (+ ((fn [b] (+ x b)) 3) y)) 42 42) env identity) 87))
    (is (= (l-eval '((fn [x y] (+ ((fn [y] (+ x y)) 3) y)) 42 42) env identity) 87))))


(deftest functions
  (testing "Calls to functions should work"
    (is (= (l-eval '(f 2) env identity) 3))
    (is (= (l-eval '(g 2 3) env identity) 5))
    (is (= (l-eval '(f (g 2 3)) env identity) 6))))

(deftest std-functions
  (testing "Eval standard functions should work"
    (is (= (l-eval (list 'map '(fn [x] (+ 1 x)) '(l-quote (1 2 3 ))) env identity) '(2 3 4)))
    (is (= (l-eval '(map (fn [x] (+ 1 x)) (l-quote (1 2 3 ))) env identity) '(2 3 4)))
    (is (= (l-eval '(filter (fn [x] (= 2 x)) (l-quote (1 2 3 ))) env identity) '(2)))
    (is (= (l-eval '(foldl + 0 (range 9)) env identity) 36))
    (is (= (l-eval '(foldl + 0 (range 99)) env identity) 4851))
    ))


(deftest cond-test
  (testing "Calls to cond should work"
    (is (= (l-eval '(cond (true 2)
                          (:else 4)) env identity) 2))

    (is (= (l-eval '(cond (false 2)
                          (:else 4)) env identity) 4))
    ))

(deftest begin
  (testing "Calls to do should work"
    (is (= (l-eval '(do (println "testing side effect")
                          (+ 2 2)) env identity) 4))


))

(deftest standard-functions
  (testing "Calls to functions should work"
    (is (= (l-eval '(cons 1 (cons 2 ())) env identity) '(1 2)))
    (is (= (l-eval '(= 1 2) env identity) false))
    (is (= (l-eval '(= 1 1) env identity) true))
    (is (= (l-eval '(= 1 2) env identity) false))
    (is (= (l-eval '(count  (l-quote (1 2))) env identity) 2))
    (is (= (l-eval '(empty?  (l-quote (1 2))) env identity) false))

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
    (is (= (let [[_ new-env] (l-eval fib-definition env identity)]
             (l-eval '(fib 7) new-env identity)) 13))
    (is (= (let [[_ new-env] (l-eval fib-definition env identity)]
             (l-eval '(fib 17) new-env identity)) 1597))))

(deftest StackOverflowError-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval fib-definition env identity)]
             (first (l-eval '(map fib (map (fn [_] 5) (range 2))) new-env identity))) 5))
    (is (= (let [[_ new-env] (l-eval fib-definition env identity)]
             (first (l-eval '(map fib (map (fn [_] 5) (range 500))) new-env identity))) 5))

    (is (= (try (let [[_ new-env] (l-eval fib-definition env)]
                  (l-eval '(map fib (map (fn [_] 5) (range 3000))) new-env))
                (catch StackOverflowError e (.toString e))) "java.lang.StackOverflowError"))
    ))


(comment
  (let [[_ new-env] (l-eval '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) env)]
    new-env)
  )
