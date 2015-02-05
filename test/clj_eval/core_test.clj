(ns clj-eval.core-test
  (:require [clojure.test :refer :all]
            [clj-eval.core :refer :all]))


(def env
  (read-string "{
f (fn [a] (+ 1 a))
g (fn [a b] (+ a b))
x 1
y 2
z 2
}"))


(deftest primitives
  (testing "Evaluation of primitives should work"
    (is (= (l-eval 'x env) 1))
    (is (= (l-eval '(+ x y) env) 3))
    (is (= (l-eval '(+ (+ x y) 1) env) 4))))

(deftest variables
  (testing "Evaluation of variables should work"
    (is (= (l-eval 'x env) 1))
    (is (= (l-eval 'y env) 2))
    (is (= (l-eval 'f env) '(fn [a] (+ 1 a))))))

(deftest expressions
  (testing "Evaluation of expressions should work"
    (is (= (l-eval '(+ 1 x) env) 2))
    (is (= (l-eval '(+ x y) env) 3))
    (is (= (l-eval '(+ (+ x y) 1) env) 4))
    (is (= (l-eval '(+ (+ 2 3) 1) env) 6))))

(deftest lambdas
  (testing "Evaluation of anonymous functions should work"
    (is (= (l-eval '((fn [x] (+ 1 x)) 42) env) 43))
    (is (= (l-eval '((fn [x y] (+ x y)) 42 42) env) 84))))


(deftest functions
  (testing "Calls to functions should work"
    (is (= (l-eval '(f 2) env) 3))
    (is (= (l-eval '(g 2 3) env) 5))
    (is (= (l-eval '(f (g 2 3)) env) 6))))
