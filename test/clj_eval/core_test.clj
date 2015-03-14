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

(deftest let-test
  (testing "Evaluation of let blocks should work"
    (is (= (l-eval-root '(let [] 3) env) 3))
    (is (= (l-eval-root '(let [a 3] a) env) 3))
    (is (= (l-eval-root '(let [a 3 b 4] (+ a b)) env) 7))
    (is (= (l-eval-root '(let [a (+ 1 2) b (+ a 1)] (+ a b)) env) 7))
    (is (= (l-eval-root '(let [a (+ 1 2) b (+ a 1) c (+ a b)] (+ a b c)) env) 14))
    (is (= (l-eval-root '(let [f (fn [a b c] (+ a b c)) a (+ 1 2) b (+ a 1) c (+ a b)] (f a b c)) env) 14))
    (is (= (l-eval-root '(let [f (fn [x y z] (+ x y z)) a (+ 1 2) b (+ a 1) c (+ a b)] (f a b c)) env) 14))

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

(deftest test-recursive-fction
  (testing "reloading recurcive fction and making sure they work"
    (is (= (first (load "
 (defrec foo (fn [x] (if (= x 99) x (foo (+ x 1)))))
 (def res (foo 4))
 (defrec foo (fn [x] (if (= x 42) x (foo (+ x 1)))))
 (list res (foo 4))
" env)) '(99 42)))))

;; currently the evaluator is strict lexical binding
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

    (is (= (first (load "
(def foo (fn [x] (list x w)))
(def bar (fn [w] (foo 1991)))
(def w 0)
(list (bar 100) (foo 3))
" env)) '((1991 100) (3 0))))  ;; "w is not a valid symbol"

    ))

;; this is commented because the binding is no longer dynamic
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

;; this bug has been fixed
(deftest test-dynamic-binding-bug
  (testing "a bug"
      (is (= (first (load "
(def w 0)
(def foo (fn [x] (list x w)))
(def bar (fn [w] (foo 1991)))
(list (bar 100) (foo 3))
" (assoc env 'w 2))) '((1991 0) (3 0)))) ))


(def  fib-definition '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))))
(def tail-rec '(def tail-rec (fn [x n] (do (if (= x n) x (tail-rec (+ x 1) n))))))

(deftest definition-recurcivity-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (l-eval-root '(fib 7) new-env)) 13))
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (l-eval-root '(fib 17) new-env)) 1597))
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (l-eval-root '(fib 17) new-env)) 1597))))

(deftest StackOverflowError-test
  (testing "Eval to tail recursive functions should not blow the stack"
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (first (l-eval-root '(map fib (map (fn [_] 5) (range 2))) new-env))) 5))
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (first (l-eval-root '(map fib (map (fn [_] 5) (range 500))) new-env))) 5))

    (comment
      (is (= (try (let [[_ new-env] (l-eval-root fib-definition env)]
                   (l-eval-root '(map fib (map (fn [_] 5) (range 3000))) new-env))
                 (catch StackOverflowError e (.toString e))) "java.lang.StackOverflowError")))
    (is (= (let [[_ new-env] (l-eval-root fib-definition env)]
             (l-eval-root '(map fib (map (fn [_] 5) (range 3000))) new-env))
           ))

    ;; 42659 ms
    (is (= (do (println "map(30000 iterations)=") (time (let [[_ new-env] (l-eval-root fib-definition env)]
                    (l-eval-root '(map fib (map (fn [_] 5) (range 30000))) new-env))))
           ))

    ;; 34982 ms
    (is (= (do (println "map-cps(30000 iterations)=") (time (let [[_ new-env] (l-eval-root fib-definition env)]
                                        (l-eval-root '(map-cps fib (map (fn [_] 5) (range 30000)) identity) new-env))))
           ))

    ;; 36766 ms
    (is (= (do (println "tail-rec(1000000 iter)=") (time (first (load "
(def tail-rec (fn [x n] (do (if (= x n) x (tail-rec (+ x 1) n)))))
(tail-rec 0 1000000)" env)))) 1000000))

    ;; 39226 ms
    (is (= (do (println "cps-rec=") (time (first (load "
(def cps-rec (fn [x n k]
  (do (if (= x n)
          (k x)
         (cps-rec (+ x 1) n k)))))
(cps-rec 0 1000000 identity)" env)))) 1000000))
    ))


(comment
  (let [[_ new-env] (l-eval-root '(def fib (fn [n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) env)]
    new-env)
  )
