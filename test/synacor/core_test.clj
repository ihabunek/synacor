(ns synacor.core-test
  (:require [clojure.test :refer :all]
            [synacor.core :refer :all]))

(deftest test-stack
  (testing "Stacking"
    (is (= @stack []))

    (stack-push 1)
    (is (= @stack [1]))

    (stack-push 2)
    (is (= @stack [1 2]))

    (stack-push 5)
    (is (= @stack [1 2 5]))

    (is (= (stack-pop) 5))
    (is (= @stack [1 2]))

    (is (= (stack-pop) 2))
    (is (= @stack [1]))

    (is (= (stack-pop) 1))
    (is (= @stack []))

    ; Can't pop empty stack
    (is (thrown? java.lang.IllegalStateException (stack-pop)))
    (is (= @stack []))))

(deftest test-memory
  (testing "Memory access"
    (is (= @memory (repeat 32775 0)))
    (is (= (memory-get 0) 0))
    (is (= (memory-get-n 0 5) [0 0 0 0 0]))

    (memory-set 0 1)
    (is (= @memory (concat [1] (repeat 32774 0))))
    (is (= (memory-get 0) 1))
    (is (= (memory-get-n 0 5) [1 0 0 0 0]))

    (memory-set 3 1)
    (is (= @memory (concat [1 0 0 1] (repeat 32771 0))))
    (is (= (memory-get 0) 1))
    (is (= (memory-get-n 0 5) [1 0 0 1 0]))

    (memory-set 0 0)
    (memory-set 3 0)
    (is (= (memory-get 0) 0))
    (is (= (memory-get 16000) 0))

    (memory-set 16000 40000)
    (is (= (memory-get 0) 0))
    (is (= (memory-get 16000) 40000))

    (is (= @memory (concat (repeat 16000 0) [40000] (repeat 16774 0))))

    (memory-load (repeat 32775 0))
    (is (= @memory (repeat 32775 0)))

    (memory-load (range 1000))
    (is (= @memory (concat (range 1000) (repeat 31775 0))))))
