(ns synacor.commands-test
  (:require [clojure.test :refer :all]
            [synacor.core :refer :all]))

(deftest test-wmem
  (testing "wmem"
    (is (= @memory (repeat 32775 0)))

    ; Write literal
    (is (= 103 (-wmem 100 0 1)))
    (is (= 103 (-wmem 100 3 1)))
    (is (= 103 (-wmem 100 5 1)))

    (is (= [1 0 0 1 0 1 0] (memory-get-n 0 7)))

    ; Write from registers
    (regs-set 0 5)
    (regs-set 1 21)

    (is (= 103 (-wmem 100 32768 32769)))
    (is (= [13 0 0 21 0 1 0] (memory-get-n 0 7)))

    ; Cleanup
    (memory-load (repeat 32775 0))
    (regs-set 0 0)
    (regs-set 1 0)))
