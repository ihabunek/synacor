(ns synacor.commands-test
  (:require [clojure.test :refer :all]
            [synacor.core :refer :all]))

(defn reg-addr [x]
  (assert (and (>= x 0) (<= x 7)))
  (+ 32768 x))

(deftest test-generics
  (testing "generics"
    (regs-set 0 0)
    (regs-set 1 0)
    (regs-set 2 0)

    (println @regs)

    ; reg0 = 5 + 7
    (is (= 104 (-add 100 (reg-addr 0) 5 7)))
    (is (= 12 (regs-get 0)))

    ; reg0 = 32767 + 5 (overflow) = 4
    (is (= 104 (-add 100 (reg-addr 0) 32767 5)))
    (is (= 4 (regs-get 0)))

    ; reg0 = 5 * 7
    (is (= 104 (-mult 100 (reg-addr 0) 5 7)))
    (is (= 35 (regs-get 0)))

    ; reg0 = 12345 * 5432 (overflow) = 14712
    (is (= 104 (-mult 100 (reg-addr 0) 12345 5432)))
    (is (= 14712 (regs-get 0)))))


(defn bin [s]
  (read-string
    (str "2r" s)))

(deftest test-not
  (testing "not"
    (regs-set 0 123)
    (-not 0 (reg-addr 0) (bin "000000000000000"))
    (is (= (bin "111111111111111") (regs-get 0)))

    (regs-set 0 123)
    (-not 0 (reg-addr 0) (bin "111111111111111"))
    (is (= (bin "000000000000000") (regs-get 0)))

    (regs-set 0 123)
    (-not 0 (reg-addr 0) (bin "101010101010101"))
    (is (= (bin "010101010101010") (regs-get 0)))

    ; Test value from reg
    (regs-set 0 123)
    (regs-set 1 (bin "000000000000000"))
    (-not 0 (reg-addr 0) (reg-addr 1))
    (is (= (bin "111111111111111") (regs-get 0)))))

(deftest test-wmem-rmem
  (testing "wmem and rmem"
    (is (= @memory (repeat 32775 0)))

    ; Write literal
    (is (= 103 (-wmem 100 0 1)))
    (is (= 103 (-wmem 100 3 2)))
    (is (= 103 (-wmem 100 5 3)))

    (is (= [1 0 0 2 0 3 0] (memory-get-n 0 7)))

    (regs-set 0 6)
    (regs-set 1 21)
    (regs-set 2 77)

    ; write value 21 (from reg 1) to address 3
    (is (= 103 (-wmem 100 3 (reg-addr 1))))
    (is (= [1 0 0 21 0 3 0] (memory-get-n 0 7)))

    ; write value 77 (from reg 2) to address 6 (from reg 0)
    (is (= 103 (-wmem 100 (reg-addr 0) (reg-addr 2))))
    (is (= [1 0 0 21 0 3 77] (memory-get-n 0 7)))

    ; read value from address 5 into register 3
    (is (= 0 (regs-get 3)))
    (is (= 103 (-rmem 100 (reg-addr 3) 5)))
    (is (= 3 (regs-get 3)))

    ; read value from address 6 (from reg 0) into register 4
    (is (= 0 (regs-get 4)))
    (is (= 103 (-rmem 100 (reg-addr 4) (reg-addr 0))))
    (is (= 77 (regs-get 4)))

    ; Cleanup
    (memory-load (repeat 32775 0))
    (regs-set 0 0)
    (regs-set 1 0)))

(deftest test-call-ret
  (testing "call and ret"
    (is (= [] @stack))

    ; call: 17 a
    ; write the address of the next instruction to the stack and jump to <a>
    (is (= 50 (-call 100 50)))
    (is (= [102] @stack))

    ; ret: 18
    ; remove the top element from the stack and jump to it; empty stack = halt
    (is (= 102 (-ret 999)))
    (is (= [] @stack))

    ; halt if stack is empty
    (is (= nil (-ret 999)))))
