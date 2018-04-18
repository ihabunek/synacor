(ns synacor.core
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split join]]
            [clojure.pprint :refer [pprint]]))

(def input (-> "challenge.bin" io/resource))

(defn slurp-bytes [in]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (io/copy (io/input-stream in) out)
    (.toByteArray out)))

(defn parse-byte [byte]
  (if (neg? byte) (+ byte 256) byte))

(defn parse-pair [[low high]]
  (let [low (parse-byte low)
        high (parse-byte high)]
    (+ low (* 256 high))))

(defn parse-binary [bytes]
  (->> bytes
    (partition 2)
    (map parse-pair)))

; -- registers -----------------------------------------------------------------

(def regs (atom (vec (repeat 8 0))))

(defn is-reg [x]
  (and (>= x 32768) (<= x 32775)))

(defn reg [x]
  (assert (is-reg x))
  (- x 32768))

(defn regs-set [reg val]
  (assert (and (>= reg 0) (<= reg 7)))
  (swap! regs assoc reg val))

(defn regs-get [reg]
  (assert (and (>= reg 0) (<= reg 7)))
  (get @regs reg 0))

; -- memory --------------------------------------------------------------------

(def memory (atom (vec (repeat 32775 0))))

(defn memory-load [data]
  (reset! memory (vec data)))

(defn memory-set [addr val]
  (assert (and (>= addr 0) (<= addr 32775)))
  (assert (and (>= val 0) (<= val 65536)))
  (swap! memory assoc addr val))

(defn memory-get [addr]
  (assert (and (>= addr 0) (<= addr 32775)))
  (get @memory addr))

(defn memory-get-n [addr n]
  (assert (and (>= addr 0) (<= addr 32775)))
  (map #(get @memory %) (range addr (+ n addr))))

; -- stack ---------------------------------------------------------------------

(def stack (atom []))

(defn stack-push [x]
  (swap! stack conj x))

(defn stack-pop []
  (let [x (last @stack)]
    (swap! stack pop)
    x))

; ------------------------------------------------------------------------------

(defn is-valid [x]
  (and (>= x 0) (<= x 32775)))

(defn value [x]
  (assert (is-valid x))
  (if (is-reg x)
    (regs-get (reg x))
    x))

(defn dump [x]
  (assert (is-valid x))
  (if (is-reg x)
    (str "{reg " (reg x) " value " (str (regs-get (reg x))) "}")
    (str "{value " x "}")))

; -- instructions --------------------------------------------------------------

(defn -halt
  "halt: 0
   stop execution and terminate the program"
  [pos & args]
  (println pos "HALT" (join args))
  nil)

(defn -set
  "set: 1 a b
   set register <a> to the value of <b>"
  [pos a b]
  (regs-set (reg a) (value b))
  (+ 3 pos))

(defn -push
  "push: 2 a
   push <a> onto the stack"
  [pos a]
  (stack-push (value a))
  (+ 2 pos))

(defn -pop
  "pop: 3 a
   remove the top element from the stack and write it into <a> empty stack = error"
  [pos a]
  (regs-set (reg a) (stack-pop))
  (+ 2 pos))

(defn -eq
  "eq: 4 a b c
   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise"
  [pos a b c]
  (regs-set (reg a) (if (= (value b) (value c)) 1 0))
  (+ 4 pos))

(defn -gt
  "gt: 5 a b c
   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise"
  [pos a b c]
  (regs-set (reg a) (if (> (value b) (value c)) 1 0))
  (+ 4 pos))

(defn -jmp
  "jmp: 6 a
   jump to <a>"
  [pos a]
  (value a))

(defn -jt
  "jt: 7 a b
   if <a> is nonzero, jump to <b>"
  [pos a b]
  (if (not (zero? (value a)))
    (value b)
    (+ 3 pos)))

(defn -jf
  "jf: 8 a b
   if <a> is zero, jump to <b>"
  [pos a b]
  (if (zero? (value a))
    (value b)
    (+ 3 pos)))

(defn generic-op [pos a b c op]
  (let [value (mod (op (value b) (value c)) 32768)]
    (regs-set (reg a) value)
    (+ 4 pos)))

(defn -add
  "add: 9 a b c
   assign into <a> the sum of <b> and <c> (modulo 32768)"
  [pos a b c]
  (generic-op pos a b c +))

(defn -mult
  "mult: 10 a b c
   store into <a> the product of <b> and <c> (modulo 32768)"
  [pos a b c]
  (generic-op pos a b c *))

(defn -mod
  "mod: 11 a b c
   store into <a> the remainder of <b> divided by <c>"
  [pos a b c]
  (generic-op pos a b c mod))

(defn -and
  "and: 12 a b c
   stores into <a> the bitwise and of <b> and <c>"
  [pos a b c]
  (generic-op pos a b c bit-and))

(defn -or
  "or: 13 a b c
   stores into <a> the bitwise or of <b> and <c>"
  [pos a b c]
  (generic-op pos a b c bit-or))

(defn -not
  "not: 14 a b
   stores 15-bit bitwise inverse of <b> in <a>"
  [pos a b]
  (regs-set (reg a)
    (-> (bit-not (value b)) (mod 32768)))
  (+ 3 pos))

(defn -rmem
  "rmem: 15 a b
   read memory at address <b> and write it to <a>"
  [pos a b]
  (regs-set (reg a)
    (memory-get (value b)))
  (+ 3 pos))

(defn -wmem
  "wmem: 16 a b
   write the value from <b> into memory at address <a>"
  [pos a b]
  (memory-set (value a) (value b))
  (+ 3 pos))

(defn -call
  "call: 17 a
   write the address of the next instruction to the stack and jump to <a>"
  [pos a]
  (stack-push (+ 2 pos))
  (value a))

(defn -ret
  "ret: 18
   remove the top element from the stack and jump to it; empty stack = halt"
  [pos]
  (if (empty? @stack)
    (-halt pos "ret: The stack is empty")
    (stack-pop)))

(defn -out
  "out: 19 a
   write the character represented by ascii code <a> to the terminal"
  [pos a]
  (print (char (value a)))
  (flush)
  (+ 2 pos))


(def read-buffer (atom ""))

(defn -in
  "in: 20 a
   read a character from the terminal and write its ascii code to <a>; it can be
   assumed that once input starts, it will continue until a newline is
   encountered; this means that you can safely read whole lines from the
   keyboard and trust that they will be fully read"
  [pos a]
  (if (empty? @read-buffer)
    (do (print "=> ")
        (flush)
        (swap! read-buffer concat (concat (read-line) "\n"))))

  (let [c (first @read-buffer)]
    (regs-set (reg a) (int c)))

  (swap! read-buffer rest)
  (+ pos 2))

(defn -noop
  "noop: 21
   no operation"
  [pos]
  (inc pos))

(defn print-step [pos code a b c]
  (println
    (case code
      0  [pos "halt"]
      1  [pos "set" (dump a) (dump b)]
      2  [pos "push" (dump a)]
      3  [pos "pop" (dump a)]
      4  [pos "eq" (dump a) (dump b) (dump c)]
      5  [pos "gt" (dump a) (dump b) (dump c)]
      6  [pos "jmp" (dump a)]
      7  [pos "jt" (dump a) (dump b)]
      8  [pos "jf" (dump a) (dump b)]
      9  [pos "add" (dump a) (dump b) (dump c)]
      10 [pos "mult" (dump a) (dump b) (dump c)]
      11 [pos "mod" (dump a) (dump b) (dump c)]
      12 [pos "and" (dump a) (dump b) (dump c)]
      13 [pos "or" (dump a) (dump b) (dump c)]
      14 [pos "not" (dump a) (dump b)]
      15 [pos "rmem" (dump a) (dump b)]
      16 [pos "wmem" (dump a) (dump b)]
      17 [pos "call" (dump a)]
      18 [pos "ret"]
      19 [pos "out" (dump a)]
      21 [pos "noop"])))

(defn step [pos code a b c]
  (case code
    0  (-halt pos)
    1  (-set pos a b)
    2  (-push pos a)
    3  (-pop pos a)
    4  (-eq pos a b c)
    5  (-gt pos a b c)
    6  (-jmp pos a)
    7  (-jt pos a b)
    8  (-jf pos a b)
    9  (-add pos a b c)
    10 (-mult pos a b c)
    11 (-mod pos a b c)
    12 (-and pos a b c)
    13 (-or pos a b c)
    14 (-not pos a b)
    15 (-rmem pos a b)
    16 (-wmem pos a b)
    17 (-call pos a)
    18 (-ret pos)
    19 (-out pos a)
    20 (-in pos a)
    21 (-noop pos)
    (-halt pos (str "Unknown code: " code))))

(defn run [program start-pos]
  (memory-load program)
  (loop [pos start-pos]
    (if (not (nil? pos))
      (let [[code a b c] (memory-get-n pos 4)
            pos (step pos code a b c)]
        (recur pos)))))

(defn -main [& args]
  (let [bytes (slurp-bytes input)
        program (parse-binary bytes)]
    (println (run program 0))))
