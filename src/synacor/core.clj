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

(defn regs-load [data]
  (reset! regs (vec data)))

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

(defn stack-load [data]
  (reset! stack (vec data)))

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

(defn get-command []
  (do (print "=> ")
      (flush)
      (read-line)))

(defn save-game [pos]
  (let [data [pos @regs @stack @memory]]
    (do (spit "save.dat" (join "\n" (map str data)))
        (println "Game saved!"))))

(defn -in
  "in: 20 a
   read a character from the terminal and write its ascii code to <a>; it can be
   assumed that once input starts, it will continue until a newline is
   encountered; this means that you can safely read whole lines from the
   keyboard and trust that they will be fully read"
  [pos a]
  (if (empty? @read-buffer)
    (let [cmd (get-command)]
      (if (= cmd "save")
        (save-game pos))
      (swap! read-buffer concat (concat cmd "\n"))))

  (let [c (first @read-buffer)]
    (regs-set (reg a) (int c)))

  (swap! read-buffer rest)
  (+ pos 2))

(defn -noop
  "noop: 21
   no operation"
  [pos]
  (inc pos))

(def commands
  { 0x00 [:halt -halt 0]
    0x01 [:set  -set  2]
    0x02 [:push -push 1]
    0x03 [:pop  -pop  1]
    0x04 [:eq   -eq   3]
    0x05 [:gt   -gt   3]
    0x06 [:jmp  -jmp  1]
    0x07 [:jt   -jt   2]
    0x08 [:jf   -jf   2]
    0x09 [:add  -add  3]
    0x0A [:mult -mult 3]
    0x0B [:mod  -mod  3]
    0x0C [:and  -and  3]
    0x0D [:or   -or   3]
    0x0E [:not  -not  2]
    0x0F [:rmem -rmem 2]
    0x10 [:wmem -wmem 2]
    0x11 [:call -call 1]
    0x12 [:ret  -ret  0]
    0x13 [:out  -out  1]
    0x14 [:in   -in   1]
    0x15 [:noop -noop 0]})

(defn get-args [program pos argc]
  (subvec program
    (inc pos)
    (+ (inc pos) argc)))

(defn get-step-at [program pos]
  (let [code (nth program pos)
        [cmd-name cmd-fn argc] (get commands code)]
    (if cmd-name
      [cmd-name cmd-fn argc (get-args program pos argc)])))

(defn run-program [start-pos]
  (loop [pos start-pos]
    (if (not (nil? pos))
      (let [[cmd-name cmd-fn argc argv] (get-step-at @memory pos)
            next-pos (apply cmd-fn (concat [pos] argv))]
        (recur next-pos)))))

(defn dump-memory [program pos]
  (println "Local memory:")
  (doseq [index (range (- pos 10) (+ pos 10))]
    (println index (get program index))))

(defn dump-arg [arg]
  (cond
    (is-reg arg) (str "R" (reg arg))
    :else arg))

(defn dump-op [pos op args]
  (println
    (format "0x%04x" pos)
    op
    (->> args (map dump-arg) (join " ")))
  (+ (inc pos) (count args)))

(defn dump-out [program pos]
  (print (format "0x%04x" pos) ":out ")
  (loop [pos pos]
    (if (not= (get program pos) 0x13)
      (do
        (println)
        pos)
      (do
        (print (char (get program (inc pos))))
        (recur (+ 2 pos))))))

(defn dump-raw [pos val]
  (println
    (format "0x%04x" pos)
    (format "0x%04x" val))
  (inc pos))

(defn dump-instruction
  "Dumps instruction at given position and returns the offset to next position."
  [program pos]
  (if-let [[op _ argc args] (get-step-at program pos)]
    (if (= op :out)
      (dump-out program pos)
      (dump-op pos op args))
    (dump-raw pos (get program pos))))

(defn dump-program [program]
  (loop [pos 0]
    (if (>= pos (count program))
      (println "END")
      (let [next-pos (dump-instruction program pos)]
        (recur next-pos)))))

(defn run-default
  "Loads the program from challange.bin and runs it from pos 0."
  []
  (let [bytes (slurp-bytes input)
        program (vec (parse-binary bytes))]
    (memory-load program)
    (run-program 0)))

(defn run-dump []
  "Loads the program from challange.bin and dumps it in human readable format."
  (let [bytes (slurp-bytes input)
        program (vec (parse-binary bytes))]
    (dump-program program)))

(defn run-resume
  "Loads the program and other state from given save file and resumes it."
  [save]
  (let [save-data (-> save slurp (split #"\n"))
        [pos regs stack memory] (map read-string save-data)]
    (regs-load regs)
    (stack-load stack)
    (memory-load memory)
    (run-program pos)))

(defn -main [& args]
  (cond
    (empty? args) (run-default)
    (= (first args) "dump") (run-dump)
    (= (first args) "resume") (run-resume (second args))
    :else (println "Invalid params.")))
