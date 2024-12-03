;; Some Clojure for you
;; Part 1
(->> (slurp "input.txt")
     (re-seq #"mul\((\d+),(\d+)\)")
     (map (partial drop 1))
     (map (partial map read-string))
     (map (partial reduce *))
     (reduce +))

;; Part 2
(defn compute [cmds]
  (loop [current-mode :enabled
         [cmd & tail] cmds
         acc 0]
    (case cmd
      nil acc
      :disabled (recur :disabled tail acc)
      :enabled (recur :enabled tail acc)
      (case current-mode
        :disabled (recur current-mode tail acc)
        :enabled (recur current-mode tail (+ acc cmd))))))

(->> (slurp "input.txt")
     (re-seq #"mul\((\d+),(\d+)\)|don\'t|do")
     (map (fn [[match a b]] (case match
                              "don't" :disabled
                              "do" :enabled
                              (* (read-string b) (read-string a)))))
     compute)
