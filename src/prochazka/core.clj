(ns prochazka.core
  (:require [clojure.string :as str]))

(defn process-file-input []
  (let [[[num-inputs] & input] (->> (slurp "input.txt")
                                    str/split-lines
                                    (map #(as-> % <>
                                                (str/split <> #"\ ")
                                                (map read-string <>))))]
    (loop [num-inputs num-inputs
           [[_ num-edges] & input] input
           ret (transient [])]
      (if (pos? num-inputs)
        (recur (dec num-inputs)
               (drop num-edges input)
               (conj! ret (take num-edges input)))
        (persistent! ret)))))

(defn solve [processed-input]
  (let [edges-frequencies (->> (reduce (fn [m [v1 v2]]
                                         (-> m (update v1 (fnil inc 0))
                                             (update v2 (fnil inc 0))))
                                       {}
                                       processed-input)
                               (remove (fn [[_ v]]
                                         (even? v))))]
    (if (empty? edges-frequencies)
      "Ano."
      (->> edges-frequencies (map #(nth % 0))
           (partition 2)
           (map (fn [[v1 v2]]
                  (str v1 \space v2)))
           (str/join \newline)
           (str "Ne.\n" (/ (count edges-frequencies) 2) \newline)))))

(defn -main []
  (->> (process-file-input)
       (map solve)
       (str/join \newline)
       (spit "output.txt")))