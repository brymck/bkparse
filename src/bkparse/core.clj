(ns bkparse.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:gen-class :main true))

(def ^:const prefix-length 12)

(def ^:const translations [
   ["^[OA"  "↑"]
   ["^[OB"  "↓"]
   ["^[OC"  "→"]
   ["^[OD"  "←"]
   ["^[OE"  "←"]
   ["^[OF"  "⇲"]
   ["^[OH"  "⇱"]
   ["^[[5~" "⇞"]
   ["^[[6~" "⇟"]
   ["^["    "⌥"]
   [" "     "Space"]
   ["^?"    "⌫"]
   ["\"-\"" " - "]
   [#"\\([\"$])" "$1"]
])

(def ^:const output-format (str "%" prefix-length "s %s\n"))

(defn space?
  "Returns true if character is whitespace"
  [chr]
  (Character/isWhitespace chr))

(defn first-index [pred coll]
  "Returns the first index in a collection matching the predicate"
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn split-on-last-space
  "Finds the index of the last space in a line"
  [line]
  (let [last-index (->> line
                        reverse
                        (first-index space?)
                        (- (count line))
                        dec)
        [fst lst] (split-at last-index line)
        lst (next lst)]
    (map #(apply str %) [fst lst])))

(defn translate
  "Makes replacements to a first string argument according to a two-item vector
  second argument"
  [s [from to]]
  (string/replace s from to))

(defn translate-keycode
  "Translate a raw keycode in the form \"^[A\""
  [code]
  (reduce translate (->> code next butlast (apply str)) translations))

(defn process-line
  "Process a line"
  [line]
  (let [[prefix suffix] (split-on-last-space line)]
    (printf output-format (translate-keycode prefix) suffix)))

(defn -main []
  (with-open [rdr (io/reader *in*)]
    (doseq [line (line-seq rdr)]
      (process-line line))))
