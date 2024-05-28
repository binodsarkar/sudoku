(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [row] (get-in board [row col])) (range (count board))))))

(defn coord-pairs [coords]
  (let [pairs []]
     (for [row coords col coords]
       (seq (conj pairs row col)))))

(defn block-coords [coord]
  (cond
    (<= coord 2) [0 1 2]
    (<= coord 5) [3 4 5]
    :else [6 7 8]))

(defn block-values [board coord]
  (let [[row col] coord]
    (set (for [x (block-coords row)
               y (block-coords col)]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
 (if (has-value? board coord)
    #{}
    (let [used-values (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values used-values))))

(defn filled? [board]
  (let [vals (set (apply concat board))]
     (not (contains? vals 0))))

(defn rows [board]
  (for [row (range 9)]
     (row-values board [row 0])))

(defn valid-groups? [seq-of-sets]
  (let [is-invalid? (fn [vals] (or (contains? vals 0) (< (count vals) 9)))]
    (empty? (filter is-invalid? seq-of-sets))))

(defn valid-rows? [board]
  (valid-groups? (rows board)))

(defn cols [board]
  (for [col (range 9)]
     (col-values board [0 col])))

(defn valid-cols? [board]
  (valid-groups? (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
         col [0 3 6]]
     (block-values board [row col])))

(defn valid-blocks? [board]
  (valid-groups? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point-helper [board board-coords]
  (cond
    (empty? board-coords) nil
    (not (has-value? board (first board-coords))) (first board-coords)
    :else (find-empty-point-helper board (rest board-coords))))

(defn find-empty-point [board]
  (find-empty-point-helper board (coord-pairs (range 9))))

(defn solve-helper [board]
  (let [filled (filled? board)]
    (cond
      (and filled (valid-solution? board)) [board]
      filled []
      :else (let [coord (find-empty-point board)]
              (for [value (valid-values-for board coord)
                    solution (solve-helper (set-value-at board coord value))]
                solution)))))

(defn solve [board]
  (first (solve-helper board)))
