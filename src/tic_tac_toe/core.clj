(ns tic-tac-toe.core
  (:gen-class))

(def starting-board
  "The default board, used when a ne game starts"
  [1 2 3 4 5 6 7 8 9])
starting-board

(defn triples
  "All possible winning lines"
  [board]
  (concat
   (partition-all 3 board)
   (list
    (take-nth 3 board)
    (take-nth 3 (drop 1 board))
    (take-nth 3 (drop 2 board))
    (take-nth 4 board)
    (take-nth 2 (drop-last 2 (drop 2 board))))))

(triples starting-board)

(defn full-board
  "Is evert cell on the board filled with either a :o or :x? 
   Takes the current board as an argument."
  [board]
  (every? #{:x :o} board))

(defn display-board
  "Displays the state of the current board, passed as an argument"
  [board]
  (let [board (map #(if (keyword? %)
                      (clojure.string/upper-case  (name %))
                      %)
                   board)]
    (println (nth board 0) (nth board 1) (nth board 2))
    (println (nth board 3) (nth board 4) (nth board 5))
    (println (nth board 6) (nth board 7) (nth board 7))))

(display-board  starting-board)

(defn player-name
  "Convert player representation, :o or :x to string, o or x"
  [player]
  (clojure.string/upper-case (name player)))


(defn tripple-winner?
  "If a line contains three of thesame player, return the player, otherwise return nil"
  [triples]
  (if (every? #{:x} triples)
    :x
    (if (every? #{:o} triples)
      :o)))

(tripple-winner? [1 2 3])
(tripple-winner? [:x 2 3])
(tripple-winner? [:x :x 3])
(tripple-winner? [:x :x :x])
(tripple-winner? [1 2 3])
(tripple-winner? [:o 2 3])
(tripple-winner? [:o :o 3])
(tripple-winner? [:o :o :o])

(defn winner?
  "returns winner if there is any, otherwise"
  [board]
  (first
   (filter #{:x :o} (map tripple-winner? (triples board)))))

(def player-sequence
  "Generate an infintie sequence for the player turns"
  (cycle [:x :o]))

(take 10 (cycle [:x :o]))

(defn next-move
  "Reads the next move from the command line using read-line and converts
  it to an integer"
  [board]
  (let [keyboard-input
        (try
          (. Integer parseInt (read-line))
          (catch Exception e nil))]
    (if (some #{keyboard-input} board)
      keyboard-input
      nil)))

(defn take-turn
  "Ask the players to make a move and inform them when they make 
 a incorrect move. "
  [player board]
  (println (str (player-name player)
                (loop [move (next-move board)] 
                  (if move 
                    (assoc board (dec move) player) 
                    (do 
                      (println (str (player-name player) 
                                    (recur (next-move board))))))))))

(defn play-game 
  "The game loop.
   We iterate through the player sequence (alternate player turns) 
   untill there is a winner or the board is full."
  [] 
  (loop [board starting-board
         player-sequence player-sequence] 
    (let [winner (winner? board)] 
       (println "Current board:") 
      (display-board board) 
      (cond
        winner       (println "Player " (player-name winner) " wins!" ) 
        (full-board? board) (println "The game is a draw.")
        :else 
        (recur 
         (take-turn (first player-sequence) sequence) board) 
        (rest player-sequence)))))

(play-game)







()
