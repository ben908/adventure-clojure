(ns adventure.core
  (:gen-class))
(require 'clojure.string)

(def init-map
  {:living-room {:desc "This is a large living room with an OLED TV with a fireplace undernearth it. There are two couches and several chairs facing the TV made out of brown leather."
                 :title "Living Room"
                 :dir {:south :garage
                       :north :dining-room
                       :west :hallway
                       :east :bathroom}
                 :contents #{}}

   :dining-room {:desc "There is a large empty table in the middle of the room. On one of the walls you see a cabinet full of fancy glassware."
                 :title "dining room"
                 :dir {:north :kitchen
                       :south :living-room}
                 :contents #{:snack}}

   :kitchen {:desc "There is a stove with nothing on it with a refrigerator and freeze on either side. There is an island in the middle of the room."
             :title "kitchen"
             :dir {:south :dining-room}
             :contents #{:power-cord}}

   :bathroom {:desc "There is a sink with mirror behind it, a toilet and a shower."
              :title "bathroom"
              :dir {:west :living-room}
              :contents #{}}

   :garage {:desc "It is cold and dark. There is the outline of 3 cars on one side and several bicycles on the other."
            :title "garage"
            :dir {:north :living-room}
            :contents #{}}

   :hallway {:desc "There are tall ceilings and door in almost every direction. There are paintings on the wall with a variety of nature scenes in them."
             :title "hallway"
             :dir {:north :bedroom
                   :west :master-bedroom
                   :south :study
                   :east :living-room}
             :contents #{}}

   :study {:desc "Bookcases line the walls and there is a large desk made out of rich mahogany. This room has a modem and surely also a very strong wifi signal."
           :title "in the study"
           :dir {:north :hallway}
           :contents #{:power-cord}}

   :bedroom {:desc "A large bed is in the corner with a small desk next to it with a dresser on the other side of the room."
             :title "bedroom"
             :dir {:south :hallway}
             :contents #{:uncharged-computer}}

   :master-bedroom {:desc "There is a large bed with nightstands on both side with a large painting above the bed."
                    :title "in the master bedroom"
                    :dir {:east :hallway
                          :south :ensuite}
                    :contents #{}}

   :ensuite {:desc "There is a large bathtub in the center of the room with a shower and toilet to the side. There is a large mirror behind two sinks which are set in a large marble counter"
             :title "master bath"
             :dir {:north :master-bedroom}
             :contents #{}}})

(def init-items
  {:uncharged-computer {:desc "This is an uncharged Laptop.  You need to use a charger to charge it before you can use it."
                        :name "an uncharged laptop"}
   :charged-computer {:desc "This a charged Laptop, once you find a room with wifi you can use it to submit your assignment."
                      :name "a charged laptop"}
   :snack {:desc "This is a tasty apple, but you are too distriracted by the notion of turning in your assignment to eat it."
           :name "a crisp apple"}
   :power-cord {:desc "This is a power cord that you can use to charge your laptop with."
                :name "a laptop charger"}})

(def init-adventurer
  {:location :living-room
   :inventory #{}
   :tick 0
   :seen #{}})

(def new-game
  {:player init-adventurer
   :items init-items
   :map init-map
   :status :not-done})

(defn status [state]
  (let [location (get (get state :player) :location)
        cur-seen (get (get state :player) :seen)
        cur-map (get state :map)
        up-player (update (update (get state :player) :seen conj location) :tick + 1)
        up-state (assoc state :player up-player)]
    (println)
    (println (str "You are in the " (get (get cur-map location) :title) ". "))
    (println "The neighboring room(s) are: ")
    (doseq [[k v] (map vector (keys (get (get cur-map location) :dir)) (vals (get (get cur-map location) :dir)))]
      (println (str v) " to the " (str k)))
    (when-not (contains? cur-seen location)
      (do
        (println (str (get (get cur-map location) :desc))) ;;print all room info
        ))
    up-state))

(defn go [state dir]
  (let [location (get (get state :player) :location)
        dest (get (get (get (get state :map) location) :dir) dir)
        up-player (assoc (get state :player) :location dest)
        up-state (assoc state :player up-player)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      up-state)))

(defn examine-room [state]
  (let [location (get (get state :player) :location)
        cur-seen (get (get state :player) :seen)
        cur-map (get state :map)
        cur-contents (get (get (get state :map) (get (get state :player) :location)) :contents)]
    (println (str (get (get cur-map location) :desc))) ;;print all room info 
    (if (empty? cur-contents) (println "This room does not have any items")
        (println (str "The room contains: ") cur-contents))
    (println))
  state)

(defn drop-item [state item]
  (let [items (get (get state :player) :inventory)
        location (get (get state :player) :location)
        up-player (update (get state :player) :inventory disj item)
        up-room (update (get (get state :map) location) :contents conj item)
        up-map (assoc (get state :map) location up-room)
        up-state (assoc (assoc state :map up-map) :player up-player)]
    (if (contains? items item)
    ;; update state for room and platyer
      up-state
      (do (println "You do not have that item, so you cannot drop it")
          state))))

(defn use-item [state item]
  (let [location (get (get state :player) :location)
        cur-inventory (get (get state :player) :inventory)
        up1-player (update (get state :player) :inventory disj :uncharged-computer)
        up2-player (update up1-player :inventory conj :charged-computer)
        up-state (assoc state :player up2-player)]
    (if (contains? cur-inventory item)
      (do (cond (= item :snack) (do (println "You are too worried about missing the due date to eat the snack.") state)
                (= item :uncharged-computer) (do (println "Use the power-cord with this in your inventory to charge it") state)
                (= item :charged-computer) (if (= location :study) (assoc state :status :done) (do (println "You don't have good wifi here so you cannot submit your assignment") state))
                (= item :power-cord) (if (contains? cur-inventory :uncharged-computer)
                                       (do
                                         (println "Your Computer is now charged!")
                                         up-state)
                                       (println "You need to have an uncharged computer in order to use the charger"))))
      (do (println "You do not have that item")
          state))))

(defn take-item [state item]
  (let [cur-contents (get (get (get state :map) (get (get state :player) :location)) :contents)
        location (get (get state :player) :location)
        up-player (update (get state :player) :inventory conj item)
        up-room (update (get (get state :map) location) :contents disj item)
        up-map (assoc (get state :map) location up-room)
        up-state (assoc (assoc state :map up-map) :player up-player)]
    (if (contains? cur-contents item)
    ;; update state for room and platyer
      up-state
      (do (println "This room does not have that item, so you cannot take it")
          state))))

(defn look-item [state item]
  (let [cur-inventory (get (get state :player) :inventory)]
    (if (contains? cur-inventory item)
      (println (get (get (get state :items) item) :desc))
      (println (str "You do not currently have the item: " item))))
  state)

(defn look-inventory [state]
  (let [cur-inventory (get (get state :player) :inventory)]
    (if (empty? cur-inventory)
      (println (str "You currently do not have any itmes"))
      (println (str "You currently have " cur-inventory))))
  state)

(defn repl [state]
  (loop [state state]
    (if (= (:status state) :not-done)
      (let [tracked-state (status state)]
        (do
          (println "What do you want to do? [M]ove/[U]se/[L]ook/[E]xamine/[T]ake/[D]rop/[Q]uit/List [I]nventory ")
          (let [choice (clojure.string/upper-case (read-line))]
            (cond (or (= choice "M") (= choice "MOVE")) (do (println "What direction?")
                                                            (let [dir (clojure.string/upper-case (read-string (read-line)))]
                                                              (cond
                                                                (or (= dir "N") (= dir "NORTH")) (recur (go tracked-state :north))
                                                                (or (= dir "W") (= dir "WEST")) (recur (go tracked-state :west))
                                                                (or (= dir "S") (= dir "SOUTH")) (recur (go tracked-state :south))
                                                                (or (= dir "E") (= dir "EAST")) (recur (go tracked-state :east))
                                                                :else (do (println "Unknown Direction") (recur tracked-state)))))

                  (or (= choice "U") (= choice "USE")) (if (empty? (get (get tracked-state :player) :inventory))
                                                         (do (println "You do no have any items you can use") (recur tracked-state))
                                                         (do (println "What item?")
                                                             (let [item (clojure.string/upper-case (read-string (read-line)))]
                                                               (cond
                                                                 (or (= item "UC") (= item "UNCHARGED-COMPUTER")) (recur (use-item tracked-state :uncharged-computer))
                                                                 (or (= item "CC") (= item "CHARGED-COMPUTER")) (recur (use-item tracked-state :charged-computer))
                                                                 (or (= item "S") (= item "SNACK")) (recur (use-item tracked-state :snack))
                                                                 (or (= item "PC") (= item "POWER-CORD")) (recur (use-item tracked-state :power-cord))
                                                                 :else (do (println "Unknown Item") (recur tracked-state))))));;use

                  (or (= choice "L") (= choice "LOOK")) (if (empty? (get (get tracked-state :player) :inventory))
                                                          (do (println "You do no have any items you can look at") (recur tracked-state))
                                                          (do (println "What item?")
                                                              (let [item (clojure.string/upper-case (read-string (read-line)))]
                                                                (cond
                                                                  (or (= item "UC") (= item "UNCHARGED-COMPUTER")) (recur (look-item tracked-state :uncharged-computer))
                                                                  (or (= item "CC") (= item "CHARGED-COMPUTER")) (recur (look-item tracked-state :charged-computer))
                                                                  (or (= item "S") (= item "SNACK")) (recur (look-item tracked-state :snack))
                                                                  (or (= item "PC") (= item "POWER-CORD")) (recur (look-item tracked-state :power-cord))
                                                                  :else (do (println "Unknown Item") (recur tracked-state))))));;look

                  (or (= choice "E") (= choice "EXAMINE")) (recur (examine-room tracked-state));;examine

                  (or (= choice "T") (= choice "TAKE")) (let [location (get (get tracked-state :player) :location)
                                                              cur-seen (get (get tracked-state :player) :seen)
                                                              cur-map (get tracked-state :map)
                                                              cur-contents (get (get (get tracked-state :map) (get (get tracked-state :player) :location)) :contents)]
                                                          (if (empty? cur-contents)
                                                            (do (println "There are not any items in this room you can take") (recur tracked-state))
                                                            (do (println "What item?")
                                                                (let [item (clojure.string/upper-case (read-string (read-line)))]
                                                                  (cond
                                                                    (or (= item "UC") (= item "UNCHARGED-COMPUTER")) (recur (take-item tracked-state :uncharged-computer))
                                                                    (or (= item "CC") (= item "CHARGED-COMPUTER")) (recur (take-item tracked-state :charged-computer))
                                                                    (or (= item "S") (= item "SNACK")) (recur (take-item tracked-state :snack))
                                                                    (or (= item "PC") (= item "POWER-CORD")) (recur (take-item tracked-state :power-cord))
                                                                    :else (do (println "Unknown Item") (recur tracked-state)))))));;take

                  (or (= choice "D") (= choice "DROP")) (if (empty? (get (get tracked-state :player) :inventory))
                                                          (do (println "You do no have any items you can drop") (recur tracked-state))
                                                          (do (println "What item?")
                                                              (let [item (clojure.string/upper-case (read-string (read-line)))]
                                                                (cond
                                                                  (or (= item "UC") (= item "UNCHARGED-COMPUTER")) (recur (drop-item tracked-state :uncharged-computer))
                                                                  (or (= item "CC") (= item "CHARGED-COMPUTER")) (recur (drop-item tracked-state :charged-computer))
                                                                  (or (= item "S") (= item "SNACK")) (recur (drop-item tracked-state :snack))
                                                                  (or (= item "PC") (= item "POWER-CORD")) (recur (drop-item tracked-state :power-cord))
                                                                  :else (do (println "Unknown Item") (recur tracked-state))))));;drop

                  (or (= choice "Q") (= choice "QUIT")) (println (str "Thank you for playing"));;quit
                  (or (= choice "I") (= choice "INVENTORY")) (recur (look-inventory tracked-state));;inventory
                  :else (do (println "Unknown Command")
                            (recur tracked-state))))))
      (do
        (println "Congradulations, you were able to find your computer, charger, use the charger, find wifi, and submit your assignment on time!")
        (println "You were able to complete this game in: " (str (get (get state :player) :tick) " commands!")))))
  (println "Game over."))

(defn -main
  ""
  [& args]
  (repl new-game))