^:kindly/hide-code
(ns index
  (:require
   [clojure.java.io :as io]
   [fastmath.core :as m]
   [fastmath.random :as r]
   [scicloj.kindly.v4.kind :as kind]
   [scicloj.tableplot.v1.hanami :as hanami]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]))

;; This tutorial assumes some familiarity with Clojure and with high-school maths (algebra, probability, statistics, _etc_.).

;; We use Clojure and [fastmath](https://github.com/generateme/fastmath) version 3 in this tutorial.

;; **Note**: this is still a work in progress. I'd love to receive your feedbacks or suggestions.

;; # Probability

;; In our daily lives, we often encounter situations where we don't know what will happen. Some of these situations can have a significant impact on our lives, while others simply spark our curiosity or provide us with a sense of excitement and anticipation. For example, during the rainy season, we might wonder if tomorrow's weather will leave us soaked, prompting us to prepare an umbrella beforehand. Similarly, hoping for our favorite sports team to win a game can fill us with eager anticipation. In our jobs as programmers, we may need to estimate the maximum requests per second our service can handle during peak hours so that we can allocate resources accordingly.

;; In probability and statistics, we refer to the act of witnessing or measuring these uncertain situations as an _experiment_ or an _observation_. An experiment is considered random when we cannot predict the outcome with certainty. In contrast, a deterministic experiment is one where the result can be predicted with near certainty, such as evaluating the expression (+ 3 4) in Clojure, which will always return 7.

;; Probability of these uncertain situations are scaled between 0% and 100%. The higher the probability, the more likely a situation to occur. A weather report telling us there's a 80% probability of raining tomorrow make us more ready to bring an umbrella tomorrow compared to report telling us 30% probability of raining. Similarly, a 80% chance for our local sports team to win might bring us a sense of calm when watching them play compared to cautious feeling if the probability of winning is just 30%.

;; ## Probability Space

;; Let's consider a classic experiment occurs in most of probability books, a coin flip experiment: we flip a coin and see whether head or tail occurs. We're doing 1 experiment or 1 trial and the possible results are head or tail. In probability theory, head or tail are possible _outcomes_ of an experiment. We could use clojure keyword to symbolize the outcomes: `:head` or `:tail`.

;; Let's consider another experiment where we flip a coin twice in an experiment. The possible sequences of _outcomes_ are ordered-pairs `[:head :tail]`, `[:tail :head]`, `[:tail :tail]`, and `[:head :head]`.

;; From a particular experiment, each `outcome` is unique. An experiment yields an `outcome` for each trial. The set of all possible outcomes of an experiment is the _sample space_ which usually denoted with S or Ω (Greek capital for Omega). So for the coin flip, the S is `#{:head :tail}` and the flip-a-coin-twice experiment, the S or Ω is `#{[:head :tail] [:tail :head] [:tail :tail] [:head :head]}`.

;; Now, we want to know whether tomorrow's maximum temperature will be 34 degrees celcius. The outcomes of the experiment depends on the range and precision of our temperature sensor device. For example, our device has temperature range from -10 to 50 degrees celcius with 0.001 precision. The sample space is `#{-10.000, -10.001, -10.002, ..... , 49.998, 49.999, 50.000}`. Compared to our coin flip experiments above, this maximum temperature experiment's sample space has more number of outcomes. In practical daily life usage, it is enough for us to know the maximum temperature in integer (ex. it's 34 degrees celcius). So, looking at the possible outcomes, we decide that we're interested to know whether tomorrow's maximum temperature is between 33.5 and 34.5 degrees celcius which is a set `#{33.500, 33.501, ..., 34.449, 34.500}`. We're interested in a subset of the outcomes.

;; In probability theory, a subset of outcomes from an experiment is called an `event`. The distinction between `outcome` and `event` is practically useful, especially in experiments with continuous or very large sample spaces.

;; Consider our maximum temperature experiment. The sample space contains all possible temperature readings (e.g., 33.501°C, 33.502°C, 33.503°C, etc.). However, in practical applications, we're often interested in broader categories rather than exact measurements. For instance, we might want to know the probability of the temperature being "around 34 degrees Celsius". We could define this as an event: "the maximum temperature is greater than or equal to 33.5°C and less than 34.5°C". This event would include all outcomes in the set `#{33.500, 33.501, ..., 34.499}`.

;; In flip-a-coin-twice experiment, we might be interested in events where only 1 :head occurs like `#{[:head :tail]}` and `#{[:tail :head]}`. The event where 2 occurs is `#{[:head :head]}`. In our single coin-toss experiment, the possible events are `#{:head}` or `#{:tail}`. Events such as `#{[:head :head]}` and `#{:head}` or `#{:tail}` are events with exactly one outcome. In probability theory, an event containing exactly one outcome is called an `elementary event`.

;; In a coin flip experiment with a fair coin where we expect both head and tail equally likely to happen, we assign both outcomes with equal probability 50%.

;; ## Tickets in a Box Model

;; We can simulate _random_ experiments in our computers. We can, programmatically, create a model of _random_ experiment and run the model simulations in our computer. The model should be small enough to capture relevant aspects of a _random_ experiment we have learned: the outcomes, sample space, events, and probabilities.

;; We will use a tickets-in-a-box model or "box model", for short. The box contains tickets and each ticket has an outcome written on it. The outcome written could be "head" or "tail" from tossing a coin, "win" or "lose" or "draw" from a sports match between 2 teams, or all possible temperature readings from maximum temperatur experiment -- it depends on the random experiment. Every potential outcome is represented on at least one ticket in the box. Certain outcomes might be found on multiple tickets. We can imagine that we shake the box to ensure that the tickets inside are randomly shuffled thus each individual ticket are equally likely to be selected. Doing an experiment means we pick a ticket from the box and then see what is written on the ticket to know the outcome.

;; In this box model, the probability of an outcome is the amount of tickets with that outcome written on them, divided by total amount of all tickets. The proportions of each outcomes written relative to the total number of tickets defines the probability property of a box. An outcome with more written tickets has more probability than other outcomes with lesser tickets. Intuitively, we are more likely to pick a ticket with higher outcome proportion. For example, a box with 7 head tickets and 3 tail tickets has same probability property like a box with 7 millions head tickets and 3 millions tail tickets and we are more likely to pick a head ticket from the box.

;; Let's create a box for our flip-a-coin experiment. The outcomes are `:head` or `:tail`. We assume we're using fair coin, so the proportions of head and tail are the same. We use 5 tickets for each outcome.

[:head :head :head :head :head
 :tail :tail :tail :tail :tail] 

;; Doing the experiment means we properly shake the box then pick a ticket. In our implementation, we use `clojure.core/rand-nth` function to simulate the acts of shuffling the box and pick a random ticket. We also don't remove the ticket from the box collection to simulate the act of putting the picked ticket back to the box. We create a function `fair-coin-box` where calling that function means we shake the box, pick a random ticket, then put the ticket in the box. The return value of the function is the outcome written on a ticket.

(defn fair-coin-box
  "Simulates shaking a box then pick a random ticket from the box then put the ticket back in the box.
   The return value is what's written on the picked ticket.
   Since we're using an immutable vector, this implicitly models:
   1. Random selection from all tickets
   2. Box always remains in original state for next pick"
  []
  (let [tickets [:head :head :head :head :head
                 :tail :tail :tail :tail :tail]]
    (rand-nth tickets)))

;; Let's do 1 trial
(fair-coin-box)

;; Let's do 5 trials
(repeatedly 5 #(fair-coin-box))

;; If we do a large number of trials, the proportions of picked tickets should be close to 50% head and 50% tail. The results approximately reflects the probability property of that box.

(frequencies (repeatedly 1000 #(fair-coin-box)))

;; ## Independence

;; It's important to understand that when we use `repeatedly` with our `fair-coin-box`, each call is an `independent` trial.  The _independence_ comes from each `rand-nth` returns a random element from the vector of tickets, independent of previous picks. _Independent_ means the probability of getting a head (or tail) on any given trial is always 0.5, regardless of what happened in any other trials.

;; This idea about _independence_ is important because, for example, in our flip-a-coin experiment with fair coin, there are some misconceptions such as:

;; 1. "After seeing three heads in a row, tails is more likely to come up next to balance things out"
;; 2. "If I've seen more heads than tails so far, the next few flips should more likely to return tails to even things up"

;; Let's focus on the simplest case that after 2 trial, the probability of seeing :head in the second trial is still 0.5, even after the first trial results in :head. We'll do the 2 trial 1000 times.

(defn two-fair-coin-trials []
  [(fair-coin-box) (fair-coin-box)])

(let [two-trials (repeatedly 1000 #(two-fair-coin-trials))]
  ;; Even if we know the first pick was head,
  ;; the second pick still has 50/50 chance
  (->> two-trials
       (filter #(= :head (first %)))
       (map second)
       frequencies))

;; This notion of independence is crucial in probability theory. The outcome of one trial doesn't influence the others. In above example, even after picking head, the count of :head and :tail are almost the same. There's no bias to :tail after a :head result.

;; ## Reproducible Model

;; Imagine that you have a fair-coin and after doing an experiment by flipping the coin 10 times, you believe you have found a fascinating pattern. You tell your colleague, "Hey, I flipped this coin 10 times and got this fascinating pattern!". But, when your colleague replicate your experiment, they get completely different result. How can they verify your experiment?

;; In game development, we might develop a gameplay with random elements and we encounter a bug. Without reproducibility randomness, it would be nearly impossible for other developers to recreate and fix the problems.

;; Let's go back to our `fair-coin-box` function implementation above.

(defn fair-coin-box
  []
  (let [tickets [:head :head :head :head :head
                 :tail :tail :tail :tail :tail]]
    (rand-nth tickets)))

;; We can imagine giving this box to our colleague but they'll get different result when calling this function 10 times.

;; Our trials
(repeatedly 10 #(fair-coin-box))

;; Our colleague trials
(repeatedly 10 #(fair-coin-box))

;; Now, what if we can, somehow, make a box that still give us a random ticket when picked, but returns reproducible result? Back to our initial implementation for our fair-coin-box and we see that the key is the `rand-nth` function. For our purpose, `rand-nth` picks an element from a random index of our vector of tickets. We could make a function which reproducibly generate a random index from our vector or we generalize and create a function that reproducibly generates a random number then we convert the produced random number to an index of our vector.

;; A function which generates a random number is called a Random Number Generator (RNG) in computer science. For our need of reproducible experiment, we're going to use Pseudo RNG or PRNG for short. PRNG when called multiple times will return a sequence of numbers that _appears_ to be random. This PRNG is stateful and deterministic which means given same initial state, it will continually produce same sequence of _pseudo_ random numbers.

;; Let's implement a simple PRNG called Linear Congruential Generator (LCG). The formula is:

^:kindly/hide-code (kind/tex "X_{n+1} = (a * X_n + c) \\mod m \\text{ where } n \\geq 0")

^:kindly/hide-code (kind/tex "m \\text{ the modulus, }  0 < m")

^:kindly/hide-code (kind/tex "a \\text{ the multiplier, }  0 \\leq a < m")

^:kindly/hide-code (kind/tex "c \\text{ the increment, }  0 \\leq c < m")

^:kindly/hide-code (kind/tex "X_0 \\text{ the starting value, }  0 \\leq X_0 < m")

;; Here's one implementation. `make-lcg` accepts the parameters and return a function to generate a random number. The implentation uses a closure that wraps an atom so that the function "remembers" the Xn to be returned.
(defn make-lcg
  "Returns a lcg function."
  [{:keys [X0 a c m]}]
  (let [state (atom X0)]
    (fn []
      (let [Xn @state
            Xn+1 (mod (+ (* a Xn) c) m)]
        (reset! state Xn+1)         ; Update the state
        Xn))))

(def lcg-0 (make-lcg {:X0 7 :a 7 :c 7 :m 10}))

;; let's generate a sequence of 20 numbers

(repeatedly 20 lcg-0)
;; => (7 6 9 0 7 6 9 0 7 6)

;; As we can see the result is a periodic sequence of 7, 6, 9, 0, then it repeats again. Mathematically inclined readers will quickly realize that there is a period of numbers because of the use of modulus in LCG formula and we choose 10 as our m. The choice of X0, a, m, and c determines the period of the generated sequence of numbers before it cycles again. It is the property of LCG that it produces a periodic sequence of _pseudo_ random numbers. We're going to choose sufficiently large m and combination of a and c to make our period large enough so we can generate many many random numbers before the numbers form a new period.

;; We're going to use m = 2^48, c = 11, a = 25214903917, which are parameters used in java.util.Random according to wikipedia (https://en.wikipedia.org/wiki/Linear_congruential_generator)

(def lcg-1 (make-lcg {:X0 2007 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(repeatedly 20 lcg-1)

;; The number is too large, we could create another functionlcg-rand whose input is the lcg function and the modulus and returns random number between 0 and 1. We just divides the number returned from lcg with the m.

(defn lcg-rand
  "returns random number between 0 and 1"
  [lcg m]
  (/ (double (lcg)) m))


(repeatedly 20 #(lcg-rand lcg-0 (Math/pow 2 48)))

(repeatedly 20 #(lcg-rand lcg-1 (Math/pow 2 48)))

;; A visualization might help us see the randomness of lcg-0 and lcg-1. Using lcg-rand to generate random numbers, we're going to plot each number against the next number in the sequence (X,Y) = (Xn, Xn+1)

(defn plot-lcg
  [lcg m]
  (let [pairs (->> (repeatedly 1000 #(lcg-rand lcg m))
                   (partition 2))  
        ds (tc/dataset pairs {:column-names [:x :y]})]
    (hanami/plot ds
                 hanami/point-chart
                 {:=x :x :=y :y})))

;; `lcg-0` with a = 7, c = 7, m = 10, and X0 = 7.
(let [pairs (->> (repeatedly 1000 lcg-0)
                 (partition 2))  
      ds (tc/dataset pairs {:column-names [:x :y]})]
  (hanami/plot ds
               hanami/point-chart
               {:=x :x :=y :y
                :=mark-size 200}))

;; Above is an LCG with a poor parameter because

;; 1. The period is short just 4 numbers, just 7,6,9, and 0.
;; 2. Only 2 pairs generated


;; `lcg-1` m = 2^48, c = 11, a = 25214903917, and X0 = 2007. Here we use lcg-rand function so that the x and y axis is small.
(let [m (Math/pow 2 48)
      pairs (->> (repeatedly 1000 #(lcg-rand lcg-1 m))
                 (partition 2))  
      ds (tc/dataset pairs {:column-names [:x :y]})]
  (hanami/plot ds
               hanami/point-chart
               {:=x :x :=y :y}))

;; Now, here we see what is called "lattice structure". There's a noticable pattern between a number and the next number in the sequence but for our present purpose, the LCG with above parameters is good enough because:

;; 1. The period length is large enough to get large amount of random numbers.
;; 2. It shows a reproducible randomness.
;; 3. The random numbers are evenly distributed between 0 and 1
;;    If plot histogram the lcg-1 values in x axis between 0 and 1, it should show roughly equal height bars which means that our lcg-1 generates equally distributed random numbers.
(let [numbers (sort (repeatedly 1000 #(lcg-rand lcg-1 (Math/pow 2 48))))
      ds (tc/dataset {:x numbers})]
  (plotly/layer-histogram ds
                          {:=x :x}))

;; Now, we have a LCG as our PRNG. We create 2 LCGs each returning same repeatable sequence of random numbers given same initial value X0.

(def lcg-A (make-lcg {:X0 2024 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(def lcg-B (make-lcg {:X0 2024 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(repeatedly 5 lcg-A)

(repeatedly 5 lcg-B)

;; Since we can create repeatable random numbers with our LCG, we create a version 2 of our fair-coin-box where the randomness comes from our LCG. This fair-coin-box now optionally accepts a seed which is the X0 from our LCG.

(defn make-fair-coin-box
  ([]
   ;; we generate a seed which is rand-int between 1 - 65432
   (fair-coin-box (rand-int 1 65432)))
  ([seed]
   (let [m (Math/pow 2 48)
         lcg (make-lcg {:X0 seed :c 25214903917 :a 11
                        :m m})]
     (fn []
       (let [tickets [:head :head :head :head :head
                      :tail :tail :tail :tail :tail]
             len (count tickets)]
         (nth tickets
              (long (* (lcg-rand lcg m) len))))))))


;; We create 2 boxes
(def box-1 (make-fair-coin-box 2007))

(def box-2 (make-fair-coin-box 2007))

;; and run 10 trials

(repeatedly 10 box-1)

(repeatedly 10 box-2)

;; Now we have reproducible results. We can imagine that you say to your colleague "Hey, I have this model that produce an interesting pattern. You can replicate my experiment by supplying the box creation with 2007 as the seed".

;; In practical work, we should use better PRNG. In Clojure ecosystem, we have fastmath library providing pseudo random generation functions. Here we use mersenne twister PRNG. with initial seed 1337

(kind/code "(require '[fastmath.random :as r ])")

;; we create the fastmath's rng
(def fm-rng (r/rng :mersenne 1337))

;; to generate random number between 0 and 1 we use drandom
(repeatedly 5 #(r/drandom fm-rng))

(def lcg-3 (make-lcg {:X0 2007 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(lcg-rand lcg-3 (Math/pow 2 48))

;; let's compare the fastmath mersenne twister rng with our lcg by plotting consecutive random number pair. We use red for our LCG and blue for mersenne-twister PRNG.

(let [m (Math/pow 2 48)
      lcg-pairs (->> (repeatedly 1000 #(lcg-rand lcg-3 m))
                     (partition 2))
      fm-rng-pairs (->> (repeatedly 1000 #(r/drandom fm-rng))
                        (partition 2))
      lcg-ds (tc/dataset lcg-pairs {:column-names [:x0 :y0]})
      fm-rng-ds (tc/dataset fm-rng-pairs {:column-names [:x1 :y1]})
      ds (-> lcg-ds
             (tc/add-column :x1 (tc/column fm-rng-ds :x1))
             (tc/add-column :y1 (tc/column fm-rng-ds :y1)))]
  (-> ds
      (hanami/layer-point {:=x :x0 :=y :y0 :=mark-color "red"})
      (hanami/layer-point {:=x :x1 :=y :y1 :=mark-color "blue"})))

;; We can see that the mersenne-twister PRNG has no discernible pattern.;;

;; ## Law of Large Numbers


;; Now we have our `make-fair-coin-box` which returns reproducible results, we're going to estimate the probability of head given n trials.

(def flip-a-coin-box (make-fair-coin-box 1337))

(defn estimate-head-optimized
  "return probability for :head from n trials of experiment modeled by flip-a-coin-box."
  [flip-a-coin-box n]
  (let [head-count (loop [i 0
                          count 0]
                     (if (< i n)
                       (recur (inc i)
                              (if (= (flip-a-coin-box) :head)
                                (inc count)
                                count))
                       count))]
    (double (/ head-count n))))


(let [ns [1 2 3 4 5 10 25 50 75 100 500 1000 5000 10000 100000]
      ps (mapv (partial estimate-head-optimized flip-a-coin-box) ns)]
  (tc/dataset {:n ns :p ps} {:dataset-name "Estimated head probability p from n experiments"}))

;; As we can see, the more we repeat the experiments, the closer we are to the true value of 50%. How many experiments do we need so that we can be confident of our estimation?

;; We'll visualize this with a plot. We're going to run 1 to 10000 trials and store the result in a dataset.

(def dataset (tc/dataset {:n (range 1 10001) :p (into [] (pmap (partial estimate-head-optimized flip-a-coin-box) (range 1 10001)))}))

(-> dataset
    (hanami/plot hanami/line-chart
              {:=x :n :=y :p}))


;; The plot shows that the larger the number of experiments, the closer our estimation to 50%. We can see it with more clarity if we use logarithmic scale for the x axis.

(-> dataset
    (hanami/plot hanami/line-chart
              {:=x :n :=y :p})
    (assoc-in [:encoding :x :scale] {:type :log})
    (update-in [:encoding :x] dissoc :type))


;; We've observed that as we increase the number of coin flips, our estimated probability for getting heads converges towards 0.5 (or 50%). This phenomenon is a practical demonstration of the Law of Large Numbers (LLN), one of the fundamental principles in probability theory and statistics.

;; *The Law of Large Numbers states that as we increase the number of trials of an independent experiment, the average of the results tends to converge to true value*.

;; So, in our coin-flipping experiment:

;; 1. Each flip is an independent trial
;; 2. The result of each trial is either head or tail
;; 3. We are interested in a trial whose result is head
;; 4. In a number of trials, the average result is the count of the occurrence of head divided by the number of trials.
;;    In our case, the average result is the estimated probability of heads.
;; 5. As we increase the number of flips, the estimated probability of heads gets closer to 0.5

;; We also can use Law of Large Numbers to estimate the probability of head from a mystery box where we don't know th proportion of heads and tails in the tickets. Here this box creation function accepts additional parameters tickets.

(defn make-mysterious-fair-coin-box
  [seed tickets]
  (let [m (Math/pow 2 48)
         lcg (make-lcg {:X0 seed :c 25214903917 :a 11
                        :m m})]
     (fn []
       (nth tickets
            (long (* (lcg-rand lcg m) (count tickets)))))))

;; We're going to create the tickets parameter using our lcg. The total number of tickets are 10, at least there's one head or tail ticket.

(def lcg-m (make-lcg {:X0 1990 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(def unknown-tickets
  (let [n-head (some #(when (< 0 %)
                        %) (repeatedly #(long (* 10 (lcg-rand lcg-m (Math/pow 2 48))))) )
        n-tail (- 10 n-head)]
    (->> []
         (into (repeat n-head :head))
         (into (repeat n-tail :tail)))))

(def mystery-box (make-mysterious-fair-coin-box 1337 unknown-tickets))

(def dataset (tc/dataset {:n (range 1 10001) :p (into [] (pmap (partial estimate-head-optimized mystery-box) (range 1 10001)))}))

;; We plot the estimation using logarithmic scale

(-> dataset
    (hanami/plot hanami/line-chart
                 {:=x :n :=y :p})
    (assoc-in [:encoding :x :scale] {:type :log})
    (update-in [:encoding :x] dissoc :type))

;; Let's see the proportion of the head tickets

(/ (double (count (filter #(= % :head) unknown-tickets))) 10)

;; Our estimation with large amount of trials resulting in a value which is close enough to the proportion of the unknown-tickets.

;; ## Central Limit Theorem
