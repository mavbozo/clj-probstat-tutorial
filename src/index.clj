^:kindly/hide-code
(ns index
  (:require
   [clojure.java.io :as io]
   [fastmath.core :as m]
   [fastmath.random :as r]
   [scicloj.kindly.v4.kind :as kind]
   [scicloj.tableplot.v1.hanami :as hanami]
   [tablecloth.api :as tc]))

;; ## Foreword

;; This introduction to probability and statistics assumes readers's familiarity with Clojure and with high-school maths (algebra, probability, statistics, _etc_.).

;; We use Clojure version >= 1.11 and several Scicloj libraries: fastmath, clay, tablecloth, and tableplot.

;; **Note**: this is still a work in progress. I'd love to receive your feedbacks or suggestions.

;; # Setup

;; In this intro, if you want to run some codes, here are the dependencies declarared in deps.edn


^:kindly/hide-code
(kind/code ":deps {org.clojure/clojure {:mvn/version \"1.11.3\"} ;; any clojure version >= 1.11
       generateme/fastmath {:mvn/version \"3.0.0-alpha1\"}
       org.scicloj/clay {:mvn/version \"2-beta16\"}
       scicloj/tablecloth {:mvn/version \"7.029.2\"}")

;; and the requires in the repl

^:kindly/hide-code
(kind/code "'(require
  [clojure.java.io :as io]
  [fastmath.core :as m]
  [fastmath.random :as r]
  [scicloj.kindly.v4.kind :as kind]
  [scicloj.tableplot.v1.hanami :as hanami]
  [tablecloth.api :as tc])")


;; # Probability

;; In our daily lives, we often encounter situations where we don't know what will happen. Some of these situations can have a significant impact on our lives, while others simply spark our curiosity or provide us with a sense of excitement and anticipation. For example, during the rainy season, we might wonder if tomorrow's weather will leave us soaked, prompting us to prepare an umbrella beforehand. Similarly, hoping for our favorite sports team to win a game can fill us with eager anticipation. In our jobs as programmers, we may need to estimate the maximum requests per second our service can handle during peak hours so that we can allocate resources accordingly.

;; In probability and statistics, we refer to the act of witnessing or measuring these uncertain situations as an _experiment_ or an _observation_. An experiment is considered random when we cannot predict the outcome with certainty. In contrast, a deterministic experiment is one where the result can be predicted with near certainty, such as evaluating the expression (+ 3 4) in Clojure, which will always return 7.

;; Probability of these uncertain situations are scaled between 0% and 100%. The higher the probability, the more likely a situation to occur. A weather report telling us there's a 80% probability of raining tomorrow make us more ready to bring an umbrella tomorrow compared to report telling us 30% probability of raining. Similarly, a 80% chance for our local sports team to win might bring us a sense of calm when watching them play compared to cautious feeling if the probability of winning is just 30%.

;; ## Probability Space

;; ### Sample Space
;; Let's consider a classic experiment which occurs in most of probability books, a coin flip experiment. We flip a coin and see whether head or tail occurs. We're doing 1 experiment or 1 trial and the possible results are heads or tails. In probability theory, those heads or tails are the possible _outcomes_ of an experiment. We could use clojure keyword to symbolize the outcomes: `:H` for heads and `:T` for tails.

;; The set of outcomes from a coin flip experiment is
^:kindly/hide-code #{:H :T}


;; For another ilustration, we have an experiment where we flip a coin 3 times — a flip-a-coin-3-times experiment. We model an outcome as a vector of individual coin flip,  ex: [:H :H :H] for an outcome where the first, second, and third coin flips are heads. 

;; We'll use this code to generate the set of outcomes for flipping a coin 3 times.

(->> (for [c1 [:H :T]
           c2 [:H :T]
           c3 [:H :T]]
       [c1 c2 c3])
     (into #{}))

;; There are 8 possible outcomes for our flip-a-coin-3-times experiment.

;; From a particular experiment, each _outcome_ is unique. An experiment yields an _outcome_ for each trial. The set of all possible _outcomes_ of an experiment is the _sample space_ which usually denoted with `S` or `Ω` (Greek capital for Omega).

;; **Outcome**: a possible result of a _random_ experiment

;; **Sample Space**: a set of possible outcomes

;; So, this is a sample space or Ω for flip-a-coin experiment
(def S-a-coin-flip #{:H :T})

;; And this is a sample space or Ω for flip-a-coin-3-times
(def S-flip-a-coin-3-times #{[:H :H :H] [:H :H :T] [:H :T :H] [:H :T :T] [:T :H :H] [:T :H :T] [:T :T :H] [:T :T :T]})

;; Here's another example. Clojure programmers who work with databases sooner or rather will have to deal with response times from database queries. The outcomes are the measured response times, for example : 1.5μs, 2ms, 100ms, 2 seconds. Theoretically, the response times are real numbers greater than 0 and we can write the sample space in mathematical notation where _t_  is the measured response times.

;; Sample space for database queries response times:

^:kindly/hide-code (kind/tex "\\Omega = \\{t \\in \\mathbb{R} \\mid 0 < t \\}")

;; Here we have a distinction between discrete sample space and continuous sample space. The sample space from the flip-a-coin experiment, `#{:H :T}` is a discrete sample space. A discrete sample space is a sample space where it is a finite or countably infinite set. I won't go far into set theory here but suffice to say that the we can enumerate the elements of a discrete sample space. Another example, the sample space from rolling a dice is the numbers on the sides of the dice `#{1, 2, 3, 4, 5, 6}`. We can contrast discrete sample space with a continuous space like database query response times where, between any two outcomes, there are infinitely many other possible outcomes. Take response times - between 1ms and 2ms, there's 1.1ms, 1.01ms, 1.001ms, and so on without end. We can not enumerate all the possible outcomes from a continuous sample space.

;; Here's one of the way we can visualize the contrast.

;; Discrete space (dice):

;; ```1 •  2 •  3 •  4 •  5 •  6 •```

;; Continuous space (response times):

^:kindly/hide-code (kind/code "
0ms ────────────────────> ∞

   Every point is a possible outcome
 ")

;; Representing the sample space is easy is Clojure

(def S-dice-roll #{1 2 3 4 5})

;; The response time sample space, mathematically, contains all real numbers greater than 0. The actual measurements are always discrete and enumerable due to:

;; 1. Finite precision of measuring instruments
;; 2. Digital storage limitation
;;    In Clojure, a value with type double (java.lang.Double) uses 8 bytes.
;; 3. Practical timing resolution of the system

;; Let's represent the response time sample space in Clojure. We're going to choose a millisecond precision and create a function to generate the sample space up to `max-time`. We can imagine that we can instruct our measuring instrument to return numbers in millisecond precision and returning just the `max-time` if the actual measurement are above it. Response times 

^:kindly/hide-code (def precision-ms 1)

^:kindly/hide-code (kind/code "(def precision-ms 1) ;; millisecond precision")

;; Response times are continuous in nature (a set of Real Numbers greater than 0), but measured and stored discretely for practical purposes.

(defn db-resp-time-sample-space
  "Generate sample space up to max-time with given precision"
  [max-time]
  (set (range 1 (inc max-time) precision-ms)))

;; If we the `max-time` is 5ms, then the sample space is

(db-resp-time-sample-space 5)

;; We must keep in mind that although the representation of the sample space is enumerable in Clojure, when analyzing the sample space, we have to act as if the sample space is a continuous space.

;; ### Event Space

;; In practice, we are more interested in groups of outcomes rather than individual outcomes.  From our database response times example in the previous sub-chapter, given `t` the measured response time, we care whether the queries are fast (`0 < t < 50ms`), okay (`50ms <= t < 200ms`), slow (`200ms <= t < 1000ms`), or timeouts (`1000ms <= t`). Or in a game of flipping a coin 3 times and we win if the heads occurs only once, we care that the result is the set `#{[:H :T :T] [:T :H :T] [:T :T :H] }`. Those groups of outcomes are called `events`.

;; In probability theory, a subset of outcomes from an experiment is called an `event`. The distinction between `outcome` and `event` is practically useful, especially in experiments with continuous or very large sample spaces.

;; **Event**: a subset of outcomes in the sample space

;; Some possible events from flipping a coin 3 times:

(def heads-occur-once #{[:H :T :T] [:T :H :T] [:T :T :H]})

(def all-heads #{[:H :H :H]})

;; In a single coin-toss experiment, possible events are `#{:H}` or `#{:T}`. 

;; Events such as `#{[:H :H :H]}` and `#{:H}` or `#{:T}` are events with exactly one outcome. In probability theory, an event containing exactly one outcome is called an `elementary event`.

;; Here are the mathematical formulation of events we care about from measuring database response times. `t` is in milliseconds.

^:kindly/hide-code (kind/tex "\\text{fast-query} = \\{\\, t \\in \\mathbb{R} \\mid 0 < t < 50 \\,\\}")

^:kindly/hide-code (kind/tex "\\text{okay-query} = \\{\\, t \\in \\mathbb{R} \\mid 50 \\le t < 200 \\,\\}")

^:kindly/hide-code (kind/tex "\\text{slow-query} = \\{\\, t \\in \\mathbb{R} \\mid 200 < t \\le 1000 \\,\\}")

^:kindly/hide-code (kind/tex "\\text{timeouts-query} = \\{\\, t \\in \\mathbb{R} \\mid 1000 < t \\,\\}")

;; In Clojure, we could represent those events in the form of predicate function.

(defn fast-query? [t] (and (< 0 t) (< t 50)))
(defn okay-query? [t] (and (<= 50 t) (< t 200)))
(defn slow-query? [t] (and (<= 200 t) (< t 1000)))
(defn timeouts? [t] (<= 1000 t))

;; Since an event is a subset of the sample space, the set of all possible events is the powerset of the sample space. There are a total of 2ⁿ possible events where n is the number of elements in the sample space. In practical applications, we define an _event space_ which contains events which are meaningful for our domain, measurable, and useful for decision making.

;; **Event space**: a set of events, 𝓕. In practice, only contains events which are interesting to us.

;; In flipping coin 3 times, we might be interested in whether heads appear once or all heads appears, so the event space is defined as:

(def flip-coin-3-times-event-space #{heads-occur-once all-heads})

;; In database response times measurement, our event space is defined as

(def db-resp-times-event-space #{fast-query? okay-query? slow-query? timeouts?})

;; ### Probability Measure

;; In a probability space, there is a probability measure _P_ which is a function that assigns to each event in the event space, _a_ _probability_, a number between 0 and 1. In probability theory, we assign probabilities to events (sets of outcomes), not to individual outcomes themselves. This is crucial because it allows us to reason about meaningful groups of outcomes rather than specific instances. The distinction between outcomes and events has practical implications for system design too. When we're dealing with system health, we don't care about individual response times - we care about events like "responses under 200ms" or "error rates below 1%" or "AWS SLA Level of 99.95%". Those are sets of outcomes, not individual measurements.

;; **Probability Measure or _P_**, a function which assigns probability to each event.

;; The probability measure function _P_ must follow these 3 axioms:

;; 1. Non-negativity : For any event A, P(A) >= 0. It means that probability can't be negative.
;; 2. Normalization: P(Ω) = 1, the probability of the entire sample space (Ω) being 1 comes from summing the probabilities of all elementary events. Remember, an elementary event is an event containing exactly one outcome.
;; 3. Countable Additivity: For any sequence of disjoint events A₁, A₂, A₃, ..., we have:
;; P(A₁ ∪ A₂ ∪ A₃ ∪ ...) = P(A₁) + P(A₂) + P(A₃) + ...

;; For demonstration of those 3 axioms, let's use our flip-a-coin experiment which has an event space containing all 2 elementary events "heads" and "tails". The experiment uses a fair-coin, so both heads and tails are equally likely to occur which means the probability measure function assigns equal probability 50% to "heads" and "tails" (axiom 1: non-negativity). The sum of the probability for those 2 events must be 1 (axiom 2: normalization). "heads" and "tails" are disjoint events, meaning they never occurs simultaneously. The sum of those disjoint events' probabilities equals P(head event) + P(tail event) (axiom 3: countable additivity).

;; From a real world example, think of an election with three candidates. The probability of each candidate winning must be non-negative (axiom 1), all candidates together make up the whole sample space so their probabilities sum to 1 (axiom 2), and if we group any two candidates together, their combined probability of winning is the sum of their individual probabilities (axiom 3).

;; ### Definition of Probability Space

;; **Probability Space** is a mathematical formalism of a random experiment. A probability space is a triple (Ω, 𝓕, _P_) or (sample space, event space, probability measure).

;; This probability space is a useful formal structure which gives a consistent way to think about randomness of a phenomena. Like any attempts to model a natural phenomena to a mathematical formalism, it is up to us to translate a phenomena under consideration to a probability space. In the database query response time example above, the possible response times defines the sample space and the events are groupings of range of response times according to our criteria.

;; In the next chapter, we're going to build more understanding of random experiments and their probability spaces by representing them as boxes full of tickets. I hope I can illustrate more clearly about the formalism of the probability space using tickets-in-a-box model.

;; ## Tickets in a Box Model

;; We can simulate _random_ experiments in our computers. We can, programmatically, create a model of _random_ experiment and run the model simulations in our computers. The model should be small enough to capture relevant aspects of the probability space from a _random_ experiment. The model should have these properties of a probabilty space: the outcomes, events, sample space, event space, and probabilitiy function.

;; We will use a tickets-in-a-box model or "box model", for short. The box contains tickets and each ticket has an outcome written on it. The outcome written could be "head" or "tail" from tossing a coin, "win" or "lose" or "draw" from a sports match between 2 teams, or all possible response times from database queries -- it depends on the random experiments. Every potential outcome is represented on at least one ticket in the box. Certain outcomes might be found on multiple tickets. We can imagine that we shake the box to ensure that the tickets inside are randomly shuffled thus each individual ticket are equally likely to be selected. Doing an experiment means we pick a ticket from the box and then see what is written on the ticket to know the outcome.

;; We're going to start from a random experiment of flipping a fair-coin once. We have a box we call flip-a-coin-box. The outcomes are heads and tails, so what is written on a ticket is either "head" or "tail". Let's use Clojure keywords to symbolize the outcomes written on the tickets: `:H` for "head" and `:T` for "tail". The events are either `#{:H}` or `#{:T}`. The amount of tickets with `:H` or `:T` written on them determines the probability measure. So, the probability space for this experiment consists of:

;; 1. Sample space (all possible outcomes): `#{:H :T}` whose individual member is what we see on written on a picked ticket from the box.
;; 2. Event space : `#{ #{:H} #{:T} }`, the set of possible events. We either see heads or tails.
;; 3. Probability measure.
;;    Since we're using a fair-coin in this experiment, the heads and tails are equally likely to occur. We could insert any amount of tickets as long as we have same amount of "head" and "tail" tickets. Thus, the probability measure assigns the probability of an event where head occurs #{:H} by dividing the number of "head" tickets by the total number of tickets.

;; Let's say we put 10 tickets with equal amounts of heads and tails. Here we use a vector whose elements are 5 `:H` and 5 `:T`.
(def flip-a-coin-box-tickets (-> []
                                 (into (repeat 5 :H))
                                 (into (repeat 5 :T))))

;; These represents the tickets in the box.
flip-a-coin-box-tickets

;; In this box model, the probability measure _P_ assigns a probability to an event just by simple arithmetic. It just counts the tickets where the written outcome are the members of the event set and then divide it by the total number of tickets. Here's one possible implementation of probability measure function for this box.

(defn P-flip-a-coin-box
  "the probability function `P` for flip-a-coin-box.
   returns probability of an `event`"
  [event]
  (double (/ (count (filter event flip-a-coin-box-tickets)) (count flip-a-coin-box-tickets))))

;; Clojure affords us to use sets as predicate functions. Since we represent events as clojure sets, we can use the event as predicate to filter the tickets.

;; We can enumerate assigned probabilities for `#{:H}` and `#{:T}` events from the the probability measure function by using this simple expression.
(map P-flip-a-coin-box [#{:H} #{:T}])

;; The proportions of each written outcomes relative to the total number of tickets defines the probability measure function of a box. In our flip-a-coin-box with fair-coin above, it doesn't matter whether we use 2 tickets (1 head, 1 tail), 10 tickets (5 head, 5 tail), or 1 million tickets (500 thousands head and tails ticket), the _P_ assigns same probability numbers for all events.

;; **Probabilities determine boxes** If we know or have guessed the probabilities we want to model, then we have to decide how to fill the boxes with the tickets. In this example, we decided to use a fair-coin which has 50-50% probability for "heads" or "tails" so we fill the box with equal amounts of "heads" and "tails" tickets.

;; We have sample space, event space, and probability measure function. Now, let's model the experiment. Doing the experiment means we properly shake the box then pick a ticket. The experiment is implemented as `flip-a-coin-box` function where calling that function means we shake the box, pick a random ticket, then put the ticket in the box. The return value of the function is the outcome written on a ticket. In our implementation, we use `clojure.core/rand-nth` function to simulate the acts of shuffling the box and pick a random ticket. We also don't remove the ticket from the box collection to simulate the act of putting the picked ticket back into the box. 

(defn flip-a-coin-box
  "Simulates the act of shuffling and randomly picks a ticket from a box representing random experiment of flipping a coin. The picked ticket then put back into the box.
   The return value is what is written in the picked ticket, which is the outcome of the experiment `:H` for head or `:T` for tail."
  []
  (rand-nth flip-a-coin-box-tickets))

;; For example, this is a result of doing 5 trials of the experiment.
(repeatedly 5 flip-a-coin-box)

;; **Combining Boxes** Two or more boxes can be combined.  The simultaneous drawing of tickets from the boxes is the outcome of a single _combined_ experiment.

;; We'll combine 3 flip-a-coin-box to model our flipping a coin thrice experiment using a fair coin. The result of simulteneous drawing of a ticket from each box is the result of the experiment.

(defn flip-a-coin-thrice-box
  "Simulates the act of shuffling and randomly pick a ticket from a box representing random experiment of flipping a coin 3 times. The picked ticket then put back in the box.
   The output is the outcome of the experiment for example `[:H :H :T]` for a sequence of head, head, tail.

  In this implementation, we simultenously draw tickets from 3 flip-a-coin-box-es"
  []
  [(flip-a-coin-box) (flip-a-coin-box) (flip-a-coin-box)])

;; An example of 5 repeated trials.
(repeatedly 5 flip-a-coin-thrice-box)

;; Here's the probability measure defined for this box.
(defn P-flip-a-coin-thrice-box
  "the probability measure function `P` for flip-a-coin-thrice-box.
  returns probability of an event satisfying event-pred?"
  [event-pred?]
  (let [flip-a-coin-thrice-box-tickets
        (for [t1 flip-a-coin-box-tickets
              t2 flip-a-coin-box-tickets
              t3 flip-a-coin-box-tickets]
          [t1 t2 t3])]
    (double (/ (count (filter event-pred? flip-a-coin-thrice-box-tickets))
               (count flip-a-coin-thrice-box-tickets)))))

;; We modified our previous _P_ for flip-a-coin-box so that this implementation accepts a generalized event predicate function. We're not limited to just using sets as event but instead we can use any function to act as predicate to determine whether what's written (outcome) belongs to an event. Rather than enumerating outcomes belonging to an event, we can just create a predicate function like examples below.

;; For example, we want to know the probability of heads occur only once. We can make a predicate function to filter outcomes which has 1 head occurance.

(defn heads-occur-once?
  "Given a possible outcome of flipping a coin thrice, returns true if it contains only 1 head, else false"
  [outcome]
  (= 1 (count (filter #(= :H %) outcome))))

;; The probability of seeing heads occur only once is:
(P-flip-a-coin-thrice-box heads-occur-once?)

;; For another example, what is the probability heads occur at least once? The event predicate now filters outcomes which has at least 1 head.
(defn heads-occur-at-least-once?
  "Given a possible outcome of flipping a coin thrice, returns true if it contains at least 1 head, else false"
  [outcome]
  (< 0 (count (filter #(= :H %) outcome))))

;; The probability of seeing heads occur at least once is:
(P-flip-a-coin-thrice-box heads-occur-at-least-once?)

;; Now, we are making a box for our database response times experiments. In Probability Space chapter above, the precision of our experiments is 1 ms. Thus, the sample space consists of numbers 1, 2, 3, 4, ... up to 1000 ms and those numbers are written on the tickets. In this box model, we set that each numbers from 1 to 100 are written on 100 tickets each. For the rest of the numbers, we use simple decaying function `tickets = 100 * (1/n)` to determine the number of tickes for each of them.

(defn make-db-response-time-box-tickets
  [max-time]
  (let [ ;; 100 tickets each for 1-100ms
        fast-tickets (mapcat #(repeat 100 %) (range 1 101))
        ;; Decreasing tickets for 101-1000ms
        ;; Using simple decay: tickets = 100 * (1/n) rounded up
        slower-tickets (mapcat #(repeat (Math/ceil (/ 10000 %)) %)
                               (range 101 (inc max-time)))]
    (->> [] (into fast-tickets) (into slower-tickets))))

;; We create the tickets written with the numbers from 1 - 1000.

(def db-resp-times-tickets (make-db-response-time-box-tickets 1000))

;; This is the histogram of the numbers written on the tickets.

(-> (tc/dataset {:tickets db-resp-times-tickets})
    (hanami/layer-histogram {:=x :tickets})
    (assoc :title "Histogram of Tickets Count"))

;; The probability measure function just accepts the event predicate function as input and we simply divide the number of tickets that satisfy the predicate with the count of all tickets.

(defn P-db-resp-times
  [event-pred?]
  (double (/ (count (filter event-pred? db-resp-times-tickets))
             (count db-resp-times-tickets))))

;; Using our defined event space, the probability all events is:
{:fast (P-db-resp-times fast-query?)
 :okay (P-db-resp-times okay-query?)
 :slow (P-db-resp-times slow-query?)
 :timeout (P-db-resp-times timeouts?)}

;; **Boxes determine probabilities** The number and the composition of the amount of tickets depicting various outcomes determine the probabilities of events.

;; In our database response time experiment box, we decided the amount of tickets to create for each range of outcomes. The probability for events returned from the measure function follows from the composition of the created tickets we put in the box.

;; ## Independence

;; It's important to understand that when we use `repeatedly` with our `flip-a-coin-box`, each call is an `independent` trial.  The _independence_ comes from each `rand-nth` returns a random element from the vector of tickets, independent of previous picks. _Independent_ means the probability of getting a head (or tail) on any given trial is always 0.5, regardless of what happened in any other trials.

;; This idea about _independence_ is important because, for example, in our flip-a-coin experiment with fair coin, there are some misconceptions such as:

;; 1. "After seeing three heads in a row, tails is more likely to come up next to balance things out"
;; 2. "If I've seen more heads than tails so far, the next few flips should more likely to return tails to even things up"

;; Let's focus on the simplest case that after 2 trial, heads occurances in the second trial is roughly the same as tails, even after the first trial results in head. We run the experiment 1000 times and for each trial, keep the tally of heads and tails count from the second flip if the first flip is heads.

;; The experiment
(defn flip-a-coin-twice-trials []
  [(flip-a-coin-box) (flip-a-coin-box)])

;; We create tablecloth dataset from the running tally.
(def flip-a-coin-twice-trials-ds
  (let [two-trials (repeatedly 1000 #(flip-a-coin-twice-trials))]
    (-> (reduce (fn [{:keys [nt H T] :as acc} [c1 c2]]
                  (let [nt1 (inc nt)
                        [H1 T1] (if (= :H c1)
                                  (if (= :H c2)
                                    [(inc H) T]
                                    [H (inc T)])
                                  [H T])]
                    (-> acc
                        (update :s into [{:nth-trial nt1 :count H1 :result "heads"} {:nth-trial nt1 :count T1 :result "tails"}])
                        (assoc :nt nt1 :H H1 :T T1))))
                {:s [] :nt 0 :H 0 :T 0} two-trials)
        (get :s)
        (tc/dataset {:dataset-name "Running Tally after Heads"}))))

;; This is the plot of the tally.
(-> flip-a-coin-twice-trials-ds
    (hanami/layer-line
     {:=x :nth-trial :=y :count :=color :result})
    (assoc :title "Running Tally after Heads"))

;; In above plot, we can see that the in the nth-trial, the total of occurances of heads and tails in the second flip remains close even after the first flip is heads.

;; For another demonstration, we'll analyze the count of streaks of heads from a fair coin flip experiments, ex: [:H :H] (2 streaks) or [:H :H :H] (3 streaks).

;; The dataset contains heads streak counts from 1000 trials.
(def flip-a-coin-twice-streaks-ds
  (let [n 1000
        trials (repeatedly n flip-a-coin-box)
        streak-lengths (range 1 11)
        cf (fn [length]
             (reduce (fn [acc xs]
                       (if (every? #{:H} xs)
                         (update acc :count inc)
                         acc))
                     {:streak-length length
                      :count 0}
                     (partition-all length 1 trials)))
        raw (reduce (fn [acc l] (conj acc (cf l))) []
                    streak-lengths)]
    (tc/dataset raw {:dataset-name "Heads Streak Counts"})))

;; The data
flip-a-coin-twice-streaks-ds

;; And the visualization in bar plot
(-> flip-a-coin-twice-streaks-ds
    (hanami/layer-bar {:=x :streak-length
                       :=y :count})
    (assoc :title "Heads Streaks Count from 1000 trials"))

;; In the plot above, we see that the 1 streak [:H] occurs about 500 times, well within our expectation that heads is likely to occur 50% of the times. But then, the next streak occurs about half of the previous streak, [:H :H] about 250 and [:H :H :H] about 125. The table shows us that even after seeing heads so many times, the probability of seeing heads in the next flip is still 50%.


;; This notion of independence is crucial in probability theory. The outcome of one trial doesn't influence the others. In above demonstration the count of heads and tails are almost the same. There's no bias to tails after a head result.

;; ## Pseudo-Random Generators (PRNGs)

;; Imagine that you have a fair-coin and after doing an experiment by flipping the coin 10 times, you believe you have found a fascinating pattern. You tell your colleague, "Hey, I flipped this coin 10 times and got this fascinating pattern!". But, when your colleague replicate your experiment, they get completely different result. How can they verify your experiment? In game development, we might develop a gameplay with random elements and we encounter a bug. Without reproducibile randomness, it would be nearly impossible for other developers to recreate and fix the problems.

;; Let's go back to our `flip-a-coin-box` function implementation above.

^:kindly/hide-code
(kind/code "(def flip-a-coin-box-tickets (-> []
                                 (into (repeat 5 :H))
                                 (into (repeat 5 :T))))")

^:kindly/hide-code
(kind/code "(defn flip-a-coin-box []
  (rand-nth flip-a-coin-box-tickets))")

;; We can imagine giving this box to our colleague but they'll get different result when calling this function 10 times.

;; Our trials
(repeatedly 10 #(flip-a-coin-box))

;; Our colleague trials
(repeatedly 10 #(flip-a-coin-box))

;; Now, what if we can, somehow, make a box that still give us a random ticket when picked, but returns reproducible result? In our implementation for our flip-a-coin-box, the key is the `rand-nth` function. For our purpose, `rand-nth` picks an element from a random index of our vector of tickets. We could make a function which reproducibly generate a random index from our vector or we generalize and create a function that reproducibly generates a random number then we convert the produced random number to an index of our vector.

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

;; A sequence of 20 random numbers
(repeatedly 20 lcg-0)

;; As we can see the result is a periodic sequence of 7, 6, 9, 0, then it repeats again. Mathematically inclined readers will quickly realize that there is a period of numbers because of the use of modulus in LCG formula and we choose 10 as our m. The choice of X0, a, m, and c determines the period of the generated sequence of numbers before it cycles again. It is the property of LCG that it produces a periodic sequence of _pseudo_ random numbers. We're going to choose sufficiently large m and combination of a and c to make our period large enough so we can generate many many random numbers before the numbers form a new period.

;; We're going to use m = 2^48, c = 11, a = 25214903917, which are parameters used in java.util.Random according to wikipedia (https://en.wikipedia.org/wiki/Linear_congruential_generator)

(def lcg-1 (make-lcg {:X0 2007 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

;; Let's generate 20 random numbers
(repeatedly 20 lcg-1)

;; The generated number is too large (ex: 1.9 x 10^14), we could create another function, lcg-rand, whose input is the lcg function and the modulus and returns random number between 0 and 1. We just divides the number returned from lcg with the m.

(defn lcg-rand
  "given a lcg function, returns random number between 0 and 1"
  [lcg m]
  (/ (double (lcg)) m))

;; The is the comparison when using `lcg-0` and `lcg-1` when generating 10 random numbers.
(repeatedly 10 #(lcg-rand lcg-0 (m/pow 2 48)))

(repeatedly 10 #(lcg-rand lcg-1 (m/pow 2 48)))

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
  (hanami/layer-histogram ds
                          {:=x :x}))

;; Now, we have a LCG as our PRNG. We create 2 LCGs each returning same repeatable sequence of random numbers given same initial value X0.

(def lcg-A (make-lcg {:X0 2024 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(def lcg-B (make-lcg {:X0 2024 :c 25214903917 :a 11
                      :m (Math/pow 2 48)}))

(repeatedly 5 lcg-A)

(repeatedly 5 lcg-B)

;; Since we can create repeatable random numbers with our LCG, we create a version 2 of our fair-coin-box where the randomness comes from our LCG. This fair-coin-box now optionally accepts a seed which is the X0 from our LCG.

(defn make-flip-a-coin-box
  ([]
   ;; we generate a seed which is rand-int between 1 - 65432
   (make-flip-a-coin-box (rand-int 1 65432)))
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
(def box-1 (make-flip-a-coin-box 2007))

(def box-2 (make-flip-a-coin-box 2007))

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
