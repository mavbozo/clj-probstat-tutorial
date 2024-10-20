^:kindly/hide-code
(ns index
  (:require
   [clojure.java.io :as io]
   [fastmath.core :as m]
   [fastmath.random :as r]
   [scicloj.kindly.v4.kind :as kind]
   [tablecloth.api :as tc]
   [ggplot :as gg]))

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

;; We can simulate _random_ experiments in our computers. We can, programmatically, create a model of _random_ experiment and run simulations with it. The model should be small enough to capture relevant aspects of a _random_ experiment which are things we have learned: the outcomes, sample space, events, and probabilities.

;; We will use a ticket-in-a-box model or "box-model", for short. The box contains tickets and each ticket has an outcome written on it. The outcome written could be "head" or "tail" from tossing a coin, "win" or "lose" or "draw" from a sports match between 2 teams, or all possible temperature readings from maximum temperatur experiment -- it depends on the random experiment. All possible outcomes are written at least once among the tickets. Some outcomes may appear on many tickets. The tickets are thoroughly mixed in that box. Doing an experiment means we pick a ticket from the box and the outcome of the experiment is what is written on the ticket.

;; An outcome with more written tickets has more probability than other outcomes with lesser tickets. 

;; <blockquote src="https://stats.stackexchange.com/a/54894">Instead of actually conducting the experiment, we imagine thoroughly--but blindly--mixing all the tickets and selecting just one. If we can show that the real experiment should behave as if it were conducted in this way, then we have reduced a potentially complicated (and expensive, and lengthy) real-world experiment to a simple, intuitive, thought experiment (or "statistical model"). The clarity and simplicity afforded by this model makes it possible to analyze the experiment.</blockquote>
;; source [https://stats.stackexchange.com/a/54894](https://stats.stackexchange.com/a/54894)

;; Let's create a box for our flip-a-coin experiment. The box is a clojure function `flip-a-coin-box` which, when called, simulates the act of picking a ticket from a box. The `flip-a-coin-box` returns :head or :tail which are the outcomes written on tickets.

;; For example:
^:kindly/hide-code (kind/code "(flip-a-coin-box) => :head")
^:kindly/hide-code (kind/code "(flip-a-coin-box) => :tail")

;; We use a fair coin in our experiment, so we need to ensure that when we pick a ticket from the box, :head and :tail tickets have equal chance to be picked. We can use the help of a macro `randval` in `fastmath.random` namespace, which given arguments 0.5, :head, :tail, will return :head or :tail with equal chance 50%.

^:kindly/hide-code (kind/code "(require '[fastmath.random :as r])")

(defn flip-a-coin-box []
  (r/randval 0.5 :head :tail))

;; let's do experiment 5 times

(repeatedly 5 flip-a-coin-box)

;; It's important to understand that when we use `repeatedly` with our `flip-a-coin-box` function, 
;; we're not flipping the same coin 5 times in sequence. Instead, we're simulating 5 _independent_
;; trials of the experiment.

;; We can think of this in two ways:

;; 1. Independent Trials: Each call to `flip-a-coin-box` is an independent event. 
;;    It's as if we have 5 different people, each with their own coin, flipping once simultaneously.

;; 2. Parallel Universes: Imagine we have 5 parallel universes, each identical up to the point 
;;    of the coin flip. In each universe, the coin is flipped once. We then collect the results 
;;    from all these universes.

;; This distinction is crucial in probability theory. The outcome of one flip doesn't influence 
;; the others, maintaining the 50/50 chance for each flip regardless of previous results.

;; ## Easy Probability Estimation

;; Let's estimate the probability for :head result solely based the result of the trials from our `flip-a-coin-box`. One way of estimating the probabilty for :head is by counting the :head occurrance and divide that from the total number of trials.

(defn estimate-head
  "return probability for :head from n trials"
  [n]
  (let [xf (comp (filter #(= % :head)) (map (fn [x] (if (= x :head) 1 0))))
        nhead (transduce xf + (repeatedly n flip-a-coin-box))]
    (double (m// (double nhead) n))))

;; from 10 trials

(str "estimated probability for head from 10 trials: " (estimate-head 10))

(comment
#_ comment-start

(require '[criterium.core :refer [bench quick-bench]])

(quick-bench (estimate-head 10000))

(quick-bench (estimate-head-optimized 10000))

(repeatedly 10 #(r/randval 0.5 :high :low))

(def ds0 (tc/dataset {:x (range 5)
                      :y (range 5)}))

(require '[fastmath.vector :as v])

(v/add [1 2 3] [1 2 3])

(v/add (:y ds0) (range 5))

(v/add (:y ds0) (range 5))

(v/add (range 5) (:y ds0))

#_ comment-end)



;; let's that 10 more times

^:kindly/hide-code (repeatedly 10 #(str "estimated probability for head from 10 trials: " (estimate-head 10)))

;; Many estimates are close but not exactly 0.5. What if we use 100 trials.

(str "estimated probability for head from 100 trials: " (estimate-head 100))

;; which is closer than doing 10 trials. So, let's doing 100 trials again a few times

^:kindly/hide-code (repeatedly 10 #(str "estimated probability for head from 100 trials: " (estimate-head 100)))

;; what happen if we have 10,000 trials

^:kindly/hide-code (repeatedly 5 #(str "estimated probability for head from 10000 trials: " (estimate-head 10000)))

;; Now, the estimates are really close to the true value 50%.

;; ### Law of Large Numbers

;; We create the dataset containing the number of experiments and the estimated probability for head from the last chapter. We use tablecloth library to create the dataset.

(defn estimate-head-optimized
  "return probability for :head from n trials"
  [n]
  (let [head-count (loop [i 0
                          count 0]
                     (if (< i n)
                       (recur (inc i)
                              (if (= (flip-a-coin-box) :head)
                                (inc count)
                                count))
                       count))]
    (double (/ head-count n))))

(let [ns [5 10 100 1000 10000 100000]
      ps (mapv estimate-head-optimized ns)]
  (tc/dataset {:n ns :p ps} {:dataset-name "Estimated head probability p from n experiments"}))


;; As we can see, the more we repeat the experiments, the closer we are to the true value of 50%. How many experiments do we need so that we can be confident of our estimation.

;; We'll visualize this with a plot. We're going to run 1 to 100,000 experiments and store the result in a dataset.

;; (time
;;  (def dataset (tc/dataset {:n (range 1 100001) :p (into [] (pmap estimate-head-optimized (range 1 100001)))})))

(def dataset (tc/dataset (io/file "test.csv") {:key-fn keyword}))

(def dataset1 (clojure.set/rename-keys dataset {:n :x :p :y}))

(gg/->image (gg/line-plot dataset1))

;; The plot shows that the larger the number of experiments, the closer our estimation to 50%. We can see it with more clarity if we use logarithmic scale for the x axis.

(gg/->image (gg/line-plot-log-x-scale dataset1))

;; We've observed that as we increase the number of coin flips, our estimated probability for getting heads converges towards 0.5 (or 50%). This phenomenon is a practical demonstration of the Law of Large Numbers (LLN), one of the fundamental principles in probability theory and statistics.

;; The Law of Large Numbers states that as we increase the number of trials of an independent experiment, the average of the results tends to converge to true value. So, in our coin-flipping experiment:

;; 1. Each flip is an independent trial
;; 2. The result of each trial is either head or tail
;; 3. We are interested in a trial whose result is head
;; 4. In a number of trials, the average result is the count of the occurrence of head divided by the number of trials.
;;    In our case, the average result is the estimated probability of heads.
;; 5. As we increase the number of flips, the estimated probability of heads gets closer to 0.5


;; ### Central Limit Theorem

;; ### More Tickets in a Box Examples

;; Let's consider eagle sightings observation whose outcome is the number of eagles seen on a particular day. The event could 0 eagles seen, 1 eagle seen, or 7 eagles seen, etc. Using box-model, the box is the experiment and the number written on any tickets could be integers: 0, 1, 2, 3, .. etc. We then pick a ticket from the box, see the number written on the ticket that tells us the number of eagles seen for that particular day.

;; We're going to create the eagle sightings box using Clojure and fastmath library. We want to make a function `eagle-sightings` which simulates the act of picking a ticket from a box and that function returns the outcome or number written on a picked ticket.

;; For example:
^:kindly/hide-code (kind/code "(eagle-sightings) => 1 ;; 1 eagle seen")
^:kindly/hide-code (kind/code "(eagle-sightings) => 5 ;; 5 eagles seen")

;; Since the number returned from the `eagle-sightings` evaluation is completely unknown to use beforehand, we have to make or use some kind of number generator whose resulting number is unpredictable. That number generator is usually called a random number generator. Our programs run on computers which are deterministic so at best we can create or use _pseudo-random number generators_ (or PRNG for short). 

;; **A Detour to Pseudo-Random Number Generator (PRNG) in fastmath**

;; _Pseudo-random number generators_, if they are well coded, produce deterministic streams of output that appear to be random.

;; Let's play around with PRNGs using fastmath.

;; include fastmath in deps.edn
^:kindly/hide-code (kind/code "generateme/fastmath {:mvn/version \"3.0.0-alpha1\"}")

;; require `fastmath.random`
^:kindly/hide-code (kind/code "(require '[fastmath.random :as r])")

;; We create a prng with Mersenne-Twister algorithm
(def prng0 (r/rng :mersenne))

;; In the context of eagle sightings, we use `irandom` function to get a random integer from `prng0` and we limit the sightings at maximum 100 eagles seen.

(r/irandom prng0 0 101)

;; let's take possible numbers from 10 sightings

(repeatedly 10 (fn [] (r/irandom prng0 0 101)))

;; PRNGs produce deterministic streams of output by having some kind of internal state. In Clojure parlance, PRNGs are stateful. During the course of its execution, a PRNG's state follow the same path of changes. We can set the initial state of a PRNG by specifying a state `seed`. Instances of a prng with the same algorithm and the same `seed`, produce identical stream of outputs because those instances are deterministic and start from the same state.

;; Let's create 2 Mersenne-Twister PRNGs with the same `seed` 42.
(def prng1 (r/rng :mersenne 42))
(def prng2 (r/rng :mersenne 42))

;; Both yield identical stream of numbers.
(repeatedly 10 (fn [] (r/irandom prng1 0 101)))
(repeatedly 10 (fn [] (r/irandom prng2 0 101)))

;; Supplying seed when creating prng is an important part to make our _random_ experiment reproducible.

;; **Back to eagle sightings box model**

;; So, our `eagle-sightings` implementation could be like this

(defn eagle-sightings
  "return number of eagles seen on a particular day.
  the number is an integer between from 0 to 100."
  []
  (let [rng (r/rng :mersenne)]
    (r/irandom rng 0 101)))

;; Do 2 experiments
(eagle-sightings)
(eagle-sightings)

;; By using this box model implemented through codes and running in our computer, we mimic the experiment done in the real world. We can run this experiment many times and do further analysis to the model. There's still a problem that we haven't shown that this model behave like the real world experiment. We'll tackle that problem further in this tutorial.

;; We can create reproducible model. We can imagine doing experiment with a box and then giving the box to another person. That person will have same sequence of tickets drawn from the box. By using seed, we can ensure the model produce reproducible results. Let's make a `generate-eagle-sightings-box` which given same `seed`, returns reproducible eagle sightings experiment. 

(defn generate-eagle-sightings-box
  "given `seed` for prng, returns eagle sightings function.
  The eagle sightings function returns number of eagles seen on a particular day.
  the number is an integer between from 0 to 100."
  [seed]
  (let [rng (r/rng :mersenne seed)]
    (fn eagle-sightings [] (r/irandom rng 0 101))))

;; generate experiment 1 using 1337 as seed
(def eagle-sightings-1 (generate-eagle-sightings-box 1337))

;; generate experiment 2 using 1337 as seed
(def eagle-sightings-2 (generate-eagle-sightings-box 1337))

;; run experiment 1 10 times
(repeatedly 10 (fn [] (eagle-sightings-1)))

;; run experiment 2 10 times
(repeatedly 10 (fn [] (eagle-sightings-2)))

;; both return identical results

;; ### Calculating Probability

;; One way to define probability of an event is by counting. Let's take 100,000 tickets from the box, count the occurence of tickets with 7 written on them. The probability of ticket with number 7 is the count of occurrence divided by the total number of tickets picked.

;; Generate a box with seed 1234
(def eagle-sightings-3 (generate-eagle-sightings-box 1234))

;; A function to calculate probability for a ticket outcome from n trials from a box.
(defn ticket-probability
  "given a box, calculate the probability of a ticket outcome from n trials"
  [box n ticket]
  (let [c (->> (repeatedly n (fn [] (box)))
               (filter #(= % ticket))
               count)]
    (/ (float c) n)))

;; The probabilities for a 7 ticket outcome from 10 sets of 10 trials
(repeatedly 10 (fn [] (ticket-probability eagle-sightings-3 10 7)))

;; The 7 tickets rarely occurs, so let's increase the trials to 100

(repeatedly 10 (fn [] (ticket-probability eagle-sightings-3 100 7)))

;; The 7 ticket shows up with varying probabilities. Let's see what 100,000 trials result

(repeatedly 10 (fn [] (ticket-probability eagle-sightings-3 100000 7)))

;; With large amount of trials, the probability for a 7 outcome is about 0.01.
