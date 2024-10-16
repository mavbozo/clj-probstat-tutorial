^:kindly/hide-code
(ns index
  (:require [fastmath.random :as r]
            [scicloj.kindly.v4.kind :as kind]))

;; This tutorial assumes some familiarity with Clojure and with high-school maths (algebra, probability, statistics, _etc_.).

;; We use Clojure and [fastmath](https://github.com/generateme/fastmath) version 3 in this tutorial.

;; **Note**: this is still a work in progress. I'd love to receive your feedbacks or suggestions.

;; # Probability

;; In our daily lives, we often encounter situations where we don't know what will happen. Some of these situations can have a significant impact on our lives, while others simply spark our curiosity or provide us with a sense of excitement and anticipation. For example, during the rainy season, we might wonder if tomorrow's weather will leave us soaked, prompting us to prepare an umbrella beforehand. Similarly, hoping for our favorite sports team to win a game can fill us with eager anticipation. In our jobs as programmers, we may need to estimate the maximum requests per second our service can handle during peak hours so that we can allocate resources accordingly.

;; In probability and statistics, we refer to the act of witnessing or measuring these uncertain situations as an _experiment_ or an _observation_. An experiment is considered random when we cannot predict the outcome with certainty. In contrast, a deterministic experiment is one where the result can be predicted with near certainty, such as evaluating the expression (+ 3 4) in Clojure, which will always return 7.

;; ## Probability Space

;; ### Outcomes, Sample Spaces, and Events

;; Let's consider a classic experiment occurs in most of probability books, a coin flip experiment: we flip a coin and see whether head or tail occurs. We're doing 1 experiment or 1 trial and the possible results are head or tail. In probability theory, head or tail are possible _outcomes_ of an experiment. We could use clojure keyword to symbolize the outcomes: `:head` or `:tail`.

;; Let's consider another experiment where we flip a coin twice in an experiment. The possible sequences of _outcomes_ are `[:head :tail]`, `[:tail :head]`, `[:tail :tail]`, and `[:head :head]`.

;; From a particular experiment, each `outcome` is unique. An experiment yields an `outcome` for each trial. The set of all possible outcomes of an experiment is the _sample space_ which usually denoted with S or Ω (Greek capital for Omega). So for the coin flip, the S is `#{:head :tail}` and the flip-a-coin-twice experiment, the S or Ω is `#{[:head :tail] [:tail :head] [:tail :tail] [:head :head]}`.

;; Now, we want to know whether tomorrow's maximum temperature will be 34 degrees celcius. The outcomes of the experiment depends on the range and precision of our temperature sensor device. For example, our device has temperature range from -10 to 50 degrees celcius with 0.001 precision. The sample space is `#{-10.000, -10.001, -10.002, ..... , 49.998, 49.999, 50.000}`. Compared to our coin flip experiments above, this maximum temperature experiment's sample space has more number of outcomes. In practical daily life usage, it is enough for us to know the maximum temperature in integer (ex. it's 34 degrees celcius). So, looking at the possible outcomes, we decide that we're interested to know whether tomorrow's maximum temperature is between 33.5 and 34.5 degrees celcius which is a set `#{33.500, 33.501, ..., 34.449, 34.500}`. We're interested in a subset of the outcomes.

;; In probability theory, a subset of outcomes from an experiment is called an `event`. The distinction between `outcome` and `event` is practically useful, especially in experiments with continuous or very large sample spaces.

;; Consider our maximum temperature experiment. The sample space contains all possible temperature readings (e.g., 33.501°C, 33.502°C, 33.503°C, etc.). However, in practical applications, we're often interested in broader categories rather than exact measurements. For instance, we might want to know the probability of the temperature being "around 34 degrees Celsius". We could define this as an event: "the maximum temperature is greater than or equal to 33.5°C and less than 34.5°C". This event would include all outcomes in the set `#{33.500, 33.501, ..., 34.499}`.

;; In flip-a-coin-twice experiment, we might be interested in events where only 1 :head occurs like `#{[:head :tail]}` and `#{[:tail :head]}`. The event where 2 occurs is `#{[:head :head]}`. In our single coin-toss experiment, the possible events are `#{:head}` or `#{:tail}`. Events such as `#{[:head :head]}` and `#{:head}` or `#{:tail}` are events with exactly one outcome. In probability theory, an event containing exactly one outcome is called an `elementary event`.

;; By grouping these individual outcomes into an event, we can:
;; 
;; 1. Simplify our analysis by focusing on ranges of interest rather than infinitesimal points.
;; 2. Account for measurement uncertainty or rounding in real-world applications.
;; 3. Calculate probabilities for practically meaningful scenarios.

;; In probability theory, we assign probability to `events`.

;; ### Probability Model

;; We use probability to model _random_ experiments or observations. The outcome of a _random_ experiment or observation is determined by chance and we can not predict that outcome with absolute certainty, even in principle. Examples of _random_ experiments or observations include tossing a coin, eagle sightings, tomorrow's weather, the next president of USA, _etc_.


;; A _random_ experiment yields outcome. An event is a member of possible outcomes from an experiment. We use probability model to tell us about probability of events.

;; One good way to think about probability model is a ticket-in-a-box model or "box-model", for short. This box-model replaces an observation by a box full of tickets. Each ticket has an outcome written on it. The outcome or the event written could be "head" or "tail" from tossing a coin, number of eagles seen -- it depends on what we're trying to model. All possible events are written at least once among the tickets. Some events may appear on many tickets. The tickets are thoroughly mixed in that box. We get an outcome of an experiment by picking a ticket from the box and see what's written on it.

;; Let's consider eagle sightings observation whose outcome is the number of eagles seen on a particular day. The event could 0 eagles seen, 1 eagle seen, or 7 eagles seen, etc. Using box-model, the box is the experiment and the number written on any tickets could be integers: 0, 1, 2, 3, .. etc. We then pick a ticket from the box, see the number written on the ticket that tells us the number of eagles seen for that particular day.

;; We're going to create the eagle sightings box using Clojure and fastmath library. We want to make a function `eagle-sightings` which simulates the act of picking a ticket from a box and that function returns the event or number written on a picked ticket.

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
