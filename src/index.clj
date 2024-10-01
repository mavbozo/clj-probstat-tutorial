^:kindly/hide-code
(ns index
  (:require [fastmath.random :as r]
            [scicloj.kindly.v4.kind :as kind]))

;; This tutorial assumes some familiarity with Clojure and with high-school maths (algebra, probability, statistics, _etc_.).

;; We use Clojure and [fastmath](https://github.com/generateme/fastmath) version 3 in this tutorial.

;; **Note**: this is still a work in progress. I'd love to receive your feedbacks or suggestions.

;; # Probability

;; We use probability to model _random_ experiments or observations. The outcome of a _random_ experiment or observation is determined by chance and we can not predict that outcome with absolute certainty, even in principle. Examples of _random_ experiments or observations include tossing a coin, eagle sightings, tomorrow's weather, the next president of USA, _etc_.

;; ## Probability Model

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

;; ## Probability

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
