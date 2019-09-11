(ns b16
  (:use [trigger.trigger]
        [trigger.synths]
        [trigger.algo]
        [trigger.speech]
        [trigger.samples]
        [trigger.trg_fx] [overtone.core]) (:require [viritystone.tone :as t]))

(future
  (println "Begin loading SuperDirt samples")
  (load-all-SuperDirt-samples)
  (println "Samples loaded"))

(defn add-tts-sample [name path nosamples]
  (future
    (println "Begin loading sample " name)
    (add-sample name (string-to-buffer (generate-markov-text path nosamples)))
    (println "Sample" name "loaded") ))


;;;;;;
;;Raahaaoooaaaaaaaaaaaaaaeeeeeeee0000000000!
;;;
(trg :smp2 smp2
     :in-trg1 [1 r [r r 1 r] [r r 1 r]]
     [1 r [r r 1 r] r ]
     [[1 r 1 r] 1 r [1 1 1 r]]
     [[1 1 1 1] r [r r 1 r]  r ]
     ;[[(rep 16 1)] [(rep 8 1)] [[(rep 8 1)] r [(rep 8 1)] r]  [(rep 64 1)] ]
     :in-trg2  [r 1 r 1]
     :in-buf1  ["b bd1"]          ;(fll 16 ["b bd3" "b sn1"  "b bd5"  "b sn2"])
     :in-buf2 ["b sn1"] ["b sn9"]
     :in-step1 [2]
     :in-loop1 [0]
     :in-amp1 [1.0]
     :in-amp2 [0.27])

(trg! :smp2 :fxe trg-fx-distortion2
      :in-amount [0.99] )

(volume! :smp2 1)

(stp :fxe)

(stp :smp2)

(odoc synth?)

(sta)

(fll 3 1)

(trg :smp smp
     :in-trg [[1 r 1 r] [1 r 1 r] [1 r 1 r] [1 1 [1 1 1 r] r]]
     [[1 1 1 1] [1 r 1 r] [1 r 1 r] [1 r 1 r]]
      :in-buf ["b hc4" "b hc3" "b hc4" "b hc3"] )

(volume! :smp 0.125)

(trg! :smp :fxe1 trg-fx-distortion2
      :in-amount [0.95] )


(stp :smp)

(pp-node-tree)

(:trigger-synth (:in-trg2 (:triggers (:smp2 @trigger.trigger/synthConfig))))

(keys @trigger.samples/samplePool)

(map (fn [x] (str "n " x)) ["b1" "d3"])

(println (slw 4 (map  (fn [x] (str "n " (name x))) (map find-note-name (chd :i :e2 :melodic-minor 8))) ) )

(trg :bow2
     bowed
     :in-trg   (fst 2 [[1 r 1 r] [1 r 1 r] [1 r 1 r] [1 1 [1 1 1 r] r]])
     [[1 1 1 1] [1 r 1 r] [1 r 1 r] [1 r 1 r]]
      [1 r [r r 1 r] [r r 1 r]]
      (fst 8 [1 r [r r 1 r] r ])
     [[1 r 1 r] 1 r [1 1 1 r]]
     (fst 2  [[1 1 1 1] r [r r 1 r]  r ])
     :in-amp [0.81]
     :in-note  ["n a2"]
    ["n a2" "n c2"]
    ["n b2"]
    ["n b2" "n d2"]
    ["n a3"]
    ["n a3" "n f2"]
    ["n e2"]
    ["n e2" "n b2"]
    (slw 4 (map  (fn [x] (str "n " (name x))) (map find-note-name (chd :i :e2 :ionian 8))) )
(slw 2 (chr :e3 :7sus4))
     ;(trigger.algo/chd :i :g)
     :in-gate-select [1]
     :in-bow-offset [0.01]
     :in-bow-position  [0.8]
     :in-bow-slope [1]
     :in-vib-freq [0.127]
     :in-vib-gain [10.19]
     :in-amp [5])

(trg! :bow2 :bow2e trg-fx-echo  :in-delay-time (slw 4 [(range 0.01 0.1 0.01)]) :in-amp [1])

(trg! :bow2 :bow2d trg-fx-distortion2
      :in-amount [0.9] )


(stp :bow2)

(slw 2 (chr :e3 :7sus4))

(sta)
