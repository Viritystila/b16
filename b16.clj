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

(trg :smp2 smp2
     :in-trg1 [[r 1] [r r  r 1]]
     ;[[r  [(rep 32 1)]] 1 [r r r 1] 1 ]

     [[(rep 32 1) (rep 64 r)]
      [(rep 32 1) (rep 64 r)]
      (acc [(rep 72 1) (rep 64 r)])
      r
      r
      [(rep 64 1) (rep 8 r)]
      r
      [(rep 128 1) (rep 8 r)]
           ]
      ;[ [(rep 16 1) (rep 64 r)]  [(rep 32 1) (rep 4 r)]  [(rep 128 1) (rep 16 r)] r r [(rep 128 1) (rep 8 r)] r  [(rep 64 1) (rep 4 r)]]
     ;[[(rep 4 (dcl  [(rep 16 1)]))]  [(rep 32 1) (rep 64 r)]  [(rep 128 1) (rep 16 r)] r r [(rep 128 1) (rep 8 r)] r  [(rep 128 1) (rep 4 r)] ]
                                        ;[[(rep 16 1) (rep 32 r)] r [(rep 32 1) (rep 16 r)] r]
     :in-trg2 ":in-trg1"
     :in-buf1 ["b bd1"]  ["b bd2"]  ["b bd5"]
     :in-buf2 ["b sn1"]
     :in-amp1 [1.0]
     :in-amp2 [0.27])


(stp :smp2)

(odoc synth?)

(sta)


(trg :smp smp
     :in-trg [1 1 1 1]
     :in-buf ["b bd1"])

(stp :smp)

(pp-node-tree)

(:trigger-synth (:in-trg2 (:triggers (:smp2 @trigger.trigger/synthConfig))))
