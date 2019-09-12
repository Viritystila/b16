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

(add-tts-sample "k"  "generalx2paradisedaqx2.txt" 200)

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
     (rep 3 [r])
     ;[[1 1 1 1] [1 r 1 r] [1 r 1 r] [1 r 1 r]]
     ; [1 r [r r 1 r] [r r 1 r]]
     ; (fst 8 [1 r [r r 1 r] r ])
     ;[[1 r 1 r] 1 r [1 1 1 r]]
     ;(fst 2  [[1 1 1 1] r [r r 1 r]  r ])
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

(defsynth ss [freq 400] (out 0 (pan2 (sin-osc freq))))

(def ssf (ss))

(node-pause (:id ssf))

(node-start (:id ssf))



(node-pause 233)

(node-start 233)


(:instance-fn (to-id ssf))

(active-synths ssf)

(:id ssf)

(pp-node-tree)

(kill ssf)



(trg :smp smp
     :in-trg (rep 15 [r]) [1]
     :in-buf ["b k"]
     :in-loop [1]
     :in-step [2]
     )


(defsynth trg-fx-pitch-shift
  "A pitch shifter"
  [bus-in 0
   in-pitch-ratio 1.0
   in-pitch-dispersion 0.0
   in-time-dispersion 0.0
   in-pitch-ratio-val 1.0
   in-pitch-dispersion-val 0.0
   in-time-dispersion-val 0.1
   in-out-select 0
   in-out-select-val 0
   out-bus 0]
  (let [src               (in bus-in)
        pitch-ratio       (in:kr in-pitch-ratio-val)
        pitch-dispersion  (in:kr in-pitch-dispersion-val)
        time-dispersion   (in:kr in-time-dispersion-val)
        window-cize       0.1
        sig               (pitch-shift src window-cize pitch-ratio pitch-dispersion time-dispersion)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))


(trg! :smp :smps trg-fx-pitch-shift
      :in-pitch-ratio  [4]
      :in-pitch-dispersion [0]
      :in-time-dispersion [0])

(volume! :smp 3)

(stp :smp)

(odoc pitch)


(defsynth trg-fx-pitch-follow [bus-in 0
                               out-bus 0]
  (let [src                   (in bus-in)
        ;amp                    (amplitude:kr in 0.05 0.05)
        [freq has-freq]        (pitch src
                         :amp-threshold 0.01
                         :median 1)
        out-1 (comb-c:ar (lpf:ar in 1000)
                         0.1
                         (mul-add (distort (reciprocal (mul-add freq 2 1))) 0.05 1))]
    (replace-out out-bus (pan2 out-1))))


(trg! :smp :smps trg-fx-pitch-follow )


(stp :smps)

(odoc pitch)
