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
     ;[[(rep 16 1)] r r [(rep 8 1)]]
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

(volume! :smp2 2)

(stp :fxe)

(stp :smp2)

(odoc synth?)

(sta)

(fll 3 1)



(trg :smp smp
     :in-trg [[2 r 2 r] [2 r 2 r] [2 r 2 r] [2 2 [2 2.1 2.2 r] r]]
     [[2 2 2 2] [2 r 2 r] [2 r 2 r] [2 r 2 r]]
     :in-step ":in-trg"
      :in-buf ["b hc4" "b hc3" "b hc4" "b hc3"] )

(volume! :smp 0.125)

(trg! :smp :fxe1 trg-fx-bitcrusher
      :in-bits [8] )

(stp :fxe1)

(stp :smp)

(pp-node-tree)

(:trigger-synth (:in-trg2 (:triggers (:smp2 @trigger.trigger/synthConfig))))

(keys @trigger.samples/samplePool)

(map (fn [x] (str "n " x)) ["b1" "d3"])

(println (slw 4 (map  (fn [x] (str "n " (name x))) (map find-note-name (chd :i :e2 :melodic-minor 8))) ) )


(odoc trig)

(sta)
(trg :op overpad
     :in-trg [1 1 1 1] [0] [0] [0]
     :in-note ["n a2"] ["n c3"] ["n d3"] [[(chr :c3 :7sus4) ] r (rev [(chr :c4 :7sus4)]) r]
     :in-gate-select [0]                 ;(rep 16 [1]) (rep 16 [0])
     :in-attack [0.25]
     :in-decay  [0.2]
     :in-sustain [0.2]
     :in-release [10.75]
     :in-amp [1])


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
    (slw 4 (map  (fn [x] (str "n " (name x))) (map find-note-name (chd :i :e2 :ionian 8) )) )
(slw 2 (chr :e3 :7sus4))
     ;(trigger.algo/chd :i :g)
     :in-gate-select [1]
     :in-bow-offset [0.01]
     :in-bow-position  [0.8]
     :in-bow-slope [1]
     :in-vib-freq [0.127]
     :in-vib-gain [10.19]
     :in-amp [5])

(chd :i :e1 :ionian 8)

(println (map find-note-name (chr :c1 :7sus4)))

(stp :bow2)

;;;;;;
;;KKSKSKKAAAAAAAAAAAAAAAAWseeeeeeeee
;;;;
(do
  (trg :bow2
       bowed
       :in-trg  [(rep 16 1)]; [(rep 4 1)]  ;[(rep 4 1)]
       :in-amp [0.81]
       :in-note  ["nc1"] ["ng1"] ["nbb1"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [1]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [5])


  (trg :bow2b
       bowed
       :in-trg  [(rep 16 1)]  [(rep 4 1)] ; [(rep 64 1)] [(rep 128 1)] [(rep 264 1)] [(rep 512 1)]
       :in-amp [0.81]
       :in-note  ["nf1"] ["nbb1"] ["nc1"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [1]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [5])


  (trg :bow2c
       bowed
       :in-trg  [(rep 16 1)] [(rep 8 1)] (fll 16 [r 1]) ;[(rep 4 1)]
       :in-amp [0.81]
       :in-note   ["nf2"] ["nbb2"] ["ng2"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [1]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [5]))

(fll 16 [r 1])

(do
  (trg! :bow2 :bow2e trg-fx-echo  :in-delay-time (slw 4 [(range 0.01 0.1 0.01)]) :in-amp [0.1])

  (trg! :bow2 :bow2d trg-fx-distortion2
        :in-amount [0.7] )


  (trg! :bow2b :bow2e trg-fx-echo  :in-delay-time (slw 4 [(range 0.01 0.1 0.01)]) :in-amp [0.1])

  (trg! :bow2b :bow2d trg-fx-distortion2
        :in-amount [0.9] )


  (trg! :bow2c :bow2e trg-fx-echo  :in-delay-time (slw 4 [(range 0.01 0.1 0.01)]) :in-amp [0.11])

  (trg! :bow2c :bow2d trg-fx-distortion2
        :in-amount [0.3] ))




(trg :kick kick :in-trg ; [[1 1 1 r r r r r] 1]
      ; [(rep 16 [r 1] )]
                                        ; [[r r r 1] [r 1 1 r r r r r]]
     [(rep 16 1)]
     ;[(rep 32 1)]
     :in-f3  [ "fc1" "fg1" "f f1" "fbb1"]
      [ "fc2" "fg2" "ff2" "fbb2"]
     :in-amp [0.3])



(trg :kick2 kick :in-trg ; [[1 1 1 r r r r r] 1]
      ; [(rep 16 [r 1] )]
                                        ; [[r r r 1] [r 1 1 r r r r r]]
     [(rep 32 1)]
     (rep 2 [(rep 4 1)])
     [(rep 16 1)]
     :in-amp [0.1])


(stp :kick2)


(volume! :bow2 0.3)

(stp :bow2)



(trg :ksmp smp
     :in-trg  [r]
     :in-loop [1]
     :in-buf ["b k"]
     :in-amp [13]
     )

 (trg! :ksmp :ksmpd trg-fx-distortion2
       :in-amount (slw 8 [(range 0.1 0.9 0.05)]))


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
     :in-trg (rep 15 [r]) [r]
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
        window-cize       0.05
        sig               (pitch-shift src window-cize pitch-ratio pitch-dispersion time-dispersion)
        snd (select (in:kr in-out-select-val) [sig src])]
    (replace-out out-bus (pan2 snd))))


(trg! :ksmp :smps trg-fx-pitch-shift
      :in-pitch-ratio  [0.1 0.1 [4 4] 0.1 0.1 [4 4] 0.1 0.1]
      :in-pitch-dispersion [1]
      :in-time-dispersion [0])

(volume! :smp 3)

(stp :smps)

(odoc pitch)


(defsynth trg-fx-pitch-follow [bus-in 0
                               out-bus 0]
  (let [src                   (in bus-in)
        ;amp                    (amplitude:kr in 0.05 0.05)
        [freq has-freq]        (pitch:kr src
                                         :init-freq 800
                         :amp-threshold 0.01
                         :median 1)
        window-cize       0.05
        normfreq          (/ freq 80)
        pc                (mod normfreq 4)
        sig               (pitch-shift src window-cize pc 0.0 0.0)
        ]
    (replace-out out-bus (pan2 sig))))


(trg! :smp :smps2 trg-fx-pitch-follow )


(stp :smps2)

(odoc pitch)

(sta)
