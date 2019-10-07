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
     [[1 1 r 1] r [r r 1 r]  r ]
     ;[[1 1 r 1] r [r r 1 r]  [(rep 8 1)] ]
      [r (fll 4 [1 r])  [1 [1 1] [r r 1] [1 r 1 r]] r]
     ;[[(rep 16 1)] r r [(rep 8 1)]]
     ;[[(rep 16 1)] [(rep 8 1)] [[(rep 8 1)] r [(rep 8 1)] r]  [(rep 64 1)] ]
     :in-trg2  [r 1 r 1]
     :in-buf1  ["b bd1"]          ;(fll 16 ["b bd3" "b sn1"  "b bd5"  "b sn2"])
     :in-buf2 ["b sn1" "b sn3" ] ["b sn9"]
     :in-step1 [2]
     :in-loop1 [0]
     :in-amp1 [1.0]
     :in-amp2 [0.27])

(trg! :smp2 :fxe trg-fx-distortion2
      :in-amount [0.94] )

(volume! :smp2 0.15)

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

(volume! :smp 0.075)

(trg! :smp :fxe1 trg-fx-bitcrusher
      :in-bits [8] )

(stp :fxe1)

(stp :smp)

(pp-node-tree)

(:trigger-synth (:in-trg2 (:triggers (:smp2 @trigger.trigger/synthConfig))))

(keys @trigger.samples/samplePool)

(map (fn [x] (str "n " x)) ["b1" "d3"])

(println (slw 4 (map  (fn [x] (str "n " (name x))) (map find-note-name (chd :i :e2 :melodic-minor 8))) ) )


(sta)

(trg :op overpad
     :in-trg [1]
                                        ; (mapv (fn [x] (if (number? x) (* x 8) x )) [0.25 0.25 r 0.25])
     ;[0.25 (fll 4 [0.0125 0.0125]) 0.125 0.125]
     ;(fst 2 [0.25 (fll 4 [0.0125 0.0125]) 0.125 0.125])
     :in-note ["n a2"] ["n c3"] ["n d3"]   (slw 2 [ [(chr :c3 :7sus4) ] r (rev [(chr :d3 :7sus4)]) r])
     :in-gate-select [0]                 ;(rep 16 [1]) (rep 16 [0])
     :in-attack [0.001]
     :in-decay  [1.0001]
     :in-sustain [1.4]
     :in-release [10.3]
     :in-amp [1])

(volume! :op 0.25)


;Noise guitar
(trg :bow2o
     bowed
     :in-trg  (map-in [[1 1 r 1] [1 1] [1 1] [1 r 1 1] ] scl 0.06125 ) (map-in [(rep 8 1 )] scl (/ 1 8))
     :in-amp [0.81]
     :in-note  ["n c0" "n c1"]  ["n g0" "n c1"] ["n f0" "n f1"] ["n bb0"]
      ["n c1"]  ["n g1" "n f0"] ["n f1"] ["n bb1" "n f0"]
     :in-gate-select [0]
     :in-bow-offset [0.1]
     :in-bow-position  [1.18]
     :in-bow-slope [1]
     :in-vib-freq [0.127]
     :in-vib-gain [0.0019]
     :in-amp [1])




(trg! :bow2o :bow2oe trg-fx-echo :in-decay-time [0.125]  :in-delay-time [0.1] :in-amp [0.2])

(trg! :bow2o :bow2od trg-fx-distortion2
        :in-amount [0.99999] )

(volume! :bow2o 0.05)

(chd :i :e1 :ionian 8)

(println (map find-note-name (chr :c1 :7sus4)))

(stp :bow2o)

(sta)
;;;;;;


;;;;;;
;;KKSKSKKAAAAAAAAAAAAAAAAWseeeeeeeee
;;;;
(do
  (trg :bow2
       bowed
       :in-trg  [(rep 16 1)] ;[(rep 4 1)]  ;[(rep 4 1)]
       :in-amp [0.81]
       :in-note  ["nc1"] ["ng1"] ["nc1"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [1]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [5])


  (trg :bow2b
       bowed
       :in-trg  [(rep 16 1)] [(rep 4 1)]  [(fll 32 [r r 1 1])] ; [(rep 128 1)] [(rep 264 1)] [(rep 512 1)]
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
       :in-note   ["nbb2"] ["nbb2"] ["ng2"]
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
        :in-amount [0.95] )


  (trg! :bow2b :bow2be trg-fx-echo  :in-delay-time (slw 4 [(range 0.01 0.1 0.01)]) :in-amp [0.1])

  (trg! :bow2b :bow2bd trg-fx-distortion2
        :in-amount [0.9] )


  (trg! :bow2c :bow2ce trg-fx-echo  :in-delay-time (slw 4 [(range 0.01 0.1 0.01)]) :in-amp [0.11])

  (trg! :bow2c :bow2cd trg-fx-distortion2
        :in-amount [0.67] )

  )

(sta)
(do
  (stp :bow2)
  (stp :bow2b)
  (stp :bow2c))


(trg :gb
     grunge-bass
     :in-trg
     (rep 1 [(rep 16 "nc2")])
     (rep 1 [(rep 16 "ng2")])
     (rep 1 [(rep 16 "nbb2")])
     (rep 1 [(rep 16 "nf2")])
     :in-gate-select  [1]
     :in-amp [1]
     :in-note  ":in-trg"
     :in-a [0.001]
     :in-d [0.93]
     :in-s [0.95]
     :in-r [0.25]; (slw 32 [(range 0.1 1 0.01)])
     )

(trg! :gb :gbd trg-fx-distortion2
        :in-amount [0.95] )

(stp :gbd)

(trg! :gb :gbe trg-fx-echo
      :in-delay-time [0.2] :in-amp [0.5])

(stp :gb)

(volume! :gb 1)


(trg :kick kick :in-trg    (rep 7 [(rep 16 1 )])
     [1 r 1 [1 1] 1 r 1 1 1 1 (acc [(rep 8 1)]) 1 1 1 1 [1 1 1 1]]
     :in-f3  [ "fc1" "fg1" "f f1" "fbb1"]
     [ "fc2" "fg2" "ff2" "fbb2"]
     [[ "fc3" "fg3" "ff3" "fbb3"]
      [ "fc3" "fg3" "ff3" "fbb3"]
      [ "fc4" "fg3" "ff2" "fbb1"]]
     :in-amp [0.3])



(volume! :kick 0.25)

(stp :kick)

(acc 2 [1 2 3])
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


(add-sample "uh" (string-to-buffer "UHHHHHHHHAAAAAAAAAAAA"))

(trg :ksmp smp
     :in-trg   (rep 3 [1 r 1 r])
     [1 r 1 [1 1 1 1] 1 r 1 1 1 1 (acc [(rep 8 1)]) 1 1 1 1 [1 1 1 1]]
     ;(acc [(rep 8 1)])
     :in-loop [1]
     :in-buf ["b uh"]
     :in-amp [2]
     :in-step (fst 128 [(range 1.5 2.5 0.25)])
     )

 (trg! :ksmp :ksmpd trg-fx-distortion2
       :in-amount (slw 8 [(range 0.1 0.95 0.05)]))

(volume! :ksmp 0.5)

(slw 2 (chr :e3 :7sus4))

(sta)

(stp :ksmp)

(trg :tick ping :in-trg [(rep 60 1)] :in-amp [0])

(stp :tick)

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

;;;;;;;,
                                        ;,;;;;bddddddrrrrrrrrmmsss

(trg :samplDrum smp
     :in-trg
     (rep 3 ["b bd1"  "b sn2" [r r "b bd1" r] [ "b sn2" r "b bd1" r]])
    ;(rep 2 [["b bd1" "b sn2"]  [r ["b bd2" "b sn2"]]])
    [(evr 3 "b bass23" (partition 2 (sfl (fll 16 [["b bd2"] ["b sn3"] [r]]))))]
   ; (rep 1 [["b bd1" "b sn2"]  [r ["b bd2" "b sn2"]]])
    ;; (rep 3 [["b bd1" "b sn2"]  [r ["b bd2" ["b sn2" "b sn2"]]]])
    ;; [(evr 3 "b bass23" (partition 1 (sfl (fll 8 [["b bd2"] ["b sn3"] [r]]))))]


    ;; (rep 3 [["b bd1" "b sn2"] [r ["b bd2" [(rep 1 "b sn2")]]]])
    ;; [(evr 5 "b bass23" (partition 1 (sfl (fll 16 [["b bd2"] ["b sn2"] [r]]))))]

    ;; (rep 3 [["b bd1" "b sn2"] [r ["b bd2" ["b bd2" "b sn2"]]]])
    ;; [(evr 5 "b bass24" (partition 1 (sfl (fll 16 [["b bd4"] ["b sn3"] [r]]))))]

    ;; (rep 3 [["b bd1" "b sn2"] [r ["b bd2" [(rep 1 "b sn2")]]]])
    ;; [(evr 4 "b bass23" (partition 1 (sfl (fll 8 [["b bd2"] ["b sn2"] [r]]))))]

    ;; (rep 3 [["b bd1" "b sn2"] [r ["b bd2" ["b bd2" "b sn2"]]]])
    ;; [(evr 1 "b bass24" (partition 1 (sfl (fll 8 [["b bd4"] ["b sn3"] [r]]))))]

     :in-buf ":in-trg"
     :in-loop [0]
     :in-start-pos [0]
     :in-step [2.0]
     :in-amp [1])

(volume! :samplDrum 0.25)

(trg! :samplDrum :sde trg-fx-echo :in-delay-time [0.05] :in-decay-time [0.005] :in-amp [0.1])

(stp :samplDrum)

(defn isno [x] (if (number? x) [x]  (apply conj x)))


(defn map-inner [input fnc & args]
  (let [;_ (println "input" input)
        ;_ (println "fnc" fnc)
        ;args  (flatten (conj [input] args))
        ;_ (println "args" args)
        input   (vec input)
        ] (loop [xv      input
                 result  []]
            (if xv
              (let [fst    (first xv)
                    targs  ()]
                (if (or (number? fst) (string? fst))
                  (if (number? fst)
                    (recur (next xv) (conj result (apply fnc (flatten (conj [fst] args)))))
                    (recur (next xv) (conj result fst)))
                  (recur (next xv) (conj result (apply map-inner (seq [fst fnc args]))) )) ) result))))


(defn map-in [input fnc & args]
  (let [input (piv input)]
    (apply map-inner (seq [input fnc args]))))

(piv  [1 1 [r 0.125] 1])

(defn scl [scale_value x]  (if (number? x) (+ 0  (* x scale_value)) x ) )


(defn scls [scale_value b x]  (if (number? x) (+ b  (* x scale_value)) x ) )

(apply scls (flatten (concat (seq [2 2]) [4])) )

 (concat (seq [1 2]) [[4 5 3 4]])

(apply scls '( [1 2 [1 2 3 4]]))

(conj [1 1 2 3] 9)

(map-in [5 2 3 [9 8] r [(rep 4 22)] ] scls 2 1)

(apply scls '(1 2 3))

(trg :gb2
     vintage-bass
     :in-trg (map-in [1 1 [r 1] [(rep 6 1)]] scl 0.05)
    (map-in [1 [1 1] [1 1 1 1] 1] scl 0.05)
      (map-in  [[1 1 r 1] 1 [1 r 1  1] 1] scl 0.05)
     (map-in [1 [1 [1 1]] r [1 [1 1]]] scl 0.05)
     ;[(rep 32 1) r 1 (rep 8 1)]
     ; [1 [(rep 16 1)] r [1 1 1 1]]
    ;[(evr 4 1 (partition 1 (sfl (fll 16 [[1] [1] [r]]))))]
     :in-gate-select  [0]
     :in-amp [1]
     :in-note    ["nc3" "nc3" "nd3" "ne3"]
     ["nc3" "nd3" ["ne3" "ne3"] "nd2"]
     ;(rep 1 [(rep 16 "ng2")])
     ;(rep 1 [(rep 16 "nbb2")])
     ;(rep 1 [(rep 16 "nf3")])
     :in-a [0.125]
     :in-d [0.3]
     :in-s [0.95]
     :in-r [0.275]; (slw 32 [(range 0.1 1 0.01)])
     )

 (scl 0.25 [1 [1 1] r r])

(volume! :gb2 0.25)


 (trg! :gb2 :gb2d trg-fx-distortion2
       :in-amount [0.5])


(stp :gb2d)

(stp :gb2)

(trg :kei kick :in-trg [r 1 [r r r 1] 1])

(stp :kei)
(sta)

;;;;;;;;;;;;;;;,
;;;;;;;;;;;;;;
;;;;;;;;;;;;;;

;good bass


(trg :gb2
     vintage-bass
     :in-trg [[(rep 4 1)] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
      [[(rep 4 1)] 1 1 1 [1 1 1 1 r r r r]  [1 1 1 1 r r r r]  [1 1 1 1 r r r r]  [1 1 1 1 r r r r] 1 1 1 1 1 1 1 [(rep 8 1 )]]
     :in-gate-select  [1]
     :in-amp [1]
     :in-note    (rep 1 [(rep 16 "nc2")])
     (rep 1 [(rep 16 "ng2")])
     (rep 1 [(rep 16 "nbb2")])
     (rep 1 [(rep 16 "nf2")])
     :in-a [0.001]
     :in-d [0.93]
     :in-s [0.95]
     :in-r [0.25]; (slw 32 [(range 0.1 1 0.01)])
     )



;;;;
;;;;;;
(t/start "./b16.glsl" :width 1920 :height 1080 :cams [0] :videos ["../videos/soviet1.mp4" "../videos/uni_fixed.mp4" "../videos/soviet4.mp4" "../videos/spede_fixed.mp4"])

(t/bufferSection 0 0 16925)

(t/set-video-fixed 0 :fw)

(t/bufferSection 1 0 6460)

(t/set-video-fixed 1 :fw)

(def abm (audio-bus-monitor (get-out-bus :samplDrum)))

@abm

(on-trigger (get-trigger-id :tick :in-trg) (fn [val]
                                             (let [obv  @abm]
                                               ;(println obv)
                                               (t/set-dataArray-item 0 obv)))
            :samplDrum_obv)

(remove-event-handler :smp2_obv)

(pause! :smp2)

(play! :smp2)

(def opabm (audio-bus-monitor (get-out-bus :op)))



(on-trigger (get-trigger-id :tick :in-trg) (fn [val]
                                             (let [obv  @opabm]
                                               ;(println obv)
                                               (t/set-dataArray-item 1 obv)))
            :op_obv)

(pause! :op)

(play! :op)





(t/toggle-recording "/dev/video1")
