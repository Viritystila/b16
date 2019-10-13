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

0.5625

(set-pattern-duration (/ 1 (* 2 0.5)))

(set-pattern-delay 0.99)


;;;;;;
;;Raahaaoooaaaaaaaaaaaaaaeeeeeeee0000000000!
;;;

(add-tts-sample "k"  "generalx2paradisedaqx2.txt" 200)

(trg :smp2 smp2
     :in-trg1 ;[r]
     (fst 16 [[(rep 32 1)] r [r r 1 r] [r r 1 r]])
     ;[1 r [r r 1 r] r ]
     ;[[1 r 1 r] 1 r [1 1 1 r]]
     ;[[1 1 r 1] r [r r [(rep 4 1)] r]  r ]
     ;[[1 1 r 1] r [r r 1 r]  [(rep 16 1)] ]

                                        ; [r (fll 4 [1 r])  [1 [1 1] [r r 1] [1 r 1 r]] r]

                                        ;[[(rep 16 1)] r r [(rep 8 1)]]
     ;[[(rep 16 1)] [(rep 8 1)] [[(rep 8 1)] r [(rep 8 1)] r]  [(rep 64 1)] ]
     :in-trg2  (rep 4 [r 1 r 1]) [r 1 r [(rep 8 1)]]
     [[1 1 r 1] r [r r 1 r]  [(rep 8 1)] ]
     :in-buf1  (fll 8 ["b bd3" "b sn1"  "b bd5"  "b sn2"])
     :in-buf2 ["b sn1" "b sn3" ] ["b sn9"]
     :in-step1 [2]
     :in-loop1 [0]
     :in-loop2 [0]
     :in-amp1 [1.0]
     :in-amp2 [0.127])

(trg! :smp2 :fxe trg-fx-distortion2
      :in-amount [0.96] )

(volume! :smp2 0.45)

(stp :fxe)

(stp :smp2)

(pause! :smp2)

(play! :smp2)

(sta)

(fll 3 1)


(trg :hhsmp smp
     :in-trg [1 r r  r] [1 1 1 [ 1 1 1 r]] ;[1] (rep 3 [r]) ;[[2 r 2 r] [2 r 2 r] [2 r 2 r] [2 2 [2 2.1 2.2 r] r]]
     ;[[2 2 2 2] [2 r 2 r] [2 r 2 r] [2 r 2 r]]
     :in-step  [2]; (fst 16 [(range -3 3 0.01)])
     :in-loop [0] ;(rep 3 [0])
     :in-buf ["b cc1"] )


(volume! :hhsmp 0.1)

(stp :hhsmp)

(trg! :ksmp :ksmppc trg-fx-pitch-shift :in-pitch-ratio  [0.40] ;(fst 16 [(range 0.7 3 0.01)])
      )

(stp :ksmppc)

(volume! :ksmp 5)

(stp :ksmp)

(trg :smp smp2
     :in-trg1 [1 r r  [1 1]]
     [r  [1 1] r 1]
     [1 [r 1] r [1 1]]
     [[1 r] [1 r] r [1 r]]
     :in-step1 ":in-trg1"
     :in-loop1 [0]
     :in-start-pos1 [0]
     :in-buf1   ["b bd1"]
     :in-trg2 ;(evr 8 [[(rep 16 1)] r [(rep 32 1)] [(rep 64 1)]])
     (rep 4 [r 1 r [1 1]]
          [r  [1 1] 1 r]
          [r [1 1] 1 [r 1]]
          [[r 1] [r 1] r [r 1]])
     :in-step2 ":in-trg2"
     :in-loop2 [0]
     :in-start-pos2 [0]
     :in-buf2  (fll 16 ["b sn1" "bsn2" "bsn3" "bsn4"])
     :in-amp2 [0.1])

(volume! :smp 0.5)

(trg! :smp :fxe1 trg-fx-bitcrusher
      :in-bits [4] )

(stp :fxe1)

(stp :smp)



(trg :tb303sn
     tb303
     :in-trg  (rep 4 [r 1 1 [1 1]]
          [r  [1 1] 1 r]
          [r [1 1] 1 [1 1]]
          [[1 1] [r 1] r [r 1]])
                                        ;(map-in  (rep 1 [(rep 16 1)]) scl 0.1)
     ;(rep 2  [r])
     ;(rep 1 [(rep 16 1)])
     ;(rep 2 [r])
     :in-amp [0.4]
     :in-note  (rep 1  (fll 32 ["n c2" "n c3" "n d1"]) )
     [r]
      (fll 16 ["n d1" "n c2" "n d3"])                                  ;
     [r]
     :in-gate-select [1]
     :in-attack [0.01]
     :in-decay [0.19]
     :in-sustain [0.25]
     :in-release [0.373]
     :in-r [0.09]
     :in-cutoff  [800]
     :in-wave  [0])

(volume! :tb303sn 1)


(trg! :tb303sn :tb303e trg-fx-echo :in-decay-time [0.125]  :in-delay-time [0.1] :in-amp [1])

(stp :tb303e)

(stp :tb303sn)



(trg :tom1
     tom
     :in-trg [(evr 8 [1 1 r r] (seq [(rep 4 1)]))] ; [1] [(repeat 8 1)] [1 1 1 1] (repeat 5 [r])
     :in-stick-level [2]; [(range 0.01 5 0.01)] ; (rep 13 [0.1]) [3.915]
     :in-amp [1])

(volume! :tom1 1)

(pp-node-tree)

(stp :tom1)

(sta)

[(evr 4 [1 2] (seq [(rep 8 1)]))]

(trg :op overpad
     :in-trg ;(map-in [1 1 1 1 1 1 [1 1] [1 [1 r 1 1]]] scl 0.25)
     ;(rep 2 (fll 8 (map-in [[(rep 8 1)] [(rep 4 1)]] scl 0.01)))
     ;(map-in [(rep 32 1)] scl 0.01)
     (map-in [(rep 2 [1 r r 1])] scl 0.1)
                                        ; (mapv (fn [x] (if (number? x) (* x 8) x )) [0.25 0.25 r 0.25])
     ;[0.25 (fll 4 [0.0125 0.0125]) 0.125 0.125]
     ;(fst 2 [0.25 (fll 4 [0.0125 0.0125]) 0.125 0.125])
     :in-note (fst 16 [["n e2"] ["n a3"] ["n d3"]  ["n c3"]])
     ;(rep 4 (fll 16 ["n a2" "n c3" "n d3" "n e3"]))
                                        ;(slw 2 [ [(chr :c3 :7sus4) ] r (rev [(chr :d3 :7sus4)]) r])
     :in-gate-select [0]                 ;(rep 16 [1]) (rep 16 [0])
     :in-attack [0.001]
     :in-decay  [0.51]
     :in-sustain [0.54]
     :in-release [0.63]
     :in-amp [1])

(volume! :op 0.65)

(trg! :op :ope trg-fx-echo :in-decay-time [1.125]  :in-delay-time [0.0001] :in-amp [0.05])

(stp :ope)

(stp :op)


(trg :tb303
     tb303
     :in-trg  (rep 1 [(rep 32 1)]) ;(rep 1 [r])
     :in-amp [0.4]
     :in-note  (rep 3 (fst (fll 32 ["n c2" "n c1" "n d3"])) )
       (fst (fll 32 ["n d3" "n c2" "n d3"]))                                  ;
     ;["n g2" "n c1"] ["n f2" "n f1"] ["n bb2"]
     :in-gate-select [0]
     :in-attack [0.01]
     :in-decay [0.9]
     :in-sustain [0.5]
     :in-release [0.3]
     :in-r [0.09]
     :in-cutoff [1000] [1000]
     :in-wave [0])

(volume! :tb303 3)

(stp :tb303)

;Noise guitar
(trg :bow2o
     bowed
     :in-trg (map-in [[1 1 r 1] [1 1] [1 1] [1 r 1 1] ] scl 0.06125 ) (map-in [(rep 8 1 )] scl (/ 1 8))  (rep 2 [r])
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

(volume! :bow2o 0.3)

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
       :in-trg  (map-in [(rep 16 1)] scl (/ 1 16)) ;[(rep 4 1)]  ;[(rep 4 1)]
       :in-amp [0.81]
       :in-note  ["nc1"] ["ng1"] ["nc1"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [0.5]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [1])


  (trg :bow2b
       bowed
       :in-trg  (map-in [(rep 16 1)] scl (/ 1 16))
       (map-in  [(rep 4 1)] scl (/ 1 16))
       (map-in [(fll 32 [r r 1 1])] scl (/ 1 16)) ; [(rep 128 1)] [(rep 264 1)] [(rep 512 1)]
       :in-amp [0.81]
       :in-note  ["nf1"] ["nbb1"] ["nc1"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [0.5]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [1])


  (trg :bow2c
       bowed
       :in-trg  (map-in [(rep 16 1)] scl (/ 1 16))
       (map-in [(rep 8 1)] scl (/ 1 16))
       (map-in (fll 16 [r 1]) scl (/ 1 16))   ;[(rep 4 1)]
       :in-amp [0.81]
       :in-note   ["nbb2"] ["nbb2"] ["ng2"]
       :in-gate-select [1]
       :in-bow-offset [0.01]
       :in-bow-position  [0.8]
       :in-bow-slope [0.5]
       :in-vib-freq [0.127]
       :in-vib-gain [0.19]
       :in-amp [1]))

(do
  (trg! :bow2 :bow2e trg-fx-echo :in-delay-time (slw 4 [(range 0.001 0.1 0.001)]) :in-amp [0.2])

  (trg! :bow2 :bow2d trg-fx-distortion2
        :in-amount [0.095] )


  (trg! :bow2b :bow2be trg-fx-echo :in-delay-time (slw 4 [(range 0.001 0.1 0.001)]) :in-amp [0.2])

  (trg! :bow2b :bow2bd trg-fx-distortion2
        :in-amount [0.09] )


  (trg! :bow2c :bow2ce trg-fx-echo :in-delay-time (slw 4 [(range 0.001 0.1 0.001)]) :in-amp [0.2])

  (trg! :bow2c :bow2cd trg-fx-distortion2
        :in-amount [0.067] )

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

(lss)

(sta)

(trg :gb
     grunge-bass
     :in-trg  (fst 32 ["nc4"]) ;[r "ng3"] (fst 64 ["ng2"]) [r "nf3"] (fst 32 ["nf2"]) [r "nbb3"] (fst 32 ["nbb2"]) [r "nc3"]
      ;(fst 4 ["nc3" "nbb2" "ng2" "nf2"])
     :in-gate-select  [1]
     :in-amp [1]
     :in-note  ":in-trg"
     :in-a [0.01]
     :in-d [0.293]
     :in-s [0.295]
     :in-r [0.5]; (slw 32 [(range 0.1 1 0.01)])
     )

(trg! :gb :gbd trg-fx-distortion2
        :in-amount [0.95] )

(stp :gbd)

(stp :gbe)

(volume! :gb 1)

(pause! :gb)

(play! :gb)

(stp :gb)

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

(trg :csl
     cs80lead
     :in-trg  [(rep 4 1)]
     :in-amp [1]
     :in-freq ["fc2"] [r "fg3"] ["fg2"] [r "ff3"] ["ff2"] [r "fbb3"] ["fbb2"] [r "fc3"]  )

(volume! :csl 4)

(trg! :csl :csle trg-fx-echo
      :in-delay-time [0.02] :in-decay-time [1] :in-amp [0.5])

(stp :csl)

(lss)

(trg :ks1
     ks1
     :in-trg (rep 3 [r]) [(rep 32 1)]
     :in-dur [1]
     :in-amp [5]
     :in-note (rep 3 [(chord-degree :i :d4 :melodic-minor)])  [(chord-degree :i :d4 :melodic-major)]  (rep 3 [(chord-degree :i :d3 :ionian)])  [(rev (chord-degree :i :d4 :melodic-minor))]
     :in-decay [(range 0.01 1 0.01)]
     :in-coef [(range 0.01 0.9 0.01)] )


(stp :ks1)

(trg :hz haziti-clap :in-trg ; [[1 1 1 r r r r r] 1]
      ; [(rep 16 [r 1] )]
                                        ; [[r r r 1] [r 1 1 r r r r r]]
     (sfl (fll 32 [ r 1  r 1]))
     ;(rep 2 [(rep 8 1)])
                                        ;[(rep 16 1)]
     :in-freq  ["fc3"] ["fg3"] ["ff3"] ["fbb3"]
     :in-amp [1])

(volume! :hz 1)

(stp :hz)

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


(add-sample "ee" (string-to-buffer "EEEE"))


(add-sample "aa" (string-to-buffer "AAAAAAAAAA"))


(add-sample "oo" (string-to-buffer "OOOOOOOOOO"))

(add-sample "uhea" (string-to-buffer "UHHHHHHHHEEEEEEEAAAAAAA"))






(trg :uhsmp smp
     :in-trg [1] [r]
                                        ;[1 r 1  [(rep 7 1) r (rep 7 1) r]]
     ;(rep 3 [1 r 1 r])
     ;[1 r 1 [1 1 1 1] 1 r 1 1 1 1 (acc [(rep 8 1)]) 1 1 1 1 [1 1 1 1]]
     ;(acc [(rep 8 1)])
     :in-loop [0]
     :in-buf ["b uhea"] ;["b uh"] ["b oo"] (fst 8 ["b aa" "b ee"])
     :in-amp [2]
     :in-step (fst 1 [(range 1.25 2.75 0.25)])
     )

 (trg! :uhsmp :ksmpd trg-fx-distortion2
       :in-amount (slw 8 [(range 0.1 0.95 0.05)]))

(volume! :uhsmp 0.5)


(stp :uhsmp)

(slw 2 (chr :e3 :7sus4))

(sta)

(stp :uhsmp)

(trg :tick ping :in-trg [(rep 60 1)] :in-amp [0])

(stp :tick)


(lss)

(sta)

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
                                        ;(slw 2 [(evr 3 "b bass23" (partition 1 (sfl (fll 8 [["b bd2"] (rep 3 ["b sn3"]) (rep 4 [r])]))))])
     (rep 2 [r])
     [[r r "b bd1" "bsn1"] [(rep 8 "b bd2") r] [(rep 4 "b bd2") (rep 16 1)] ]
     [(acc [r r "b bd1" "bsn1"]) (dcl [(rep 8 "b bd2") r]) [(rep 4 "b bd2") (rep 16 1)] ]
     ;[["b bd2" r "b bd2" "b sn3"] [ r  "b bd2"  [(rep 4 "b sn3")] "b sn3"]  [(rep 8 "b bd2")] "b sn2" ]
     :in-buf ":in-trg"
     :in-loop [0]
     :in-start-pos [0]
     :in-step [2.0]
     :in-amp [1])



(trg :samplDrum smp
     :in-trg
     (rep 2 ["b bd1"  "b sn1" [r r "b bd1" r] [ "b sn2" r "b bd1" r]] )
     ["b bd1"   [(rep 8 "b sn2")] [ "b bd2" r "b bd1" r] [ "b sn1" r "b bd1" r]]
     ["b bd1"  "b sn2" [ "b bd2" r "b bd1" r] [ "b sn1" r "b bd1" r]]
    [(evr 3 "b bass23" (partition 1 (sfl (fll 16 [["b bd2"] ["b sn3"] [r]]))))]
     :in-buf ":in-trg"
     :in-loop [0]
     :in-start-pos [0]
     :in-step [2.0]
     :in-amp [1])

(volume! :samplDrum 2)

(trg! :samplDrum :sde trg-fx-echo :in-delay-time [(/ 1  0.5625)] :in-decay-time [0.05] :in-amp [0.0])

(stp :sde)

(stp :samplDrum)

(sta)


(trg :gb2
     vintage-bass
     :in-trg (map-in [1 1 [r 1] [(rep 6 1)]] scl 0.2) (rep 3 [r])
     ;[(rep 32 1) r 1 (rep 8 1)]
     ; [1 [(rep 16 1)] r [1 1 1 1]]
    ;[(evr 4 1 (partition 1 (sfl (fll 16 [[1] [1] [r]]))))]
     :in-gate-select  [1]
     :in-amp [1]
     :in-note    ["nc3" "nc3" "nd3" "ne3"]
     ;(rep 1 [(rep 16 "ng2")])
     ;(rep 1 [(rep 16 "nbb2")])
     ;(rep 1 [(rep 16 "nf3")])
     :in-a [0.0125]
     :in-d [0.3]
     :in-s [0.195]
     :in-r [0.375]; (slw 32 [(range 0.1 1 0.01)])
     )


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


(volume! :gb2 2)


 (trg! :gb2 :gb2d trg-fx-distortion2
       :in-amount [0.99])


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
     :in-gate-select  [0]
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

(volume! :gb2 2)


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
