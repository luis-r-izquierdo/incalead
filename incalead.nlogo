;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; InCaLead
;; InCaLead (Innovation, Catch-up and Leadership in Science-Based Industries) is
;; an evolutionary model of industrial catch-up
;; which incorporates industrial scientists' training and migration,
;; endogenous R&D decisions and the possibility of funding capital
;; accumulation through debt.
;; Copyright (C) 2010 Isabel Almudi, Francisco Fatas & Luis R. Izquierdo
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es


breed [firms firm]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals [

  num-firms

  time
  time-increment

  time-catch-up
  emergent-firm
  time-convergence

  list-of-firms
  max-profit-firm
  avg-competitiveness

  firms-numbers

  plotting-ticks
]


firms-own [

  capital
  performance
  r&d-over-profit

  production
  cost

  market-share
  price

  profit

  competitiveness

  r&d-expenditure
  r&d-productivity

  debt
  financial-needs-indicator

  init-tech-frontier
  tech-frontier
  exp-rate-tech-frontier

  attractiveness
  immigration-ratio
  working-scientists
  working-scientists-share

  trained-scientists
  increase-in-trained-scientists

  scientists-wages
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to startup
  clear-all
  set num-firms 3
  setup-firms
  setup-variables
  my-setup-plots
  reset-ticks
end

to setup-firms

  create-ordered-firms num-firms

  set list-of-firms (sort firms)

  assign-values-to-firms "capital"                        read-from-string initial-capital
  assign-values-to-firms "performance"                    read-from-string initial-performance
  assign-values-to-firms "init-tech-frontier"             read-from-string initial-tech-frontier
  assign-values-to-firms "exp-rate-tech-frontier"         read-from-string expansion-rate-tech-frontier
  assign-values-to-firms "immigration-ratio"              read-from-string initial-immigration-ratio
  assign-values-to-firms "trained-scientists"             read-from-string initial-budget
  assign-values-to-firms "increase-in-trained-scientists" read-from-string budget-growth-increase
  assign-values-to-firms "scientists-wages"               read-from-string initial-scientists-wages
  assign-values-to-firms "r&d-over-profit"                read-from-string initial-r&d-over-profit
  assign-values-to-firms "debt"                           read-from-string initial-debt

  do-initial-working-scientists

end

to do-initial-working-scientists
  let total-capital (sum [capital] of firms)
  ask firms [
    set working-scientists
    (r&d-over-profit * (capital ^ 2) * (1 + (interest-rate + debt-amortization-rate) * (debt / capital)))
    / (total-capital * capital-productivity * scientists-wages)
  ]
end

to setup-variables
  set time-increment 0.0009765625

  set time-catch-up "N/A"
  set emergent-firm min-one-of firms [capital]
    ;; note that if there is more than one firm with minimum capital, one will be taken at random

  set time-convergence "N/A"
end

to my-setup-plots
  set firms-numbers (n-values (count firms) [?])

  setup-plot "Market Share"
  setup-plot "R&D Expenditure"
  setup-plot "Scientists wages"
  setup-plot "Share of total scientists"
  setup-plot "Debt to Capital Ratio"
  setup-plot "Immigration share"
end


to setup-plot [name]
  let i 0

  set-current-plot name
  set-plot-y-range 0 0.01
  foreach firms-numbers [
    create-temporary-plot-pen (word ?)
    set-plot-pen-mode 2
    set-plot-pen-color 25 + 40 * i
    set i (i + 1)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN-TIME PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;; Main procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;

to go

  ;; demand-supply subsystem

  ask firms [update-production]
  do-market-shares
  ask firms [update-cost]
  ask firms [update-price]
  do-profits

  ask firms [update-r&d-expenditure]
  do-competitiveness

  do-financial-needs-indicators

  ;; universities subsystem

  ask firms [update-tech-frontier]
  ask firms [update-r&d-productivity]
  ask firms [update-scientists-wages]
  do-working-scientists-share

  ;; check times

  check-times

  ;; do plots

  if int time >= plotting-ticks [
    set plotting-ticks (plotting-ticks + 1)
    do-plots display no-display
  ]

  ;; advance time

  tick
  set time (time-increment * ticks)

  ;; update state variables

  ask firms [update-debt]
  do-capitals
  do-r&d-over-profits

  do-performances
  do-working-scientists
  ask firms [update-trained-scientists]
  do-immigration-ratios

end


;; DEMAND-SUPPLY SUBSYSTEM

to update-production
  set production (capital-productivity * capital)
end

to do-market-shares
  let total-production (sum [production] of firms)
  ask firms [
    set market-share (production / total-production)
    if (market-share < 0) or (market-share > 1) [
      user-message (word "The model has produced a market share outside the interval [0, 1]. Please re-run it with a lower time-increment.")
      stop
    ]
  ]
end

to update-cost
  set cost (1 + (interest-rate + debt-amortization-rate) * (debt / capital)) / capital-productivity
end

to update-price
  set price (1 + market-share) * cost
end

to do-profits
  ask firms [set profit (market-share * cost)]
  let firms-with-max-profit (firms with-max [profit])

  set max-profit-firm one-of firms-with-max-profit
    ;; note that if there's a tie, one of the firms
    ;; with the maximum profit will be chosen at random
end

to update-r&d-expenditure
 set r&d-expenditure (r&d-over-profit * profit * production)
end

to do-competitiveness
  let avg-performance mean ([performance] of firms)
  let avg-price mean ([price] of firms)

  ask firms [
    set competitiveness  (
      (1 - price/performance-sensitivity) * ((performance - avg-performance) / avg-performance)
      - (price/performance-sensitivity * ((price - avg-price) / avg-price))
      )
  ]

  set avg-competitiveness sum [market-share * competitiveness] of firms
end

to do-financial-needs-indicators
  ask firms [
    set financial-needs-indicator (demand-growth-rate + competitiveness - avg-competitiveness) / ((1 - r&d-over-profit) * profit * capital-productivity)
  ]
end


;; UNIVERSITIES SUBSYSTEM

to update-tech-frontier
  set tech-frontier (init-tech-frontier * exp (exp-rate-tech-frontier * time))
end


to update-r&d-productivity
  set r&d-productivity ((tech-frontier - performance) / performance)
end


to update-scientists-wages
  set scientists-wages (r&d-expenditure / working-scientists)
end

to do-working-scientists-share
  let total-working-scientist (sum [working-scientists] of firms)
  ask firms [
    set working-scientists-share ( working-scientists / total-working-scientist )
  ]
end

;; CHECK TIMES

to check-times

  if time-catch-up = "N/A" [
    if [market-share] of emergent-firm > (1 / (2 * num-firms)) [
      set time-catch-up time
    ]
  ]

  if time-convergence = "N/A" [
    let market-shares (map [[market-share] of ?] list-of-firms)
    let not-different-list [not-different-market-share-from-others? market-shares] of firms
    if (reduce [?1 and ?2] not-different-list) [
      set time-convergence time
    ]
  ]

end

to-report not-different-market-share-from-others? [market-shares]
  let my-market-share market-share
  report reduce [?1 and ?2] (map [abs (? - my-market-share) < 0.01] market-shares)
end

;; UPDATE STATE VARIABLES

to update-debt
  set debt (debt + time-increment * (
    (ifelse-value (financial-needs-indicator > 1)
      [(financial-needs-indicator - 1) * (1 - r&d-over-profit) * profit * production]
      [0]) - debt-amortization-rate * debt
    ))
end

to do-capitals
  ask firms [
    set capital (capital +  time-increment * capital * (demand-growth-rate + competitiveness - avg-competitiveness))
  ]
end

to do-r&d-over-profits
  let max-profit-r&d ([r&d-over-profit] of max-profit-firm)

  ask firms [
    set r&d-over-profit (r&d-over-profit +  time-increment * learning-rate * (max-profit-r&d - r&d-over-profit))
  ]
end

to do-performances
  ask firms [
    set performance (performance + time-increment * performance * (r&d-productivity * working-scientists-share))
  ]
end

to do-working-scientists
  let total-trained-scientists (sum [trained-scientists] of firms)
  let total-working-scientists (sum [working-scientists] of firms)
  ask firms [
    set working-scientists (working-scientists +  time-increment * (
    (stay-in-country-ratio * trained-scientists) +
    (1 - stay-in-country-ratio) * (immigration-ratio * (total-trained-scientists + total-working-scientists) - working-scientists)
    ))
  ]
end

to update-trained-scientists
  set trained-scientists (trained-scientists + increase-in-trained-scientists *  time-increment)
end

to do-immigration-ratios
  let avg-wages mean [scientists-wages] of firms
  let avg-r&d-productivity mean [r&d-productivity] of firms

  ask firms [
    set attractiveness immigration-ratio-sensitivity * (
      (1 - r&d-prod/wage-sensitivity) * ((scientists-wages - avg-wages) / avg-wages)
      + r&d-prod/wage-sensitivity * ((r&d-productivity - avg-r&d-productivity) / avg-r&d-productivity)
      )
  ]

  let avg-attractiveness sum [immigration-ratio * attractiveness] of firms

  ask firms [
    set immigration-ratio (immigration-ratio + immigration-ratio *  time-increment * (attractiveness - avg-attractiveness))
    if (immigration-ratio < 0) or (immigration-ratio > 1) [
      user-message (word "The model has produced an immigration share outside the interval [0, 1]. Please re-run it with a lower time-increment.")
      stop
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Plots       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to do-plots

  set-current-plot "Market Share"
  plot-all (map [[market-share] of ?] list-of-firms)

  set-current-plot "R&D Expenditure"
  plot-all (map [[r&d-expenditure] of ?] list-of-firms)

  set-current-plot "Share of total scientists"
  plot-all (map [[working-scientists-share] of ?] list-of-firms)

  set-current-plot "Scientists wages"
  plot-all (map [[scientists-wages] of ?] list-of-firms)

  set-current-plot "Debt to Capital Ratio"
  plot-all (map [([debt] of ?1) / ([capital] of ?1)] list-of-firms)

  set-current-plot "Immigration share"
  plot-all (map [[immigration-ratio] of ?] list-of-firms)

end

to plot-all [l]
  let i 0
  foreach firms-numbers [
    set-current-plot-pen (word ?)
    plotxy time (item i l)
    set i (i + 1)
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Supporting procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to assign-values-to-firms [var-name values]
  (foreach list-of-firms values [
    ask ?1 [run (word "set " var-name " " ?2)]
  ])
end

to-report round-to-digits [number digits]
  let epsilon 0.0001
  if ((number >= (- epsilon)) and (number <= epsilon)) [report 0.0]
  let power-of-ten ((ceiling (log (abs number) 10)) - digits)
  if-else power-of-ten > 0
    ;; Be careful with round, as it returns an integer (very short range).
    [ report (round (number / (10.0 ^ power-of-ten))) * (10.0 ^ power-of-ten) ]
    [ report (round (number * (10.0 ^ (- power-of-ten)))) / (10.0 ^ (- power-of-ten)) ]
end
@#$#@#$#@
GRAPHICS-WINDOW
680
425
925
536
2
2
16.0
1
1
1
1
1
0
0
0
1
-2
2
-2
2
1
1
0
ticks
30.0

SLIDER
12
69
285
102
price/performance-sensitivity
price/performance-sensitivity
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
346
263
591
296
learning-rate
learning-rate
0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
661
36
848
69
stay-in-country-ratio
stay-in-country-ratio
0
1
0.9
0.01
1
NIL
HORIZONTAL

BUTTON
674
231
774
275
setup
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
674
284
775
332
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
13
136
287
169
capital-productivity
capital-productivity
0.01
10
1
0.01
1
NIL
HORIZONTAL

PLOT
13
425
333
644
Market share
time
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

PLOT
343
650
664
869
R&D Expenditure
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

INPUTBOX
141
172
288
232
initial-performance
[0.8 0.75 0.7]
1
0
String

INPUTBOX
347
200
591
260
initial-r&d-over-profit
[0.1 0.08 0.06]
1
0
String

TEXTBOX
13
10
124
28
Demand
13
0.0
1

INPUTBOX
13
172
136
232
initial-capital
[130 60 10]
1
0
String

SLIDER
12
31
286
64
demand-growth-rate
demand-growth-rate
0
1
0.05
0.01
1
NIL
HORIZONTAL

TEXTBOX
13
115
135
133
Production
13
0.0
1

TEXTBOX
15
249
102
267
Debt
13
0.0
1

SLIDER
13
272
287
305
interest-rate
interest-rate
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
13
310
287
343
debt-amortization-rate
debt-amortization-rate
0
1
0.2
0.01
1
NIL
HORIZONTAL

INPUTBOX
12
349
286
409
initial-debt
[100 30 0]
1
0
String

TEXTBOX
348
179
474
197
R&D Spending
13
0.0
1

TEXTBOX
353
10
553
30
Innovation
13
0.0
1

INPUTBOX
345
33
589
93
initial-tech-frontier
[1 1 1]
1
0
String

INPUTBOX
346
97
590
157
expansion-rate-tech-frontier
[0.01 0.01 0.01]
1
0
String

TEXTBOX
346
326
462
344
University Systems
13
0.0
1

SLIDER
661
129
848
162
r&d-prod/wage-sensitivity
r&d-prod/wage-sensitivity
0
1
0.5
0.01
1
NIL
HORIZONTAL

INPUTBOX
856
37
991
97
initial-immigration-ratio
[0.45 0.45 0.1]
1
0
String

INPUTBOX
344
349
444
409
initial-budget
[0.9 0.9 0.2]
1
0
String

INPUTBOX
451
349
589
409
budget-growth-increase
[0.2 0.2 0.2]
1
0
String

INPUTBOX
856
103
991
163
initial-scientists-wages
[0.05 0.05 0.05]
1
0
String

PLOT
343
425
664
644
Share of total scientists
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

PLOT
13
649
333
869
Debt to Capital Ratio
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

PLOT
672
425
994
644
Immigration share
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

MONITOR
785
257
872
302
time
max (list (time - time-increment) 0)
3
1
11

PLOT
672
650
994
868
Scientists wages
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

MONITOR
884
286
990
331
convergence time
time-convergence
3
1
11

SLIDER
661
82
848
115
immigration-ratio-sensitivity
immigration-ratio-sensitivity
0
1
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
883
232
990
277
catch-up time
time-catch-up
3
1
11

BUTTON
228
672
299
705
rescale
set-current-plot \"Debt to Capital Ratio\"\nlet ratios (map [([debt] of ?1) / ([capital] of ?1)] list-of-firms)\nset-plot-y-range (round-to-digits min ratios 6) ((round-to-digits max ratios 6) + 0.0009765625)\nset-plot-x-range ((round-to-digits time 6) - 0.0009765625) (round-to-digits time 6)
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
663
10
837
28
Scientists' mobility
13
0.0
1

@#$#@#$#@
## WHAT IS IT?

This section could give a general understanding of what the model is trying to show or explain.

## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
