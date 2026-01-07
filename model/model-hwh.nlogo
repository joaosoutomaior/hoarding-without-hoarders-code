;==============================================================
;                          MODEL
;==============================================================
;                  Hoarding without hoarders:
;  opportunity hoarding in the absence of exclusionary behaviors
;
;           Joao M. Souto-Maior, Stanford University
;                Last updated on December 2025

;==================================================================
;                         CITATION
;==================================================================
; TBA: paper conditionally accepted at Rationality and Society

;==================================================================
;                       LICENSE DETAILS
;==================================================================
; Copyright © 2026 João M. Souto-Maior
; This NetLogo model is licensed under the Creative Commons
; Attribution–NonCommercial 4.0 International License (CC BY-NC 4.0).

;==================================================================
;                        MAIN VARIABLES
;==================================================================

extensions [ csv ]

undirected-link-breed [ edges edge ]

globals [
  ;============================ general global variables
  folder_name
  run_id
  file
  network_dataset
  row
  total_spots
  filled_spots
  considered
  available-spots

  ;============================ outcome variable of interest
  n_b
  pct_b
  pct_adopted_b
  n_w
  pct_adopted_w


  ;============================ network variables
  pct_ties_inter_race
  pct_ties_inter_race_w
  pct_ties_inter_race_b

  pct_ties_inter_class
  pct_ties_inter_class_w
  pct_ties_inter_class_b

  n_ties
  n_ties_b
  n_ties_w

  sim_homophily_w
  sim_homophily_b
]

turtles-own [
  ;============================ general variables
  e_i
  prob_e_i
  race_i
  acad_i
  ses_i
  ses_percentile_i
  influence_potential_i
  influence_potential_percentile_i
  acad_percentile_i
  social_capital_i
  pcolor_i
  rejected?
  pct_filled
  net_target_pct_same_race
  no_initial_net_access_i

  ;============================ network variables
  n_potential_ties_i
  n_ties_i
  n_ties_inter_race_i
  n_ties_inter_class_i
  pct_ties_inter_race_i
  pct_ties_inter_class_i
]
;=========================================================
;                    SET UP ENVIRONMENT
;=========================================================

to setup
  clear-all
  reset-ticks
  ; for sensitivity: remove variation in the network formation model
  ifelse specific_seed = FALSE
  [
    with-local-randomness [
      random-seed seed_number
      create-school
      create-agents
      initial-social-capital
      set-prob-enroll
      create-network
    ]
  ]
  [
    create-school
    create-agents
    compute-ses-percentiles
    compute-acad-percentiles
    initial-social-capital
    count-net-resource
    set-prob-enroll
    create-network
  ]
  visualize-students
  ask links [
        set thickness 0
        set color [0 0 0 25]
      ]
end

;=========================================================
;                  CREATE SCHOOL
;=========================================================

to create-school

  ;============================ fixed parameters
  set total_spots round(pct_spots * n)
  set filled_spots []

  set pct_b 1 - pct_w
  set n_b n * pct_b
  set n_w n * pct_w

  ;============================ school desity calculation (optional)
  ; to make sure that population_density is fixed when we vary school size
  let population_density 0.6
  let total_patches n / population_density
  let new_world_width sqrt total_patches
  resize-world 0 new_world_width 0 new_world_width

  ;============================ change color of patches
  ask patches [
    set pcolor white
  ]
end

;=========================================================
;                  CREATE AGENTS
;=========================================================

to create-agents
  file-close-all
  set-default-shape turtles "person"

  create-turtles n_w [
    set size 0.4
    set color green

    set race_i "White"
    set ses_i random-normal 0 ses_variance
    set acad_i ses_i + random-normal 0 acad_error_variance
    set influence_potential_i 0.5 * (ses_i + (random-normal 0 1))
    set social_capital_i 0
    set e_i 0
    set n_potential_ties_i random-float net_degree_w

    set rejected? 0
    set pcolor_i white

    set net_target_pct_same_race net_homophily * (1 - pct_w) + pct_w
  ]

  create-turtles n_b [
    set size 0.4
    set color red

    set race_i "Black"
    set ses_i random-normal (-1 * bw_ses_diff) ses_variance
    set acad_i ses_i + random-normal 0 acad_error_variance
    set social_capital_i 0
    set influence_potential_i 0.5 * (ses_i + (random-normal (-1 * bw_ses_diff) 1))
    set e_i 0
    set n_potential_ties_i random-float net_degree_b

    set rejected? 0
    set pcolor_i white

    set net_target_pct_same_race net_homophily * (1 - pct_b) + pct_b
  ]

  ;============================ define initial position of turtles (for vizualization purposes only)
  ask turtles [
    let empty patches with [not any? turtles-here]
    move-to one-of empty
  ]
end

;=========================================================
;               Initial information
;=========================================================

to compute-ses-percentiles
  let all-ses [ses_i] of turtles
  let sorted-ses sort all-ses

  ;; Build a frequency table: each distinct SES value (uniq-ses)
  ;; and how many agents hold that value (counts), while the full
  ;; sorted-ses list with duplicates is kept for percentile math.
  let uniq-ses remove-duplicates sorted-ses
  let counts   map [v -> length filter [x -> x = v] sorted-ses ] uniq-ses

  ;; cumulative counts up to each unique value
  let cum 0
  let cumcounts []
  foreach counts [ c ->
    set cum cum + c
    set cumcounts lput cum cumcounts
  ]

  ;; less-than counts and percentile for each unique value
  let lesscounts (map [[cc c] -> cc - c] cumcounts counts)
  let pcts       (map [[less c] -> ((less + 0.5 * c) / n)] lesscounts counts)  ;; 0–1

  ;; assign to each turtle by looking up its ses_i in uniq-ses
  ask turtles [
    let v ses_i
    let idx position v uniq-ses
    set ses_percentile_i item idx pcts
  ]
end

to compute-acad-percentiles
  let all-acad [acad_i] of turtles
  let sorted-acad sort all-acad

  ;; Build a frequency table: each distinct SES value (uniq-ses)
  ;; and how many agents hold that value (counts), while the full
  ;; sorted-ses list with duplicates is kept for percentile math.
  let uniq-acad remove-duplicates sorted-acad
  let counts   map [v -> length filter [x -> x = v] sorted-acad ] uniq-acad

  ;; cumulative counts up to each unique value
  let cum 0
  let cumcounts []
  foreach counts [ c ->
    set cum cum + c
    set cumcounts lput cum cumcounts
  ]

  ;; less-than counts and percentile for each unique value
  let lesscounts (map [[cc c] -> cc - c] cumcounts counts)
  let pcts       (map [[less c] -> ((less + 0.5 * c) / n)] lesscounts counts)  ;; 0–1

  ;; assign to each turtle by looking up its ses_i in uniq-ses
  ask turtles [
    let v acad_i
    let idx position v uniq-acad
    set acad_percentile_i item idx pcts
  ]
end

to compute-influence-percentiles
  let all-influence_potential [influence_potential_i] of turtles
  let sorted-influence_potential sort all-influence_potential

  ;; Build a frequency table: each distinct SES value (uniq-ses)
  ;; and how many agents hold that value (counts), while the full
  ;; sorted-ses list with duplicates is kept for percentile math.
  let uniq-influence_potential remove-duplicates sorted-influence_potential
  let counts   map [v -> length filter [x -> x = v] sorted-influence_potential ] uniq-influence_potential

  ;; cumulative counts up to each unique value
  let cum 0
  let cumcounts []
  foreach counts [ c ->
    set cum cum + c
    set cumcounts lput cum cumcounts
  ]

  ;; less-than counts and percentile for each unique value
  let lesscounts (map [[cc c] -> cc - c] cumcounts counts)
  let pcts       (map [[less c] -> ((less + 0.5 * c) / n)] lesscounts counts)  ;; 0–1

  ;; assign to each turtle by looking up its ses_i in uniq-ses
  ask turtles [
    let v influence_potential_i
    let idx position v uniq-influence_potential
    set influence_potential_percentile_i item idx pcts
  ]
end

to initial-social-capital
  ;; collect all ses_i values
  let all-ses [ses_i] of turtles
  let sorted-ses sort all-ses
  let n_values length sorted-ses

  ;; position of the 90th percentile (top 10%)
  let cutoff-position floor (0.9 * n_values)
  ;; cutoff value
  let cutoff-value item cutoff-position sorted-ses

  ;; assign social_capital_i
  ask turtles [
    ifelse ses_i >= cutoff-value
      [ set social_capital_i 1 ]
      [ set social_capital_i 0
        set no_initial_net_access_i TRUE]
  ]
end

to set-prob-enroll
    ask turtles[
      set prob_e_i 100 * (admin_pressures * influence_potential_i + (1 - admin_pressures) * acad_percentile_i)
    ]
end

to visualize-students
  ask turtles with [social_capital_i = 1] [
    set pcolor_i 45
    ask patch-here [
      set pcolor 45
    ]
  ]
end

;=========================================================
;                    GO PROCEDURES
;=========================================================
to go
  ifelse model_type = "Network formation model"
  [
    stop
  ]
  [
    tick
    main
    if length filled_spots >= total_spots [
      stop
    ]
  ]
end

;=========================================================
;                    MAIN PROCEDURES
;=========================================================
to main
  spread-info
  count-net-resource
  enrollment-decision
end

;==================================================================
;                        NETWORK FORMATION
;==================================================================

to create-network
  ask turtles [
    form-new-tie
  ]
  update-network-ties
  update-network-stats
end

to form-new-tie
  let myrace race_i
  let myclass ses_i
  let my-ties edge-neighbors
  let candidates other turtles with [(not member? self my-ties)]

  repeat n_potential_ties_i
  [
    ifelse random-float 100 <= 100 * net_target_pct_same_race
    [
      let same-race-candidates (candidates with [race_i = myrace])
      if any? same-race-candidates [
        let same-race-tie one-of same-race-candidates
        create-edge-with same-race-tie
      ]
    ]
    [
      let diff-race-candidates (candidates with [race_i != myrace])
      if any? diff-race-candidates [
        let diff-race-tie one-of diff-race-candidates
        create-edge-with diff-race-tie
      ]
    ]
  ]
end

to update-network-ties
ask turtles [
    let my-ties edge-neighbors
    if any? my-ties [
      set n_ties_i count my-ties
      set n_ties_inter_race_i count my-ties with [race_i = [race_i] of myself]
      set n_ties_inter_class_i count my-ties with [ses_i = [ses_i] of myself]
      set pct_ties_inter_race_i n_ties_inter_race_i / n_ties_i
      set pct_ties_inter_class_i n_ties_inter_class_i / n_ties_i
    ]
  ]
end

;=========================================================
;               DIFFUSION OF INFORMATION
;=========================================================

to spread-info
  ;============================ peer spread of information (based on the SIR model of diffusion)
  let candidates turtles with [social_capital_i = 1]
  if count candidates > 0 [
  ask candidates [
      let giver-race race_i
      let my-ties edge-neighbors with [social_capital_i = 0]
      if any? my-ties
      [
        let my-random-ties shuffle sort my-ties
        ask one-of my-random-ties [
          ifelse race_i = giver-race
          [
            if random-float 100 <= 100 * prob_adoption [
              set social_capital_i 1
              ask patch-here [
                set pcolor 45
              ]
            ]
          ]
          [
            if random-float 100 <= 100 * prob_adoption * (1 - ingroup_favoritism) [
              set social_capital_i 1
              ask patch-here [
                set pcolor 45
              ]
            ]
          ]
        ]
      ]
    ]
  ]
end

;=========================================================
;                  COURSE ENROLLMENT
;=========================================================
to enroll
  ifelse random-float 100 < prob_e_i
  [
    set filled_spots lput 1 filled_spots
    set e_i 1
  ]
  [
    set rejected? 1
  ]
end

to enrollment-decision

  set available-spots total_spots - length filled_spots
  if available-spots > 0 [
    let eligible turtles with [e_i = 0 and rejected? = 0]
    if any? eligible [
      set considered nobody
      ifelse net_diffusion = TRUE
      [
        set considered eligible with [social_capital_i = 1]
      ]
      [
        set considered eligible
      ]
      if any? considered [
        ifelse (count considered) <= available-spots
          [
            ask considered [
              set pct_filled (length filled_spots) / total_spots
              enroll
            ]
        ]
        [
          ask n-of available-spots considered [
            set pct_filled (length filled_spots) / total_spots
            enroll
          ]
        ]
      ]
    ]
  ]
  if ticks > 1000 [
    ask n-of available-spots turtles [enroll]
  ]
end

;==========================================================
;                    NETWORK STATS
;==========================================================
to update-network-stats
  set n_ties sum [n_ties_i] of turtles / n
  set n_ties_w sum [n_ties_i] of turtles with [race_i = "White"] / n_w
  set n_ties_b sum [n_ties_i] of turtles with [race_i = "Black"] / n_b

  set pct_ties_inter_race sum [pct_ties_inter_race_i] of turtles / n
  set pct_ties_inter_race_w sum [pct_ties_inter_race_i] of turtles with [race_i = "White"] / n_w
  set pct_ties_inter_race_b sum [pct_ties_inter_race_i] of turtles with [race_i = "Black"] / n_b

  set pct_ties_inter_class sum [pct_ties_inter_class_i] of turtles / n
  set pct_ties_inter_class_w sum [pct_ties_inter_class_i] of turtles with [race_i = "White"] / n_w
  set pct_ties_inter_class_b sum [pct_ties_inter_class_i] of turtles with [race_i = "Black"] / n_b

  set sim_homophily_w (pct_ties_inter_race_w - pct_w) / (1 - pct_w)
  set sim_homophily_b (pct_ties_inter_race_b - pct_b) / (1 - pct_b)
end

;==========================================================
;                 COUNT NET RESOURCE
;==========================================================

to count-net-resource
  set pct_adopted_w (count turtles with [race_i = "White" and no_initial_net_access_i = TRUE and social_capital_i = 1]) / (count turtles with [race_i = "White" and no_initial_net_access_i = TRUE])
  set pct_adopted_b (count turtles with [race_i = "Black" and no_initial_net_access_i = TRUE and social_capital_i = 1]) / (count turtles with [race_i = "Black" and no_initial_net_access_i = TRUE])
end

;==========================================================
;                     EXPORT DATA
;==========================================================

to export-simdata
  ;; Ensure folder name exists
  if not is-string? folder_name [ set folder_name "data-output" ]

  ;; Get run_id robustly
  set run_id (ifelse-value is-number? behaviorspace-run-number [behaviorspace-run-number] [1])

  ;; Full path
  let fname (word folder_name "/results.csv")

  ;; If file already exists, delete it
  if behaviorspace-run-number = 1 and file-exists? fname [
    file-delete fname
  ]

  ;; Write header only once
  if not file-exists? fname [
    file-open fname
    file-print csv:to-row [
      "run_id"
      "race_i"
      "no_initial_net_access_i"
      "ses_percentile_i"
      "e_i"
      "net_homophily"
      "bw_ses_diff"
      "bw_influence_diff"
      "ingroup_favoritism"
      "admin_pressures"
      "net_diffusion"
      "net_degree_b"
      "pct_w"
      "n"
      "ses_variance"
      "acad_error_variance"
      "prob_adoption"
    ]
    file-close
  ]

  ;; Write turtle rows
  file-open fname
  ask turtles [
    ;; double-check all variables are defined
    if is-number? e_i and is-number? ses_percentile_i and is-string? race_i [
      file-print csv:to-row (list
      run_id
      race_i
      no_initial_net_access_i
      ses_percentile_i
      e_i
      net_homophily
      bw_ses_diff
      bw_influence_diff
      ingroup_favoritism
      admin_pressures
      net_diffusion
      net_degree_b
      pct_w
      n
      ses_variance
      acad_error_variance
      prob_adoption)
    ]
  ]
  file-flush
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
380
55
1011
687
-1
-1
23.97
1
14
1
1
1
0
1
1
1
0
25
0
25
1
1
1
ticks
30.0

BUTTON
380
10
470
43
setup
setup
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
560
10
650
43
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
470
10
560
43
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
195
10
365
55
model_type
model_type
"Network formation model" "Full model"
1

SLIDER
200
190
370
223
ingroup_favoritism
ingroup_favoritism
0
1
0.0
0.1
1
NIL
HORIZONTAL

INPUTBOX
20
10
190
70
seed_number
1.0
1
0
Number

SWITCH
20
85
190
118
specific_seed
specific_seed
1
1
-1000

SLIDER
20
260
190
293
bw_ses_diff
bw_ses_diff
0
1
0.56
0.1
1
NIL
HORIZONTAL

SLIDER
20
225
190
258
pct_spots
pct_spots
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
20
365
190
398
net_degree_w
net_degree_w
0
10
8.0
1
1
NIL
HORIZONTAL

SLIDER
20
330
190
363
net_homophily
net_homophily
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
20
190
190
223
pct_w
pct_w
0
1
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
1160
145
1300
190
NIL
n_ties
2
1
11

MONITOR
1020
100
1160
145
NIL
pct_ties_inter_class_w
2
1
11

MONITOR
1020
145
1160
190
NIL
pct_ties_inter_class_b
2
1
11

MONITOR
1020
55
1160
100
N spots left
total_spots - length filled_spots
17
1
11

MONITOR
1160
100
1300
145
NIL
n_ties_w
17
1
11

MONITOR
1160
55
1300
100
NIL
n_ties_b
17
1
11

SWITCH
200
85
365
118
net_diffusion
net_diffusion
0
1
-1000

SLIDER
20
400
190
433
net_degree_b
net_degree_b
1
10
4.0
1
1
NIL
HORIZONTAL

PLOT
1020
190
1300
455
Resource adoption over time
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Whites" 1.0 0 -13840069 true "" "plot pct_adopted_w"
"Blacks" 1.0 0 -5298144 true "" "plot pct_adopted_b"

SLIDER
200
225
370
258
bw_influence_diff
bw_influence_diff
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
200
155
370
188
admin_pressures
admin_pressures
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
20
155
190
188
n
n
0
5000
400.0
1
1
NIL
HORIZONTAL

SLIDER
200
365
372
398
ses_variance
ses_variance
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
200
400
372
433
acad_error_variance
acad_error_variance
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
200
330
372
363
prob_adoption
prob_adoption
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
200
135
350
153
Mechanisms parameters\n
11
0.0
1

TEXTBOX
20
310
180
336
Network-formation parameters\n
11
0.0
1

TEXTBOX
20
135
170
153
Basic characteristics
11
0.0
1

TEXTBOX
200
310
350
328
Additional sensitivity
11
0.0
1

TEXTBOX
20
485
350
556
Notes for BehaviorSpace experiments: (a) in the current set up, BehaviorSpace experiments accurately produce a full individual-level dataset only if simultaneous runs in parallel are set to 1; (b) output folders need to be created in your data directory before running simulations.
11
0.0
1

@#$#@#$#@
## WHAT IS IT?


## HOW IT WORKS


## HOW TO USE IT


## THINGS TO NOTICE


## THINGS TO TRY


## EXTENDING THE MODEL


## NETLOGO FEATURES


## RELATED MODELS


## CREDITS AND REFERENCES


## HOW TO CITE
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
NetLogo 6.3.0
@#$#@#$#@
setup
repeat 20 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="main" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/main"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="network-formation" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>n_ties_b</metric>
    <metric>n_ties_w</metric>
    <metric>sim_homophily_w</metric>
    <metric>sim_homophily_b</metric>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Network formation model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct_w" first="0.1" step="0.1" last="0.9"/>
    <steppedValueSet variable="net_homophily" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="resource-access" repetitions="1000" runMetricsEveryStep="true">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>pct_adopted_w</metric>
    <metric>pct_adopted_b</metric>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-homophily-consolidation" repetitions="500" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "sens-homophily-consolidation"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <steppedValueSet variable="bw_ses_diff" first="0" step="0.25" last="0.75"/>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="net_homophily" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-ingroup-favoritism" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-ingroup-favoritism"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="ingroup_favoritism" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-admin-pressures" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-admin-pressures"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="admin_pressures" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-racial-composition" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-racial-composition"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct_w" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-degree" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-degree"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="net_degree_b" first="4" step="0.5" last="8"/>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="network-formation-degree" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup</setup>
    <go>go</go>
    <metric>n_ties_b</metric>
    <metric>n_ties_w</metric>
    <metric>sim_homophily_w</metric>
    <metric>sim_homophily_b</metric>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Network formation model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pct_w" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="net_degree_b" first="4" step="1" last="8"/>
    <enumeratedValueSet variable="net_diffusion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-ses-variance" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-ses-variance"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <steppedValueSet variable="ses_variance" first="0.25" step="0.25" last="1"/>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-no-reversal-case" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-no-reversal-case"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="n">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-prob-adoption" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-prob-adoption"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prob_adoption" first="0.1" step="0.2" last="0.9"/>
  </experiment>
  <experiment name="sens-acad-error-variance" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-acad-error-variance"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="n">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="acad_error_variance" first="0.25" step="0.25" last="1"/>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sens-n" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed behaviorspace-run-number
setup
set folder_name "../data/sens-n"</setup>
    <go>go</go>
    <final>export-simdata</final>
    <enumeratedValueSet variable="model_type">
      <value value="&quot;Full model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specific_seed">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed_number">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_spots">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bw_ses_diff">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pct_w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_homophily">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_w">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_degree_b">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="net_diffusion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup_favoritism">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="admin_pressures">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="n" first="1000" step="1000" last="3000"/>
    <enumeratedValueSet variable="ses_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acad_error_variance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_adoption">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
1
@#$#@#$#@
