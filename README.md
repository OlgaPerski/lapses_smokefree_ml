# SmokeFree ML Variable List


| Variable nr | Variable name | Description | Time-invariant vs. time-varying |
| :----: | :--------: | :--------: | :--------: |
| 1 | account_id | Unique user ID | Time-invariant |
| 2 | adjusted_quit_date | Quit date set on the app, stored in UTC (with timezone offset applied) | Time-invariant |
| 3 | cigs_per_day | Cigarettes per day (CPD) reported at baseline (cut-off set to 100 CPD) | Time-invariant |
| 4 | adjusted_craving_record_created | Time when craving record was entered into the app (with timezone offset applied) | Time-varying |
| 5 | craving_severity | Scale (1-10) of how severe the craving was | Time-varying |
| 6 | **craving_did_smoke** | Whether the user smoked (outcome variable of interest) | Time-varying |
| 7 | craving_env_alone | Whether the user was alone | Time-varying |
| 8 | craving_env_colleagues | Whether the user was with colleagues | Time-varying |
| 9 | craving_env_family | Whether the user was with family members | Time-varying |
| 10 | craving_env_friends | Whether the user was with friends | Time-varying |
| 11 | craving_env_partner | Whether the user was with their partner | Time-varying |
| 12 | craving_env_strangers | Whether the user was with strangers | Time-varying |
| 13 | craving_occupation_at_bar_or_event | Whether the user was at a bar/an event | Time-varying |
| 14 | craving_occupation_chatting | Whether the user was chatting | Time-varying |
| 15 | craving_occupation_coffee_or_tea | Whether the user was drinking coffee/tea | Time-varying |
| 16 | craving_occupation_drinking | Whether the user was drinking alcohol | Time-varying |
| 17 | craving_occupation_driving | Whether the user was driving | Time-varying |
| 18 | craving_occupation_going_to_bed | Whether the user was going to bed | Time-varying |
| 19 | craving_occupation_just_eaten | Whether the user had just eaten | Time-varying |
| 20 | craving_occupation_just_had_sex | Whether the user had just had sex | Time-varying |
| 21 | craving_occupation_reading | Whether the user was reading | Time-varying |
| 22 | craving_occupation_relaxing | Whether the user was relaxing | Time-varying |
| 23 | craving_occupation_taking_a_break | Whether the user was taking a break | Time-varying |
| 24 | craving_occupation_thinking | Whether the user was thinking | Time-varying |
| 25 | craving_occupation_waking_up | Whether the user was waking up | Time-varying |
| 26 | craving_occupation_working | Whether the user was working | Time-varying |
| 27 | craving_occupation_other | Whether the user was doing something else | Time-varying |
| 28 | craving_feeling_annoyed | Whether the user was feeling annoyed | Time-varying |
| 29 | craving_feeling_anxious | Whether the user was feeling anxious | Time-varying |
| 30 | craving_feeling_bored | Whether the user was feeling bored | Time-varying |
| 31 | craving_feeling_down | Whether the user was feeling down | Time-varying |
| 32 | craving_feeling_happy | Whether the user was feeling happy | Time-varying |
| 33 | craving_feeling_hungry | Whether the user was feeling hungry | Time-varying |
| 34 | craving_feeling_lonely | Whether the user was feeling lonely | Time-varying |
| 35 | craving_feeling_stressed | Whether the user was feeling stressed | Time-varying |
| 36 | craving_feeling_other | Whether the user was feeling something else| Time-varying |
| 37 | wkday_craving | Day of the week (Mon-Sun) | Time-varying |
| 38 | time_of_day | Time of day (morning, midday, evening, night) | Time-varying |
| 39 | event_nr | Rolling tally of craving records for each user | Time-varying |
| 40 | days_since_qd | Rolling tally of days since the quit date for each craving record and user | Time-varying |
| 41 | mins_since_prev_rep | Minutes since the previous craving record for each user | Time-varying |
