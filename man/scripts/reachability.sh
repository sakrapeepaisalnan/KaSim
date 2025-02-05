cat kasa_options.head > LOG_mute.txt && \
cat kasa_options.head > LOG_low.txt && \
cat kasa_options.head > LOG_medium_OK.txt && \
cat kasa_options.head > LOG_medium_KO.txt && \
cat kasa_options.head > LOG_high_init.txt && \
cat kasa_options.head > LOG_high_rule.txt && \
cat kasa_options.head > LOG_full.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka | fold -w 61 -s >> LOG_low.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-reachability-analysis Mute | fold -w 61 -s >> LOG_mute.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-reachability-analysis Medium > LOG_medium.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-reachability-analysis High >> LOG_high.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-reachability-analysis Full > LOG_pre_full.txt && \
cat kasa_options.foot >> LOG_mute.txt && \
cat kasa_options.foot >> LOG_low.txt && \
grep -h --before-context 1 -m 1 "is satisfied" LOG_medium.txt | fold -w 81 -s >> LOG_medium_OK.txt && \
grep -h --before-context 1 -m 1 "not satisfied yet" LOG_medium.txt | fold -w 81 -s >> LOG_medium_KO.txt && \
grep -h --after-context 2  "initial state" LOG_high.txt | grep -h --before-context 2 "R\|E" | fold -w 81 -s >> LOG_high_init.txt && \
grep -h --before-context 2 --after-context 12 -m 1 "is satisfied" LOG_high.txt  | fold -w 81 -s >> LOG_high_rule.txt && \
grep -h --after-context 22 -m 1 "Applying" LOG_pre_full.txt | fold -w 81 -s >> LOG_full.txt && \
cat kasa_options.foot >> LOG_medium_OK.txt && \
cat kasa_options.foot >> LOG_medium_KO.txt && \
cat kasa_options.foot >> LOG_high_init.txt && \
cat kasa_options.foot >> LOG_high_rule.txt && \
cat kasa_options.foot >> LOG_full.txt && \
mv LOG*.txt ../generated_img/ #&& \
#rm LOG*.txt

rm -r output/
