Bjó til hassað form af öllum réttum svörum í Lík&Töl og setti inn upplýsingar um eðli spurningar, s.s. fjölda svarmöguleika, noNOTA/NOTA+/NOTA-/AOTA+/AOTA- og plonePath

Geymt hér, í þessari möppu sem bighashfile.txt:
wc bighashfile.txt
    6056   30280  776659 bighashfile.txt
Í raun eru þetta alls 380 ólík rétt svör (auk NOTA+), sem koma fyrir á 6056 stöðum:
awk '{print $3}' < bighashfile.txt | sort|uniq -c|sort -n|wc
     381
og það sama (þ.e. 380) kemur úr því að telja einkvæmar línur úr öllum posans skránum:
for i in `find ~/Dropbox/tw-wk ~/Dropbox/fyrirl/ -name many'*'hash -exec dirname \{\} \;`; do cat $i/posans; done|sort | uniq -c|wc -l
     380
Slatti af þessum réttu svörum eru endurnýtt, t.d. í tutor-web æfingu, síðmisserisprófi og lokaprófi (einkvæmni tekur #posans úr 467 í 380 svör).

Í hverri línu eru fyrst upplýsingar sem tengjast staðsetningu í tutor-web og %ID, sem saman mynda plonePath breytuna í mySQL.

Svo kemur hass af rétta svartextanum, þvínæst fjöldi svarmöguleika og að lokum kemur noNOTA, NOTA+, NOTA-, AOTA+, AOTA-:

/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101001 d18ffc3823fa91864594d6ef3af147f6425f91f9be100e01345d14242c7d39e0 5 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101007 NOTA+ 4 NOTA+
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101008 6ca005fb0347609c3c591c54d3979c1fb56cfdb24db846f7e08afadd6493e6cf 8 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101009 6ca005fb0347609c3c591c54d3979c1fb56cfdb24db846f7e08afadd6493e6cf 7 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101010 88051f2081caee8044b2f81e446904a424bd4f0d0ff786861ac8ccb6cf0d481c 7 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101011 88051f2081caee8044b2f81e446904a424bd4f0d0ff786861ac8ccb6cf0d481c 4 NOTA-
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101018 NOTA+ 4 NOTA+
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101028 ef43fd1bbcc2aae7bb3c537a8f4f3a4ea05b8b77c3270c874d569715c682b366 4 NOTA-
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101029 20ae611ef65bb89a3eedf9f54886d6a018dd666498a0886dfa0508d11e44e4dd 8 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101030 37be57b8abaaad0e1e1efad611b0d4ff5a5ee080aa22e00ed51a0e2131ccb69e 4 AOTA-
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101031 88051f2081caee8044b2f81e446904a424bd4f0d0ff786861ac8ccb6cf0d481c 8 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101032 d046be923b5e8aff79407058840c0da17aec43cbf92f301be26d408a77a6e880 5 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101033 ef43fd1bbcc2aae7bb3c537a8f4f3a4ea05b8b77c3270c874d569715c682b366 4 AOTA-
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101034 20ae611ef65bb89a3eedf9f54886d6a018dd666498a0886dfa0508d11e44e4dd 7 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101035 0d11a81a755056fd0b4f7c8c0db30d3c16890115b8c23ccdd8514024f6ff7234 8 noNOTA
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101036 c4dc3812571e362c5c0b407abc55e8874e859ea22fd2bf98434430f9190eb0c5 4 AOTA-
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101037 d33d452fe6c114b84b262a26839d9f12475a94a7fabb66cdce039dc2b6fdc90b 4 AOTA+
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101037 e66ddc0b45cd5f09fe78f5eead02b8f275758f299dfb5ce73ae8a0e2e38c777c 4 AOTA+
/tutor-web/stats/lotmidmisseri2020/lecture010/ Qgen-q101037 f11f10666aa26de68b8fd7da020da9abd11d8bc53fade83bd07bfcd79ac2772e 4 AOTA+

Kóði:
noNOTA er "venjuleg" spurning  með svarmöguleikum (a), ... (geta verið frá 3 upp í 8 möguleikar)
ef er ekki noNOTA eru alltaf 4 svarmöguleikar:
NOTA+ er með "None Of The Above" og það er rétt svar.
NOTA- er með "None Of The Above" og það er rangt svar.
AOTA+ er með "All Of The Above" og það er rétt svar.
AOTA- er með "All Of The Above" og það er rangt svar.

Athugið að AOTA+ línurnar eru 3 fyrir hverja spurningu, því í þeim spurningum fær nemandinn að sjá 3 rétt svör

Þetta eru söfnin:
find ~/Dropbox/tw-wk ~/Dropbox/fyrirl/ -name many'*'hash|sort
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/LoT_2020/sidmisseri2020/sidmisseri2020-d1/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/LoT_2020/sidmisseri2020/sidmisseri2020-d2/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/LoT_2020/sidmisseri2020/sidmisseri2020-d3/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/LoT_2020/sidmisseri2020/sidmisseri2020-d4/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d1/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d2/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d3/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d4/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d5/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d6/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d7/manyAOTAsPois.hash
/Users/gstefans/Dropbox/fyrirl//kennsla/LikTol/prof/lokaprof/2020/lokaprof2020-d8/manyAOTAsPois.hash
/Users/gstefans/Dropbox/tw-wk/tw-wk-old/stat201.98/set1GeneralPval-ICE/manyAOTAsPois.hash
/Users/gstefans/Dropbox/tw-wk/tw-wk-old/stat201.98/set2Generalt-test-ICE/manyAOTAsPois.hash
/Users/gstefans/Dropbox/tw-wk/tw-wk-old/stat201.98/set3RegInterpret-ICE/manyAOTAsPois.hash
/Users/gstefans/Dropbox/tw-wk/tw-wk-old/stat201.99/set1GeneralPval/manyAOTAsPois.hash
/Users/gstefans/Dropbox/tw-wk/tw-wk-old/stat201.99/set2Generalt-test/manyAOTAsPois.hash
/Users/gstefans/Dropbox/tw-wk/tw-wk-old/stat201.99/set3RegInterpret/manyAOTAsPois.hash

