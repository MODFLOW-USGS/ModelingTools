..\..\bin\ucode_2005  11.in    ex1
DEL                          ex1.#11uout-sen-analysis+por
RENAME ex1.#uout             ex1.#11uout-sen-analysis+por
rem -----------------------------------------
..\..\bin\ucode_2005  12.in    ex1
DEL                          ex1.#12createinitfiles
RENAME ex1.#ucreateinitfiles ex1.#12createinitfiles
rem -----------------------------------------
..\..\bin\residual_analysis.exe  ex1
DEL                  ex1.#13uout-resan
RENAME ex1.#resan    ex1.#13uout-resan
rem -----------------------------------------
..\..\bin\residual_analysis_adv.exe  ex1
DEL                          ex1.#13uout-resanadv
RENAME ex1.#resanadv         ex1.#13uout-resanadv
rem -----------------------------------------
..\..\bin\ucode_2005 14.in      ex1
DEL                          ex1.#14umodlin
RENAME ex1.#umodlin          ex1.#14umodlin
rem -----------------------------------------
..\..\bin\model_linearity.exe   ex1
DEL                          ex1.#14modlin
RENAME ex1.#modlin           ex1.#14modlin
rem -----------------------------------------
..\..\bin\ucode_2005   15.in ex1
DEL                          ex1.#15upred
RENAME ex1.#upred            ex1.#15upred
rem -----------------------------------------
..\..\bin\linear_uncertainty.exe  ex1
DEL                          ex1.#16linunc
RENAME ex1.#linunc           ex1.#16linunc
rem -----------------------------------------
..\..\bin\corfac_plus.exe    ex1
DEL                          ex1.#17corfac_conf
RENAME ex1.#corfac_conf      ex1.#17corfac_conf
rem -----------------------------------------
..\..\bin\ucode_2005.exe    18.in  ex1
DEL                          ex1.#18umodlinadv_conf
RENAME ex1.#umodlinadv_conf  ex1.#18umodlinadv_conf
rem -----------------------------------------
..\..\bin\model_linearity_adv.exe  ex1
DEL                          ex1.#18modlinadv_conf
RENAME ex1.#modlinadv_conf   ex1.#18modlinadv_conf
rem -----------------------------------------
call 19-ucode-nonlinear-intervals.bat
