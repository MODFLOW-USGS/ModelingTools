..\..\bin\ucode_2005 03.in           ex1
DEL                  ex1.#03uout-parest
RENAME ex1.#uout     ex1.#03uout-parest
..\..\bin\residual_analysis.exe      ex1
DEL                  ex1.#03uout-resan
RENAME ex1.#resan    ex1.#03uout-resan
..\..\bin\residual_analysis_adv.exe  ex1
DEL                  ex1.#03uout-resanadv
RENAME ex1.#resanadv ex1.#03uout-resanadv
pause
