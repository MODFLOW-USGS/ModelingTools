library dmath;

uses
  utypes,   uminmax,  uround,   umath,    utrigo,   uhyper,   ugamma,
  udigamma, uigamma,  ubeta,    uibeta,   ulambert, ufact,    ubinom,
  upoidist, uexpdist, unormal,  ugamdist, uibtdist, uigmdist, uinvnorm,
  uinvgam,  uinvbeta, ugausjor, ulineq,   ucholesk, ulu,      uqr,
  usvd,     ueigval,  ueigvec,  ueigsym,  ujacobi,  uminbrak, ugoldsrc,
  ulinmin,  unewton,  umarq,    ubfgs,    usimplex, ubisect,  unewteq,
  usecant,  unewteqs, ubroyden, upolynom, urtpol1,  urtpol2,  urtpol3,
  urtpol4,  urootpol, upolutil, utrapint, ugausleg, urkf,     ufft,
  urandom,  uranmwc,  uranmt,   uranuvag, urangaus, uranmult, umcmc,
  usimann,  ugenalg,  umeansd,  ucorrel,  uqsort,   umedian,  uskew,
  uinterv,  ustudind, ustdpair, uanova1,  uanova2,  usnedeco, ubartlet,
  ukhi2,    uwoolf,   unonpar,  udistrib, ulinfit,  upolfit,  umulfit,
  usvdfit,  unlfit,   ufracfit, uexpfit,  uiexpfit, uexlfit,  umichfit,
  umintfit, uhillfit, ulogifit, upkfit,   upowfit,  ugamfit,  uregtest,
  upca,     ueval,    ustrings, utexplot, uhsvrgb, ucomplex, 
  {$IFDEF DELPHI}
  uwinstr,  uwinplot;
  {$ELSE}
  uplot;
  {$ENDIF}

  {#BACKUP ..\changes.txt}
  {#BACKUP ..\dmath.pdf}
  {#BACKUP ..\dmath.txt}
  {#BACKUP ..\lgpl.txt}
  {#BACKUP ..\units\*.pas}
  {#BACKUP ..\units\filelist.txt}
  {#BACKUP ..\units\types.inc}
  {#BACKUP ..\demo\bgi\curfit\*.pas}
  {#BACKUP ..\demo\bgi\fmath\*.pas}
  {#BACKUP ..\demo\bgi\fourier\*.pas}
  {#BACKUP ..\demo\bgi\nist\*.pas}
  {#BACKUP ..\demo\bgi\nist\*.dat}
  {#BACKUP ..\demo\bgi\regmodel\*.pas}
  {#BACKUP ..\demo\bgi\regmodel\*.dat}
  {#BACKUP ..\demo\bgi\stat\*.pas}
  {#BACKUP ..\demo\console\complex\*.pas}
  {#BACKUP ..\demo\console\curfit\*.pas}
  {#BACKUP ..\demo\console\equation\*.pas}
  {#BACKUP ..\demo\console\equation\*.inc}
  {#BACKUP ..\demo\console\fmath\*.pas}
  {#BACKUP ..\demo\console\fmath\*.dat}
  {#BACKUP ..\demo\console\fourier\*.pas}
  {#BACKUP ..\demo\console\integral\*.pas}
  {#BACKUP ..\demo\console\integral\*.dat}
  {#BACKUP ..\demo\console\matrices\*.pas}
  {#BACKUP ..\demo\console\matrices\*.dat}
  {#BACKUP ..\demo\console\optim\*.pas}
  {#BACKUP ..\demo\console\optim\*.inc}
  {#BACKUP ..\demo\console\polynom\*.pas}
  {#BACKUP ..\demo\console\proba\*.pas}
  {#BACKUP ..\demo\console\random\*.pas}
  {#BACKUP ..\demo\console\random\*.txt}
  {#BACKUP ..\demo\console\random\*.dat}
  {#BACKUP ..\demo\console\stat\*.pas}
  {#BACKUP ..\demo\gui\calc\*.pas}
  {#BACKUP ..\demo\gui\calc\*.dpr}
  {#BACKUP ..\demo\gui\calc\*.dfm}
  {#BACKUP ..\demo\gui\curfit\*.pas}
  {#BACKUP ..\demo\gui\curfit\*.dpr}
  {#BACKUP ..\demo\gui\curfit\*.dfm}
  {#BACKUP ..\demo\gui\curfit\*.dat}
  {#BACKUP ..\demo\gui\curfit\*.gcf}
  {#BACKUP ..\demo\gui\dialogs\*.pas}
  {#BACKUP ..\demo\gui\dialogs\*.dpr}
  {#BACKUP ..\demo\gui\dialogs\*.dfm}
  {#BACKUP ..\demo\gui\fplot\*.pas}
  {#BACKUP ..\demo\gui\fplot\*.dpr}
  {#BACKUP ..\demo\gui\fplot\*.dfm}
  {#BACKUP ..\demo\gui\hsv_rgb\*.pas}
  {#BACKUP ..\demo\gui\hsv_rgb\*.dpr}
  {#BACKUP ..\demo\gui\hsv_rgb\*.dfm}
  {#BACKUP ..\demo\gui\linsolve\*.pas}
  {#BACKUP ..\demo\gui\linsolve\*.dpr}
  {#BACKUP ..\demo\gui\linsolve\*.dfm}
  {#BACKUP ..\demo\gui\mandel\*.pas}
  {#BACKUP ..\demo\gui\mandel\*.dpr}
  {#BACKUP ..\demo\gui\mandel\*.dfm}
  {#BACKUP ..\demo\gui\mandel\*.par}
  {#BACKUP ..\demo\gui\polysolve\*.pas}
  {#BACKUP ..\demo\gui\polysolve\*.dpr}
  {#BACKUP ..\demo\gui\polysolve\*.dfm}
  {#BACKUP ..\demo\gui\polysolve\*.dat}
  {#BACKUP ..\demo\gui\probcalc\*.pas}
  {#BACKUP ..\demo\gui\probcalc\*.dpr}
  {#BACKUP ..\demo\gui\probcalc\*.dfm}
  {#BACKUP ..\demo\gui\quadrit\*.pas}
  {#BACKUP ..\demo\gui\quadrit\*.dpr}
  {#BACKUP ..\demo\gui\quadrit\*.dfm}

exports
  SetErrCode,                  { Sets error code }
  DefaultVal,                  { Sets error code and default function value }
  MathErr,                     { Returns the error code }
  SetAutoInit,                 { Sets automatic array initialization }
  DimVector,                   { Allocates a real vector }
  DimIntVector,                { Allocates an integer vector }
  DimCompVector,               { Allocates a complex vector }
  DimBoolVector,               { Allocates a boolean vector }
  DimStrVector,                { Allocates a string vector }
  DimMatrix,                   { Allocates a real matrix }
  DimIntMatrix,                { Allocates an integer matrix }
  DimCompMatrix,               { Allocates a complex matrix }
  DimBoolMatrix,               { Allocates a boolean matrix }
  DimStrMatrix,                { Allocates a string matrix }
  FMin,                        { Minimum of 2 reals }
  FMax,                        { Maximum of 2 reals }
  IMin,                        { Minimum of 2 integers }
  IMax,                        { Maximum of 2 integers }
  Sgn,                         { Sign, Sgn(0) = 1 }
  Sgn0,                        { Sign, Sgn(0) = 0 }
  DSgn,                        { DSgn(A, B) = Sgn(B) * |A| }
  FSwap,                       { Exchanges 2 reals }
  ISwap,                       { Exchanges 2 integers }
  RoundN,                      { Rounds a number to N decimal places }
  Ceil,                        { Ceiling function }
  Floor,                       { Floor function }
  Expo,                        { Exponential (with bound checking) }
  Exp2,                        { Exponential, base 2 }
  Exp10,                       { Exponential, base 10 }
  Log,                         { Natural log (with bound checking) }
  Log2,                        { Log, base 2 }
  Log10,                       { Log, base 10 }
  LogA,                        { Log, base A }
  IntPower,                    { Power (integer exponent) }
  Power,                       { Power (real exponent) }
  Pythag,                      { Sqrt(X^2 + Y^2) }
  FixAngle,                    { Set argument in -Pi..Pi }
  Tan,                         { Tangent }
  ArcSin,                      { Arc sinus }
  ArcCos,                      { Arc cosinus }
  ArcTan2,                     { Angle (Ox, OM) with M(X,Y) }
  Sinh,                        { Hyperbolic sine }
  Cosh,                        { Hyperbolic cosine }
  Tanh,                        { Hyperbolic tangent }
  ArcSinh,                     { Inverse hyperbolic sine }
  ArcCosh,                     { Inverse hyperbolic cosine }
  ArcTanh,                     { Inverse hyperbolic tangent }
  SinhCosh,                    { Sinh and Cosh }
  Gamma,                       { Gamma function }
  LnGamma,                     { Logarithm of Gamma function }
  SgnGamma,                    { Sign of Gamma function }
  Stirling,                    { Stirling's formula for Gamma }
  StirLog,                     { Stirling's formula for LnGamma }
  DiGamma,                     { DiGamma function }
  TriGamma,                    { TriGamma function }
  IGamma,                      { Incomplete Gamma function }
  JGamma,                      { Complement of incomplete Gamma function }
  Erf,                         { Error function }
  Erfc,                        { Complement of error function }
  Beta,                        { Beta function }
  IBeta,                       { Incomplete Beta function }
  LambertW,                    { Lambert's W-function }
  Fact,                        { Factorial }
  Binomial,                    { Binomial coefficient }
  PBinom,                      { Probability of binomial distribution }
  PPoisson,                    { Probability of Poisson distribution }
  DExpo,                       { Density of exponential distribution }
  FExpo,                       { Cumulative prob. of exponential dist. }
  DNorm,                       { Density of standard normal distribution }
  DBeta,                       { Density of Beta distribution }
  DGamma,                      { Density of Gamma distribution }
  DKhi2,                       { Density of Khi-2 distribution }
  DStudent,                    { Density of Student's distribution }
  DSnedecor,                   { Density of Fisher-Snedecor distribution }
  FBeta,                       { Cumulative prob. of Beta distribution }
  FBinom,                      { Cumulative prob. of Binomial distribution }
  FStudent,                    { Cumulative prob. of Student's distribution }
  PStudent,                    { Prob(|t| > X) for Student's distribution }
  FSnedecor,                   { Cumulative prob. of Fisher-Snedecor distribution }
  PSnedecor,                   { Prob(F > X) for Fisher-Snedecor distribution }
  FGamma,                      { Cumulative prob. of Gamma distribution }
  FPoisson,                    { Cumulative prob. of Poisson distribution }
  FNorm,                       { Cumulative prob. of standard normal distribution }
  PNorm,                       { Prob(|U| > X) for standard normal distribution }
  FKhi2,                       { Cumulative prob. of Khi-2 distribution }
  PKhi2,                       { Prob(Khi2 > X) for Khi-2 distribution }
  InvNorm,                     { Inverse of normal distribution }
  InvGamma,                    { Inverse of incomplete Gamma function }
  InvKhi2,                     { Inverse of khi-2 distribution }
  InvBeta,                     { Inverse of incomplete Beta function }
  InvStudent,                  { Inverse of Student's t-distribution }
  InvSnedecor,                 { Inverse of Snedecor's F-distribution }
  Cmplx,                       { Returns the complex number X + iY }
  Polar,                       { Returns the complex number R * [cos(Theta) + i * sin(Theta)] }
  CReal,                       { Returns the real part of Z }
  CImag,                       { Returns the imaginary part of Z }
  CSgn,                        { Complex sign }
  CSwap,                       { Exchanges two complex numbers }
  CAbs,                        { Modulus of Z }
  CAbs2,                       { Squared modulus of Z }
  CArg,                        { Argument of Z }
  CConj,                       { Complex conjugate }
  CNeg,                        { Change sign }
  CAdd,                        { Complex addition }
  CSub,                        { Complex subtraction }
  CMul,                        { Complex multiplication }
  CSqr,                        { Complex square }
  CDiv,                        { Complex division }
  CInv,                        { Complex inverse }
  CSqrt,                       { Principal part of complex square root }
  CLn,                         { Principal part of complex logarithm }
  CExp,                        { Complex exponential }
  CRoot,                       { All N-th roots: Z^(1/N) }
  CPower,                      { Power with complex exponent }
  CIntPower,                   { Power with integer exponent }
  CRealPower,                  { Power with real exponent }
  CPoly,                       { Complex polynomial with real coeff. }
  CSin,                        { Complex sine }
  CCos,                        { Complex cosine }
  CSinCos,                     { Complex sine and cosine }
  CTan,                        { Complex tangent }
  CArcSin,                     { Complex arc sine }
  CArcCos,                     { Complex arc cosine }
  CArcTan,                     { Complex arc tangent }
  CSinh,                       { Complex hyperbolic sine }
  CCosh,                       { Complex hyperbolic cosine }
  CSinhCosh,                   { Complex hyperbolic sine and cosine }
  CTanh,                       { Complex hyperbolic tangent }
  CArcSinh,                    { Complex hyperbolic arc sine }
  CArcCosh,                    { Complex hyperbolic arc cosine }
  CArcTanh,                    { Complex hyperbolic arc tangent }
  CLnGamma,                    { Logarithm of complex Gamma function }
  GaussJordan,                 { Linear equation system (Gauss-Jordan method) }
  LinEq,                       { Linear equation system (Gauss-Jordan method) }
  Cholesky,                    { Cholesky factorization }
  LU_Decomp,                   { LU decomposition }
  LU_Solve,                    { Linear equation system (LU method) }
  QR_Decomp,                   { QR decomposition }
  QR_Solve,                    { Linear equation system (QR method) }
  SV_Decomp,                   { Singular value decomposition }
  SV_Solve,                    { Linear equation system (SVD method) }
  SV_SetZero,                  { Set lowest singular values to zero }
  SV_Approx,                   { Matrix approximation from SVD }
  EigenVals,                   { Eigenvalues of a general square matrix }
  EigenVect,                   { Eigenvalues and eigenvectors of a general square matrix }
  EigenSym,                    { Eigenvalues and eigenvectors of a symmetric matrix (SVD method) }
  Jacobi,                      { Eigenvalues and eigenvectors of a symmetric matrix (Jacobi method) }
  MinBrack,                    { Brackets the minimum of a function }
  GoldSearch,                  { Minimization of a function of one variable (golden search) }
  LinMin,                      { Minimization of a function of several variables along a line }
  Newton,                      { Minimization of a function of several var. (Newton's method) }
  SaveNewton,                  { Save Newton iterations in a file }
  Marquardt,                   { Minimization of a function of several var. (Marquardt's method) }
  SaveMarquardt,               { Save Marquardt iterations in a file }
  BFGS,                        { Minimization of a function of several var. (BFGS method) }
  SaveBFGS,                    { Save BFGS iterations in a file }
  Simplex,                     { Minimization of a function of several variables (Simplex) }
  SaveSimplex,                 { Save Simplex iterations in a file }
  RootBrack,                   { Brackets solution of equation }
  Bisect,                      { Nonlinear equation (bisection method) }
  NewtEq,                      { Nonlinear equation (Newton-Raphson method) }
  Secant,                      { Nonlinear equation (secant method) }
  NewtEqs,                     { Nonlinear equation system (Newton-Raphson method) }
  Broyden,                     { Nonlinear equation system (Broyden's method) }
  Poly,                        { Evaluates a polynomial }
  RFrac,                       { Evaluates a rational fraction }
  RootPol1,                    { Root of linear equation }
  RootPol2,                    { Roots of quadratic equation }
  RootPol3,                    { Roots of cubic equation }
  RootPol4,                    { Roots of quartic equation }
  RootPol,                     { Roots of polynomial from companion matrix }
  SetRealRoots,                { Set the imaginary part of a root to zero }
  SortRoots,                   { Sorts the roots of a polynomial }
  TrapInt,                     { Integration by trapezoidal rule }
  GausLeg,                     { Gauss-Legendre integration }
  GausLeg0,                    { Gauss-Legendre integration (lower bound=0) }
  Convol,                      { Convolution by Gauss-Legendre integration }
  ConvTrap,                    { Convolution by trapezoidal rule }
  RKF45,                       { Integration of a system of differential equations }
  FFT,                         { Fast Fourier Transform }
  IFFT,                        { Inverse Fast Fourier Transform }
  FFT_Integer,                 { Fast Fourier Transform for integer data }
  FFT_Integer_Cleanup,         { Clear memory after a call to FFT_Integer }
  CalcFrequency,               { Direct computation of Fourier Transform }
  SetRNG,                      { Select random number generator }
  InitGen,                     { Initialize random number generator }
  IRanGen,                     { 32-bit random integer in [-2^31 .. 2^31 - 1] }
  IRanGen31,                   { 31-bit random integer in [0 .. 2^31 - 1] }
  RanGen1,                     { 32-bit random real in [0,1] }
  RanGen2,                     { 32-bit random real in [0,1) }
  RanGen3,                     { 32-bit random real in (0,1) }
  RanGen53,                    { 53-bit random real in [0,1) }
  InitMWC,                     { Initialize Multiply-With-Carry generator }
  IRanMWC,                     { 32-bit random integer from MWC generator }
  InitMT,                      { Initialize Mersenne Twister generator with a seed }
  InitMTbyArray,               { Initialize MT generator with an array }
  IRanMT,                      { 32-bit random integer from MT generator }
  InitUVAG,                    { Initialize UVAG generator with a seed }
  InitUVAGbyString,            { Initialize UVAG generator with a string }
  IRanUVAG,                    { 32-bit random integer from UVAG generator }
  RanGaussStd,                 { Random number from standard normal distribution }
  RanGauss,                    { Random number from normal distribution }
  RanMult,                     { Random vector from multinormal distrib. (correlated) }
  RanMultIndep,                { Random vector from multinormal distrib. (uncorrelated) }
  InitMHParams,                { Initialize Metropolis-Hastings parameters }
  GetMHParams,                 { Returns Metropolis-Hastings parameters }
  Hastings,                    { Simulation of a p.d.f. by Metropolis-Hastings }
  InitSAParams,                { Initialize Simulated Annealing parameters }
  SA_CreateLogFile,            { Initialize log file for Simulated Annealing }
  SimAnn,                      { Minimization of a function of several var. by Simulated Annealing }
  InitGAParams,                { Initialize Genetic Algorithm parameters }
  GA_CreateLogFile,            { Initialize log file for Genetic Algorithm }
  GenAlg,                      { Minimization of a function of several var. by Genetic Algorithm }
  Min,                         { Minimum of statistical sample }
  Max,                         { Maximum of statistical sample }
  Mean,                        { Sample mean }
  Median,                      { Sample median }
  StDev,                       { Standard deviation estimated from sample }
  StDevP,                      { Standard deviation of population }
  Correl,                      { Correlation coefficient }
  Skewness,                    { Sample skewness }
  Kurtosis,                    { Sample kurtosis }
  QSort,                       { Quick sort (ascending order) }
  DQSort,                      { Quick sort (descending order) }
  Interval,                    { Determines an interval for a set of values }
  AutoScale,                   { Automatic scale determination }
  StudIndep,                   { Student t-test for independent samples }
  StudPaired,                  { Student t-test for paired samples }
  AnOVa1,                      { One-way analysis of variance }
  AnOVa2,                      { Two-way analysis of variance }
  Snedecor,                    { Comparison of two variances }
  Bartlett,                    { Comparison of several variances }
  Khi2_Conform,                { Khi-2 test for conformity }
  Khi2_Indep,                  { Khi-2 test for independence }
  Woolf_Conform,               { Woolf's test for conformity }
  Woolf_Indep,                 { Woolf's test for independence }
  Mann_Whitney,                { Mann-Whitney test }
  Wilcoxon,                    { Wilcoxon test }
  Kruskal_Wallis,              { Kruskal-Wallis test }
  DimStatClassVector,          { Allocates an array of statistical classes }
  Distrib,                     { Distributes an array into statistical classes }
  LinFit,                      { Linear regression }
  WLinFit,                     { Weighted linear regression }
  SVDLinFit,                   { Linear regression by SVD method }
  WSVDLinFit,                  { Weighted linear regression by SVD method }
  MulFit,                      { Multiple linear regression by Gauss-Jordan method }
  WMulFit,                     { Weighted multiple linear regression by Gauss-Jordan method }
  SVDFit,                      { Multiple linear regression by SVD method }
  WSVDFit,                     { Weighted multiple linear regression by SVD method }
  PolFit,                      { Polynomial regression }
  WPolFit,                     { Weighted polynomial regression }
  SVDPolFit,                   { Polynomial regression by SVD method }
  WSVDPolFit,                  { Weighted polynomial regression by SVD method }
  SetOptAlgo,                  { Selects optimization algorithm for nonlinear regression }
  GetOptAlgo,                  { Returns the optimization algorithm }
  SetMaxParam,                 { Sets the maximal number of regression parameters }
  GetMaxParam,                 { Returns the maximal number of regression parameters }
  SetParamBounds,              { Sets the bounds on a regression parameter }
  GetParamBounds,              { Returns the bounds on a regression parameter }
  NLFit,                       { Nonlinear regression }
  WNLFit,                      { Weighted nonlinear regression }
  SetMCFile,                   { Set file for saving MCMC simulations }
  SimFit,                      { Simulation of unweighted nonlinear regression by MCMC }
  WSimFit,                     { Simulation of weighted nonlinear regression by MCMC }
  FracFit,                     { Unweighted fit of rational fraction }
  WFracFit,                    { Weighted fit of rational fraction }
  FracFit_Func,                { Regression function (rational fraction) }
  ExpFit,                      { Unweighted fit of sum of exponentials }
  WExpFit,                     { Weighted fit of sum of exponentials }
  ExpFit_Func,                 { Regression function (sum of exponentials) }
  IncExpFit,                   { Unweighted fit of increasing exponential }
  WIncExpFit,                  { Weighted fit of increasing exponential }
  IncExpFit_Func,              { Regression function (increasing exponential) }
  ExpLinFit,                   { Unweighted fit of the "exponential + linear" model }
  WExpLinFit,                  { Weighted fit of the "exponential + linear" model }
  ExpLinFit_Func,              { Regression function (exponential + linear) }
  MichFit,                     { Unweighted fit of Michaelis equation }
  WMichFit,                    { Weighted fit of Michaelis equation }
  MichFit_Func,                { Regression function (Michaelis equation) }
  MintFit,                     { Unweighted fit of the integrated Michaelis equation }
  WMintFit,                    { Weighted fit of the integrated Michaelis equation }
  MintFit_Func,                { Regression function (integrated Michaelis equation) }
  HillFit,                     { Unweighted fit of the Hill equation }
  WHillFit,                    { Weighted fit of the Hill equation }
  HillFit_Func,                { Regression function (Hill equation) }
  LogiFit,                     { Unweighted fit of the logistic function }
  WLogiFit,                    { Weighted fit of the logistic function }
  LogiFit_Func,                { Regression function (logistic function) }
  PKFit,                       { Unweighted fit of the acid-base titration curve }
  WPKFit,                      { Weighted fit of the acid-base titration curve }
  PKFit_Func,                  { Regression function (acid-base titration curve) }
  PowFit,                      { Unweighted fit of power function }
  WPowFit,                     { Weighted fit of power function }
  PowFit_Func,                 { Regression function (power function) }
  GammaFit,                    { Unweighted fit of gamma distribution function }
  WGammaFit,                   { Weighted fit of gamma distribution function }
  GammaFit_Func,               { Regression function (gamma distribution function) }
  RegTest,                     { Test of unweighted regression }
  WRegTest,                    { Test of weighted regression }
  VecMean,                     { Computes mean vector }
  VecSD,                       { Computes vector of standard deviations }
  MatVarCov,                   { Computes variance-covariance matrix }
  MatCorrel,                   { Computes correlation matrix }
  PCA,                         { Principal component analysis of correlation matrix }
  ScaleVar,                    { Scales a set of variables }
  PrinFac,                     { Computes principal factors }
  LTrim,                       { Remove leading blanks }
  RTrim,                       { Remove trailing blanks }
  Trim,                        { Remove leading and trailing blanks }
  StrChar,                     { Generate string by repeating a character }
  RFill,                       { Complete string with trailing blanks }
  LFill,                       { Complete string with leading blanks }
  CFill,                       { Center string }
  Replace,                     { Replace a character }
  Extract,                     { Extract field from string }
  Parse,                       { Parse string into several fields }
  SetFormat,                   { Set numeric format }
  FloatStr,                    { Convert real number to string }
  IntStr,                      { Convert integer to string }
  CompStr,                     { Convert complex number to string }
{$IFDEF DELPHI}
  StrDec,                      { Set decimal separator to the symbol defined in SysUtils }
  IsNumeric,                   { Test if a string represents a number }
  ReadNumFromEdit,             { Read a numeric value from an Edit control }
  WriteNumToFile,              { Write numeric value to file }
{$ENDIF}
  InitGraphics,                { Initializes BGI or Delphi graphics }
  SetWindow,                   { Sets the graphic window }
  SetOxScale,                  { Sets the scale on the Ox axis }
  SetOyScale,                  { Sets the scale on the Oy axis }
  GetOxScale,                  { Returns the scale on the Ox axis }
  GetOyScale,                  { Returns the scale on the Oy axis }
  SetGraphTitle,               { Sets the graph title }
  SetOxTitle,                  { Sets the title for the Ox axis }
  SetOyTitle,                  { Sets the title for the Oy axis }
  GetGraphTitle,               { Returns the graph title }
  GetOxTitle,                  { Returns the title for the Ox axis }
  GetOyTitle,                  { Returns the title for the Oy axis }
{$IFNDEF DELPHI}
  SetTitleFont,                { Sets the font for the main graph title }
  SetOxFont,                   { Sets the font for the Ox title }
  SetOyFont,                   { Sets the font for the Oy title }
  SetLgdFont,                  { Sets the font for the legends }
  SetClipping,                 { Clips the graphic to the current viewport }
  LeaveGraphics,               { Quits the graphic mode }
{$ENDIF}
  PlotOxAxis,                  { Plots the Ox axis }
  PlotOyAxis,                  { Plots the Oy axis }
  WriteGraphTitle,             { Writes title of graph }
  PlotGrid,                    { Plots a grid on the graph }
  SetMaxCurv,                  { Sets maximum number of curves }
  SetPointParam,               { Sets point parameters }
  SetLineParam,                { Sets line parameters }
  SetCurvLegend,               { Sets curve legend }
  SetCurvStep,                 { Sets curve step }
  GetMaxCurv,                  { Returns maximum number of curves }
  GetPointParam,               { Returns point parameters }
  GetLineParam,                { Returns line parameters }
  GetCurvLegend,               { Returns curve legend }
  GetCurvStep,                 { Returns curve step }
  PlotPoint,                   { Plots a point }
  PlotCurve,                   { Plots a curve }
  PlotCurveWithErrorBars,      { Plots a curve with error bars }
  PlotFunc,                    { Plots a function }
  WriteLegend,                 { Writes legends for all curves }
  WriteLegendSelect,           { Writes legends for selected curves }
  ConRec,                      { Contour plot }
  Xpixel,                      { Converts user abscissa X to screen coordinate }
  Ypixel,                      { Converts user ordinate Y to screen coordinate }
  Xuser,                       { Converts screen coordinate X to user abscissa }
  Yuser,                       { Converts screen coordinate Y to user ordinate }
  TeX_InitGraphics,            { Initializes LaTeX graphics }
  TeX_SetWindow,               { Sets the graphic window }
  TeX_SetOxScale,              { Sets the scale on the Ox axis }
  TeX_SetOyScale,              { Sets the scale on the Oy axis }
  TeX_SetGraphTitle,           { Sets the graph title }
  TeX_SetOxTitle,              { Sets the title for the Ox axis }
  TeX_SetOyTitle,              { Sets the title for the Oy axis }
  TeX_PlotOxAxis,              { Plots the Ox axis }
  TeX_PlotOyAxis,              { Plots the Oy axis }
  TeX_WriteGraphTitle,         { Writes title of graph }
  TeX_PlotGrid,                { Plots a grid on the graph }
  TeX_SetMaxCurv,              { Sets maximum number of curves }
  TeX_SetPointParam,           { Sets point parameters }
  TeX_SetLineParam,            { Sets line parameters }
  TeX_SetCurvLegend,           { Sets curve legend }
  TeX_SetCurvStep,             { Sets curve step }
  TeX_PlotCurve,               { Plots a curve }
  TeX_PlotCurveWithErrorBars,  { Plots a curve with error bars }
  TeX_PlotFunc,                { Plots a function }
  TeX_WriteLegend,             { Writes legends for all curves }
  TeX_WriteLegendSelect,       { Writes legends for selected curves }
  TeX_ConRec,                  { Contour plot }
  Xcm,                         { Converts user abscissa X to cm }
  Ycm,                         { Converts user ordinate Y to cm }
  TeX_LeaveGraphics,           { Quits the graphic mode }
  HSVtoRGB,                    { Converts HSV to RGB }
  RGBtoHSV,                    { Converts RGB to HSV }
  InitEval,                    { Initializes the math parser }
  Eval,                        { Evaluates an expression }
  SetVariable,                 { Assigns a value to a variable }
  SetFunction;                 { Adds a function to the parser }

begin
end.

