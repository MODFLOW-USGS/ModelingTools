unit TripackTypes;

interface

{$IFNDEF Debug}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

uses FastGeo;

//const
  // Tripack values
//  NROW=9;
//  NMAX = 100;
//  NCMAX=5;
//  NTMX=2*NMAX;
//  N6=6*NMAX;
//  LWK=2*NMAX;

// SRFPACK values
//  NROW=9;
//  NMAX = 5000;
//  NCMAX=5;
//  NTMX=2*NMAX;
//  N6=6*NMAX;
//  LWK=2*NMAX;
//  NJ=13;
//  NI=13;

type
  // These array types are all fixed length arrays in the original Fortran.
  // Here, they have been converted to dynamic arrays
  // (which are similar to allocatable arrays in Fortran).
  // The original fixed length array declarations are shown in
  // commented-out code above each type declaration.

  TIntArray = array of integer;

//  TNrowIntArray = array[0..NROW-1] of longint;
  TNrowIntArray = TIntArray;

//  TNcmaxIntArray = array[0..NCMAX-1] of longint;
  TNcmaxIntArray = TIntArray;

//  TNmaxIntArray = array[0..NMAX-1] of longint;
  TNmaxIntArray = TIntArray;

//  TN6IntArray = array[0..N6-1] of longint;
  TN6IntArray = TIntArray;

//  TLwkIntArray = array[0..LWK-1] of longint;
  TLwkIntArray = TIntArray;

//  TNtmx_LwkIntArray = array[0..NTMX-1] of TNrowIntArray;
  TNtmx_LwkIntArray = array of TNrowIntArray;

  TFloatArray = array of TFloat;

//  TNmaxSingleArray = array[0..NMAX-1] of single;
  TNmaxSingleArray = TFloatArray;

//  T2_NmaxSinglearray = array[0..NMAX-1]  of array[0..1] of single;

//  TNiSingleArray = array[0..NI-1] of single;
  TNiSingleArray = TFloatArray;

//  TN6SingleArray = array[0..N6-1] of single;
  TN6SingleArray = TFloatArray;

//  TNi_NjSingleArray = array[0..NJ-1]  of array[0..NI-1] of single;
  TNi_NjSingleArray = array of array of TFloat;

//  TNtnxSingleArray = array[0..NTMX-1] of single;
  TNtnxSingleArray = TFloatArray;

// The following commented-out procedure declarations were used for debugging
// purposes during translation of the TRIPACK routines from Fortran to
// Object Pascal.  As each subroutine was translated, The Object Pascal
// version were tested by comaring their results to the results when the
// Fortran routines were called.

//procedure TRMTST_ext (var N: longint; var X,Y: TNmaxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var LNEW: longint; var TOL: single;
//  var LUN:longint; var ARMAX: single; var IER:longint);
//  stdcall; external 'tripack.dll';

//procedure ADDCST_ext (var NCC: longint; var LCC: TNcmaxIntArray;
//  var N: longint; var X,Y: TNmaxSingleArray; var LWK: longint; var IWK: TLwkIntArray;
//  var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure ADDNOD_ext (var K: longint; var XK,YK: single; var IST, NCC: longint;
// var LCC: T1Intarray; var N: longint; var X,Y: TNmaxSingleArray; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LNEW, IER: longint);
//  stdcall; external 'tripack.dll';

//FUNCTION AREAP_ext (var X,Y: TNmaxSingleArray; var NB: longint; var NODES: TLwkIntArray): single;
//  stdcall; external 'tripack.dll';

//procedure BDYADD_ext (var KK,I1,I2: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LNEW: longint);
//  stdcall; external 'tripack.dll';

//procedure BNODES_ext (var N: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var NODES: TLwkIntArray; var NB,NA,NT: longint);
//  stdcall; external 'tripack.dll';

//procedure CIRCUM_ext (var X1,Y1,X2,Y2,X3,Y3: single; var RATIO: longbool;
//  var XC,YC,CR, SA,AR: single);
//  stdcall; external 'tripack.dll';

//FUNCTION CRTRI_ext (var NCC: longint; var LCC: T1Intarray; var I1,I2,I3: longint): longbool;
//  stdcall; external 'tripack.dll';

//procedure DELARC_ext (var N,IO1,IO2: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LNEW, IER: longint);
//  stdcall; external 'tripack.dll';

//procedure DELNB_ext (var N0,NB,N: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LNEW, LPH: longint);
//  stdcall; external 'tripack.dll';

//procedure DELNOD_ext (var K,NCC: longint; var LCC: TNcmaxIntArray; var N: longint;
//  var X,Y: TNmaxSingleArray; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LNEW,LWK: longint; var IWK: TLwkIntArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure EDGE_ext (var IN1,IN2: longint; var X,Y: TNmaxSingleArray;
//  var LWK: longint; var IWK: TLwkIntArray; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure GETNP_ext (var NCC: longint; var LCC: TNcmaxIntArray; var N: longint;
//  var X,Y: TNmaxSingleArray; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var L: longint; var NPTS: TLwkIntArray;
//  var DS: TNmaxSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//FUNCTION INDXCC_ext (var NCC: longint; var LCC: T1Intarray; var N: longint;
//  var LIST: TN6IntArray; var LEND: TNmaxIntArray): longint;
//  stdcall; external 'tripack.dll';

//procedure INSERT_ext (var K,LP: longint; var LIST,LPTR: TN6IntArray; var LNEW: longint);
//  stdcall; external 'tripack.dll';

//procedure INTADD_ext (var KK,I1,I2,I3: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LNEW: longint);
//  stdcall; external 'tripack.dll';

//FUNCTION INTSEC_ext (var X1,Y1,X2,Y2,X3,Y3,X4,Y4: single): longbool;
//  stdcall; external 'tripack.dll';

//FUNCTION JRAND_ext (var N, IX,IY,IZ: longint): longint;
//  stdcall; external 'tripack.dll';

//FUNCTION LEFT_ext (var X1,Y1,X2,Y2,X0,Y0:single): longbool;
//  stdcall; external 'tripack.dll';

//FUNCTION LSTPTR_ext (var LPL,NB: longint; var LIST: TN6IntArray;
//  var LPTR: TN6IntArray): longint;
//  stdcall; external 'tripack.dll';

//FUNCTION NBCNT_ext (var LPL: longint; var LPTR: TN6IntArray): longint;
//  stdcall; external 'tripack.dll';

//FUNCTION NEARND_ext (var XP,YP: single; var IST,N: longint; var X,Y: TNmaxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var DSQ: single): longint;
//  stdcall; external 'tripack.dll';

//procedure OPTIM_ext (var X,Y: TNmaxSingleArray; var NA: longint;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var NIT: longint;
//  var IWK: longint; var IER: longint);
//  // IWK is the beginning of an array.
//  stdcall; external 'tripack.dll';

//FUNCTION STORE_ext (var X: single): single;
//  stdcall; external 'tripack.dll';

//procedure StoreSwtol_ext(var value: single);
//  stdcall; external 'tripack.dll';

//procedure SWAP_ext (var IN1,IN2,IO1,IO2: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var LP21: longint);
//  stdcall; external 'tripack.dll';
//
//FUNCTION SWPTST_ext (var IN1,IN2,IO1,IO2: longint; var X,Y: TNmaxSingleArray): longbool;
//  stdcall; external 'tripack.dll';

//procedure TRFIND_ext (var NST: longint; var PX,PY: single; var N: longint;
//  var X,Y: TNmaxSingleArray; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var I1, I2,I3: longint);
//  stdcall; external 'tripack.dll';

//procedure TRLIST_ext (var NCC: longint; var LCC: TNcmaxIntArray;
//  var N: longint; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var NROW, NT: longint;
//  var LTRI: TNtmx_LwkIntArray; var LCT: TNcmaxIntArray;
//  var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure TRLPRT_ext (var NCC: longint; var LCT: TNcmaxIntArray;
//  var N: longint; var X,Y: TNmaxSingleArray;
//  var NROW,NT: longint; var LTRI: TNtmx_LwkIntArray; var LOUT: longint;
//  var PRNTX: longbool);
//  stdcall; external 'tripack.dll';

//procedure TRMESH_ext (var N: longint; var X,Y: TNmaxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var LNEW: longint; var NEAR: TLwkIntArray;
//  var NEXT: longint;
//  var DIST: TNtnxSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure TRPLOT_ext (var LUN: longint; var PLTSIZ,
//WX1,WX2,WY1,WY2: single; var NCC: longint; var LCC: TNcmaxIntArray;
//var N: longint; var X,Y: TNmaxSingleArray; var LIST,LPTR : TN6IntArray; var LEND: TNmaxIntArray;
//var NUMBR: longbool; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure TRPRNT_ext (var NCC: longint; var LCC: TNcmaxIntArray;
//  var N: longint; var X,Y: TNmaxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var LOUT: longint;
//  var PRNTX: longbool);
//  stdcall; external 'tripack.dll';

implementation

{$IFNDEF Debug}
  {$HINTS ON}
  {$WARNINGS ON}
{$ENDIF}

end.



