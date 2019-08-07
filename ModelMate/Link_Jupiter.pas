unit Link_Jupiter;

interface

  uses GlobalTypesUnit;

procedure aj_ini(
  var UcodeFile: string255;
  var Ifail: LongInt;
  LenStr: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_bas_ini_getoptions(
  var Ivb: LongInt;
  var Deriv_Int: string255;
  var Path2MergedFile: string255;
  var Ifail: LongInt;
  LenStr1: LongInt;
  LenStr2: LongInt);
  stdcall; external 'accjupiter.dll';

  procedure aj_ini_ucode_control_data(
  var Pred: LongInt;
  var Optimiz: LongInt;
  var Sens: LongInt;
  var Lin: LongInt;
  var NonLinInt: LongInt;
  var StdErr1: LongInt;
  var WriteDerPars: LongInt;
  var WritePriInfo: LongInt;
  var EigenVal: LongInt;
  var DataEx: LongInt;
  var CrInitFiles: LongInt;
  var LinAdv: string255;
  var ModName: string255;
  var ModLenUnit: string255;
  var ModMassUnit: string255;
  var ModTimeUnit: string255;
  var SosSurf: string255;
  var SosFile: string255;
  var StrtRes: string255;
  var IntRes: string255;
  var FinRes: string255;
  var StrtSens: string255;
  var IntSens: string255;
  var FinSens: string255;
  var Ifail: LongInt;
  var ErrMsg: string255;
  LenStr1: LongInt;
  LenStr2: LongInt;
  LenStr3: LongInt;
  LenStr4: LongInt;
  LenStr5: LongInt;
  LenStr6: LongInt;
  LenStr7: LongInt;
  LenStr8: LongInt;
  LenStr9: LongInt;
  LenStr10: LongInt;
  LenStr11: LongInt;
  LenStr12: LongInt;
  LenStr13: LongInt;
  LenStr14: LongInt);
  stdcall; external 'accjupiter.dll';

  procedure aj_ini_reg_gn_controls(
  var MaxIt: LongInt;
  var LQuasiNewton: LongInt;
  var IQNIter: LongInt;
  var IOmitDefault: LongInt;
  var LStats_On_Nonconverge: LongInt;
  var LOmitInsensitive: LongInt;
  var IConsecMax: LongInt;
  var DTolPar: double;
  var DTolSosc: double;
  var DMaxChg: double;
  var DMqrtDir: double;
  var DMqrtFac: double;
  var DMqrtInc: double;
  var DQNSosr: double;
  var DMinSenRat: double;
  var DReincSenRat: double;
  var DTolParWtos: double;
  var DMaxStep: double;
  var CMaxChgRealm: string255;
  var CTrustReg: string255;
  var IFail: LongInt;
  LenStr1: LongInt;
  LenStr2: LongInt);
  stdcall; external 'accjupiter.dll';


procedure aj_ini_model_command_lines(
  var NCommands: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_get_model_command_by_purpose(
  var Purp: string255;
  var Cmd: string255;
  LenStr1: LongInt;
  LenStr2: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_derived_parameters(
  var NDerPars: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_observation_data(
  var NObs: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_observation_groups(
  var NObsGps: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_parameter_data(
  var NPars: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_parameter_groups(
  var NParGps: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_prediction_data(
  var NPred: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_prediction_groups(
  var NPredGps: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_linear_prior_information(
  var NPri: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_prior_information_groups(
  var NPriGps: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_model_input_files(
  var NMIFiles: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_model_output_files(
  var NMOFiles: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_ini_parallel(
  var KCtrl: LongInt;
  var NumRunners: LongInt;
  var IParallel: LongInt;
  var Wait: double;
  var WaitRunners: double;
  var VerboseRunner: LongInt;
  var IAutoStopRunners: LongInt;
  var TimeoutFac: double);
  stdcall; external 'accjupiter.dll';

procedure aj_get_character_by_keyword(
  var Keyword: string255;
  var I: LongInt;
  var CValue: string255;
  LenStr1: LongInt;
  LenStr2: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_get_double_by_keyword(
  var Keyword: string255;
  var I: LongInt;
  var DValue: double;
  LenStr1: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_get_integer_by_keyword(
  var Keyword: string255;
  var I: LongInt;
  var IValue: LongInt;
  LenStr1: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_get_logical_by_keyword(
  var Keyword: string255;
  var I: LongInt;
  var IValue: LongInt;
  LenStr1: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_get_parameter_group_name(
  var Index: LongInt;
  var Name: string255;
  LenStr1: LongInt);
  stdcall; external 'accjupiter.dll';

//procedure aj_get_parameter_group_names(
//  var Dim: LongInt;
//  var Names: string255;
//  LenStr1: LongInt);
//  stdcall; external 'accjupiter.dll';

procedure aj_utl_case(
  var WORDIN: string255;
  var WORDOUT: string255;
  var ICASE: LongInt;
  LenStr1: Longint;
  LenStr2: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_utl_wrtsig(
  var IFAIL: LongInt;
  var VAL: double;
  var WORD: string255;
  var NW, PRECPR: LongInt;
  var TVAL: double;
  var NOPNT: LongInt;
  LenStr: LongInt);
  stdcall; external 'accjupiter.dll';

procedure aj_cln();
  stdcall; external 'accjupiter.dll';

implementation

end.
