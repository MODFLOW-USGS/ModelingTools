{This file lists the parameter roots for data sets that will be estimated with
pilot point parameters by PEST. Each root must be unique. Parameter names are
limited to 12 characters so the root must be shorter than that to accomodate
including the parameter ID, layer number, and pilot point number in the
ultimate parameter name.}
unit PestParamRoots;

interface

const
  // MODFLOW 6
  // CUSB package
  CSUB_CG_SKE_CR = 'CSKE';
  CSUB_CG_THETA  = 'CTHE';
  CCUB_SGM       = 'CSGM';
  CSUB_SGS       = 'CSGS';
  // Initial Conditions
  IC_STRT        = 'STRT';
  // NPF package
  NPF_Angle1     = 'Ang1';
  NPF_Angle2     = 'Ang2';
  NPF_Angle3     = 'Ang3';
  NPF_K          = 'K';
  NPF_K22        = 'K22';
  NPF_K33        = 'K33';
  NPF_WETDRY     = 'WtDr';
  // Storage package
  STO_SS         = 'SS';
  STO_SY         = 'SY';

implementation

end.
