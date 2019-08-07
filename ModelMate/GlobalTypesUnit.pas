unit GlobalTypesUnit;

interface

  uses Classes, GlobalBasicData, IniFiles, SysUtils, Utilities;

type
{ Data types found to be supported by Delphi-Fortran mixed-
  language programming: string[] arrays, double, and LongInt.
}
    { Arrays }
    string255 = string[255];
    string40 = string[40];
    string20 = string[20];
    string12 = string[12];

    { Enumerations }
    // Data Item types
    TItemType = (itDouble, itInteger, itBool, itString, itCombined);
    // Attribute Control Method options.
    // (Assign columns in data grids.)
    TControlMethod = (cmByGroup, cmByItem, cmByDefault);
    // Advanced-Test-Model-Linearity mode options
    TAdvLinOpt = (opConf, opPred);
    // SOS Method options
    TSosMethod = (smKeywords, smFile);
    // Data file type options
    TDataFileType = (ftFixedFormat, ftCsv);
    // Model IO Files Use
    TMIOFileUse = (fuInput, fuOutput);
    // Model Identifier
    TModelID = (midModflow2000, midModflow2005, midGeneric);
    // Model Use (calibration or prediction)
    TModelUse = (muCalib, muPred);
    // Group use.
    TGrpUse = (guParGroup, guObsGroup, guPredGroup, guPriGroup, guUnknown);

    // Parameter Attribute types
    TParamAttType = (
      { Attribute types defined for JUPITER API }
      patParamName, patGroupName, patDerived,
      patTransform, patAdjustable,
      patStartValue, patLowerBound, patUpperBound,
      { Attribute types defined for UCODE }
      patConstrain, patConstraints,
      patLowerValue, patUpperValue,
      patMaxChange, patNonLinearInterval,
      patPerturbAmt, patReasRange, patScalePval, patSenMethod,
      patSosIncrement, patTolPar
      // TODO 2 : Add Parameter Attribute type for prediction only
      { Attribute types defined for ... }
    );

    // Dependent Attribute types.
    TDepAttType = (
      { Attribute types defined for observations and predictions in JUPITER API }
      datObsName, datObsValue, datStatistic, datStatFlag,
      datPredName, datRefValue, datMeasStatistic, datMeasStatFlag,
      datGroupName, datEquation, datUseFlag, datPlotSymbol,
      datWtMultiplier, datCovMatrix,
      { Attribute types defined for observations and predictions in UCODE }
      datNonDetect, datWtOSConstant
      { Attribute types defined for ... }
    );

    // Prior Information Attribute types.
    TPriAttType = (
      { Attribute types defined for prior information in JUPITER API }
      piatPriorName, piatPriValue, piatStatistic, piatStatFlag,
      piatGroupName, piatEquation, piatUseFlag, piatPlotSymbol,
      piatWtMultiplier, piatCovMatrix
    );

    // Active Application.
    TActiveApp = (aaUcode, aaPest, aaApp3, aaApp4);

    // String pair: a string used as value in input block and an explanation
    // used in drop-down list
    TStringPair = class(TPersistent)
      public
        Value: string;
        Explanation: string;
    end;

    // Program locations.
    TProgramLocations = class(TPersistent)
      private
        fUcode2005Location: string;
        fModflow2005Location: string;
        fModflow2000Location: string;
        fGWChartLocation: string;
        fResidAnalysisLocation: string;
        fResidAnalysisAdvLocation: string;
      public
        constructor Create;
        procedure Assign(Source: TPersistent); override;
        procedure WriteToIniFile(IniFile: TMemInifile);
        procedure ReadFromIniFile(IniFile: TMemInifile);
      published
        property Ucode2005Location : string read fUcode2005Location write fUcode2005Location;
        property Modflow2005Location: string read fModflow2005Location
                                             write fModflow2005Location;
        property Modflow2000Location: string read fModflow2000Location
                                             write fModflow2000Location;
        property GWChartLocation: string read fGWChartLocation
                                         write fGWChartLocation;
        property ResidAnalysisLocation: string read fResidAnalysisLocation
                                               write fResidAnalysisLocation;
        property ResidAnalysisAdvLocation: string read fResidAnalysisAdvLocation
                                                  write fResidAnalysisAdvLocation;
    end;

    // Grid cell
    type GridCell = record
      Row, Column: integer;
      Checked: boolean;
      TextOld, TextNew: string;
    end;

var
  setATBoolean: set of TParamAttType;
  GlobalProgramLocations: TProgramLocations;

  function GT_ParAttKeyword(PAT: TParamAttType): string;
  function GT_DepAttKeyword(DAT: TDepAttType): string;
  function GT_PriAttKeyword(PriAt: TPriAttType): string;
  procedure InitializeGlobalTypesUnit;
  function ConvertString(const AString: string12): string; overload; inline;
  function ConvertString(const AString: string20): string; overload; inline;
  function ConvertString(const AString: string40): string; overload; inline;
  function ConvertString(const AString: string255): string; overload; inline;

  function ConvertString12(const AString: string): string12; inline;
  function ConvertString20(const AString: string): string20; inline;
  function ConvertString40(const AString: string): string40; inline;
  function ConvertString255(const AString: string): string255; inline;

implementation

resourcestring
  StrProgramLocations = 'Program Locations';
  StrMODFLOW2000 = 'MODFLOW-2000';
  StrModflow2000DefaultPath = 'C:\WRDAPP\mf2k.1_19\bin\mf2005.exe';
  StrMODFLOW2005 = 'MODFLOW-2005';
  StrModflow2005DefaultPath = 'C:\WRDAPP\MF2005.1_8\Bin\mf2005.exe';
  StrUCODE2005 = 'UCODE2005';
  StrUCODE2005DefaultPath = 'C:\WRDAPP\ucode_2005_1.021\bin\ucode_2005.exe';
  StrGWChart = 'GWChart';
  StrGWChartDefaultPath = 'C:\WRDAPP\GW_Chart\Gw_chart.exe';
  StrResidAnalysis = 'ResidAnalysis';
  StrResidAnalysisDefaultPath = 'C:\WRDAPP\ucode_2005_1.021\bin\residual_analysis.exe';
  StrResidAnalysisAdv = 'ResidAnalysisAdv';
  StrResidAnalysisAdvDefaultPath = 'C:\WRDAPP\ucode_2005_1.021\bin\residual_analysis_adv.exe';

function ConvertString(const AString: string12): string; overload; inline;
begin
  result := string(AString);
end;

function ConvertString(const AString: string20): string; overload; inline;
begin
  result := string(AString);
end;

function ConvertString(const AString: string40): string; overload; inline;
begin
  result := string(AString);
end;

function ConvertString(const AString: string255): string; overload; inline;
begin
  result := string(AString);
end;

function ConvertString12(const AString: string): string12; inline;
begin
  result := string12(AnsiString(AString));
end;

function ConvertString20(const AString: string): string20; inline;
begin
  result := string20(AnsiString(AString));
end;

function ConvertString40(const AString: string): string40; inline;
begin
  result := string40(AnsiString(AString));
end;

function ConvertString255(const AString: string): string255; inline;
begin
  result := string255(AnsiString(AString));
end;



function GT_ParAttKeyword(PAT: TParamAttType): string;
begin
  case PAT of
    patParamName: result := 'ParamName';
    patGroupName: result := 'GroupName';
    patDerived: result := 'not_a_keyword';
    patTransform: result := 'Transform';
    patAdjustable: result := 'Adjustable';
    patStartValue: result := 'StartValue';
    // The next two are Ucode-specific.
    // Pest likely will use LowerBound and UpperBound.
    // To support Pest, add argument of type TActiveApp and use "case" here.
    patLowerBound: result := 'LowerConstraint';
    patUpperBound: result := 'UpperConstraint';
    patConstrain: result := 'Constrain';
    patConstraints: result := 'not_a_keyword';
    patLowerValue: result := 'LowerValue';
    patUpperValue: result := 'UpperValue';
    patMaxChange: result := 'MaxChange';
    patNonLinearInterval: result := 'NonLinearInterval';
    patPerturbAmt: result := 'PerturbAmt';
    patReasRange: result := 'not_a_keyword';
    patScalePval: result := 'ScalePval';
    patSenMethod: result := 'SenMethod';
    patSosIncrement: result := 'SOSIncrement';
    patTolPar: result := 'TolPar';
  end;
end; // function GT_ParAttKeyword.

function GT_DepAttKeyword(DAT: TDepAttType): string;
begin
  case DAT of
    datObsName: result := 'ObsName';
    datObsValue: result := 'ObsValue';
    datStatistic: result := 'Statistic';
    datStatFlag: result := 'StatFlag';
    datPredName: result := 'PredName';
    datRefValue: result := 'RefValue';
    datMeasStatistic: result := 'MeasStatistic';
    datMeasStatFlag: result := 'MeasStatFlag';
    datGroupName: result := 'GroupName';
    datEquation: result := 'Equation';
    datUseFlag: result := 'UseFlag';
    datPlotSymbol: result := 'PlotSymbol';
    datWtMultiplier: result := 'WtMultiplier';
    datCovMatrix: result := 'CovMatrix';
    datNonDetect: result := 'NonDetect';
    datWtOSConstant: result := 'WtOSConstant';
  end;
end; // function GT_DepAttKeyword.

function GT_PriAttKeyword(PriAT: TPriAttType): string;
begin
  case PriAT of
    piatPriorName: result := 'PriorName';
    piatPriValue: result := 'PriorInfoValue';
    piatStatistic: result := 'Statistic';
    piatStatFlag: result := 'StatFlag';
    piatGroupName: result := 'GroupName';
    piatEquation: result := 'Equation';
    piatUseFlag: result := 'UseFlag';
    piatPlotSymbol: result := 'PlotSymbol';
    piatWtMultiplier: result := 'WtMultiplier';
    piatCovMatrix: result := 'CovMatrix';
  end;
end; // function GT_PriAttKeyword.

{ TProgramLocations }

procedure TProgramLocations.Assign(Source: TPersistent);
begin
  if Source is TProgramLocations then
    begin
      Modflow2000Location := TProgramLocations(Source).Modflow2000Location;
      Modflow2005Location := TProgramLocations(Source).Modflow2005Location;
      Ucode2005Location := TProgramLocations(Source).Ucode2005Location;
      GWChartLocation := TProgramLocations(Source).GWChartLocation;
      ResidAnalysisLocation := TProgramLocations(Source).ResidAnalysisLocation;
      ResidAnalysisAdvLocation := TProgramLocations(Source).ResidAnalysisAdvLocation;
    end
  else
    begin
      inherited;
    end;
end;

constructor TProgramLocations.Create;
begin
  fModflow2000Location := StrModflow2000DefaultPath;
  fModflow2005Location := StrModflow2005DefaultPath;
  fUcode2005Location := StrUCODE2005DefaultPath;
  fGWChartLocation := StrGWChartDefaultPath;
  fResidAnalysisLocation := StrResidAnalysisDefaultPath;
  fResidAnalysisAdvLocation := StrResidAnalysisAdvDefaultPath;
end;

procedure TProgramLocations.ReadFromIniFile(IniFile: TMemInifile);
begin
  Modflow2000Location := IniFile.ReadString(StrProgramLocations, StrMODFLOW2000,
    StrModflow2000DefaultPath);
  Modflow2005Location := IniFile.ReadString(StrProgramLocations, StrMODFLOW2005,
    StrModflow2005DefaultPath);
  Ucode2005Location := IniFile.ReadString(StrProgramLocations, StrUcode2005,
    StrUcode2005DefaultPath);
  GWChartLocation := IniFile.ReadString(StrProgramLocations, StrGWChart,
    StrGWChartDefaultPath);
  ResidAnalysisLocation := IniFile.ReadString(StrProgramLocations, StrResidAnalysis,
    StrResidAnalysisDefaultPath);
  ResidAnalysisAdvLocation := IniFile.ReadString(StrProgramLocations, StrResidAnalysisAdv,
    StrResidAnalysisAdvDefaultPath);
end;

procedure TProgramLocations.WriteToIniFile(IniFile: TMemInifile);
begin
  IniFile.WriteString(StrProgramLocations, StrUCODE2005, Ucode2005Location);
  IniFile.WriteString(StrProgramLocations, StrMODFLOW2005, Modflow2005Location);
  IniFile.WriteString(StrProgramLocations, StrMODFLOW2000, Modflow2000Location);
  IniFile.WriteString(StrProgramLocations, StrGWChart, GWChartLocation);
  IniFile.WriteString(StrProgramLocations, StrResidAnalysis, ResidAnalysisLocation);
  IniFile.WriteString(StrProgramLocations, StrResidAnalysisAdv, ResidAnalysisAdvLocation);
end;

//###################################################################

procedure InitializeGlobalTypesUnit;
begin
  setATBoolean := [patAdjustable, patConstrain, patTransform, patConstrain];
  GlobalProgramLocations := TProgramLocations.Create;
end;

initialization
  InitializeGlobalTypesUnit;

finalization
  GlobalProgramLocations.Free;  

end.
