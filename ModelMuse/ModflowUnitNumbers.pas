unit ModflowUnitNumbers;

interface

uses SysUtils, Classes, GoPhastTypes;

const
  StrLIST = 'LIST';
  StrDIS = 'DIS';
  StrCBC = 'CBC';
  StrBAS = 'BAS6';
  StrLPF = 'LPF';
  StrHUF2 = 'HUF2';
  StrZONE = 'ZONE';
  StrMULT = 'MULT';
  StrCHD = 'CHD';
  StrPCG = 'PCG';
  StrPCGN = 'PCGN';
  StrGHB = 'GHB';
  StrWEL = 'WEL';
  StrRIV = 'RIV';
  StrDRN = 'DRN';
  StrDRT = 'DRT';
  StrRCH = 'RCH';
  StrEVT = 'EVT';
  StrETS = 'ETS';
  StrRES = 'RES';
  StrLAK = 'LAK';
  StrSFR = 'SFR';
  StrSTR = 'STR';
  StrFHB = 'FHB';
  StrUZF = 'UZF';
  StrGMG = 'GMG';
  StrIUNITMHC = 'IUNITMHC';
  StrSIP = 'SIP';
  StrDE4 = 'DE4';
  StrHEAD = 'HEAD';
  StrDRAWDOWN = 'DRAWDOWN';
  StrOC = 'OC';
  StrGAG = 'GAGE';
  StrHOB = 'HOB';
  StrIUHOBSV = 'IUHOBSV';
  StrHFB = 'HFB6';
  StrUNIT = 'UNIT';
  StrDROB = 'DROB';
  StrIUDROBSV = 'IUDROBSV';
  StrGBOB = 'GBOB';
  StrIUGBOBSV = 'IUGBOBSV';
  StrRVOB = 'RVOB';
  StrIURVOBSV = 'IURVOBSV';
  StrCHOB = 'CHOB';
  StrIUCHOBSV = 'IUCHOBSV';
  StrPVAL = 'PVAL';
  StrIOHUFHEADS = 'IOHUFHEADS';
  StrIOHUFFLOWS = 'IOHUFFLOWS';
  StrKDEP = 'KDEP';
  StrLVDA = 'LVDA';
  StrISTCB2 = 'ISTCB2';
  StrMNW2 = 'MNW2';
  StrMNWI = 'MNWI';
  StrMNWI_Wells = 'MNWI_Wells';
  StrMNWI_SummarizeByWell = 'MNWI_SummarizeByWell';
  StrMNWI_SummarizeByNode = 'MNWI_SummarizeByNode';
  StrBCF = 'BCF6';
  StrSUB = 'SUB';
  StrSWT = 'SWT';
  StrSUBSaveRestart = 'SUB_SaveRestart';
  StrSUBReadRestart = 'SUB_ReadRestart';
  StrHydmodOut = 'HYDMOD_OUT';

  StrSubSUB_Out = 'SUB_SUB_OUT';
  StrSubCOM_ML_Out = 'SUB_COM_ML_OUT';
  StrSubCOM_IS_Out = 'SUB_COM_IS_OUT';
  StrSub_VD_Out = 'SUB_VD_OUT';
  StrSub_NDPCH_Out = 'SUB_NDPCH_OUT';
  StrSub_DPCH_Out = 'SUB_DPCH_OUT';
  StrZoneBudget = 'ZONEBUDGET';
  StrHYD = 'HYD';

  StrSwtSUB_Out             = 'SWT_SUB_OUT';
  StrSwtComML_Out           = 'SWT_COM_ML_OUT';
  StrSwtCOM_IS_Out          = 'SWT_COM_IS_OUT';
  StrSwt_VD_Out             = 'SWT_VD_OUT';
  StrSwt_PreCon_Out         = 'SWT_PreCon_OUT';
  StrSwt_DeltaPreCon_Out    = 'SWT_DeltaPreCon_OUT';
  StrSwt_GeoStat_Out        = 'SWT_GeoStat_OUT';
  StrSwt_DeltaGeoStat_Out   = 'SWT_DeltaGeoStat_OUT';
  StrSwt_EffStress_Out      = 'SWT_EffStress_OUT';
  StrSwt_DeltaEffStress_Out = 'SWT_DeltaEffStress_OUT';
  StrSwt_VoidRatio_Out      = 'SWT_VoidRatio_OUT';
  StrSwt_ThickCompSed_Out   = 'SWT_ThickCompSed_OUT';
  StrSwt_LayerCentElev_Out  = 'SWT_LayerCentElev_OUT';
  BAS_InitialHeads          = 'BAS_InitialHeads';
  BFH_Heads                 = 'BFH_Heads';
  BFH_Fluxes                = 'BFH_Fluxes';
  StrNWT                    = 'NWT';
  StrUPW                    = 'UPW';
  StrBTN                    = 'BTN';
  StrADV                    = 'ADV';
  StrDSP                    = 'DSP';
  StrSSM                    = 'SSM';
  StrRCT                    = 'RCT';
  StrGCG                    = 'GCG';
  StrTOB                    = 'TOB';
  StrLMT6                   = 'LMT6';
  StrLMT7                   = 'LMT7';
  StrFTL                    = 'FTL';
  StrFt6CBC                 = 'FT6';
  StrFt6BHD                 = 'FT6';
  StrFt6DisGrb              = 'FT6';
  StrCNF                    = 'CNF';
  StrPHIRAMPOut = 'PHIRAMP_Out';
  StrLKT                    = 'LKT';
  StrSFT                    = 'SFT';
  StrCTS                    = 'CTS';
//  SrtCTO                    = 'CTO';

  StrSTR_OUT = 'STR_OUT';
  StrSTOB = 'STOB';
  StrIUSTOBSV = 'IUSTOBSV';
  StrFMP = 'FMP';
  StrFmpSupplyDemand = 'FMPSupDem';
  StrFmpFarmBudgetCompact = 'FmpFarmBudgetCompact';
  StrFmpFarmBudgetDetailed = 'FmpFarmBudgetDetailed';
  StrFmpOFE = 'FmpOFE';
  StrFmpCID = 'FmpCID';
  StrFmpFID = 'FmpFID';
  StrFmpRoot = 'FmpRoot';
  StrFmpCropUse = 'FmpCropUse';
  StrFmpETR = 'FmpETR';
  StrFmpETFrac = 'FmpETFrac';
  StrFmpSwLosses = 'FmpSwLosses';
  StrFmpPFLX = 'FmpPFLX';
  StrFmpCropFunc = 'FmpCropFunc';
  StrFmpWaterCost = 'FmpWaterCost';
  StrFmpDeliveries = 'FmpDeliveries';
  StrFmpSemiRouteDeliv = 'FmpSemiRouteDeliv';
  StrFmpSemiRouteReturn = 'FmpSemiRouteReturn';
  StrFmpCall= 'FmpCall';
  StrCFP = 'CFP';
  StrCRCH = 'CRCH';
  StrCOC = 'COC';
  StrSWI = 'SWI2';
  StrSWI_Obs = 'SWI_Obs';
  StrSWI_Zeta = 'SWI_Zeta';

  StrSWR = 'SWR';
  StrSwrReachGroupStage = 'SWR_ReachGroupStage';
  StrSwrReachStage = 'SWR_ReachStage';
  StrSwrReachExchange = 'SWR_ReachExchange';
  StrSwrLateral = 'SWR_Lateral';
  StrSwrStructure = 'SWR_Structure';
  StrSwrTimeSteps = 'SWR_TimeSteps';
  StrSwrRiver = 'SWR_RIV';
  StrSwrObs = 'SWR_OBS';
  StrSwrDirectRunoff = 'SWR_DirectRunoff';
  StrSwrConvergenceHistory = 'SWR_ConvergenceHistory';
  StrMNW1 = 'MNW1';
  StrMnw1WellOutput = 'MNW1_WellOutput';
  StrMnw1ByNode = 'MNW1_ByNode';
  StrMnw1QSum = 'MNW1_QSum';

  StrMAW = 'MAW6';

  StrNPF= 'NPF6';
  StrRip = 'RIP';
  StrRipPlantGroupET = 'RipPlantGroupET';
  StrWBGT = 'WBGT';

  StrUzfRecharge = 'UzfRecharge';
  StrUzfDischarge = 'UzfDischarge';

  StrSubElasCompML = 'SubElasCompML';
  StrSubInelasCompML = 'SubInelasCompML';
  StrSubElasCompIB = 'SubElasCompIB';
  StrSubInelasCompIB = 'SubInelasCompIB';

  StrUzt = 'UZT2';
  StrIC = 'IC';
  StrOBS6 = 'OBS6';

  StrUZF6 = 'UZF6';

  Solvers: array[0..5] of string = (StrPCG, StrPCGN, StrGMG, StrSIP, StrDE4, StrNWT);
  FlowPackages: array[0..4] of string = (StrLPF, StrHUF2, StrBCF, StrUPW, StrNPF);


type
  TCustomUnitNumberItem = class(TCollectionItem)
  private
    FKey: string;
    FUnitNumber: integer;
    procedure SetKey(const Value: string);
    procedure SetUnitNumber(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Key: string read FKey write SetKey;
    property UnitNumber: integer read FUnitNumber write SetUnitNumber;
  end;

  TUnitNumberItem = class(TCustomUnitNumberItem)
  private
    FDescription: string;
    procedure SetDescription(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    property Description: string read FDescription write SetDescription;
  end;

  TExternalFileItem = class(TCustomUnitNumberItem)
  private
    FFileName: string;
    procedure SetFileName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

  TCustomUnitNumbers = class(TPhastCollection)
  private
    FAddedValue: integer;
  public
    function IndexOf(Key: string): integer;
    function UnitNumber(const Key: string): integer;
    // @name is added to the value that would otherwise be returned by
    // @link(UnitNumber).  It is used with MODFLOW-LGR child models
    // to ensure that unit numbers do not conflict between parent and
    // child models.
    property AddedValue: integer read FAddedValue write FAddedValue;
  end;

  // @name stores instances of @link(TUnitNumberItem).
  // Each @link(TUnitNumberItem) associates a key with a
  // unit number and description. The unit numbers can be used when
  // exporting MODFLOW input files.
  TUnitNumbers = class(TCustomUnitNumbers)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
    FSequentialUnitNumber: integer;
  private
    procedure CreateDefaultItems;
    procedure RemoveObsoleteItems;
  public
    procedure Initialize;
    function SequentialUnitNumber: Integer;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  // @name stores instances of @link(TExternalFileItem).
  // Each @link(TExternalFileItem) associates a key with a
  // unit number, description, and file name. The file names are
  // the names that will be used for external files that will
  // be included in the name file.
  TExternalFiles = class(TCustomUnitNumbers)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

const
  Mt3dList = 16;
  Mt3dBtn = 1;
  Mt3dFtl = 10;
  Mt3dAdv = 2;
  Mt3dDsp = 3;
  Mt3dSSM = 4;
  // The MT3D-USGS says the unit number for CTS is 6 but it is actually 20.
  mt3dCTS = 20;
  Mt3dUzt = 7;
  Mt3dRct = 8;
  Mt3dGcg = 9;
  Mt3dTob = 12;
  Mt3dCnf = 17;
  mt3dLKT = 18;
  mt3dSFT = 19;
  mt3dSftObs = 40;
  nt3dLktOut = 81;
  Mt3dUcnMobileStart = 201;
  Mt3dUcnImmobileStart = 301;
  Mt3dObsStart = 401;
  Mt3dMassStart = 601;

  Mt3dTob_inConcObs = 29;
  Mt3dTob_inFluxObs = 30;
  Mt3dTob_inSaveObs = 31;
  Mt3dFL6Cbc = 25;
  Mt3dFl6Bhd = 26;
  Mt3dDisGrb = 27;
  Mt3dCtsOut = 28;
  Mt3d_ISSGOUT = 32;


const
{
  unit numbers that were used previously.
  GlobalFileUnit = 10;
  SFR_PrintUnit = 30;
  UZF_GageStartUnit = 32;

  MODPATH reserves several numbers in the range
  80 to 99 for internal use.

  FMP3 uses unit numbers 1001-1011

  CFP uses unit number 999 and consecutive unit numbers beginning at 201
}
  CellFlowsUnitNumber = 9;
  // @name is the unit number in MODFLOW
  // on which the listing file will be printed.
  ListFileUnit = 11;
  DiscretizationUnit = 12;
  BasicUnit = 13;
  LPF_Unit = 14;
  ZoneUnit = 15;
  MultUnit = 16;
  CHD_Unit = 17;
  PCG_Unit = 18;
  GHB_Unit = 19;
  WEL_Unit = 20;
  RIV_Unit = 21;
  DRN_Unit = 22;
  DRT_Unit = 23;
  RCH_Unit = 24;
  EVT_Unit = 25;
  ETS_Unit = 26;
  RES_Unit = 27;
  LAK_Unit = 28;
  SFR_Unit = 29;
  UZF_Unit = 31;
  GMG_Unit = 33;
  GMG_HeadChangeUnit = 34;
  SIP_Unit = 35;
  DE4_Unit = 36;
  HeadDataUnit = 37;
  DrawdownDataUnit = 38;
  OC_Unit = 39;
  GAG_Unit = 40;
  HOB_Unit = 41;
  HOB_Output_Unit = 42;
  HFB_Unit = 43;
  DROB_Unit = 44;
  IUDROBSV = 45;
  GBOB_Unit = 46;
  IUGBOBSV = 47;
  RVOB_Unit = 48;
  IURVOBSV = 49;
  CHOB_Unit = 50;
  IUCHOBSV = 51;
  PVAL_Unit = 52;
  HUF_Unit = 53;
  IOHUFHEADS = 54;
  IOHUFFLOWS = 55;
  KDEP_Unit = 56;
  LVDA_Unit = 57;
  StreamFlows = 58;
  MNW2_Unit = 59;
  MNWI_Unit = 60;
  MNWI_Wells_Unit = 61;
  MNWI_SummarizeByWell_Unit = 62;
  MNWI_SummarizeByNode_Unit = 63;
  BcfUnit = 64;
  SubUnit = 65;
  SubUnitSave = 66;
  SubUnitRead = 67;
  SWT_Unit = 68;
  HYD_Unit = 69;

  // Unit numbers 70 - 95 are reserved for the user.
  // If the reserved unit numbers are changed, the documentation
  // for the MODFLOW Name File dialog box must be updated.

  // 100 In MT3DMS, this is equivalent to INTERNAL in MF2005.
  // 101 In MT3DMS, data arranged in block format
  // 102 In MT3DMS, data arranged in zone format
  // 103 In MT3DMS, data arranged in list-directed or free format.

{
  MT3DMS Input/Output Files       File Type     Preserved Unit
  Output Listing File*            LIST          16
  Package Options
  Basic Transport*                BTN           1
  Flow-Transport Link*            FTL           10
  Advection                       ADV           2
  Dispersion                      DSP           3
  Sink/Source Mixing              SSM           4
  Reaction                        RCT           8
  Generalized Conjugate Gradient  GCG           9
  Transport Observation           TOB           7
  HSS Time-Varying Source         HSS           13
  Output Files
  Model Configuration File        CNF           17
  Unformatted Concentration File
  (dissolved phase)
                                  UCN           200+species index
  Unformatted Concentration File
  (sorbed/immobile phase)
                                  UCN           300+species index
  Concentrations Observation File OBS           400+species index
  Mass Budget Summary File        MAS           600+species index
  *Note: these files are always required for every simulation.
}

  SubSUB_Out = 110;
  SubCOM_ML_Out = 111;
  SubCOM_IS_Out = 112;
  Sub_VD_Out = 113;
  Sub_NDPCH_Out = 114;
  Sub_DPCH_Out = 115;

  SwtSUB_Out = 116;
  SwtComML_Out = 117;
  SwtCOM_IS_Out = 118;
  Swt_VD_Out = 119;
  Swt_PreCon_Out = 120;
  Swt_DeltaPreCon_Out = 121;
  Swt_GeoStat_Out = 122;
  Swt_DeltaGeoStat_Out = 123;
  Swt_EffStress_Out = 124;
  Swt_DeltaEffStress_Out = 125;
  Swt_VoidRatio_Out = 126;
  Swt_ThickCompSed_Out = 127;
  Swt_LayerCentElev_Out = 128;

  Hydmod_Out = 129;
  BasInitialHeads = 130;

  BFH_Head = 131;
  BFH_Flux = 132;
  NWT_Unit = 133;
  UPW_Unit = 134;
  LMT6_Unit = 135;
  FTL_Unit = 136;
  PCGN_Unit = 137;
  PCGN_UNIT_PC = 138;
  PCGN_UNIT_TS = 139;
  PCGN_IPUNIT  = 140;

  NwtPhrampOut = 141;
  STR_UNIT = 142;
  STR_OutUNIT = 143;
  STOB_UNIT = 144;
  STOB_OutUnit = 145;
  FHB_Unit = 146;
  FMP_Unit = 147;
  FmpSupDem_Unit = 148;
  FmpFarmBudgetCompact_Unit = 149;
  FmpFarmBudgetDetailed_Unit = 150;
  FmpOFE_Unit = 151;
  FmpCID_Unit = 152;
  FmpRoot_Unit = 153;
  FmpCropUse_Unit = 154;
  FmpETR_Unit = 155;
  FmpETFrac_Unit = 156;
  FmpSwLosses_Unit = 157;
  FmpPFLX_Unit = 158;
  FmpCropFunc_Unit = 159;
  FmpWaterCost_Unit = 160;
  FmpDeliveries_Unit = 161;
  FmpSemiRouteDeliv_Unit = 162;
  FmpSemiRouteReturn_Unit = 163;
  FmpCall_Unit = 164;
  CFP_Unit = 165;
  CRCH_Unit = 166;
  COC_Unit = 167;
  SWI_Unit = 168;
  SWI_Obs_Unit = 169;
  SWI_Zeta_Unit = 170;
  SWR_Unit = 171;
  SWR_ReachGroupStage_Unit = 172;
  SWR_ReachStage_Unit = 173;
  SWR_ReachExchange_Unit = 174;
  SWR_Lateral_Unit = 175;
  SWR_Structure_Unit = 176;
  SWR_TimeSteps_Unit = 177;
  SWR_River_Unit = 178;
  SWR_Obs_Unit = 179;
  SWR_DirectRunoff_Unit = 180;
  SWR_ConvergenceHistory_Unit = 181;
  MNW1_Unit = 182;
  MNW1_WellOutputUnit = 183;
  MNW1_ByNodeUnit = 184;
  MNW1_QSumUnit = 185;
  FmpFID_Unit = 186;
  RIP_Unit = 187;
  RipPlantGroupUnit = 188;
  WBGT_Unit = 189;

  UZF_RechargeUnit = 190;
  UZF_DischargeUnit = 191;

  SubElasCompMLUnit = 192;
  SubInlasCompMLUnit = 193;
  SubElasCompIBUnit = 194;
  SubInlasCompIBUnit = 195;
  IC_Unit = 196;


  GageOutputStartUnit = 20205;

  ConcentrationStartUnit = 305;

implementation


{ TUnitNumbers }

procedure TUnitNumbers.Assign(Source: TPersistent);
begin
  inherited;
  CreateDefaultItems;
  RemoveObsoleteItems;
end;

constructor TUnitNumbers.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TUnitNumberItem, InvalidateModelEvent);
  CreateDefaultItems;
end;

procedure TUnitNumbers.CreateDefaultItems;
var
  Item: TUnitNumberItem;
  procedure AddItem(const Key: string; UnitNumber: integer;
    const Description: string = '');
  var
    Position: integer;
  begin
    Position:= IndexOf(Key);
    if Position < 0 then
    begin
      Item := Add as TUnitNumberItem;
      Item.Key := Key;
      Item.UnitNumber := UnitNumber;
    end
    else
    begin
      Item := Items[Position] as TUnitNumberItem;
    end;
    if Description = '' then
    begin
      Item.Description := Key;
    end
    else
    begin
      Item.Description := Description;
    end;
  end;
begin
  AddItem(StrLIST, ListFileUnit);
  AddItem(StrDIS, DiscretizationUnit);
  AddItem(StrCBC, CellFlowsUnitNumber, 'Cell-by-Cell Budget File');
  AddItem(StrBAS, BasicUnit);
  AddItem(StrLPF, LPF_Unit);
  AddItem(StrZONE, ZoneUnit);
  AddItem(StrMULT, MultUnit);
  AddItem(StrCHD, CHD_Unit);
  AddItem(StrPCG, PCG_Unit);
  AddItem(StrPCGN, PCGN_Unit);
  AddItem(StrNWT, NWT_Unit);
  AddItem(StrGHB, GHB_Unit);
  AddItem(StrWEL, WEL_Unit);
  AddItem(StrRIV, RIV_Unit);
  AddItem(StrDRN, DRN_Unit);
  AddItem(StrDRT, DRT_Unit);
  AddItem(StrRCH, RCH_Unit);
  AddItem(StrEVT, EVT_Unit);
  AddItem(StrETS, ETS_Unit);
  AddItem(StrRES, RES_Unit);
  AddItem(StrLAK, LAK_Unit);
  AddItem(StrSFR, SFR_Unit);
  AddItem(StrSTR, STR_Unit);
  AddItem(StrFHB, FHB_Unit);
  AddItem(StrUZF, UZF_Unit);
  AddItem(StrGMG, GMG_Unit);
  AddItem(StrIUNITMHC, GMG_HeadChangeUnit, 'GMG Head Change Unit');
  AddItem(StrSIP, SIP_Unit);
  AddItem(StrDE4, DE4_Unit);
  AddItem(StrHEAD, HeadDataUnit, 'Head output file');
  AddItem(StrDRAWDOWN, DrawdownDataUnit, 'Drawdown output file');
  AddItem(StrOC, OC_Unit);
  AddItem(StrGAG, GAG_Unit);
  AddItem(StrHOB, HOB_Unit);
  AddItem(StrIUHOBSV, HOB_Output_Unit, 'Head Observations output file');
  AddItem(StrHFB, HFB_Unit);
  AddItem(StrUNIT, GageOutputStartUnit, 'First Gage Unit');
  AddItem(StrDROB, DROB_Unit);
  AddItem(StrIUDROBSV, IUDROBSV);
  AddItem(StrGBOB, GBOB_Unit);
  AddItem(StrIUGBOBSV, IUGBOBSV);
  AddItem(StrRVOB, RVOB_Unit);
  AddItem(StrIURVOBSV, IURVOBSV);
  AddItem(StrCHOB, CHOB_Unit);
  AddItem(StrIUCHOBSV, IUCHOBSV);
  AddItem(StrPVAL, PVAL_Unit);
  AddItem(StrHUF2, HUF_Unit);
  AddItem(StrIOHUFHEADS, IOHUFHEADS);
  AddItem(StrIOHUFFLOWS, IOHUFFLOWS);
  AddItem(StrKDEP, KDEP_Unit);
  AddItem(StrLVDA, LVDA_Unit);
  AddItem(StrISTCB2, StreamFlows);
  AddItem(StrMNW2, MNW2_Unit);
  AddItem(StrMNWI, MNWI_Unit);
  AddItem(StrMNWI_Wells, MNWI_Wells_Unit);
  AddItem(StrMNWI_SummarizeByWell, MNWI_SummarizeByWell_Unit);
  AddItem(StrMNWI_SummarizeByNode, MNWI_SummarizeByNode_Unit);
  AddItem(StrBCF, BcfUnit);
  AddItem(StrSUB, SubUnit);
  AddItem(StrSUBSaveRestart, SubUnitSave);
  AddItem(StrSUBReadRestart, SubUnitRead);
  AddItem(StrHydmodOut, Hydmod_Out);
  AddItem(StrHYD, HYD_Unit);
  AddItem(StrSWT, SWT_Unit);



  AddItem(StrSubSUB_Out, SubSUB_Out);
  AddItem(StrSubCOM_ML_Out, SubCOM_ML_Out);
  AddItem(StrSubCOM_IS_Out, SubCOM_IS_Out);
  AddItem(StrSub_VD_Out, Sub_VD_Out);
  AddItem(StrSub_NDPCH_Out, Sub_NDPCH_Out);
  AddItem(StrSub_DPCH_Out, Sub_DPCH_Out);

  AddItem(StrSwtSUB_Out            ,SwtSUB_Out            );
  AddItem(StrSwtComML_Out          ,SwtComML_Out          );
  AddItem(StrSwtCOM_IS_Out         ,SwtCOM_IS_Out         );
  AddItem(StrSwt_VD_Out            ,Swt_VD_Out            );
  AddItem(StrSwt_PreCon_Out        ,Swt_PreCon_Out        );
  AddItem(StrSwt_DeltaPreCon_Out   ,Swt_DeltaPreCon_Out   );
  AddItem(StrSwt_GeoStat_Out       ,Swt_GeoStat_Out       );
  AddItem(StrSwt_DeltaGeoStat_Out  ,Swt_DeltaGeoStat_Out  );
  AddItem(StrSwt_EffStress_Out     ,Swt_EffStress_Out     );
  AddItem(StrSwt_DeltaEffStress_Out,Swt_DeltaEffStress_Out);
  AddItem(StrSwt_VoidRatio_Out     ,Swt_VoidRatio_Out     );
  AddItem(StrSwt_ThickCompSed_Out  ,Swt_ThickCompSed_Out  );
  AddItem(StrSwt_LayerCentElev_Out ,Swt_LayerCentElev_Out );

  AddItem(BAS_InitialHeads, BasInitialHeads);

  AddItem(BFH_Heads,  BFH_Head);
  AddItem(BFH_Fluxes, BFH_Flux);

  AddItem(StrUPW ,UPW_Unit);
  AddItem(StrLMT6, LMT6_Unit);
  AddItem(StrLMT7, LMT6_Unit);
  AddItem(StrFTL, FTL_Unit);

  AddItem(StrPHIRAMPOut, NwtPhrampOut);

  AddItem(StrSTR_OUT, STR_OutUNIT);

  AddItem(StrSTOB, STOB_UNIT);
  AddItem(StrIUSTOBSV, STOB_OutUnit);
  AddItem(StrFMP, FMP_Unit);
  AddItem(StrFmpSupplyDemand, FmpSupDem_Unit);
  AddItem(StrFmpFarmBudgetCompact, FmpFarmBudgetCompact_Unit);
  AddItem(StrFmpFarmBudgetDetailed, FmpFarmBudgetDetailed_Unit);
  AddItem(StrFmpOFE, FmpOFE_Unit);
  AddItem(StrFmpCID, FmpCID_Unit);
  AddItem(StrFmpFID, FmpFID_Unit);
  AddItem(StrFmpRoot, FmpRoot_Unit);
  AddItem(StrFmpCropUse, FmpCropUse_Unit);
  AddItem(StrFmpETR, FmpETR_Unit);
  AddItem(StrFmpETFrac, FmpETFrac_Unit);
  AddItem(StrFmpSwLosses, FmpSwLosses_Unit);
  AddItem(StrFmpPFLX, FmpPFLX_Unit);
  AddItem(StrFmpCropFunc, FmpCropFunc_Unit);
  AddItem(StrFmpWaterCost, FmpWaterCost_Unit);
  AddItem(StrFmpDeliveries, FmpDeliveries_Unit);
  AddItem(StrFmpSemiRouteDeliv, FmpSemiRouteDeliv_Unit);
  AddItem(StrFmpSemiRouteReturn, FmpSemiRouteReturn_Unit);
  AddItem(StrFmpCall, FmpCall_Unit);
  AddItem(StrCFP, CFP_Unit);
  AddItem(StrCRCH, CRCH_Unit);
  AddItem(StrCOC, COC_Unit);
  AddItem(StrSWI, SWI_Unit);
  AddItem(StrSWI_Obs, SWI_Obs_Unit);
  AddItem(StrSWI_Zeta, SWI_Zeta_Unit);
  AddItem(StrSWR, SWR_Unit);
  AddItem(StrSwrReachGroupStage, SWR_ReachGroupStage_Unit);
  AddItem(StrSwrReachStage, SWR_ReachStage_Unit);
  AddItem(StrSwrReachExchange, SWR_ReachExchange_Unit);
  AddItem(StrSwrLateral, SWR_Lateral_Unit);
  AddItem(StrSwrStructure, SWR_Structure_Unit);
  AddItem(StrSwrTimeSteps, SWR_TimeSteps_Unit);
  AddItem(StrSwrRiver, SWR_River_Unit);
  AddItem(StrSwrObs, SWR_Obs_Unit);
  AddItem(StrSwrDirectRunoff, SWR_DirectRunoff_Unit);
  AddItem(StrSwrConvergenceHistory, SWR_ConvergenceHistory_Unit);
  AddItem(StrMNW1, MNW1_Unit);
  AddItem(StrMnw1WellOutput, MNW1_WellOutputUnit);
  AddItem(StrMnw1ByNode, MNW1_ByNodeUnit);
  AddItem(StrMnw1QSum, MNW1_QSumUnit);
  AddItem(StrRIP, RIP_Unit);
  AddItem(StrRipPlantGroupET, RipPlantGroupUnit);
  AddItem(StrWBGT, WBGT_Unit);
  AddItem(StrUzfRecharge, UZF_RechargeUnit);
  AddItem(StrUzfDischarge, UZF_DischargeUnit);

  AddItem(StrSubElasCompML, SubElasCompMLUnit);
  AddItem(StrSubInelasCompML, SubInlasCompMLUnit);
  AddItem(StrSubElasCompIB, SubElasCompIBUnit);
  AddItem(StrSubInelasCompIB, SubInlasCompIBUnit);
  AddItem(StrIC, IC_Unit);
end;

procedure TUnitNumbers.Initialize;
begin
  FSequentialUnitNumber := -1;
end;

procedure TUnitNumbers.RemoveObsoleteItems;
  procedure RemoveItem(const Key: string);
  var
    Position: Integer;
  begin
    Position:= IndexOf(Key);
    if Position < 0 then
    begin
      Delete(Position);
    end;
  end;
begin
  // Currently there are no obsolete items so none are removed.
end;

function TUnitNumbers.SequentialUnitNumber: Integer;
begin
  if FSequentialUnitNumber < 0 then
  begin
    FSequentialUnitNumber := UnitNumber(StrUNIT);
  end
  else
  begin
    Inc(FSequentialUnitNumber);
  end;
  result := FSequentialUnitNumber;
end;

{ TCustomUnitNumberItem }

procedure TCustomUnitNumberItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomUnitNumberItem;
begin
  if Source is TCustomUnitNumberItem then
  begin
    SourceItem := TCustomUnitNumberItem(Source);
    Key := SourceItem.Key;
    UnitNumber := SourceItem.UnitNumber;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomUnitNumberItem.SetKey(const Value: string);
begin
  if FKey <> Value then
  begin
    (Collection as TPhastCollection).InvalidateModel;
    FKey := Value;
  end;
end;

procedure TCustomUnitNumberItem.SetUnitNumber(const Value: integer);
begin
  if FUnitNumber <> Value then
  begin
    (Collection as TPhastCollection).InvalidateModel;
    FUnitNumber := Value;
  end;
end;

{ TUnitNumberItem }

procedure TUnitNumberItem.Assign(Source: TPersistent);
var
  SourceItem: TUnitNumberItem;
begin
  if Source is TUnitNumberItem then
  begin
    SourceItem := TUnitNumberItem(Source);
    Description := SourceItem.Description;
    inherited;
  end;
end;

procedure TUnitNumberItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

{ TExternalFiles }

constructor TExternalFiles.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TExternalFileItem, InvalidateModelEvent);
end;

{ TExternalFileItem }

procedure TExternalFileItem.Assign(Source: TPersistent);
var
  SourceItem: TExternalFileItem;
begin
  if Source is TExternalFileItem then
  begin
    SourceItem := TExternalFileItem(Source);
    FileName := SourceItem.FileName;
  end;
  inherited;
end;

procedure TExternalFileItem.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    (Collection as TPhastCollection).InvalidateModel;
    FFileName := Value;
  end;
end;

{ TCustomUnitNumbers }

function TCustomUnitNumbers.IndexOf(Key: string): integer;
var
  Index: Integer;
  Item: TCustomUnitNumberItem;
begin
  Key := UpperCase(Key);
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomUnitNumberItem;
    if UpperCase(Item.Key) = Key then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

function TCustomUnitNumbers.UnitNumber(const Key: string): integer;
var
  Item: TCustomUnitNumberItem;
begin
  result := IndexOf(Key);
  if result >= 0 then
  begin
    Item := Items[result] as TCustomUnitNumberItem;
    result := Item.UnitNumber + AddedValue;
  end
  else
  begin
    Assert(False);
  end;
end;

end.
