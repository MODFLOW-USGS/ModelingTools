unit ExtensionTypeUnit;

interface

uses Classes, sysUtils, Generics.Collections, Generics.Defaults;

Type
  TExtensionType = (etModelInput, etModelOutput, etModpathInput,
  etModpathOutput, etZoneBudgetInput, etZoneBudgetOutput, etMt3dmsInput,
  etMt3dmsOutput,
{$IFDEF SwiObsExtractor}
  etSwiObsExtInput, etSwiObsExtOutput,
{$ENDIF}
  etOtherInput,
  etOtherOutput, etAncillary);

  TExtensionObject = Class(TObject)
    Extension: string;
    ExtensionType: TExtensionType;
    Description: string;
  end;

  TExtensionList = class (TObjectList<TExtensionObject>)
  public
    procedure SetDefaultExtensions;
    procedure SortRecords;
    procedure SortFunction;
    procedure SortDescription;
  end;

var
  ExtensionTypeNames: TStringList;

function ExtractFileExtendedExt(FileName: string): string;

implementation

function ExtractFileExtendedExt(FileName: string): string;
var
  AnExt: string;
begin
  result := '';
  repeat
    AnExt := ExtractFileExt(FileName);
    result := AnExt + result;
//    result := ExtractFileExt(FileName);
    FileName := ChangeFileExt(FileName, '');
  until ((AnExt = '') or AnsiSameText(AnExt, '.reference'));
end;

{ TExtensionList }

procedure TExtensionList.SetDefaultExtensions;
var
  ExtRec: TExtensionObject;
begin
  Clear;

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.nam';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.nam.archive';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mt_nam';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnm';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mt_nam.archive';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mto';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Observation Output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.jtf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'Jupiter Template file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dis';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Discretization file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bas';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Basic Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lpf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Layer Property Flow Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zon';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Zone Array input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mlt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Mulitplier Array input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.chd';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Time-Variant Specified Head Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pcg';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Preconditioned Conjugate Gradient Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ghb';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW General Head Boundary Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.wel';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Well Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.riv';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW River Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.drn';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Drain Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.drt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Drain-Return Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rch';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Recharge Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.evt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Evapotranspiration Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ets';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Evapotranspiration Segments Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.res';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Reservoir Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lak';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Lake Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sfr';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Streamflow Routing Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.uzf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Unsaturated Zone Flow Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.uzfg';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Unsaturated Zone Flow Package gage output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gmg';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Geometric Multigrid Flow Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sip';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Strongly Implicit Procedure Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.de4';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Direct Solver Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.oc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Output Control input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gag';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Gage Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_hob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Head Observation Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hfb';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Horizontal Flow Barrier Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.strt';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Starting Locations input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpm';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Main input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpbas';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Basic input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.tim';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Time input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mprsp';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Response input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mprsp.archive';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Response input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpsim';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Simulation input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpsim.archive';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODPATH Simulation input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.huf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Hydrogeologic Unit Flow Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.kdep';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Hydraulic-Conductivity Depth-Dependence Capability input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lvda';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Model-Layer Variable-Direction Horizontal Anisotropy Capability input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnw2';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Multi-Node Well Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bcf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Block-Centered Flow Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sub';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Subsidence and Aquifer-System Compaction Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zb_zones';
  ExtRec.ExtensionType := etZonebudgetInput;
  ExtRec.Description := 'ZONEBUDGET Zone input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zb_response';
  ExtRec.ExtensionType := etZonebudgetInput;
  ExtRec.Description := 'ZONEBUDGET Response file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zb_response.archive';
  ExtRec.ExtensionType := etZonebudgetInput;
  ExtRec.Description := 'ZONEBUDGET Response file';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.swt';
//  ExtRec.ExtensionType := etModelInput;
//  ExtRec.Description := 'MODFLOW Subsidence and Aquifer-System Compaction Package for Water-Table Aquifers input file';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hyd';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW HYDMOD Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hyd_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW HYDMOD Package output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lgr';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-LGR Local Grid Refinement input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lgr.archive';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-LGR Local Grid Refinement input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.nwt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-NWT Newton Solver Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.upw';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-NWT Upstream Weighting Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.btn';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Basic Transport Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.adv';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Advection Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dsp';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Dispersion Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ssm';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Sink and Source Mixing Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rct';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Chemical Reactions Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gcg';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Generalized Conjugate Gradient Solver Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.tob';
  ExtRec.ExtensionType := etMt3dmsInput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Transport Observation Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lmt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Link to MT3DMS or MT3D-USGS input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pcgn';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Preconditioned Conjugate Gradient Solver with Improved Nonlinear Control input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.FluxBcs';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Flow time-dependent sources and boundary conditions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.UFluxBcs';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA concentration or enerty time-dependent sources and boundary conditions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SPecPBcs';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Specified pressure time-dependent sources and boundary conditions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SPecUBcs';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Specified concentration or temperature time-dependent sources and boundary conditions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.8d';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA input file data set 8D';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.inp';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Main input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ics';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Initial Conditions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.str';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Stream Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fhb';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Flow and Head Boundary Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fmp';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ROOT';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 11';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SW_Losses';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 13';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.PSI';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 14';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ET_Func';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 15';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.TimeSeries';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 16';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.IFALLOW';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 17';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.CropFunc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 34';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.WaterCost';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 35';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.NonRouteDeliv';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 36';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SemiRouteDeliv';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 37a';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SemiRouteReturn';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 37b';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.CALL';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 39';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.FID';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 26';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.OFE';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 7';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.CID';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 28';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.CropUse';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 30a';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ETR';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 30b';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ET_Frac';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Farm Process input file data set 12';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cfp';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-CFP Conduit Flow Process input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swi';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Seawater Intrusion Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swr';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-NWT Surface-Water Routing Process input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnw1';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Multi-Node Well Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ic';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW 6 Initial Conditions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.npf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW 6 Node Property Flow package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sto';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW 6 Storage package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sms';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW 6 Sparse Matrix Solution package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.tdis';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW 6 Time-Discretization input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rip';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Riparean Evapotranspiration Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lkin';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Lake input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bcof';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Fluid Source Boundary Condition input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bcos';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Solute or Energy Source Boundary Condition input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bcop';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Specified Pressure input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bcou';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA Specified Concentration or Temperature input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fil';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA File Assignment input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fil.archive';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SUTRA File Assignment input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pval';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Parameter Value input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rch.R_Mult*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Recharge Multiplier Array';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.evt.ET_Mult*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Evapotranspiration Multiplier Array';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ets.ETS_Mult*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Evapotranspiration Segments Multiplier Array';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rch.R_Zone*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Recharge Zone Array';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.evt.ET_Zone*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Evapotranspiration Zone Array';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ets.ETS_Zone*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Evapotranspiration Segments Zone Array';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ftl';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW MT3DMS or MT3D-USGS Flow Transport Link output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lst';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Listing file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fhd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Formatted Head file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bhd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Binary Head file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fdn';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Formatted Drawdown file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bdn';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Binary Drawdown file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cbc';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Cell-By-Cell flow file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bud';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Cell-By-Cell flow file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.huf_fhd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW HUF Formatted Head file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.huf_bhd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW HUF Binary Head file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.huf_flow';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW HUF Flow File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Sub_Out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Combined SUB output file';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.Swt_Out';
//  ExtRec.ExtensionType := etModelOutput;
//  ExtRec.Description := 'MODFLOW Combined SWT output file';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SubSubOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SUB Subsidence output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SubComMlOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SUB Compaction by model layer output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SubComIsOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SUB Compaction by interbed system output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SubVdOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SUB Vertical displacement output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SubNdCritHeadOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SUB Critical head for no-delay interbeds output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SubDCritHeadOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SUB Critical head for delay interbeds output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtSubOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Subsidence output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtComMLOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Compaction by model layer output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtComIsOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Compaction by interbed system output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtVDOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Vertical displacement output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtPreConStrOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Preconsolidation stress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtDeltaPreConStrOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Change in preconsolidation stress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtGeoStatOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Geostatic stress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtDeltaGeoStatOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Change in geostatic stress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtEffStressOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Effective stress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtDeltaEffStressOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Change in effective stress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtVoidRatioOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Void ratio output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtThickCompSedOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Thickness of compressible sediments output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SwtLayerCentElevOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWT Layer-center elevation output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ucn';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Concentration output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zta';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Zeta Surface output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_ReachGroupFlows_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Reach Group Flows ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_ReachGroupFlows_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Reach Group Flows binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_ReachStage_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Reach Stage ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_ReachStage_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Reach Stage binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_ReachExchange_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Reach Exchange ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_ReachExchange_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Reach Exchange binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_LateralFlow_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Lateral Flow ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_LateralFlow_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Lateral Flow binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_StructureFlow_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Structure Flow ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_StructureFlow_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Structure Flow binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_TimeStepLength_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Time Step Length ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_TimeStepLength_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Time Step Length binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_Convergence';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Convergence output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_RIV';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW River package file created by the SWR Package';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_Obs_A';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Observation ASCII output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_Obs_B';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Observation binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.Swr_DirectRunoff';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWR Direct Runoff output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.nod';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA Node output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ele';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA Element output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.OUT';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA main output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.FDS_BIN';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Farm Process Demand and Supply binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.FB_COMPACT_BIN_OUT';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Farm Process Farm Compact Budget binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.FB_DETAILS_BIN_OUT';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Farm Process Farm Detailed Budget binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.PFLX';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Farm Process Precipitation input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fpi';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'WellFootprint input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fplst';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'WellFootprint listing file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fpb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'WellFootprint binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fpt';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'WellFootprint text output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bfh_head';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-LGR Boundary Flow and Head Package file for heads';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bfh_flux';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-LGR Boundary Flow and Head Package file for flows';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.csv';
  ExtRec.ExtensionType := etZonebudgetOutput;
  ExtRec.Description := 'ZONEBUDGET Comma Separated Values file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.2.csv';
  ExtRec.ExtensionType := etZonebudgetOutput;
  ExtRec.Description := 'ZONEBUDGET Comma Separated Values file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hob_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Head Observations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_chob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Specified Heads input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gbob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Observations of Flow at General Head Boundaries input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_rvob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Rivers input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_drob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Drains input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_stob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Streams input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rvob_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Rivers output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.chob_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Specified Heads output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gbob_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Observations of Flow at General Head Boundaries output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.drob_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Drains output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.stob_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Observations of Flow at Streams output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.obs';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA Observation Output file in OBS format';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.obc';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA Observation Output file in OBC format';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rst';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA Restart file or MODFLOW SUB package restart file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.smy';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'SUTRA Simulation Progress file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zb.lst';
  ExtRec.ExtensionType := etZonebudgetOutput;
  ExtRec.Description := 'ZONEBUDGET Listing File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.zblst';
  ExtRec.ExtensionType := etZonebudgetOutput;
  ExtRec.Description := 'ZONEBUDGET Listing File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gsf';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'PEST Grid Specification file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gpt';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'ModelMuse text file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gpb';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'ModelMuse binary file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mmZLib';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'ModelMuse compressed binary file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.axml';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'ModelMuse Archive Information file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.reference';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'Georeference file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.shp';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile shapes file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.shx';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile shape index file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dbf';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile attribute database';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.prj';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Projection file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sbn';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile spatial index of the features';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sbx';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile spatial index of the features';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fbn';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile spatial index of the read-only features';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.fbx';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile spatial index of the read-only features';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ain';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile attribute index of the active fields in a table';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.aix';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile attribute index of the active fields in a table';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.aix';
//  ExtRec.ExtensionType := etModpathInput;
//  ExtRec.Description := 'Shapefile geocoding index for read-write datasets';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mxs';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile geocoding index for read-write datasets (ODB format)';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.atx';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile an attribute index for the .dbf file in the form of shapefile.columnname.atx (ArcGIS 8 and later)';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.shp.xml';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile geospatial metadata in XML format, such as ISO 19115 or other XML schema';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cpg';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile used to specify the code page (only for .dbf) for identifying the character encoding to be used';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.qix';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Shapefile an alternative quadtree spatial index used by MapServer and GDAL/OGR software';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpn';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpn.archive';
  ExtRec.ExtensionType := etModpathInput;
  ExtRec.Description := 'MODPATH Name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.end';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Endpoint file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.end_bin';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Binary Endpoint file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.path';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Pathline file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.path_bin';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Binary Pathline file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ts';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Timeseries file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ts_bin';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Binary Timeseries file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cbf';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Budget file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mplst';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Listing file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.log';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH log file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mls';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS listing file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '._mas';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Mass Budget Output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dat';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'PHAST thermodynamic database input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.trans.dat';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'PHAST transport input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.chem.dat';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'PHAST chemical-reactions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.chem.dat';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST chemical-reactions output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.head.dat';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.h5';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST data output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.kd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST fluid and solute-dispersive conductance distributions output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.head';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST potentiometric head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.xyz.head';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST potentiometric head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.comps';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST component concentration distributions output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.xyz.comps';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST component concentration distributions output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.chem';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST selected chemical information output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.xyz.chem';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST selected chemical information output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.vel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST velocity distribution output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.xyz.vel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST velocity distribution output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.wel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST well data output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.xyz.wel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST well data output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.bal';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST regional fluid-flow and solute-flow rates and the regional cumulative-flow results output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.bcf';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST fluid and solute flow rates through boundaries output file';
  Add(ExtRec);


  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cnf';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Grid Configuration output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.O.probdef';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST Flow and transport problem definition output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST Selected Output';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sfrg*';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SFR Gage output';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lakg*';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW LAK Gage output';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.restart.gz';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST restart file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.restart.backup.gz';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'PHAST backup restart file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.advobs';
  ExtRec.ExtensionType := etModpathOutput;
  ExtRec.Description := 'MODPATH Advection Observation file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.UzfRch';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW UZF Recharge output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.UzfDisch';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW UZF Discharge output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.wel_tab*';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-NWT Well tab file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gmgout';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW GMG output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.wbgt';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-OWHM water budget summary file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwi';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW MNWI package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwi_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW MNWI package output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.wel_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Well package input file created by MNWI package';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.QSUM_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW MNWI package flow rates by well output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.BYND_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW MNWI package flow rates by node output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pcgn_pc';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW PCGN package progress output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pcgn_ts';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW PCGN package solver time output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pcgn_ip';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW PCGN package Picard iterations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.coc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW CFP Conduit Output Control File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.crch';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW CFP Conduit Recharge File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swi_obs';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW SWI Observation File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rip_et';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-OWM RIP Package location, land-surface elevation, and transpiration rates for each plant functional subgroup';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mfx';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Mass-Flux Observation text output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pst';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS Concentration and Mass-Flux Observation binary output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ocn';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS Calculated Concentrations and Residuals Observation text output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.str_flow';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Stream (STR) Routed Flow Output File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.wel_dewater.txt';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-NWT Well Package Dewatered Well Output File';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mmb';
  ExtRec.ExtensionType := etAncillary;
  ExtRec.Description := 'Argus ONE project file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-2000 batch input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rpt';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 batch report file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mass';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3DMS or MT3D-USGS mass balance summary file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.GSURF';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Ground surface file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SID';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Soil ID file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.SoilProp';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-OWHM Soil properties file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bat';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'Batch file used to run a model';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bat.archive';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'Batch file used to run a model';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.lsg';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 Global output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sor';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-2000 Slice-Successive Overrelaxation input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gwt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Transport name file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sen';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-2000 Sensitivity input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.seb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 Binary Sensitivity output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sea';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 ASCII Sensitivity output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pes';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-2000 Parameter Estimation input file';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.prt';
//  ExtRec.ExtensionType := etModelOutput;
//  ExtRec.Description := 'MODFLOW-2000 ASCII Sensitivity output file';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.oad';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-2000 Advection Observations input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.prt';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 Advection Observations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.moc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Transport input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.crc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Recharge Concentration input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cn2';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT ASCII concentration output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cna';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT ASCII concentration output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cnb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Binary concentration output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.vla';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT ASCII velocity output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.vlb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Binary velocity output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pta';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT ASCII particle output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ptb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Binary particle output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.age';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Age input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dp';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Double Porosity input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dk';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Simple Reactions input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Obsesrvation input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.oba';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Obsesrvation output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ibs';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-2000 Interbed Storage input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.iss';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 Interbed Storage subsidence output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.isc';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 Interbed Storage compaction output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ish';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 Interbed Preconsolidation Head output file';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.hyd';
//  ExtRec.ExtensionType := etModelInput;
//  ExtRec.Description := 'MODFLOW Hydmod Input file';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hyo';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Hydmod Input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.chfb';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Horizontal Flow Barrier Transport Properties input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hhd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Formatted HUF Head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hbh';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Binary HUF Head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwiOut';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'Multi-Node well information package output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwiWel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'Multi-Node well information package Well output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwiQsum';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'Multi-Node well information package Flow output file summarized by well';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwiByNd';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'Multi-Node well information package Flow output file summarized by node';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.wl1';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'Multi-Node Well package Well output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.byn';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'BYNODE output file from MNW package';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.qsu';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'QSUM output file from MNW package';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.bflx';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Boundary flux input file';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.lvda';
//  ExtRec.ExtensionType := etModelInput;
//  ExtRec.Description := 'MODFLOW Layer Variable Directional Anisotropy input file';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hflw';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW HUF package interpolated flows output file';
  Add(ExtRec);

//  ExtRec := TExtensionObject.Create;
//  ExtRec.Extension := '.kdep';
//  ExtRec.ExtensionType := etModelInput;
//  ExtRec.Description := 'MODFLOW Depth dependant hydraulic conductivity input file';
//  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.subo';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Subsidence package output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.subr';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Subsidence package restart file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.vdf';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SEAWAT Variable Density Flow input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gmgmhc';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Maximum head changed for Geometeric Multigrid package output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ipda';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Initial Particle Density File-Array-Based input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ipa';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Initial Particle Density File-Array-Based input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ipdl';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Initial Particle Density File-List-Based input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ipl';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Initial Particle Density File-List-Based input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.cbdy';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Concentration Boundary input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.decvar';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Decision Variable input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.objfnc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Objective Function input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.varcon';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Variable Constraints input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sumcon';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Summation Constraints input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.hedcon';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Head Constraints input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.strmcon';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Stream Constraints input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.stavar';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM State Variables input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.soln';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM Solution and Output control input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.rsp';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWM Response matrix output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.msp';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWM Mathematical Programming System output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.gwm';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM GWM input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwo';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWM MNW1 observations input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mnwo_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWM MNW1 observations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ptob';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Particle observations input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.pto';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Particle observations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mpto';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Multinode Well Particle observations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.sstr';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Starting Stress Period input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.prtp';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT PTRP input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.GWMWFILE.wel';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Well Package input file created by MODFLOW-GWM';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.uzfo';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Unsaturated Zone Flow output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.isgout';
  ExtRec.ExtensionType := etMt3dmsOutput;
  ExtRec.Description := 'MT3D MNW concentrations output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swt';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW Subsidence and Aquifer-System Compaction Package for Water-Table Aquifers Input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swt_out';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW Subsidence and Aquifer-System Compaction Package for Water-Table Aquifers Output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.vsc';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'SEAWAT Viscosity Package input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mbrp';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Generalized Mass Balance output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.mbit';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Generalized and Detailed Mass Balance output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ccbd';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW-GWT Constant Concentration input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.vbal';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-GWT Volume Balancing output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.p00';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW-2000 error output file';
  Add(ExtRec);

{$IFDEF SwiObsExtractor}
  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swi_obsi';
  ExtRec.ExtensionType := etSwiObsExtInput;
  ExtRec.Description := 'SWI Observation Extractor input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swi_obsi.archive';
  ExtRec.ExtensionType := etSwiObsExtInput;
  ExtRec.Description := 'SWI Observation Extractor input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.swi_obsi_out';
  ExtRec.ExtensionType := etSwiObsExtOutput;
  ExtRec.Description := 'SWI Observation Extractor output file';
  Add(ExtRec);
{$ENDIF}

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw';
  ExtRec.ExtensionType := etModelInput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility groundwater input file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw_out_head';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility text groundwater head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw_out_head.bin';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility binary groundwater head output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw_out_drawdown.csv';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility text groundwater drawdown output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw_out_drawdown.bin';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility binary groundwater drawdown output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw_out_flow-ja-face.csv';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility text groundwater flow observation output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.ob_gw_out_flow-ja-face.bin';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 Observation Utility binary groundwater flow observation output file';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.dis.grb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 binary grid output file for structured grid model';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.disv.grb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 binary grid output file for discretization with vertices model';
  Add(ExtRec);

  ExtRec := TExtensionObject.Create;
  ExtRec.Extension := '.disu.grb';
  ExtRec.ExtensionType := etModelOutput;
  ExtRec.Description := 'MODFLOW 6 binary grid output file for unstructured grid model';
  Add(ExtRec);

  SortRecords;
end;

procedure TExtensionList.SortDescription;
begin
  Sort(TComparer<TExtensionObject>.Construct(function
    (const Left, Right: TExtensionObject): Integer
  begin
    Result := AnsiCompareText(Left.Description,Right.Description);
  end));
end;

procedure TExtensionList.SortFunction;
begin
  Sort(TComparer<TExtensionObject>.Construct(function
    (const Left, Right: TExtensionObject): Integer
  begin
    Result := Ord(Left.ExtensionType) - Ord(Right.ExtensionType);
    if result = 0 then
    begin
      Result := AnsiCompareText(Left.Extension,Right.Extension);
    end;
  end));
end;

procedure TExtensionList.SortRecords;
begin
  Sort(TComparer<TExtensionObject>.Construct(function
    (const Left, Right: TExtensionObject): Integer
  begin
    Result := AnsiCompareText(Left.Extension,Right.Extension);
  end));
end;

initialization

  ExtensionTypeNames := TStringList.Create;
  ExtensionTypeNames.Add('Model input');
  ExtensionTypeNames.Add('Model output');
  ExtensionTypeNames.Add('Modpath input');
  ExtensionTypeNames.Add('Modpath output');
  ExtensionTypeNames.Add('Zonebudget input');
  ExtensionTypeNames.Add('Zonebudget output');
  ExtensionTypeNames.Add('MT3D input');
  ExtensionTypeNames.Add('MT3D output');
{$IFDEF SwiObsExtractor}
  ExtensionTypeNames.Add('Swi Observation Extractor input');
  ExtensionTypeNames.Add('Swi Observation Extractor output');
{$ENDIF}
  ExtensionTypeNames.Add('Other input');
  ExtensionTypeNames.Add('Other output');
  ExtensionTypeNames.Add('Ancillary');

  Assert(ExtensionTypeNames.Count = Ord(High(TExtensionType))+1);

finalization
  ExtensionTypeNames.Free
end.
