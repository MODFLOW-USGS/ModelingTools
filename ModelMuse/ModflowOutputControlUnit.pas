unit ModflowOutputControlUnit;

interface

uses SysUtils, Classes, GoPhastTypes, Mt3dmsTimesUnit;

type
  TCellSaveFormat = (csfNone, csfBinary, csfListing, csfBoth);
  TFrequencyChoice = (fcTimeSteps, fcStressPeriods);
  TOutputFileType = (oftText, oftBinary);
  TWrapping = (wStrip, wWrap);

  TExtFormatPrefix = (efpNone, efp1P);
  TNumberFormat = (nfF, nfD, nfE, nfEN, nfEs, nfG);

  TPrintFormat = (nf11G_10_3, nf9G_13_6, nf15F_7_1, nf15F_7_2, nf15F_7_3,
    nf15F_7_4, nf20F_5_0, nf20F_5_1, nf20F_5_2, nf20F_5_3, nf20F_5_4,
    nf10G_11_4, nf10F_6_0, nf10F_6_1, nf10F_6_2, nf10F_6_3, nf10F_6_4);

  // used in MF-6;
  TPrintNumberType = (pntExponential, pntFixed, pntGeneral, pntScientific);

  TOutputSuppression = (osNoOutput, osShowError, osShowErrorWarning, osShowAll);

  TExternalFormat = class(TGoPhastPersistent)
  private
    FNumberFormat: TNumberFormat;
    FExtFormatPrefix: TExtFormatPrefix;
    FWidth: integer;
    FDecimals: integer;
    procedure SetDecimals(const Value: integer);
    procedure SetExtFormatPrefix(const Value: TExtFormatPrefix);
    procedure SetNumberFormat(const Value: TNumberFormat);
    procedure SetWidth(const Value: integer);
    procedure Initialize;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    function FullFormat: string;
  published
    property ExtFormatPrefix: TExtFormatPrefix read FExtFormatPrefix
      write SetExtFormatPrefix;
    property NumberFormat: TNumberFormat read FNumberFormat
      write SetNumberFormat;
    property Width: integer read FWidth write SetWidth;
    property Decimals: integer read FDecimals write SetDecimals;
  end;

  // @name is used to control the output of heads and drawdowns.
  THeadDrawdownOutputControl = class(TGoPhastPersistent)
  private
    FWrapping: TWrapping;
    FSaveInExternalFile: boolean;
    FPrintInListing: boolean;
    FPrintFormat: TPrintFormat;
    FExternalFormat: TExternalFormat;
    FOutputFileType: TOutputFileType;
    FFrequency: integer;
    FFrequencyChoice: TFrequencyChoice;
    procedure SetExternalFormat(const Value: TExternalFormat);
    procedure SetFrequency(const Value: integer);
    procedure SetFrequencyChoice(const Value: TFrequencyChoice);
    procedure SetOutputFileType(const Value: TOutputFileType);
    procedure SetPrintFormat(const Value: TPrintFormat);
    procedure SetPrintInListing(const Value: boolean);
    procedure SetSaveInExternalFile(const Value: boolean);
    procedure SetWrapping(const Value: TWrapping);
    procedure Initialize;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    Destructor Destroy; override;
    function PrintCode: integer;
    function FormatDefined: boolean;
  published
    property ExternalFormat: TExternalFormat read FExternalFormat
      write SetExternalFormat;
    property Frequency: integer read FFrequency write SetFrequency;
    property FrequencyChoice: TFrequencyChoice read FFrequencyChoice
      write SetFrequencyChoice;
    property OutputFileType: TOutputFileType read FOutputFileType
      write SetOutputFileType;
    property PrintFormat: TPrintFormat read FPrintFormat write SetPrintFormat;
    property PrintInListing: boolean read FPrintInListing
      write SetPrintInListing;
    property SaveInExternalFile: boolean read FSaveInExternalFile
      write SetSaveInExternalFile;
    property Wrapping: TWrapping read FWrapping write SetWrapping;
  end;

  TModflowOutputControl = class(TGoPhastPersistent)
  private
    FPrintInputArrays: boolean;
    FSaveCellFlows: TCellSaveFormat;
    FPrintInputCellLists: boolean;
    FCompact: boolean;
    FDrawdownOC: THeadDrawdownOutputControl;
    FHeadOC: THeadDrawdownOutputControl;
    FComments: TStrings;
    FBudgetFrequencyChoice: TFrequencyChoice;
    FBudgetFrequency: integer;
    FPrintObservations: boolean;
    FOutputSuppression: TOutputSuppression;
    FSaveBudgetSummary: boolean;
    FConcentrationOC: THeadDrawdownOutputControl;
    FSaveBudgetCSV: Boolean;
    FExportArrays: Boolean;
    procedure SetPrintInputArrays(const Value: boolean);
    procedure SetSaveCellFlows(const Value: TCellSaveFormat);
    procedure SetPrintInputCellLists(const Value: boolean);
    procedure SetCompact(const Value: boolean);
    procedure SetDrawdownOC(const Value: THeadDrawdownOutputControl);
    procedure SetHeadOC(const Value: THeadDrawdownOutputControl);
    procedure SetComments(const Value: TStrings);
    procedure SetBudgetFrequency(const Value: integer);
    procedure SetBudgetFrequencyChoice(const Value: TFrequencyChoice);
    procedure SetPrintObservations(const Value: boolean);
    procedure SetOutputSuppression(const Value: TOutputSuppression);
    procedure SetSaveBudgetSummary(const Value: boolean);
    procedure SetConcentrationOC(const Value: THeadDrawdownOutputControl);
    procedure SetSaveBudgetCSV(const Value: Boolean);
    procedure SetExportArrays(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    Destructor Destroy; override;
    procedure Initialize;
    function FormattedOutputUsed: Boolean;
  published
    // @unorderedList(
    // @item(IPRN in Array reading utility module)
    // @item(PROPRINT option in Pval file for MODFLOW-OWHM)
    // @item(IPTFLG in SWR)
    // @item(PRINTCOORD in DIS)
    // )
    property PrintInputArrays: boolean read FPrintInputArrays
      write SetPrintInputArrays default True;
    // @name is used to set a number of variables in different packages.
    // @unorderedList(
    // @item(IRDFLG and IPTFLG in STR)
    // @item(IFHBPT in FHB)
    // @item(NOPRINT in FMP)
    // @item(IPTFLG in SWR)
    // @item(IWELPTin MNW1)
    // @item(NOPRINT in @link(TCustomListWriter))
    // @item(IRDFLG in SFR)
    // )
    property PrintInputCellLists: boolean read FPrintInputCellLists
      write SetPrintInputCellLists default True;
    // NOPRINT option in CHOB, DROB, GBOB, HOB, RBOB, and STOB.
    // DTOB does not include the NOPRINT option.
    property PrintObservations: boolean read FPrintObservations
      write SetPrintObservations default True;
    { TODO -cMODFLOW 6-2015 : SaveCellFlows is different in MODFLOW 6. You can both save and print values. }
    // @name is used in @link(TCustomModflowWriter.GetFlowUnitNumber)
    property SaveCellFlows: TCellSaveFormat read FSaveCellFlows
      write SetSaveCellFlows default csfBinary;
    // @name controls "COMPACT BUDGET AUXILIARY"
    property Compact: boolean read FCompact write SetCompact;
    property HeadOC: THeadDrawdownOutputControl read FHeadOC write SetHeadOC;
    property DrawdownOC: THeadDrawdownOutputControl read FDrawdownOC
      write SetDrawdownOC;
    property Comments: TStrings read FComments write SetComments;
    // helps control SAVE BUDGET and PRINT BUDGET in Output Control
    property BudgetFrequency: integer read FBudgetFrequency
      write SetBudgetFrequency;
    // helps control SAVE BUDGET and PRINT BUDGET in Output Control
    property BudgetFrequencyChoice: TFrequencyChoice read FBudgetFrequencyChoice
      write SetBudgetFrequencyChoice;
    // LSTLVL in Name file (MODFLOW-OWHM).
    property OutputSuppression: TOutputSuppression read FOutputSuppression
      write SetOutputSuppression default osShowAll;
    // WBGT in OC file (MODFLOW-OWHM).
    property SaveBudgetSummary: boolean read FSaveBudgetSummary
      write SetSaveBudgetSummary default True;
    property ConcentrationOC: THeadDrawdownOutputControl read FConcentrationOC
      write SetConcentrationOC;
    // BUDGETCSV FILEOUT
    Property SaveBudgetCSV: Boolean read FSaveBudgetCSV write SetSaveBudgetCSV;
    // EXPORT_ARRAY_ASCII
    property ExportArrays: Boolean read FExportArrays write SetExportArrays;
  end;

  TMt3dmsOutputFreq = (mofSpecifiedTimes, mofEndOfSimulation, mofPeriodic);

  TMt3dmsOutputControl = class(TGoPhastPersistent)
  private
    FOutputFreqChoice: TMt3dmsOutputFreq;
    FSaveConcentrations: boolean;
    FPeriodicOutputCount: integer;
    FOutputTimes: TMt3dmsOutputTimeCollection;
    FObservationFrequency: integer;
    FMassBalanceFrequency: integer;
    FSummarizeMassBalance: boolean;
    procedure SetOutputFreqChoice(const Value: TMt3dmsOutputFreq);
    procedure SetOutputTimes(const Value: TMt3dmsOutputTimeCollection);
    procedure SetPeriodicOutputCount(const Value: integer);
    procedure SetSaveConcentrations(const Value: boolean);
    procedure SetMassBalanceFrequency(const Value: integer);
    procedure SetObservationFrequency(const Value: integer);
    procedure SetSummarizeMassBalance(const Value: boolean);
  public
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize;
  published
    // SAVUCN
    property SaveConcentrations: boolean read FSaveConcentrations
      write SetSaveConcentrations;
    // NPRS
    property OutputFreqChoice: TMt3dmsOutputFreq read FOutputFreqChoice
      write SetOutputFreqChoice;
    // NPRS
    property PeriodicOutputCount: integer read FPeriodicOutputCount
      write SetPeriodicOutputCount;
    // NPRS, TIMPRS
    property OutputTimes: TMt3dmsOutputTimeCollection read FOutputTimes
      write SetOutputTimes;
    // NPROBS
    property ObservationFrequency: integer read FObservationFrequency
      write SetObservationFrequency;
    // NPRMAS
    property MassBalanceFrequency: integer read FMassBalanceFrequency
      write SetMassBalanceFrequency;
    // CHKMAS
    property SummarizeMassBalance: boolean read FSummarizeMassBalance
      write SetSummarizeMassBalance;
  end;


implementation



{ TModflowOutputControl }

procedure TModflowOutputControl.Assign(Source: TPersistent);
var
  SourceOutputControl: TModflowOutputControl;
begin
  if Source is TModflowOutputControl then
  begin
    SourceOutputControl := TModflowOutputControl(Source);
    PrintInputArrays := SourceOutputControl.PrintInputArrays;
    SaveCellFlows := SourceOutputControl.SaveCellFlows;
    PrintObservations := SourceOutputControl.PrintObservations;
    Compact := SourceOutputControl.Compact;
    HeadOC := SourceOutputControl.HeadOC;
    DrawdownOC := SourceOutputControl.DrawdownOC;
    Comments := SourceOutputControl.Comments;
    BudgetFrequency := SourceOutputControl.BudgetFrequency;
    BudgetFrequencyChoice := SourceOutputControl.BudgetFrequencyChoice;
    PrintInputCellLists := SourceOutputControl.PrintInputCellLists;
    OutputSuppression := SourceOutputControl.OutputSuppression;
    SaveBudgetSummary := SourceOutputControl.SaveBudgetSummary;
    ConcentrationOC := SourceOutputControl.ConcentrationOC;
    SaveBudgetCSV := SourceOutputControl.SaveBudgetCSV;
    ExportArrays := SourceOutputControl.ExportArrays;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowOutputControl.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FHeadOC := THeadDrawdownOutputControl.Create(InvalidateModelEvent);
  FDrawdownOC := THeadDrawdownOutputControl.Create(InvalidateModelEvent);
  FConcentrationOC := THeadDrawdownOutputControl.Create(InvalidateModelEvent);
  FComments := TStringList.Create;
  Initialize;
end;

destructor TModflowOutputControl.Destroy;
begin
  FComments.Free;
  FHeadOC.Free;
  FDrawdownOC.Free;
  FConcentrationOC.Free;
  inherited;
end;

function TModflowOutputControl.FormattedOutputUsed: Boolean;
begin
  result := (HeadOC.SaveInExternalFile and (HeadOC.OutputFileType = oftText))
    or (DrawdownOC.SaveInExternalFile and (DrawdownOC.OutputFileType = oftText));
end;

procedure TModflowOutputControl.Initialize;
begin
  FBudgetFrequency := 1;
  FBudgetFrequencyChoice := fcTimeSteps;
  FPrintInputArrays := True;
  FPrintInputCellLists := True;
  FPrintObservations := True;
  FSaveCellFlows := csfBinary;
  FOutputSuppression := osShowAll;
  FCompact := True;
  FSaveBudgetSummary := True;
  FComments.Clear;
  HeadOc.Initialize;
  DrawdownOC.Initialize;
  FSaveBudgetCSV := False;
  FExportArrays := False;
end;

procedure TModflowOutputControl.SetBudgetFrequency(const Value: integer);
begin
  if FBudgetFrequency <> Value then
  begin
    Assert(Value >= 1);
    FBudgetFrequency := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetBudgetFrequencyChoice(
  const Value: TFrequencyChoice);
begin
  if FBudgetFrequencyChoice <> Value then
  begin
    FBudgetFrequencyChoice := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetComments(const Value: TStrings);
var
  Changed: boolean;
  LineIndex: Integer;
begin
  Changed := FComments.Count <> Value.Count;
  if not Changed then
  begin
    for LineIndex := 0 to FComments.Count - 1 do
    begin
      Changed := FComments[LineIndex] <> Value[LineIndex];
      if Changed then
      begin
        break;
      end;
    end;
  end;
  if Changed then
  begin
    InvalidateModel;
    FComments.Assign(Value);
  end;
end;

procedure TModflowOutputControl.SetCompact(const Value: boolean);
begin
  if FCompact <> Value then
  begin
    FCompact := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetConcentrationOC(
  const Value: THeadDrawdownOutputControl);
begin
  FConcentrationOC.Assign(Value);
end;

procedure TModflowOutputControl.SetDrawdownOC(
  const Value: THeadDrawdownOutputControl);
begin
  FDrawdownOC.Assign(Value);
end;

procedure TModflowOutputControl.SetExportArrays(const Value: Boolean);
begin
  if FExportArrays <> Value then
  begin
    FExportArrays := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetHeadOC(
  const Value: THeadDrawdownOutputControl);
begin
  FHeadOC.Assign(Value);
end;

procedure TModflowOutputControl.SetOutputSuppression(
  const Value: TOutputSuppression);
begin
  if FOutputSuppression <> Value then
  begin
    FOutputSuppression := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetPrintInputArrays(const Value: boolean);
begin
  if FPrintInputArrays <> Value then
  begin
    FPrintInputArrays := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetPrintInputCellLists(const Value: boolean);
begin
  if FPrintInputCellLists <> Value then
  begin
    FPrintInputCellLists := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetPrintObservations(const Value: boolean);
begin
  if FPrintObservations <> Value then
  begin
    FPrintObservations := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetSaveBudgetCSV(const Value: Boolean);
begin
  if FSaveBudgetCSV <> Value then
  begin
    FSaveBudgetCSV := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetSaveBudgetSummary(const Value: boolean);
begin
  if FSaveBudgetSummary <> Value then
  begin
    FSaveBudgetSummary := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetSaveCellFlows(const Value: TCellSaveFormat);
begin
  if FSaveCellFlows <> Value then
  begin
    FSaveCellFlows := Value;
    InvalidateModel;
  end;
end;

{ TExternalFormat }

procedure TExternalFormat.Assign(Source: TPersistent);
var
  SourceFormat: TExternalFormat;
begin
  if Source is TExternalFormat then
  begin
    SourceFormat := TExternalFormat(Source);
    ExtFormatPrefix := SourceFormat.ExtFormatPrefix;
    NumberFormat := SourceFormat.NumberFormat;
    Width := SourceFormat.Width;
    Decimals := SourceFormat.Decimals;
  end
  else
  begin
    inherited;
  end;
end;

constructor TExternalFormat.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
//  if Model = nil then
//  begin
//    inherited Create(nil);
//  end
//  else
//  begin
//    inherited Create(Model.Invalidate);
//  end;
  Initialize;
end;

function TExternalFormat.FullFormat: string;
begin
  result := '(10(1X';
  case ExtFormatPrefix of
    efpNone: ;
    efp1P: result := result + '1P';
    else Assert(False);
  end;
  case NumberFormat of
    nfF: result := result + 'F';
    nfD: result := result + 'D';
    nfE: result := result + 'E';
    nfEN: result := result + 'EN';
    nfEs: result := result + 'Es';
    nfG: result := result + 'G';
    else Assert(False);
  end;
  result := result + IntToStr(Width) + '.' + IntToStr(Decimals) + '))';
end;

procedure TExternalFormat.Initialize;
begin
  FNumberFormat := nfE;
  FExtFormatPrefix := efp1P;
  FWidth := 13;
  FDecimals := 5;
end;

procedure TExternalFormat.SetDecimals(const Value: integer);
begin
  if FDecimals <> Value then
  begin
    Assert(Value >= 1);
    FDecimals := Value;
    InvalidateModel;
  end;
end;

procedure TExternalFormat.SetExtFormatPrefix(const Value: TExtFormatPrefix);
begin
  if FExtFormatPrefix <> Value then
  begin
    FExtFormatPrefix := Value;
    InvalidateModel;
  end;
end;

procedure TExternalFormat.SetNumberFormat(const Value: TNumberFormat);
begin
  if FNumberFormat <> Value then
  begin
    FNumberFormat := Value;
    InvalidateModel;
  end;
end;

procedure TExternalFormat.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
  begin
    Assert(Value >= 1);
    FWidth := Value;
    InvalidateModel;
  end;
end;

{ THeadDrawdownOutputControl }

procedure THeadDrawdownOutputControl.Assign(Source: TPersistent);
var
  SourceHD: THeadDrawdownOutputControl;
begin
  if Source is THeadDrawdownOutputControl then
  begin
    SourceHD := THeadDrawdownOutputControl(Source);
    ExternalFormat := SourceHD.ExternalFormat;
    Frequency := SourceHD.Frequency;
    FrequencyChoice := SourceHD.FrequencyChoice;
    OutputFileType := SourceHD.OutputFileType;
    PrintFormat := SourceHD.PrintFormat;
    PrintInListing := SourceHD.PrintInListing;
    SaveInExternalFile := SourceHD.SaveInExternalFile;
    Wrapping := SourceHD.Wrapping;
  end
  else
  begin
    inherited;
  end;
end;

constructor THeadDrawdownOutputControl.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FExternalFormat := TExternalFormat.Create(InvalidateModelEvent);
  Initialize;
end;

destructor THeadDrawdownOutputControl.Destroy;
begin
  FExternalFormat.Free;
  inherited;
end;

function THeadDrawdownOutputControl.FormatDefined: boolean;
begin
  result := SaveInExternalFile and (OutputFileType = oftText);
end;

function THeadDrawdownOutputControl.PrintCode: integer;
begin
  result := Ord(PrintFormat)+1;
  if Wrapping = wStrip then
  begin
    result := -result;
  end;
end;

procedure THeadDrawdownOutputControl.Initialize;
begin
  FWrapping := wStrip;
  FSaveInExternalFile := True;
  FPrintInListing := False;
  FPrintFormat := nf10G_11_4;
  FOutputFileType := oftText;
  FFrequency := 1;
  FFrequencyChoice := fcTimeSteps;
  ExternalFormat.Initialize;
end;

procedure THeadDrawdownOutputControl.SetExternalFormat(
  const Value: TExternalFormat);
begin
  FExternalFormat.Assign(Value);
end;

procedure THeadDrawdownOutputControl.SetFrequency(const Value: integer);
begin
  if FFrequency <> Value then
  begin
    Assert(Value >= 1);
    FFrequency := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetFrequencyChoice(
  const Value: TFrequencyChoice);
begin
  if FFrequencyChoice <> Value then
  begin
    FFrequencyChoice := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetOutputFileType(
  const Value: TOutputFileType);
begin
  if FOutputFileType <> Value then
  begin
    FOutputFileType := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetPrintFormat(const Value: TPrintFormat);
begin
  if FPrintFormat <> Value then
  begin
    FPrintFormat := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetPrintInListing(const Value: boolean);
begin
  if FPrintInListing <> Value then
  begin
    FPrintInListing := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetSaveInExternalFile(
  const Value: boolean);
begin
  if FSaveInExternalFile <> Value then
  begin
    FSaveInExternalFile := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetWrapping(const Value: TWrapping);
begin
  if FWrapping <> Value then
  begin
    FWrapping := Value;
    InvalidateModel;
  end;
end;

{ TMt3dmsOutputControl }

procedure TMt3dmsOutputControl.Assign(Source: TPersistent);
var
  SourceControl: TMt3dmsOutputControl;
begin
  if Source is TMt3dmsOutputControl then
  begin
    SourceControl := TMt3dmsOutputControl(Source);
//    PrintConcentration := SourceControl.PrintConcentration;
//    PrintParticles := SourceControl.PrintParticles;
//    PrintRetardation := SourceControl.PrintRetardation;
//    PrintDispersion := SourceControl.PrintDispersion;
    SaveConcentrations := SourceControl.SaveConcentrations;
    OutputFreqChoice := SourceControl.OutputFreqChoice;
    PeriodicOutputCount := SourceControl.PeriodicOutputCount;
    OutputTimes := SourceControl.OutputTimes;
    ObservationFrequency := SourceControl.ObservationFrequency;
    MassBalanceFrequency := SourceControl.MassBalanceFrequency;
    SummarizeMassBalance := SourceControl.SummarizeMassBalance;
  end
  else
  begin
    inherited;
  end;
end;

constructor TMt3dmsOutputControl.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(InvalidateModelEvent);
  FOutputTimes := TMt3dmsOutputTimeCollection.Create(InvalidateModelEvent);
  Initialize;
end;

destructor TMt3dmsOutputControl.Destroy;
begin
  FOutputTimes.Free;
  inherited;
end;

procedure TMt3dmsOutputControl.Initialize;
begin
  FOutputFreqChoice := mofEndOfSimulation;
  FSaveConcentrations := True;
  FPeriodicOutputCount := 1;
  FOutputTimes.Clear;
  FMassBalanceFrequency := 0;
  FObservationFrequency := 0;
  FSummarizeMassBalance := True;
end;

procedure TMt3dmsOutputControl.SetMassBalanceFrequency(const Value: integer);
begin
  SetIntegerProperty(FMassBalanceFrequency, Value);
end;

procedure TMt3dmsOutputControl.SetObservationFrequency(const Value: integer);
begin
  SetIntegerProperty(FObservationFrequency, Value);
end;

procedure TMt3dmsOutputControl.SetOutputFreqChoice(
  const Value: TMt3dmsOutputFreq);
begin
  if FOutputFreqChoice <> Value then
  begin
    FOutputFreqChoice := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsOutputControl.SetOutputTimes(
  const Value: TMt3dmsOutputTimeCollection);
begin
  FOutputTimes.Assign(Value);
end;

procedure TMt3dmsOutputControl.SetPeriodicOutputCount(const Value: integer);
begin
  SetIntegerProperty(FPeriodicOutputCount, Value);
end;

//procedure TMt3dmsOutputControl.SetPrintConcentration(const Value: Integer);
//begin
//  SetIntegerProperty(FPrintConcentration, Value);
//end;
//
//procedure TMt3dmsOutputControl.SetPrintDispersion(const Value: integer);
//begin
//  SetIntegerProperty(FPrintDispersion, Value);
//end;
//
//procedure TMt3dmsOutputControl.SetPrintParticles(const Value: integer);
//begin
//  SetIntegerProperty(FPrintParticles, Value);
//end;
//
//procedure TMt3dmsOutputControl.SetPrintRetardation(const Value: integer);
//begin
//  SetIntegerProperty(FPrintRetardation, Value);
//end;

procedure TMt3dmsOutputControl.SetSaveConcentrations(const Value: boolean);
begin
  SetBooleanProperty(FSaveConcentrations, Value);
end;

procedure TMt3dmsOutputControl.SetSummarizeMassBalance(const Value: boolean);
begin
  SetBooleanProperty(FSummarizeMassBalance, Value);
end;

end.
