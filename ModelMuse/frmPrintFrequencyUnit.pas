{@abstract(The main purpose of @name is to define @link(TfrmPrintFrequency)
  which is used to edit
  frmGoPhast.Model.@link(TPhastModel.PrintFrequency).)
  @name also defines @link(TUndoPrintFrequency) which
  is used to undo or redo changes in
  frmGoPhast.Model.@link(TPhastModel.PrintFrequency).}
unit frmPrintFrequencyUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, 
  Grids, RbwDataGrid4, Contnrs, PrintFrequency,
  ExtCtrls, UndoItems, ArgusDataEntry, JvExStdCtrls, JvCombobox, JvListComb;   

type
  {@abstract(@name is used to edit
    frmGoPhast.Model.@link(TPhastModel.PrintFrequency).)}
  TfrmPrintFrequency = class(TfrmCustomGoPhast)
    // See @link(btnAddClick).
    btnAdd: TButton;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // See @link(btnDeleteClick).
    btnDelete: TButton;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // See @link(btnInsertClick).
    btnInsert: TButton;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name specifies the value of
    // frmGoPhast.Model.PrintFrequency.@link(
    // TPrintFrequencyCollection.SaveFinalHeads).
    cbSaveFinalHeads: TCheckBox;
    // @name holds the controls at the bottom of @classname.
    pnlBottom: TPanel;
    // @name holds nearly all the data for frmGoPhast.Model.PrintFrequency
    // (@link(TPrintFrequencyCollection)).
    rdgPrintFrequency: TRbwDataGrid4;
    pnlTop: TPanel;
    rdeTime: TRbwDataEntry;
    comboUnits: TJvImageComboBox;
    // @name adds a new time for print frequency.
    procedure btnAddClick(Sender: TObject);
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name deletes a time for print frequency.
    procedure btnDeleteClick(Sender: TObject);
    // @name deletes a time for print frequency.
    procedure btnInsertClick(Sender: TObject);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
    procedure rdgPrintFrequencySelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure rdgPrintFrequencyBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgPrintFrequencySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgPrintFrequencyColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgPrintFrequencyHorizontalScroll(Sender: TObject);
    procedure rdgPrintFrequencyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeTimeChange(Sender: TObject);
    procedure comboUnitsChange(Sender: TObject);
    procedure rdgPrintFrequencyDistributeTextProgress(Sender: TObject; Position,
      Max: Integer);
  private
    FDeletingTime: Boolean;
    // @name retrieves frmGoPhast.Model.@link(TPhastModel.PrintFrequency).)}
    procedure GetData;
    // @name initializes a new column in @link(rdgPrintFrequency).
    procedure InitializeColumn(const ACol: integer;
      const PrintFreq: TPrintFrequencyItem);
    {@name sets the value of
      frmGoPhast.Model.@link(TPhastModel.PrintFrequency).}
    procedure SetData;
    procedure LayoutMultiEditControls;
    procedure EnableAMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
      OddCol: Boolean);
    // @name initializes a new column in @link(rdgPrintFrequency).
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to undo or redo changes in
      frmGoPhast.Model.@link(TPhastModel.PrintFrequency).)}
  TUndoPrintFrequency = class(TCustomUndo)
  protected
    // @name: TPrintFrequencyCollection;
    // @name stores a copy of
    // frmGoPhast.Model.@link(TPhastModel.PrintFrequency)
    // as it existed when @classname was created.
    FOldPrintFrequency: TPrintFrequencyCollection;
    // @name describes what @classname does.
    function Description: string; override;
  public
    // @name: TPrintFrequencyCollection;
    // @name stores a new value for
    // frmGoPhast.Model.@link(TPhastModel.PrintFrequency)
    FNewPrintFrequency: TPrintFrequencyCollection;
    // @name creates an instance of @classname, initializes the fields
    // and assigns @link(FOldPrintFrequency).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns @link(FNewPrintFrequency) to
    // frmGoPhast.Model.@link(TPhastModel.PrintFrequency)
    procedure DoCommand; override;
    // @name assigns @link(FOldPrintFrequency) to
    // frmGoPhast.Model.@link(TPhastModel.PrintFrequency)
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrPrintFrequency = 'print frequency';
  StrOutput = 'Output';
  StrOutputFile = 'Output file';
  StrTime = 'Time';
  StrFlowRatesInBounda = 'Flow rates in boundary condition cells';
  StrComponents = 'Components';
  StrFlowBalance = 'Flow balance';
  StrForceChemistryPrin = 'Force chemistry print';
  StrHDFChemisty = 'HDF chemisty';
  StrHDFHeads = 'HDF heads';
  StrHDFVelocities = 'HDF velocities';
  StrHeads = 'Heads';
  StrProgressStatistics = 'Progress statistics';
  StrRestart = 'Restart';
  StrRestartFile = 'restart file';
  StrVelocities = 'Velocities';
  StrWells = 'Wells';
  StrXYZChemistry = 'XYZ chemistry';
  StrXYZComponents = 'XYZ components';
  StrXYZHeads = 'XYZ heads';
  StrXYZVelocities = 'XYZ velocities';
  StrXYZWells = 'XYZ wells';
  StrBoundaryConditions = 'Boundary conditions';
  StrEndOfPeriodDefaul = 'End of period default';
  StrTimeFormat = 'Time = %s';
  StrUnits = 'Units';

{$R *.dfm}

{ TfrmPrintFrequency }

procedure TfrmPrintFrequency.FormCreate(Sender: TObject);
begin
  inherited;
  Constraints.MinWidth := Width;
  rdgPrintFrequency.Cells[0, Ord(pfrName)] := StrOutput;
  rdgPrintFrequency.Cells[1, Ord(pfrName)] := StrOutputFile;
  rdgPrintFrequency.Cells[0, Ord(pfrTime)] := StrTime;
  rdgPrintFrequency.Cells[0, Ord(pfrFlowRate)] := StrFlowRatesInBounda;
  rdgPrintFrequency.Cells[1, Ord(pfrFlowRate)] := '*.O.bcf';
  rdgPrintFrequency.Cells[0, Ord(pfrComponents)] := StrComponents;
  rdgPrintFrequency.Cells[1, Ord(pfrComponents)] := '*.O.comps';
  rdgPrintFrequency.Cells[0, Ord(pfrConductance)] := StrConductance;
  rdgPrintFrequency.Cells[1, Ord(pfrConductance)] := '*.O.kd';
  rdgPrintFrequency.Cells[0, Ord(pfrFlowBalance)] := StrFlowBalance;
  rdgPrintFrequency.Cells[1, Ord(pfrFlowBalance)] := '*.O.bal';
  rdgPrintFrequency.Cells[0, Ord(pfrChemPrint)] := StrForceChemistryPrin;
  rdgPrintFrequency.Cells[1, Ord(pfrChemPrint)] := '*.O.chem';
  rdgPrintFrequency.Cells[0, Ord(pfrHDFChem)] := StrHDFChemisty;
  rdgPrintFrequency.Cells[1, Ord(pfrHDFChem)] := '*.h5';
  rdgPrintFrequency.Cells[0, Ord(pfrHDFHeads)] := StrHDFHeads;
  rdgPrintFrequency.Cells[1, Ord(pfrHDFHeads)] := '*.h5';
  rdgPrintFrequency.Cells[0, Ord(pfrHDFVelocity)] := StrHDFVelocities;
  rdgPrintFrequency.Cells[1, Ord(pfrHDFVelocity)] := '*.h5';
  rdgPrintFrequency.Cells[0, Ord(pfrHeads)] := StrHeads;
  rdgPrintFrequency.Cells[1, Ord(pfrHeads)] := '*.O.head';
  rdgPrintFrequency.Cells[0, Ord(pfrProgress)] := StrProgressStatistics;
  rdgPrintFrequency.Cells[1, Ord(pfrProgress)] := '*.O.log';
  rdgPrintFrequency.Cells[0, Ord(pfrRestart)] := StrRestart;
  rdgPrintFrequency.Cells[1, Ord(pfrRestart)] := StrRestartFile;
  rdgPrintFrequency.Cells[0, Ord(pfrVelocities)] := StrVelocities;
  rdgPrintFrequency.Cells[1, Ord(pfrVelocities)] := '*.O.vel';
  rdgPrintFrequency.Cells[0, Ord(pfrWells)] := StrWells;
  rdgPrintFrequency.Cells[1, Ord(pfrWells)] := '*.O.wel';
  rdgPrintFrequency.Cells[0, Ord(pfrXYZChem)] := StrXYZChemistry;
  rdgPrintFrequency.Cells[1, Ord(pfrXYZChem)] := '*.xyz.chem';
  rdgPrintFrequency.Cells[0, Ord(pfrXYZComponents)] := StrXYZComponents;
  rdgPrintFrequency.Cells[1, Ord(pfrXYZComponents)] := '*.xyz.comps';
  rdgPrintFrequency.Cells[0, Ord(pfrXYZHeads)] := StrXYZHeads;
  rdgPrintFrequency.Cells[1, Ord(pfrXYZHeads)] := '*.xyz.head';
  rdgPrintFrequency.Cells[0, Ord(pfrXYZVelocities)] := StrXYZVelocities;
  rdgPrintFrequency.Cells[1, Ord(pfrXYZVelocities)] := '*.xyz.vel';
  rdgPrintFrequency.Cells[0, Ord(pfrXYZWells)] := StrXYZWells;
  rdgPrintFrequency.Cells[1, Ord(pfrXYZWells)] := '*.xyz.wel';
  rdgPrintFrequency.Cells[0, Ord(pfrBoundaryConditions)] := StrBoundaryConditions;
  rdgPrintFrequency.Cells[1, Ord(pfrBoundaryConditions)] := '*.O.probdef';
  rdgPrintFrequency.Cells[0, Ord(pfrDefault)] := StrEndOfPeriodDefaul;
  rdgPrintFrequency.Cells[1, Ord(pfrDefault)] := '*.*';
  //  rdgPrintFrequencyNew.Cells[0, Ord(pfrSaveFinalHeads)] := 'Save final heads';
  //  rdgPrintFrequencyNew.Cells[1, Ord(pfrSaveFinalHeads)] := '*.head.dat';
  rdgPrintFrequency.Row := 2;

  rdgPrintFrequency.UseSpecialFormat[2, Ord(pfrBoundaryConditions)] := True;
  rdgPrintFrequency.SpecialFormat[2, Ord(pfrBoundaryConditions)] := rcf4Boolean;
  rdgPrintFrequency.UseSpecialFormat[2, Ord(pfrDefault)] := True;
  rdgPrintFrequency.SpecialFormat[2, Ord(pfrDefault)] := rcf4Boolean;

  GetData;
end;

procedure TfrmPrintFrequency.btnDeleteClick(Sender: TObject);
var
  Col: integer;
begin
  inherited;
  FDeletingTime := True;
  try
    Col := rdgPrintFrequency.Column;
    if Odd(Col) then
    begin
      Dec(Col);
    end;
    if (Col >= 2) and (Col < rdgPrintFrequency.ColCount-2) then
    begin
      // The condition can not be (Col >= 2) and (Col <= rdgPrintFrequency.ColCount-2)
      // because that lets all the column after the fixed columns be deleted.
      rdgPrintFrequency.DeleteColumn(Col);
      rdgPrintFrequency.DeleteColumn(Col);
    end;
  finally
    FDeletingTime := False;
  end;
end;

procedure TfrmPrintFrequency.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmPrintFrequency.comboUnitsChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  inherited;
  for ColIndex := 2 to rdgPrintFrequency.ColCount - 1 do
  begin
    if not Odd(ColIndex) then
    begin
      continue;
    end;
    ChangeSelectedCellsInColumn(rdgPrintFrequency,
      ColIndex, comboUnits.Text);
  end;
end;

procedure TfrmPrintFrequency.GetData;
var
  TimeIndex: integer;
  PrintFreq: TPrintFrequencyItem;
  RowIndex: TPrintFrequencyRow;
begin
  cbSaveFinalHeads.Checked := frmGoPhast.PhastModel.PrintFrequency.SaveFinalHeads;

  rdgPrintFrequency.ColCount := frmGoPhast.PhastModel.PrintFrequency.Count*2 + 2;
  for TimeIndex := 0 to frmGoPhast.PhastModel.PrintFrequency.Count - 1 do
  begin
    if TimeIndex > 0 then
    begin
      rdgPrintFrequency.Columns[TimeIndex*2 +2].Assign(
        rdgPrintFrequency.Columns[2]);
      rdgPrintFrequency.Columns[TimeIndex*2 +3].Assign(
        rdgPrintFrequency.Columns[3]);
      rdgPrintFrequency.UseSpecialFormat[TimeIndex*2 +2, Ord(pfrBoundaryConditions)] := True;
      rdgPrintFrequency.UseSpecialFormat[TimeIndex*2 +2, Ord(pfrDefault)] := True;
      rdgPrintFrequency.SpecialFormat[TimeIndex*2 +2, Ord(pfrBoundaryConditions)] := rcf4Boolean;
      rdgPrintFrequency.SpecialFormat[TimeIndex*2 +2, Ord(pfrDefault)] := rcf4Boolean;
    end;
    PrintFreq := frmGoPhast.PhastModel.PrintFrequency.Items[TimeIndex]
      as TPrintFrequencyItem;
    rdgPrintFrequency.Checked[TimeIndex*2 + 2, Ord(pfrBoundaryConditions)] :=
      PrintFreq.BoundaryConditions;
    rdgPrintFrequency.Checked[TimeIndex*2 + 2, Ord(pfrDefault)] :=
      PrintFreq.EndOfPeriodDefault;
    {rdgPrintFrequencyNew.Checked[TimeIndex*2 + 2, Ord(pfrSaveFinalHeads)] :=
      PrintFreq.SaveFinalHeads; }
    rdgPrintFrequency.Cells[TimeIndex*2 + 2, Ord(pfrTime)] :=
      FloatToStr(PrintFreq.Time);
    rdgPrintFrequency.Cells[TimeIndex*2 + 2, Ord(pfrName)] := Format(StrTimeFormat,
      [rdgPrintFrequency.Cells[TimeIndex*2 + 2, Ord(pfrTime)]]);
    rdgPrintFrequency.Cells[TimeIndex*2 + 3, Ord(pfrName)] := StrUnits;

    for RowIndex := pfrFlowRate to pfrXYZWells do
    begin
      rdgPrintFrequency.Cells[TimeIndex*2 + 2, Ord(RowIndex)] :=
        FloatToStr(PrintFreq.Rates[RowIndex]);
      rdgPrintFrequency.Cells[TimeIndex*2 + 3, Ord(RowIndex)] :=
        rdgPrintFrequency.Columns[TimeIndex*2 + 3].PickList[
          Ord(PrintFreq.Units[RowIndex])];
    end;
  end;
end;

procedure TfrmPrintFrequency.SetData;
var
  TimeIndex: integer;
  PrintFreq: TPrintFrequencyItem;
  RowIndex: TPrintFrequencyRow;
  Time: double;
  TimeLocation: integer;
  PriorUpToDate: boolean;
  Undo: TUndoPrintFrequency;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoPrintFrequency.Create;
  try
    with Undo.FNewPrintFrequency do
    begin
//      Time := 0;
      Clear;
      SaveFinalHeads := cbSaveFinalHeads.Checked;

      for TimeIndex := 1 to ((rdgPrintFrequency.ColCount) div 2) - 1 do
      begin
        if rdgPrintFrequency.Cells[TimeIndex*2, Ord(pfrTime)] = '' then
        begin
          Continue;
        end;

        try
          Time := StrToFloat(rdgPrintFrequency.Cells[TimeIndex*2, Ord(pfrTime)]);
        except on EConvertError do
          begin
            Continue;
          end;
        end;

        PrintFreq := RetrieveByTime(Time);
        if (PrintFreq = nil) then
        begin
          PrintFreq := Add as TPrintFrequencyItem;
        end
        else
        begin
          if PrintFreq.Time <> Time then
          begin
            TimeLocation := PriorTimeIndex(Time) + 1;
            if TimeLocation >= Count then
            begin
              PrintFreq := Add as TPrintFrequencyItem;
            end
            else
            begin
              PrintFreq := Insert(TimeLocation) as TPrintFrequencyItem;
            end;
          end;
        end;
        PrintFreq.Time := Time;
        PrintFreq.BoundaryConditions
          := rdgPrintFrequency.Checked[TimeIndex*2, Ord(pfrBoundaryConditions)];
        {PrintFreq.SaveFinalHeads
          := (rdgPrintFrequencyNew.Checked[TimeIndex*2, Ord(pfrSaveFinalHeads)];}
        PrintFreq.EndOfPeriodDefault
          := rdgPrintFrequency.Checked[TimeIndex*2, Ord(pfrDefault)];

        for RowIndex := pfrFlowRate to pfrXYZWells do
        begin
          PrintFreq.Units[RowIndex] :=
            TFrequencyUnits(rdgPrintFrequency.Columns[TimeIndex*2+1].PickList.IndexOf(
            rdgPrintFrequency.Cells[TimeIndex*2+1, Ord(RowIndex)]));
          PrintFreq.Rates[RowIndex] := StrToFloat(rdgPrintFrequency.Cells[TimeIndex*2, Ord(RowIndex)]);
        end;
      end;

    end;
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo)
end;

procedure TfrmPrintFrequency.LayoutMultiEditControls;
var
  Col: Integer;
  Index: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Col := 2;
  for Index := 2 to rdgPrintFrequency.ColCount - 1 do
  begin
    if Odd(Index) then
    begin
      Continue;
    end;
    if rdgPrintFrequency.ColVisible[Index] then
    begin
      Col := Index;
      break;
    end;
  end;
  LayoutControls(rdgPrintFrequency, rdeTime, nil, Col);
  LayoutControls(rdgPrintFrequency, comboUnits, nil, Col+1);
end;

procedure TfrmPrintFrequency.InitializeColumn(const ACol: integer;
  const PrintFreq: TPrintFrequencyItem);
var
  RowIndex: TPrintFrequencyRow;
begin
  if ACol > 2 then
  begin
    rdgPrintFrequency.Columns[ACol].Assign(
      rdgPrintFrequency.Columns[ACol-2]);
    rdgPrintFrequency.Columns[ACol+1].Assign(
      rdgPrintFrequency.Columns[ACol-1]);
  end
  else if rdgPrintFrequency.ColCount >= 6 then
  begin
    rdgPrintFrequency.Columns[ACol].Assign(
      rdgPrintFrequency.Columns[ACol+2]);
    rdgPrintFrequency.Columns[ACol+1].Assign(
      rdgPrintFrequency.Columns[ACol+3]);
  end
  else
  begin
    Assert(False);
  end;

  rdgPrintFrequency.UseSpecialFormat[ACol, Ord(pfrBoundaryConditions)] := True;
  rdgPrintFrequency.UseSpecialFormat[ACol, Ord(pfrDefault)] := True;
  rdgPrintFrequency.SpecialFormat[ACol, Ord(pfrBoundaryConditions)] := rcf4Boolean;
  rdgPrintFrequency.SpecialFormat[ACol, Ord(pfrDefault)] := rcf4Boolean;

  rdgPrintFrequency.Checked[ACol, Ord(pfrBoundaryConditions)] :=
    PrintFreq.BoundaryConditions;
  rdgPrintFrequency.Checked[ACol, Ord(pfrDefault)] :=
    PrintFreq.EndOfPeriodDefault;
  rdgPrintFrequency.Cells[ACol, Ord(pfrTime)] := FloatToStr(PrintFreq.Time);
  rdgPrintFrequency.Cells[ACol, Ord(pfrName)] := Format(StrTimeFormat,
    [rdgPrintFrequency.Cells[ACol, Ord(pfrTime)]]);
  rdgPrintFrequency.Cells[ACol+1, Ord(pfrName)] := StrUnits;

  for RowIndex := pfrFlowRate to pfrXYZWells do
  begin
    rdgPrintFrequency.Cells[ACol+1,Ord(RowIndex)] :=
      rdgPrintFrequency.Columns[ACol+1].PickList[Ord(PrintFreq.Units[RowIndex])];
    rdgPrintFrequency.Cells[ACol,Ord(RowIndex)] := FloatToStr(PrintFreq.Rates[RowIndex]);
    if ACol > 2 then
    begin
      rdgPrintFrequency.Cells[ACol+1,Ord(RowIndex)] :=
        rdgPrintFrequency.Cells[ACol-1,Ord(RowIndex)];
      rdgPrintFrequency.Cells[ACol,Ord(RowIndex)] :=
        rdgPrintFrequency.Cells[ACol-2,Ord(RowIndex)];
    end;
  end;
end;


procedure TfrmPrintFrequency.btnAddClick(Sender: TObject);
var
  Time: double;
  PrintFreq: TPrintFrequencyItem;
begin
  inherited;

  rdgPrintFrequency.ColCount := rdgPrintFrequency.ColCount + 2;

  if rdgPrintFrequency.ColCount > 3 then
  begin
    try
      Time := StrToFloat(rdgPrintFrequency.
        Cells[rdgPrintFrequency.ColCount - 4, Ord(pfrTime)]);
    except on EConvertError do
      begin
        Time := 0;
      end
    end;
  end
  else
  begin
    Time := 0
  end;
  PrintFreq := frmGoPhast.PhastModel.PrintFrequency.RetrieveByTime(Time);
  InitializeColumn(rdgPrintFrequency.ColCount - 2, PrintFreq);
end;

procedure TfrmPrintFrequency.btnInsertClick(Sender: TObject);
var
  Time: double;
  PrintFreq: TPrintFrequencyItem;
  Col: Integer;
begin
  inherited;
  Col := rdgPrintFrequency.Column;
  if Odd(Col) then
  begin
    Dec(Col);
  end;

  rdgPrintFrequency.InsertColumn(Col);
  rdgPrintFrequency.InsertColumn(Col);

  if Col > 2 then
  begin
    try
      Time := StrToFloat(rdgPrintFrequency.
        Cells[Col - 2, Ord(pfrTime)]);
    except on EConvertError do
      begin
        Time := 0;
      end
    end;
  end
  else
  begin
    Time := 0
  end;

  PrintFreq := frmGoPhast.PhastModel.PrintFrequency.RetrieveByTime(Time);
  InitializeColumn(Col, PrintFreq);
end;

procedure TfrmPrintFrequency.rdeTimeChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  inherited;
  for ColIndex := 2 to rdgPrintFrequency.ColCount - 1 do
  begin
    if Odd(ColIndex) then
    begin
      continue;
    end;
    ChangeSelectedCellsInColumn(rdgPrintFrequency,
      ColIndex,rdeTime.Text);
  end;
end;

procedure TfrmPrintFrequency.rdgPrintFrequencyBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  Col: Integer;
  CanSelect: Boolean;
begin
  inherited;
  Col := ACol;
  if Odd(Col) then
  begin
    Dec(Col);
  end;
  if (ARow >= rdgPrintFrequency.FixedRows)
    and ((Col = rdgPrintFrequency.Col)
    or (Col = rdgPrintFrequency.Col-1))
    and not rdgPrintFrequency.IsSelectedCell(ACol, ARow) then
  begin
    CanSelect := True;
    rdgPrintFrequencySelectCell(Sender,ACol, ARow, CanSelect);
    if CanSelect then
    begin
      rdgPrintFrequency.Canvas.Brush.Color := clAqua;
    end;
  end;
end;

procedure TfrmPrintFrequency.rdgPrintFrequencyColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TfrmPrintFrequency.rdgPrintFrequencyDistributeTextProgress(
  Sender: TObject; Position, Max: Integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  inherited;
  if Odd(rdgPrintFrequency.ColCount) then
  begin
    rdgPrintFrequency.ColCount := rdgPrintFrequency.ColCount + 1;
  end;
  for ColIndex := 4 to rdgPrintFrequency.ColCount - 1 do
  begin
    rdgPrintFrequency.Columns[ColIndex].Assign(
      rdgPrintFrequency.Columns[ColIndex-2]);
    if Not Odd(ColIndex) then
    begin
      rdgPrintFrequency.UseSpecialFormat[ColIndex,
        Ord(pfrBoundaryConditions)] := True;
      rdgPrintFrequency.UseSpecialFormat[ColIndex,
        Ord(pfrDefault)] := True;
      rdgPrintFrequency.SpecialFormat[ColIndex,
        Ord(pfrBoundaryConditions)] := rcf4Boolean;
      rdgPrintFrequency.SpecialFormat[ColIndex,
        Ord(pfrDefault)] := rcf4Boolean;
    end;
    for RowIndex := 0 to rdgPrintFrequency.RowCount - 1 do
    begin
      if rdgPrintFrequency.Cells[ColIndex,RowIndex] = '' then
      begin
        rdgPrintFrequency.Cells[ColIndex,RowIndex] :=
          rdgPrintFrequency.Cells[ColIndex-2,RowIndex]
      end;
    end;
  end;
end;

procedure TfrmPrintFrequency.rdgPrintFrequencyHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TfrmPrintFrequency.EnableAMultiEditControl(Grid: TRbwDataGrid4;
  AControl: TControl; OddCol: Boolean);
var
  ShouldEnable: boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  ShouldEnable := False;

  for ColIndex := 2 to Grid.ColCount - 1 do
  begin
    if Odd(ColIndex) <> OddCol then
    begin
      continue;
    end;
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  AControl.Enabled := ShouldEnable;
end;


procedure TfrmPrintFrequency.rdgPrintFrequencyMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableAMultiEditControl(rdgPrintFrequency, rdeTime, False);
  EnableAMultiEditControl(rdgPrintFrequency, comboUnits, True);
end;

procedure TfrmPrintFrequency.rdgPrintFrequencySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if FDeletingTime then
  begin
    Exit;
  end;
  if (ACol = 2) and (ARow = Ord(pfrTime)) then
  begin
    CanSelect := False;
  end;
  if (ACol >= rdgPrintFrequency.FixedCols)
    and (ACol < rdgPrintFrequency.ColCount-1)
    and (ARow > rdgPrintFrequency.FixedRows)
    and not (ARow in [Ord(pfrTime), Ord(pfrBoundaryConditions), Ord(pfrDefault)])
    and not Odd(ACol)
    and (rdgPrintFrequency.Columns[ACol+1].PickList.Count > 0) then
  begin
    if rdgPrintFrequency.Cells[ACol+1, ARow] =
      rdgPrintFrequency.Columns[ACol+1].PickList[
      rdgPrintFrequency.Columns[ACol+1].PickList.Count-1] then
    begin
      CanSelect := False;
    end;
  end;
  if (ACol >= rdgPrintFrequency.FixedCols)
    and(ARow in [Ord(pfrTime), Ord(pfrBoundaryConditions), Ord(pfrDefault)])
    and Odd(ACol) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmPrintFrequency.rdgPrintFrequencySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  FreqUnit: TFrequencyUnits;
begin
  inherited;
  if FDeletingTime then
  begin
    Exit;
  end;
  if (ARow = Ord(pfrTime)) and not Odd(ACol) then
  begin
    rdgPrintFrequency.Cells[ACol, Ord(pfrName)] := Format(StrTimeFormat, [Value]);
  end;
  if (ACol >= rdgPrintFrequency.FixedCols) and
    (ARow >= rdgPrintFrequency.FixedRows)
    and Odd(ACol)
    and (ARow in [Ord(pfrFlowRate)..Ord(pfrXYZWells)]) then
  begin
    FreqUnit := TFrequencyUnits(rdgPrintFrequency.Columns[ACol].PickList.
      IndexOf(rdgPrintFrequency.Cells[ACol, ARow]));

    if FreqUnit = fuStep then
    begin
      rdgPrintFrequency.UseSpecialFormat[ACol-1, ARow] := True;
      rdgPrintFrequency.SpecialFormat[ACol-1, ARow] := rcf4Integer;
    end
    else
    begin
      rdgPrintFrequency.UseSpecialFormat[ACol-1, ARow] := False;
    end;
  end;
end;

{ TUndoPrintFrequency }

constructor TUndoPrintFrequency.Create;
begin
  FOldPrintFrequency := TPrintFrequencyCollection.Create(nil);
  FOldPrintFrequency.Assign(frmGoPhast.PhastModel.PrintFrequency);
  FNewPrintFrequency := TPrintFrequencyCollection.Create(nil);
end;

function TUndoPrintFrequency.Description: string;
begin
  result := StrPrintFrequency;
end;

destructor TUndoPrintFrequency.Destroy;
begin
  FOldPrintFrequency.Free;
  FNewPrintFrequency.Free;
  inherited;
end;

procedure TUndoPrintFrequency.DoCommand;
begin
  frmGoPhast.PhastModel.PrintFrequency.Assign(FNewPrintFrequency);
end;

procedure TUndoPrintFrequency.Undo;
begin
  frmGoPhast.PhastModel.PrintFrequency.Assign(FOldPrintFrequency);
end;

end.

