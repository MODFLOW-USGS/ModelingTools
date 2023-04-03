{@abstract(The main purpose of @name is to define @link(TfrmTimeControl)
which is used to edit frmGoPhast.Model.@link(TPhastModel.Times).)

@name also defines @link(TUndoTimes) which
is used to set or undo the setting of
frmGoPhast.Model.@link(TPhastModel.Times).}
unit frmTimeControlUnit;

interface

uses
  System.UITypes, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Grids, RbwDataGrid4, ExtCtrls,
  Buttons, UndoItems, TimeUnit, Mask, JvExMask, JvSpin,
  ArgusDataEntry;

type
  {@abstract(@name is used to edit frmGoPhast.Model.@link(TPhastModel.Times).)}
  TfrmTimeControl = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name displays "Number of Periods".
    lblNumberOfPeriods: TLabel;
    // @name holds the controls at the bottom of @classname.
    pnlBottom: TPanel;
    // @name specifies the number of stress periods.
    // See @link(seTimeCountChange).
    seTimeCount: TJvSpinEdit;
    // @name holds the time step sizes and ending times of the stress periods.
    dgTime: TRbwDataGrid4;
    rdeStartTime: TRbwDataEntry;
    lblStartTime: TLabel;
    pnlTop: TPanel;
    rdeLength: TRbwDataEntry;
    // @name calls @link(CheckTimes) if it returns @true, @name calls
    // @link(SetData).  Otherwise ModalResult is set to mrNone.
    procedure btnOKClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name changes the number of rows in @link(dgTime) to be consistent
    // with the value in @link(seTimeCount).
    procedure seTimeCountChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dgTimeColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgTimeHorizontalScroll(Sender: TObject);
    procedure dgTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeLengthChange(Sender: TObject);
    procedure dgTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    // @name checks that the ending times of all the stress periods are unique.
    // If they aren't, the user will be given a chance to cancel
    // setting the data. If the user chooses to continue, the the rows with
    // the duplicate ending times will be deleted before setting the data.
    function CheckTimes: boolean;
    // @name displays frmGoPhast.Model.@link(TPhastModel.Times) in @classname.
    procedure GetData;
    // @name sets frmGoPhast.Model.@link(TPhastModel.Times) with the values
    // in @classname using a @link(TUndoTimes).
    procedure SetData;
    procedure LayoutMultiRowEditControl;
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to set or undo the setting of
    frmGoPhast.Model.@link(TPhastModel.Times).)}
  TUndoTimes = class(TCustomUndo)
  protected
    // @name: @link(TTimeCollection);
    // @name is a copy of frmGoPhast.Model.@link(TPhastModel.Times)
    // as it existed when this instance of @classname was created.
    FOldTimes: TTimeCollection;
    // @name describes what @classname does.
    function Description: string; override;
  public
    // @name: @link(TTimeCollection);
    // @name is a new version of frmGoPhast.Model.@link(TPhastModel.Times).
    FNewTimes: TTimeCollection;
    // @name creates an instance of @classname.
    // @name creates @link(FOldTimes) and @link(FNewTimes) and assigns
    // frmGoPhast.Model.@link(TPhastModel.Times) to @link(FOldTimes).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns @link(FNewTimes) to
    // frmGoPhast.Model.@link(TPhastModel.Times).
    procedure DoCommand; override;
    // @name assigns @link(FOldTimes) to
    // frmGoPhast.Model.@link(TPhastModel.Times).
    procedure Undo; override;
  end;

implementation

uses Contnrs, Math, frmGoPhastUnit, RealListUnit, IntListUnit, GoPhastTypes;

resourcestring
  StrTimeControl = 'time control';
  StrTimeStepLength = 'Time step length';
  StrSomeOfTheEndingT = 'Some of the ending times in the table are not uniqu' +
  'e. Only the first of the stress-periods with a non-unique ending time wil' +
  'l be used.  Do you want to continue?';

{$R *.dfm}

type
  TTempTime = class(TObject)
    TimeStepLength: double;
    EndingTime: double;
  end;

  // @name is used to identify columns in
  // @link(TfrmTimeControl).@link(TfrmTimeControl.dgTime).
  // @value(tcBlank the empty first column.)
  // @value(tcStepSize the column for specifying the step size.)
  // @value(tcEndingTime the column for specifying the ending time.)
  TTimeColumns = (tcBlank, tcStepSize, tcEndingTime);

function SortTimes(Item1, Item2: Pointer): Integer;
var
  Difference: double;
  T2, T1: TTempTime;
begin
  T1 := Item1;
  T2 := Item2;
  Difference := T2.EndingTime - T1.EndingTime;
  result := -Sign(Difference);
end;

procedure TfrmTimeControl.FormCreate(Sender: TObject);
begin
  inherited;
  dgTime.Cells[Ord(tcStepSize), 0] := StrTimeStepLength;
  dgTime.Cells[Ord(tcEndingTime), 0] := StrEndingTime;      
  seTimeCount.MaxValue := MAXINT;
  dgTime.ColWidths[0] := 20;
  GetData;
end;

procedure TfrmTimeControl.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = 9) and (ActiveControl = dgTime)
    and (dgTime.Col = dgTime.ColCount -1)
    and (dgTime.Row = dgTime.RowCount -1) then
  begin
    ActiveControl := seTimeCount;
  end;
end;

procedure TfrmTimeControl.GetData;
var
  Index: integer;
  Item: TTimeItem;
begin
  with frmGoPhast.PhastModel.Times do
  begin
    seTimeCount.Value := Count;
    seTimeCountChange(nil);
    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TTimeItem;
      dgTime.Cells[Ord(tcStepSize), Index + 1] :=
        FloatToStr(Item.TimeStepLength);
      dgTime.Cells[Ord(tcEndingTime), Index + 1] := FloatToStr(Item.EndingTime);
    end;
    rdeStartTime.Text := FloatToStr(StartTime.Value);
  end;
end;

procedure TfrmTimeControl.SetData;
var
  Index: integer;
  List: TObjectList;
  TempItem, Temp1, Temp2: TTempTime;
  TimeItem: TTimeItem;
  PriorUpToDate: boolean;
  Undo: TUndoTimes;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoTimes.Create;
  try
    List := TObjectList.Create;
    try
      for Index := 1 to dgTime.RowCount - 1 do
      begin
        TempItem := TTempTime.Create;
        List.Add(TempItem);
        try
          TempItem.TimeStepLength := StrToFloat(
            dgTime.Cells[Ord(tcStepSize), Index]);
        except on EconvertError do
          begin
            TempItem.TimeStepLength := 1;
          end;
        end;
        try
          TempItem.EndingTime := StrToFloat(
            dgTime.Cells[Ord(tcEndingTime), Index]);
        except on EconvertError do
          begin
            TempItem.EndingTime := 1;
          end;
        end;
      end;

      List.Sort(SortTimes);
      for Index := List.Count - 1 downto 1 do
      begin
        Temp1 := List[Index] as TTempTime;
        Temp2 := List[Index - 1] as TTempTime;
        if Temp1.EndingTime = Temp2.EndingTime then
        begin
          List.Delete(Index);
        end;
      end;

      with Undo.FNewTimes do
      begin
        Clear;
        for Index := 0 to List.Count - 1 do
        begin
          TempItem := List[Index] as TTempTime;
          TimeItem := Add as TTimeItem;
          TimeItem.TimeStepLength := TempItem.TimeStepLength;
          TimeItem.EndingTime := TempItem.EndingTime;
        end;
        StartTime.Value := StrToFloat(rdeStartTime.Text);
      end;
    finally
      List.Free;
    end;
  except
    Undo.Free;
    raise
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmTimeControl.LayoutMultiRowEditControl;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  LayoutControls(dgTime, rdeLength, nil, Ord(tcStepSize));
end;

procedure TfrmTimeControl.rdeLengthChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcStepSize), rdeLength.Text);
end;

procedure TfrmTimeControl.seTimeCountChange(Sender: TObject);
var
  RowIndex, ColIndex: integer;
begin
  inherited;
  dgTime.RowCount := seTimeCount.AsInteger + 1;
  for RowIndex := 2 to dgTime.RowCount - 1 do
  begin
    for ColIndex := Ord(Succ(tcBlank)) to dgTime.ColCount - 1 do
    begin
      if dgTime.Cells[ColIndex, RowIndex] = '' then
      begin
        dgTime.Cells[ColIndex, RowIndex] :=
          dgTime.Cells[ColIndex, RowIndex - 1];
      end;
    end;
  end;
end;

function TfrmTimeControl.CheckTimes: boolean;
var
  ARealList: TRealList;
  Index: integer;
  Value: double;
  Rows: TIntegerList;
begin
//  result := False;
  ARealList := TRealList.Create;
  Rows := TIntegerList.Create;
  try
    ARealList.Sorted := True;
    for Index := 1 to dgTime.RowCount -1 do
    begin
      try
        Value := StrToFloat(dgTime.Cells[Ord(tcEndingTime), Index]);
        if ARealList.IndexOf(Value) < 0 then
        begin
          ARealList.Add(Value);
        end
        else
        begin
          Rows.Add(Index);
        end;
      except on EConvertError do
        begin
          Rows.Add(Index);
        end;
      end;
    end;
    if ARealList.Count = dgTime.RowCount -1 then
    begin
      result := True;
    end
    else
    begin
      result := MessageDlg(StrSomeOfTheEndingT,
        mtInformation, [mbYes, mbNo], 0) = mrYes;
      if result then
      begin
        for Index := Rows.Count -1 downto 0 do
        begin
          dgTime.DeleteRow(Rows[Index]);
        end;
      end;
    end;
  finally
    ARealList.Free;
    Rows.Free;
  end;
end;

procedure TfrmTimeControl.dgTimeColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControl;
end;

procedure TfrmTimeControl.dgTimeHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControl;
end;

procedure TfrmTimeControl.dgTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(dgTime, rdeLength, Ord(tcStepSize));

end;

procedure TfrmTimeControl.dgTimeSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  seTimeCount.AsInteger := dgTime.RowCount -1;
  seTimeCountChange(nil);
end;

procedure TfrmTimeControl.btnOKClick(Sender: TObject);
begin
  inherited;
  if CheckTimes then
  begin
    SetData;
  end
  else
  begin
    ModalResult := mrNone;
  end;
end;

{ TUndoTimes }

constructor TUndoTimes.Create;
begin
  FOldTimes := TTimeCollection.Create(nil);
  FOldTimes.Assign(frmGoPhast.PhastModel.Times);
  FNewTimes := TTimeCollection.Create(nil);
end;

function TUndoTimes.Description: string;
begin
  result := StrTimeControl;
end;

destructor TUndoTimes.Destroy;
begin
  FOldTimes.Free;
  FNewTimes.Free;
  inherited;
end;

procedure TUndoTimes.DoCommand;
begin
  frmGoPhast.PhastModel.Times.Assign(FNewTimes);
end;

procedure TUndoTimes.Undo;
begin
  frmGoPhast.PhastModel.Times.Assign(FOldTimes);
end;

end.

