unit frmGlobalVariablesUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Mask, JvExMask,
  JvSpin, Grids, RbwDataGrid4, GlobalVariablesUnit, UndoItems,
  System.Generics.Collections;

type
  TGlobalVariableColumns = (gvName, gvType, gvValue, gvComment);

  TfrmGlobalVariables = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rdgGlobalVariables: TRbwDataGrid4;
    seGlobalVariableCount: TJvSpinEdit;
    Label1: TLabel;
    btnDelete: TButton;
    btnImportGlobalVariables: TButton;
    btnSaveGlobalVariables: TButton;
    dlgOpenGlobVar: TOpenDialog;
    dlgSaveGlobalVariables: TSaveDialog;
    procedure seGlobalVariableCountChange(Sender: TObject);
    procedure rdgGlobalVariablesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure rdgGlobalVariablesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure rdgGlobalVariablesEndUpdate(Sender: TObject);
    procedure rdgGlobalVariablesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnImportGlobalVariablesClick(Sender: TObject);
    procedure btnSaveGlobalVariablesClick(Sender: TObject);
    procedure rdgGlobalVariablesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure seGlobalVariableCountEnter(Sender: TObject);
  private
    FInitializingRow: Boolean;
    FNewGlobals: TGlobalVariables;
    VariableNames: TStringList;
    OkVariables: array of boolean;
    FFormAdjusted: Boolean;
    FDeletedGlobalVariables: TStringList;
    function GenerateNewName(Root: string = 'NewGlobalVariable';
      const CurrentRow: integer = -1): string;
    procedure GetData;
    procedure SetData;
    procedure InitializeNewRow(ARow: integer);
    procedure UpdateSpecialFormat(RowIndex: integer);
    function GenerateNewRoot(const Root: string): string;
    procedure AdjustFormSize;
    function CanGetVariable(ARow: Integer): Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoGlobalVariables = class(TCustomUndo)
  private
    FNewGlobalVariables: TGlobalVariables;
    FOldGlobalVariables: TGlobalVariables;
    FOldNames: TStringList;
    FNewNames: TStringList;
    procedure UpdateFormulas(NewNames: TStringList; OldNames: TStringList);
  public
    Constructor Create(var NewGlobals: TGlobalVariables;
      var OldNames, NewNames: TStringList);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TGridCrack = class(TRbwDataGrid4);

var
  frmGlobalVariables: TfrmGlobalVariables = nil;

implementation

uses RbwParser, frmGoPhastUnit, DataSetUnit, PhastModelUnit, ReadGlobalsUnit,
  IntListUnit, ModelMuseUtilities;

resourcestring
  StrChangeGlobalVariab = 'change global variables';
  StrErrorReadingGlobal = 'Error reading global variables file. The following ' +
  'global variable names from the global variables file are not included in ' +
  'the model.'#13#10'%s';
  StrErrorReadingValue = 'Error reading value for global variable %s.';
  StrName = 'Name';
  StrType = 'Type';
  StrValue = 'Value';
  StrComment = 'Comment';
  StrNewGlobalVariable = 'NewGlobalVariable';
  StrYouCanGiveAVaria = 'You can''t give a variable the same name as a variable' +
  ' that is being deleted. Instead, delete the old variable, click "Apply", ' +
  'and then apply the old name to a new variable.';
  StrUnableToConvertReal = 'Unable to convert %0:s to a real number for variable %1:s';
  StrUnableToConvertInteger = 'Unable to convert %0:s to an integer for variable %1:s';


{$R *.dfm}

function TfrmGlobalVariables.CanGetVariable(ARow: Integer): Boolean;
begin
  Result := True;
  if (ARow < Length(OkVariables)) and not OkVariables[ARow] then
  begin
    Result := False;
  end;
  if FDeletedGlobalVariables.IndexOf(
    rdgGlobalVariables.Cells[Ord(gvName), ARow]) >= 0 then
  begin
    Result := False;
  end;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if (ACol = Ord(gvName)) and (ARow >= rdgGlobalVariables.FixedRows) then
  begin
    if not CanGetVariable(ARow) then
    begin
      rdgGlobalVariables.Canvas.Brush.Color := clRed;
    end;

//    if (ARow < Length(OkVariables)) and not OkVariables[ARow] then
//    begin
//      rdgGlobalVariables.Canvas.Brush.Color := clRed;
//    end;
//    if FDeletedGlobalVariables.IndexOf(
//      rdgGlobalVariables.Cells[ACol, ARow]) >= 0 then
//    begin
//      rdgGlobalVariables.Canvas.Brush.Color := clRed;
//    end;
  end;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ShouldEnableOkButton: Boolean;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  if VariableNames = nil then
  begin
    Exit;
  end;
  if (rdgGlobalVariables.Cells[Ord(gvName),1] <> '') or (rdgGlobalVariables.RowCount > 2) then
  begin
    seGlobalVariableCount.AsInteger := rdgGlobalVariables.RowCount -1;
  end;
  VariableNames.Assign(rdgGlobalVariables.Cols[Ord(gvName)]);
  SetLength(OkVariables, VariableNames.Count);
  VariableNames[0] := '';
  VariableNames.CaseSensitive := False;
  ShouldEnableOkButton := True;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for RowIndex := 1 to VariableNames.Count - 1 do
  begin
    OkVariables[RowIndex] := (VariableNames[RowIndex] = '')
      or (VariableNames.IndexOf(VariableNames[RowIndex]) = RowIndex);
    if (DataArrayManager.IndexOfDataSet(VariableNames[RowIndex]) >= 0)
      or (DataArrayManager.IndexOfBoundaryDataSet(VariableNames[RowIndex]) >= 0) then
    begin
      OkVariables[RowIndex] := False;
    end;
    if not OkVariables[RowIndex] then
    begin
      ShouldEnableOkButton := False;
    end;
  end;
  btnOK.Enabled := ShouldEnableOkButton;
  rdgGlobalVariables.Invalidate;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  rdgGlobalVariables.Options := rdgGlobalVariables.Options + [goAlwaysShowEditor];
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  GlobalVariable: TGlobalVariable;
begin
  inherited;
  if (ACol >= 0) and (ARow >= rdgGlobalVariables.FixedRows)
    and CanGetVariable(ARow) then
  begin
    GlobalVariable := rdgGlobalVariables.Objects[Ord(gvName), ARow] as TGlobalVariable;
    if (GlobalVariable <> nil) and (GlobalVariable.Locked) then
    begin
      CanSelect := False;
    end;
  end;
//  CanSelect := seGlobalVariableCount.AsInteger >= 1;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  Column: TGlobalVariableColumns;
  NewValue: string;
begin
  inherited;
  if FInitializingRow then
  begin
    Exit;
  end;
  if (ARow > 0) then
  begin
    Column := TGlobalVariableColumns(ACol);
    case Column of
      gvName:
        begin
          NewValue := GenerateNewRoot(Value);
          if NewValue <> Value then
          begin
            rdgGlobalVariables.Cells[ACol, ARow] := NewValue
          end;
          if (Value <> '') then
          begin
            if rdgGlobalVariables.Cells[Ord(gvType), ARow] = '' then
            begin
              rdgGlobalVariables.ItemIndex[Ord(gvType), ARow] := 0;
              if rdgGlobalVariables.Cells[Ord(gvValue), ARow] = '' then
              begin
                rdgGlobalVariables.Cells[Ord(gvValue), ARow] := '0';
              end;
            end;
          end;
        end;
      gvType:
        begin
          UpdateSpecialFormat(ARow);
        end;
      gvValue:
        begin

        end;
      gvComment:
        begin

        end;
      else Assert(False);
    end;

  end;
end;

procedure TfrmGlobalVariables.seGlobalVariableCountChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
  NewGlobalVariable: TGlobalVariable;
  OldGlobalVariable: TGlobalVariable;
begin
  inherited;
  while seGlobalVariableCount.AsInteger > FNewGlobals.Count do
  begin
    FNewGlobals.Add;
  end;

  while seGlobalVariableCount.AsInteger < FNewGlobals.Count do
  begin
    OldGlobalVariable := rdgGlobalVariables.Objects[Ord(gvName),
      FNewGlobals.Count] as TGlobalVariable;
    if (OldGlobalVariable <> nil) and OldGlobalVariable.Locked then
    begin
      seGlobalVariableCount.AsInteger := FNewGlobals.Count;
      break;
    end;
    NewGlobalVariable := rdgGlobalVariables.Objects[Ord(gvType),
      FNewGlobals.Count] as TGlobalVariable;
    if NewGlobalVariable <> nil then
    begin
      FDeletedGlobalVariables.Add(NewGlobalVariable.Name);
    end;
    FNewGlobals.Delete(FNewGlobals.Count-1);
    rdgGlobalVariables.Invalidate;
  end;

  rdgGlobalVariables.BeginUpdate;
  try
    if seGlobalVariableCount.AsInteger >= 1 then
    begin
      rdgGlobalVariables.RowCount := seGlobalVariableCount.AsInteger +1;
    end
    else
    begin
      rdgGlobalVariables.RowCount := 2;
      for ColIndex := 0 to rdgGlobalVariables.ColCount - 1 do
      begin
        rdgGlobalVariables.Cells[ColIndex,1] := '';
      end;
      rdgGlobalVariables.Objects[Ord(gvName), 1] := nil;
      rdgGlobalVariables.Objects[Ord(gvType), 1] := nil;
    end;
    for RowIndex := 1 to seGlobalVariableCount.AsInteger do
    begin
      if rdgGlobalVariables.Cells[Ord(gvName), RowIndex] = '' then
      begin
        InitializeNewRow(RowIndex);
      end;
    end;
  finally
    rdgGlobalVariables.EndUpdate;
  end;
  btnDelete.Enabled := seGlobalVariableCount.AsInteger >= 1;
end;

procedure TfrmGlobalVariables.seGlobalVariableCountEnter(Sender: TObject);
begin
  inherited;
  TGridCrack(rdgGlobalVariables).HideEdit;
end;

procedure TfrmGlobalVariables.SetData;
var
  Undo: TUndoGlobalVariables;
  RowIndex: Integer;
  OK: boolean;
  ColIndex: Integer;
  Variable: TGlobalVariable;
  AFormat: TRbwDataType;
  ItemIndex: integer;
  OldNames: TStringList;
  NewNames: TStringList;
  OldName: TComponentName;
  NewName: string;
begin
  OldNames := TStringList.Create;
  NewNames := TStringList.Create;
  try
    ItemIndex := 0;
    for RowIndex := 1 to seGlobalVariableCount.AsInteger do
    begin
      OK:= True;
      for ColIndex := 0 to 1 do
      begin
        OK := OK and (rdgGlobalVariables.Cells[ColIndex,RowIndex] <> '');
      end;
      if OK then
      begin
        AFormat := TRbwDataType(rdgGlobalVariables.ItemIndex[Ord(gvType),RowIndex]);
        if AFormat in [rdtDouble, rdtInteger] then
        begin
          Ok := (rdgGlobalVariables.Cells[Ord(gvValue),RowIndex] <> '')
        end;
        if Ok then
        begin
          Variable := FNewGlobals.Variables[ItemIndex];
          OldName := Variable.Name;
          NewName := rdgGlobalVariables.Cells[Ord(gvName),RowIndex];
          if (OldName <> NewName)
            and (OldName <> '')
            and (NewName <> '') then
          begin
            OldNames.Add(OldName);
            NewNames.Add(NewName);
          end;
          Variable.Name := NewName;
          Variable.Format := AFormat;
          case Variable.Format of
            rdtDouble:
              begin
                try
                  Variable.RealValue := StrToFloat(rdgGlobalVariables.
                    Cells[Ord(gvValue),RowIndex])
                except on EConvertError do
                  begin
                    try
                      Variable.RealValue := FortranStrToFloat(
                        rdgGlobalVariables.Cells[Ord(gvValue),RowIndex])
                    except on EConvertError do
                      begin
                        Beep;
                        MessageDlg(Format(
                          StrUnableToConvertReal,
                          [rdgGlobalVariables.Cells[Ord(gvValue),RowIndex],
                          Variable.Name]), mtError, [mbOK], 0);
                        Exit;
                      end;
                    end;
                  end;
                end;
              end;
            rdtInteger:
              begin
                try
                  Variable.IntegerValue := StrToInt(rdgGlobalVariables.
                    Cells[Ord(gvValue),RowIndex])
                except on EConvertError do
                  begin
                    Beep;
                    MessageDlg(Format(StrUnableToConvertInteger,
                      [rdgGlobalVariables.Cells[Ord(gvValue),RowIndex],
                      Variable.Name]), mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
              end;
            rdtBoolean:
              begin
                Variable.BooleanValue :=
                  rdgGlobalVariables.Checked[Ord(gvValue),RowIndex];
              end;
            rdtString:
              begin
                Variable.StringValue := rdgGlobalVariables.
                  Cells[Ord(gvValue),RowIndex];
              end;
            else Assert(False);
          end;
          Variable.Comment := rdgGlobalVariables.Cells[Ord(gvComment), RowIndex];
          Inc(ItemIndex);
        end
        else
        begin
          FNewGlobals.Delete(ItemIndex);
        end;
      end
      else
      begin
        FNewGlobals.Delete(ItemIndex);
      end;
    end;
    Undo := TUndoGlobalVariables.Create(FNewGlobals, OldNames, NewNames);
    frmGoPhast.UndoStack.Submit(Undo);
    GetData;
  finally
    NewNames.Free;
    OldNames.Free;
  end;
end;

procedure TfrmGlobalVariables.btnDeleteClick(Sender: TObject);
var
  GlobalVariable: TGlobalVariable;
  OldGlobalVariable: TGlobalVariable;
begin
  inherited;
  if (rdgGlobalVariables.Row > 0) then
  begin
    OldGlobalVariable := rdgGlobalVariables.
      Objects[Ord(gvType), rdgGlobalVariables.Row] as TGlobalVariable;
    GlobalVariable := rdgGlobalVariables.
      Objects[Ord(gvName), rdgGlobalVariables.Row] as TGlobalVariable;
    if (GlobalVariable <> nil) and (GlobalVariable.Locked) then
    begin
      Exit;
    end;
    FNewGlobals.Delete(rdgGlobalVariables.Row-1);
    if OldGlobalVariable <> nil then
    begin
      FDeletedGlobalVariables.Add(OldGlobalVariable.Name);
    end;

    if seGlobalVariableCount.AsInteger > 1 then
    begin
      rdgGlobalVariables.DeleteRow(rdgGlobalVariables.Row);
    end
    else
    begin
      rdgGlobalVariables.Objects[Ord(gvName), rdgGlobalVariables.Row] := nil;
      rdgGlobalVariables.Objects[Ord(gvType), rdgGlobalVariables.Row] := nil;
    end;
    seGlobalVariableCount.AsInteger := seGlobalVariableCount.AsInteger-1;
  end;
end;

procedure TfrmGlobalVariables.btnImportGlobalVariablesClick(Sender: TObject);
var
  List: TGlobalList;
  ValIndex: Integer;
  Item: TGlobalItem;
  InvalidVariables: TStringList;
  Variables: TStringList;
  IntList: TIntegerList;
  RowIndex: Integer;
  ItemIndex: Integer;
  AFormat: TRbwDataType;
  AFloat: Extended;
  AnInt: Integer;
  AValue: string;
begin
  inherited;
  if dlgOpenGlobVar.Execute then
  begin
    List := TGlobalList.Create;
    try
      if ReadGlobalFile(dlgOpenGlobVar.FileName, List) then
      begin
        InvalidVariables := TStringList.Create;
        Variables := TStringList.Create;
        IntList := TIntegerList.Create;
        try
          Variables.Assign(rdgGlobalVariables.Cols[Ord(gvName)]);
          Variables.Delete(0);
          Variables.CaseSensitive := False;
          for ValIndex := 0 to List.Count - 1 do
          begin
            Item := List[ValIndex];
            RowIndex := Variables.IndexOf(Item.Name) + 1;
            if RowIndex >= 1 then
            begin
              IntList.Add(RowIndex)
            end
            else
            begin
              InvalidVariables.Add(Item.Name)
            end;
          end;
          if InvalidVariables.Count > 0 then
          begin
            Beep;
            MessageDlg(Format(StrErrorReadingGlobal, [InvalidVariables.Text]),
              mtError, [mbOK], 0);
          end
          else
          begin
            for ValIndex := 0 to List.Count - 1 do
            begin
              Item := List[ValIndex];
              RowIndex := IntList[ValIndex];
              ItemIndex := rdgGlobalVariables.ItemIndex[Ord(gvType), RowIndex];
              Assert(ItemIndex >= 0);
              AFormat := TRbwDataType(ItemIndex);
              case AFormat of
                rdtDouble:
                  begin
                    if TryStrToFloat(Item.Value, AFloat) then
                    begin
                      rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := Item.Value;
                    end
                    else
                    begin
                      Beep;
                      MessageDlg(Format(StrErrorReadingValue, [Item.Name]), mtError, [mbOK], 0);
                      Break;
                    end;
                  end;
                rdtInteger:
                  begin
                    if TryStrToInt(Item.Value, AnInt) then
                    begin
                      rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := Item.Value;
                    end
                    else
                    begin
                      Beep;
                      MessageDlg(Format(StrErrorReadingValue, [Item.Name]), mtError, [mbOK], 0);
                      Break;
                    end;
                  end;
                rdtBoolean:
                  begin
                    if SameText(Item.Value, 'True') then
                    begin
                      rdgGlobalVariables.Checked[Ord(gvValue), RowIndex] := True;
                    end
                    else if SameText(Item.Value, 'False') then
                    begin
                      rdgGlobalVariables.Checked[Ord(gvValue), RowIndex] := False;
                    end
                    else
                    begin
                      Beep;
                      MessageDlg(Format(StrErrorReadingValue, [Item.Name]), mtError, [mbOK], 0);
                      Break;
                    end;
                  end;
                rdtString:
                  begin
                    AValue := Item.Value;
                    if (Length(AValue) > 0) then
                    begin
                      if AValue[1] = '"' then
                      begin
                        AValue := Copy(AValue, 2, MaxInt);
                        if (Length(AValue) > 0) and (AValue[Length(AValue)] = '"') then
                        begin
                          AValue := Copy(AValue, 1, Length(AValue)-1);
                        end;
                      end;
                    end;
                    rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := AValue;
                  end;
                else Assert(False);
              end;
            end;
          end;
        finally
          InvalidVariables.Free;
          Variables.Free;
          IntList.Free;
        end;

      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TfrmGlobalVariables.btnOKClick(Sender: TObject);
var
  OldVariableNames: TStringList;
  RowIndex: Integer;
begin
  inherited;
  OldVariableNames := TStringList.Create;
  try
    for RowIndex := 1 to rdgGlobalVariables.RowCount - 1 do
    begin
      if FDeletedGlobalVariables.IndexOf(
        rdgGlobalVariables.Cells[Ord(gvName), RowIndex]) >= 0 then
      begin
        Beep;
        MessageDlg(StrYouCanGiveAVaria, mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
    end;
  finally
    OldVariableNames.Free;
  end;
  SetData;
end;

procedure TfrmGlobalVariables.btnSaveGlobalVariablesClick(Sender: TObject);
var
  GloVar: TStringList;
  RowIndex: Integer;
  AString: string;
  ItemIndex: Integer;
  AFormat: TRbwDataType;
  AChar: Char;
begin
  inherited;
  if dlgSaveGlobalVariables.Execute then
  begin
    GloVar := TStringList.Create;
    try
      AChar := FormatSettings.DecimalSeparator;
      try
        FormatSettings.DecimalSeparator := '.';
        for RowIndex := 1 to seGlobalVariableCount.AsInteger do
        begin
          AString := rdgGlobalVariables.Cells[Ord(gvName), RowIndex] + ' ';
          ItemIndex := rdgGlobalVariables.ItemIndex[Ord(gvType), RowIndex];
          Assert(ItemIndex >= 0);
          AFormat := TRbwDataType(ItemIndex);
          case AFormat of
            rdtDouble, rdtInteger, rdtString:
              begin
                AString := AString + rdgGlobalVariables.Cells[Ord(gvValue), RowIndex];
              end;
            rdtBoolean:
              begin
                if rdgGlobalVariables.Checked[Ord(gvValue), RowIndex] then
                begin
                  AString := AString + 'True';
                end
                else
                begin
                  AString := AString + 'False';
                end;
              end
            else Assert(False);
          end;
          GloVar.Add(AString);
        end;
      finally
        FormatSettings.DecimalSeparator := AChar;
      end;
      try
        GloVar.SaveToFile(dlgSaveGlobalVariables.FileName);
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      GloVar.Free;
    end;
  end;
end;

procedure TfrmGlobalVariables.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Release;
  frmGlobalVariables := nil;
end;

procedure TfrmGlobalVariables.FormCreate(Sender: TObject);
begin
  inherited;
  VariableNames := TStringList.Create;
  rdgGlobalVariables.Cells[0,0] := StrName;
  rdgGlobalVariables.Cells[1,0] := StrType;
  rdgGlobalVariables.Cells[2,0] := StrValue;
  rdgGlobalVariables.Cells[3,0] := StrComment;

  FDeletedGlobalVariables := TStringList.Create;
  FDeletedGlobalVariables.CaseSensitive := False;
  GetData;
end;

procedure TfrmGlobalVariables.FormDestroy(Sender: TObject);
begin
  inherited;
  FDeletedGlobalVariables.Free;
  FNewGlobals.Free;
  VariableNames.Free;
end;

type
  TStringGridCrack = class(TStringGrid);

function TfrmGlobalVariables.GenerateNewName(Root: string = 'NewGlobalVariable';
  const CurrentRow: integer = -1): string;
var
  Names: TStringList;
  Index: integer;
begin
  Root := Trim(Root);
  if Root = '' then
  begin
    Root := StrNewGlobalVariable;
  end;

  // This function generates a name for a data set that is valid
  // and does not conflict with the names of any existing data sets.
  Names := TStringList.Create;
  try
    // The names of all the data sets are in rdgGlobalVariables.Cols[Ord(gvName)]
    Names.Assign(rdgGlobalVariables.Cols[Ord(gvName)]);
    // There may be names stored for rows that are no longer present.
    // Get rid of them.
    while Names.Count > rdgGlobalVariables.RowCount do
    begin
      Names.Delete(Names.Count - 1);
    end;
    if CurrentRow > 0 then
    begin
      Names.Delete(CurrentRow);
    end;
    // The name in the first row is a header for the names column
    // not the name of a data set so get rid of it.
    Names.Delete(0);
    // Get rid of any blanks.
    for Index := Names.Count - 1 downto 0 do
    begin
      if Names[Index] = '' then
      begin
        Names.Delete(Index);
      end;
    end;

    Names.AddStrings(frmGoPhast.PhastModel.DataArrayManager.DataSetNames);
    Names.AddStrings(frmGoPhast.PhastModel.Mf6TimesSeries.TimeSeriesNames);

    result := PhastModelUnit.GenerateNewName(Root, Names);
  finally
    Names.Free;
  end;
end;

procedure TfrmGlobalVariables.UpdateSpecialFormat(RowIndex: integer);
var
  Format: TRbwDataType;
  ItemIndex: integer;
begin
  rdgGlobalVariables.UseSpecialFormat[Ord(gvValue), RowIndex] := True;
  ItemIndex := rdgGlobalVariables.Columns[Ord(gvType)].Picklist.
      IndexOf(rdgGlobalVariables.Cells[Ord(gvType), RowIndex]);
  Format := TRbwDataType(ItemIndex);
  case Format of
    rdtDouble:
      begin
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4Real;
      end;
    rdtInteger:
      begin
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4Integer;
      end;
    rdtBoolean:
      begin
        rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := '';
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4Boolean;
      end;
    rdtString:
      begin
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4String;
      end;
    else Assert(False);
  end;
end;

function TfrmGlobalVariables.GenerateNewRoot(const Root: string): string;
begin
  result := Trim(Root);
  if result = '' then
  begin
    Exit;
  end;
  result := PhastModelUnit.GenerateNewRoot(result);
end;

procedure TfrmGlobalVariables.AdjustFormSize;
const
  ScrollBarWidth = 80;
  ExtraRowWidth = 2;
var
  TotalWidth: Integer;
  ColIndex: Integer;
  TotalHeight: Integer;
  RowIndex: Integer;
begin
  if FFormAdjusted then
  begin
    Exit;
  end;
  try
    TotalWidth := ScrollBarWidth
      + rdgGlobalVariables.GridLineWidth * rdgGlobalVariables.ColCount;
    for ColIndex := 0 to rdgGlobalVariables.ColCount - 1 do
    begin
      TotalWidth := TotalWidth + rdgGlobalVariables.ColWidths[ColIndex];
    end;
    if TotalWidth > Screen.Width then
    begin
      TotalWidth := Screen.Width;
    end;
    if Width < TotalWidth then
    begin
      Width := TotalWidth;
    end;
    TotalHeight := Height - rdgGlobalVariables.Height
      + rdgGlobalVariables.GridLineWidth * rdgGlobalVariables.RowCount;
    for RowIndex := 0 to rdgGlobalVariables.RowCount - 1 do
    begin
      TotalHeight := TotalHeight + rdgGlobalVariables.RowHeights[RowIndex]
        + ExtraRowWidth;
    end;
    if TotalHeight > Screen.Height then
    begin
      TotalHeight := Screen.Height
    end;
    if TotalHeight > Height then
    begin
      Height := TotalHeight;
    end;
  finally
    FFormAdjusted := True;
  end;
end;

procedure TfrmGlobalVariables.GetData;
var
  Index: Integer;
  GlobalVariable: TGlobalVariable;
  RowIndex: integer;
begin
  FDeletedGlobalVariables.Clear;
  FNewGlobals := TGlobalVariables.Create(nil);
  frmGoPhast.PhastModel.GlobalVariables.Sort;
  FNewGlobals.Assign(frmGoPhast.PhastModel.GlobalVariables);
  rdgGlobalVariables.BeginUpdate;
  try
    seGlobalVariableCount.AsInteger := frmGoPhast.PhastModel.GlobalVariables.Count;
    seGlobalVariableCountChange(nil);
    for Index := 0 to FNewGlobals.Count - 1 do
    begin
      RowIndex := Index+1;
      GlobalVariable := FNewGlobals[Index];
      rdgGlobalVariables.Cells[Ord(gvName), RowIndex] := GlobalVariable.Name;
      rdgGlobalVariables.Cells[Ord(gvType), RowIndex] :=
        rdgGlobalVariables.Columns[Ord(gvType)].PickList[
        Ord(GlobalVariable.Format)];
      UpdateSpecialFormat(RowIndex);
      case GlobalVariable.Format of
        rdtDouble:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] :=
              FloatToStr(GlobalVariable.RealValue);
          end;
        rdtInteger:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] :=
              IntToStr(GlobalVariable.IntegerValue);
          end;
        rdtBoolean:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := '';
            rdgGlobalVariables.Checked[Ord(gvValue), RowIndex] :=
              GlobalVariable.BooleanValue;
          end;
        rdtString:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] :=
              GlobalVariable.StringValue;
          end;
        else Assert(False);
      end;
      rdgGlobalVariables.Cells[Ord(gvComment), RowIndex] :=
        GlobalVariable.Comment;
      rdgGlobalVariables.Objects[Ord(gvName), RowIndex] := GlobalVariable;
      rdgGlobalVariables.Objects[Ord(gvType), RowIndex] :=
        frmGoPhast.PhastModel.GlobalVariables[Index];

    end;
  finally
    rdgGlobalVariables.EndUpdate;
  end;
  AdjustFormSize;
end;

procedure TfrmGlobalVariables.InitializeNewRow(ARow: integer);
begin
  FInitializingRow := True;
  try
    rdgGlobalVariables.Cells[Ord(gvName),ARow] := GenerateNewName;
    rdgGlobalVariables.Cells[Ord(gvType),ARow] :=
      rdgGlobalVariables.Columns[Ord(gvType)].PickList[0];
    rdgGlobalVariables.Cells[Ord(gvValue),ARow] := '0';
    rdgGlobalVariables.UseSpecialFormat[Ord(gvValue),ARow] := True;
    rdgGlobalVariables.SpecialFormat[Ord(gvValue),ARow] := rcf4Real;
  finally
    FInitializingRow := False;
  end;
end;

{ TUndoGlobalVariables }

constructor TUndoGlobalVariables.Create(var NewGlobals: TGlobalVariables;
      var OldNames, NewNames: TStringList);
begin
  FNewGlobalVariables := NewGlobals;
  NewGlobals := nil;
  FOldGlobalVariables := TGlobalVariables.Create(nil);
  FOldGlobalVariables.Assign(frmGoPhast.PhastModel.GlobalVariables);
  FOldNames := OldNames;
  OldNames := nil;
  FNewNames := NewNames;
  NewNames := nil;
  FNewNames.CaseSensitive := False;
  FNewNames.Sort;
end;

function TUndoGlobalVariables.Description: string;
begin
  result := StrChangeGlobalVariab;
end;

destructor TUndoGlobalVariables.Destroy;
begin
  FOldNames.Free;
  FNewNames.Free;
  FNewGlobalVariables.Free;
  FOldGlobalVariables.Free;
  inherited;
end;

procedure TUndoGlobalVariables.DoCommand;
begin
  UpdateFormulas(FNewNames, FOldNames);
  frmGoPhast.PhastModel.GlobalVariables := FNewGlobalVariables;
  if FOldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.FormulaManager.RestoreSubscriptions;
  end;
end;

procedure TUndoGlobalVariables.Undo;
begin
  UpdateFormulas(FOldNames, FNewNames);
  frmGoPhast.PhastModel.GlobalVariables := FOldGlobalVariables;
  if FOldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.FormulaManager.RestoreSubscriptions;
  end;
end;

procedure TUndoGlobalVariables.UpdateFormulas(NewNames: TStringList; OldNames: TStringList);
begin
  Assert(OldNames.Count = NewNames.Count);
  if OldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.UpdateFormulas(OldNames, NewNames);
  end;
end;

initialization

finalization
  FreeAndNil(frmGlobalVariables);

end.
