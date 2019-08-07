unit frmSwrTabfilesUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons,
  ExtCtrls, frameGridUnit, ModflowSwrTabfilesUnit, UndoItems, JvExStdCtrls,
  JvCombobox, JvListComb;

type
  TTabColumn = (tcITAB, tcType, tcFileName, tcFileType, tcInterpolation, tcMethod, tcNameValue);

  TUndoTabFiles = class(TCustomUndo)
  private
    FOldTabFiles: TTabFileCollection;
    FNewTabFiles: TTabFileCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var TabFiles: TTabFileCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmSwrTabfiles = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    frameTabFiles: TframeGrid;
    dlgOpen: TOpenDialog;
    pnlTob: TPanel;
    comboType: TJvImageComboBox;
    comboFileType: TJvImageComboBox;
    comboInterpolation: TJvImageComboBox;
    comboMethod: TJvImageComboBox;
    procedure frameTabFilesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormCreate(Sender: TObject); override;
    procedure frameTabFilesGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnOKClick(Sender: TObject);
    procedure frameTabFilesGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameTabFilesseNumberChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure frameTabFilesGridHorizontalScroll(Sender: TObject);
    procedure frameTabFilesGridColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure frameTabFilesGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frameTabFilesGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboTypeChange(Sender: TObject);
    procedure comboFileTypeChange(Sender: TObject);
    procedure comboInterpolationChange(Sender: TObject);
    procedure comboMethodChange(Sender: TObject);
  private
    procedure GetDataForAModel(TabFiles: TTabFileCollection);
    procedure SetDataForAModel(TabFiles: TTabFileCollection);
    procedure GetData;
    procedure SetData;
    procedure LayoutMultiRowEditControls;
    procedure EnableMultiRowEditControl(ACol: Integer; AControl: TControl);
    procedure SetSelectedValues(const AValue: string; AColumn: integer);

    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  frmSelectSwrObjectsUnit, RbwDataGrid4, frmGoPhastUnit,
  frmSwrVertexNumbersUnit;

resourcestring
  StrITAB = 'ITAB';
  StrCTABTYPE = 'Type of data (CTABTYPE)';
  StrFileName = 'File Name (ITABUNIT)';
  StrFileTypeITABUNIT = 'File Type (ITABUNIT)';
  StrInterpolation = 'Interpolation (CINTP)';
  StrSpecificationMethod = 'Specification Method (ITABRCH)';
  StrObjectNameOrValue = 'Object Names, Vertex Value, or Reach Numbers (ITABRCH)';
  StrUndefinedName = 'One or more of the tab files you defined does not have' +
  ' a file name defined.';
  StrBadFileName = 'One or more of the tabfiles you defined does not exist.';

{$R *.dfm}

procedure TfrmSwrTabfiles.btnOKClick(Sender: TObject);
var
  RowIndex: Integer;
  MissingName: Boolean;
  Badname: Boolean;
  TabFileName: string;
begin
  inherited;

  MissingName := False;
  Badname := False;
  for RowIndex := 1 to frameTabFiles.seNumber.asInteger do
  begin
    TabFileName := frameTabFiles.Grid.Cells[Ord(tcFileName), RowIndex];
    if Trim(TabFileName) = '' then
    begin
      MissingName := True;
    end
    else if not FileExists(TabFileName) then
    begin
      Badname := True;
    end;
  end;

  if MissingName then
  begin
    Beep;
    MessageDlg(StrUndefinedName, mtError, [mbOK], 0);
  end;

  if Badname then
  begin
    Beep;
    MessageDlg(StrBadFileName, mtError, [mbOK], 0);
  end;

  SetData;
end;

procedure TfrmSwrTabfiles.comboFileTypeChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboFileType.Text, Ord(tcFileType));
end;

procedure TfrmSwrTabfiles.comboInterpolationChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboInterpolation.Text, Ord(tcInterpolation));
end;

procedure TfrmSwrTabfiles.comboMethodChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboMethod.Text, Ord(tcMethod));
end;

procedure TfrmSwrTabfiles.comboTypeChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboType.Text, Ord(tcType));
end;

procedure TfrmSwrTabfiles.EnableMultiRowEditControl(ACol: Integer;
  AControl: TControl);
var
  RowIndex: Integer;
  ShouldEnable: Boolean;
begin
  ShouldEnable := False;
  for RowIndex := frameTabFiles.Grid.FixedRows to frameTabFiles.Grid.RowCount - 1 do
  begin
    ShouldEnable := frameTabFiles.Grid.IsSelectedCell(ACol,RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  AControl.Enabled := ShouldEnable;

end;

procedure TfrmSwrTabfiles.FormCreate(Sender: TObject);
var
  ColIndex: Integer;
begin
  inherited;
//  FTabFiles := TTabFileCollection.Create(nil);
  frameTabFiles.Grid.BeginUpdate;
  try

    frameTabFiles.Grid.Cells[Ord(tcITAB), 0] := StrITAB;
    frameTabFiles.Grid.Cells[Ord(tcType), 0] := StrCTABTYPE;
    frameTabFiles.Grid.Cells[Ord(tcFileName), 0] := StrFileName;
    frameTabFiles.Grid.Cells[Ord(tcFileType), 0] := StrFileTypeITABUNIT;
    frameTabFiles.Grid.Cells[Ord(tcInterpolation), 0] := StrInterpolation;
    frameTabFiles.Grid.Cells[Ord(tcMethod), 0] := StrSpecificationMethod;
    frameTabFiles.Grid.Cells[Ord(tcNameValue), 0] := StrObjectNameOrValue;
    frameTabFiles.Grid.ColWidths[Ord(tcFileName)] := 100;

  finally
    frameTabFiles.Grid.EndUpdate;
  end;
  for ColIndex := 1 to frameTabFiles.Grid.ColCount - 1 do
  begin
    frameTabFiles.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  comboType.Items.Assign(frameTabFiles.Grid.Columns[Ord(tcType)].PickList);
  comboFileType.Items.Assign(frameTabFiles.Grid.Columns[Ord(tcFileType)].PickList);
  comboInterpolation.Items.Assign(frameTabFiles.Grid.Columns[Ord(tcInterpolation)].PickList);
  comboMethod.Items.Assign(frameTabFiles.Grid.Columns[Ord(tcMethod)].PickList);

  GetData;
end;

procedure TfrmSwrTabfiles.FormResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls
end;

procedure TfrmSwrTabfiles.frameTabFilesGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  frmSelect: TfrmSelectSwrObjects;
  SelectMethod: TReachSelectionMethod;
  frmSelectReaches: TfrmSwrVertexNumbers;
begin
  inherited;
  if ACol = Ord(tcFileName) then
  begin
    dlgOpen.FileName := frameTabFiles.Grid.Cells[ACol, ARow];
    if dlgOpen.Execute then
    begin
      frameTabFiles.Grid.Cells[ACol, ARow] := dlgOpen.FileName;
    end;
  end
  else
  begin
    Assert(ACol = Ord(tcNameValue));
    SelectMethod := TReachSelectionMethod(frameTabFiles.Grid.
      ItemIndex[Ord(tcMethod),ARow]);
    Assert(SelectMethod in [rsmObjects, rsmReaches]);
    case SelectMethod of
      rsmObjects:
        begin
          // select objects
          frmSelect := TfrmSelectSwrObjects.Create(nil);
          try
            frmSelect.GetData(frameTabFiles.Grid.Cells[ACol, ARow]);
            if frmSelect.ShowModal = mrOK then
            begin
              frameTabFiles.Grid.Cells[ACol, ARow] := frmSelect.SelectedObjects;
            end;
          finally
            frmSelect.Free;
          end
        end;
      rsmReaches:
        begin
          // select reaches
          frmSelectReaches := TfrmSwrVertexNumbers.Create(nil);
          try
            frmSelectReaches.GetData(frameTabFiles.Grid.Cells[ACol, ARow]);
            if frmSelectReaches.ShowModal = mrOK then
            begin
              frameTabFiles.Grid.Cells[ACol, ARow] := frmSelectReaches.SetData;
            end;
          finally
            frmSelectReaches.Free;
          end
        end;
      else Assert(False);
    end;
  end;
end;

procedure TfrmSwrTabfiles.frameTabFilesGridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmSwrTabfiles.frameTabFilesGridHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmSwrTabfiles.frameTabFilesGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmSwrTabfiles.frameTabFilesGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiRowEditControl(Ord(tcType), comboType);
  EnableMultiRowEditControl(Ord(tcFileType), comboFileType);
  EnableMultiRowEditControl(Ord(tcInterpolation), comboInterpolation);
  EnableMultiRowEditControl(Ord(tcMethod), comboMethod);
end;

procedure TfrmSwrTabfiles.frameTabFilesGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  SelectMethod: TReachSelectionMethod;
begin
  inherited;
  if (ACol = Ord(tcNameValue)) and (ARow >= frameTabFiles.Grid.FixedRows) then
  begin
    CanSelect := (frameTabFiles.Grid.Cells[Ord(tcMethod),ARow] <> '')
      and (frameTabFiles.Grid.ItemIndex[Ord(tcType),ARow] >= 0);
//      and (TTabType(frameTabFiles.Grid.ItemIndex[Ord(tcType),ARow]) <> ttStructure);
    if CanSelect then
    begin
      SelectMethod := TReachSelectionMethod(frameTabFiles.Grid.
        ItemIndex[Ord(tcMethod),ARow]);
      case SelectMethod of
        rsmAll:
          begin
            CanSelect := False;
          end;
        rsmObjects, rsmReaches:
          begin
            if not frameTabFiles.Grid.Drawing then
            begin
              frameTabFiles.Grid.Columns[Ord(tcNameValue)].UseButton := True;
              frameTabFiles.Grid.UseSpecialFormat[ACol, ARow] := False;
            end;
          end;
        rsmValue:
          begin
            if not frameTabFiles.Grid.Drawing then
            begin
              frameTabFiles.Grid.Columns[Ord(tcNameValue)].UseButton := False;
              frameTabFiles.Grid.SpecialFormat[ACol, ARow] := rcf4Integer;
              frameTabFiles.Grid.UseSpecialFormat[ACol, ARow] := True;
            end;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TfrmSwrTabfiles.frameTabFilesGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if (ARow >= frameTabFiles.Grid.FixedRows) and (ACol = Ord(tcMethod)) then
  begin
    frameTabFiles.Grid.Invalidate;
  end;
end;

procedure TfrmSwrTabfiles.frameTabFilesseNumberChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameTabFiles.seNumberChange(Sender);
  frameTabFiles.Grid.BeginUpdate;
  try
    for RowIndex := 1 to frameTabFiles.Grid.RowCount - 1 do
    begin
      frameTabFiles.Grid.Cells[Ord(tcITAB), RowIndex] := IntToStr(RowIndex);
    end;
  finally
    frameTabFiles.Grid.EndUpdate
  end;
end;

procedure TfrmSwrTabfiles.GetData;
begin
  { TODO -cFMP : Needs updating for FMP }
  GetDataForAModel(frmGoPhast.PhastModel.SwrTabFiles);
end;

procedure TfrmSwrTabfiles.GetDataForAModel(TabFiles: TTabFileCollection);
var
  ColIndex: Integer;
  ItemIndex: Integer;
  Item: TTabFileItem;
  Joiner: TStringList;
  Reaches: string;
  ReachIndex: integer;
begin
//  TabFiles.Assign(TabFiles);
  frameTabFiles.seNumber.asInteger := TabFiles.Count;
  if TabFiles.Count = 0 then
  begin
    for ColIndex := 0 to frameTabFiles.Grid.ColCount - 1 do
    begin
      frameTabFiles.Grid.Cells[ColIndex,1] := '';
    end;
  end;
  for ItemIndex := 0 to TabFiles.Count - 1 do
  begin
    Item := TabFiles[ItemIndex];
    frameTabFiles.Grid.ItemIndex[Ord(tcType),ItemIndex+1] := Ord(Item.TabType);
    frameTabFiles.Grid.Cells[Ord(tcFileName),ItemIndex+1] := Item.FullTabFileName;
    frameTabFiles.Grid.ItemIndex[Ord(tcFileType),ItemIndex+1] := Ord(Item.TabFormat);
    frameTabFiles.Grid.ItemIndex[Ord(tcInterpolation),ItemIndex+1] := Ord(Item.InterpolationMethod);
    frameTabFiles.Grid.ItemIndex[Ord(tcMethod),ItemIndex+1] := Ord(Item.ReachSelectionMethod);
    case Item.ReachSelectionMethod of
      rsmAll:
        begin
          frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1] := '';
        end;
      rsmObjects:
        begin
          frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1] := Item.ObjectNames;
        end;
      rsmValue:
        begin
          frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1] := IntToStr(Item.Value);
        end;
      rsmReaches:
        begin
          Joiner := TStringList.Create;
          try
            for ReachIndex := 0 to Item.Reaches.Count - 1 do
            begin
              Joiner.Add(IntToStr(Item.Reaches[ReachIndex].Value));
            end;
            Reaches := Joiner.CommaText;
            Reaches := StringReplace(Reaches, ',', ', ', [rfReplaceAll]);
            frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1] := Reaches;
          finally
            Joiner.Free;
          end;
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end;
end;

procedure TfrmSwrTabfiles.LayoutMultiRowEditControls;
begin
  LayoutControls(frameTabFiles.Grid, comboType, nil, Ord(tcType));
  LayoutControls(frameTabFiles.Grid, comboFileType, nil, Ord(tcFileType));
  LayoutControls(frameTabFiles.Grid, comboInterpolation, nil, Ord(tcInterpolation));
  LayoutControls(frameTabFiles.Grid, comboMethod, nil, Ord(tcMethod));
end;

procedure TfrmSwrTabfiles.SetData;
var
  TabFiles: TTabFileCollection;
begin
  { TODO -cFMP : Needs updating for FMP }
  TabFiles := TTabFileCollection.Create(nil);
  try
    SetDataForAModel(TabFiles);
    frmGoPhast.UndoStack.Submit(TUndoTabFiles.Create(TabFiles));
  finally
    TabFiles.Free;
  end;
end;

procedure TfrmSwrTabfiles.SetDataForAModel(TabFiles: TTabFileCollection);
var
  ItemIndex: integer;
  Item: TTabFileItem;
  SelectIndex: integer;
  Splitter: TStringList;
  ReachCount: Integer;
  AReach: Integer;
  ReachIndex: Integer;
begin
  TabFiles.Count := frameTabFiles.seNumber.asInteger;
  for ItemIndex := 0 to TabFiles.Count - 1 do
  begin
    Item := TabFiles[ItemIndex];
    SelectIndex := frameTabFiles.Grid.ItemIndex[Ord(tcType),ItemIndex+1];
    if SelectIndex > 0 then
    begin
      Item.TabType := TTabType(SelectIndex);
    end;
    Item.FullTabFileName := frameTabFiles.Grid.Cells[Ord(tcFileName),ItemIndex+1];

    SelectIndex := frameTabFiles.Grid.ItemIndex[Ord(tcFileType),ItemIndex+1];
    if SelectIndex > 0 then
    begin
      Item.TabFormat := TTabFormat(SelectIndex);
    end;

    SelectIndex := frameTabFiles.Grid.ItemIndex[Ord(tcInterpolation),ItemIndex+1];
    if SelectIndex > 0 then
    begin
      Item.InterpolationMethod := TInterpolationMethod(SelectIndex);
    end;
    SelectIndex := frameTabFiles.Grid.ItemIndex[Ord(tcMethod),ItemIndex+1];
    if SelectIndex > 0 then
    begin
      Item.ReachSelectionMethod := TReachSelectionMethod(SelectIndex);
    end;
    case Item.ReachSelectionMethod of
      rsmAll: ; // do nothing.
      rsmObjects:
        begin
          Item.ObjectNames := frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1];
        end;
      rsmValue:
        begin
          Item.Value := StrToIntDef(frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1], 0)
        end;
      rsmReaches:
        begin
          Splitter := TStringList.Create;
          try
            Splitter.CommaText := frameTabFiles.Grid.Cells[Ord(tcNameValue),ItemIndex+1];
            ReachCount := 0;
            for ReachIndex := 0 to Splitter.Count - 1 do
            begin
              if TryStrToInt(Splitter[ReachIndex], AReach) then
              begin
                if ReachCount >= Item.Reaches.Count then
                begin
                  Item.Reaches.Add;
                end;
                Item.Reaches[ReachIndex].Value := AReach;
                Inc(ReachCount);
              end;
            end;
            Item.Reaches.Count := ReachCount;
          finally
            Splitter.Free;
          end;
        end
      else
        Assert(False);
    end;
  end;
end;

procedure TfrmSwrTabfiles.SetSelectedValues(const AValue: string;
  AColumn: integer);
var
  RowIndex: Integer;
  CanSelect: Boolean;
begin
  frameTabFiles.Grid.BeginUpdate;
  try
    for RowIndex := frameTabFiles.Grid.FixedRows to frameTabFiles.Grid.RowCount - 1 do
    begin
      if frameTabFiles.Grid.IsSelectedCell(AColumn, RowIndex) then
      begin
        CanSelect := True;
        if Assigned(frameTabFiles.Grid.OnSelectCell) then
        begin
          frameTabFiles.Grid.OnSelectCell(frameTabFiles.Grid, AColumn, RowIndex, CanSelect);
        end;
        if CanSelect then
        begin
          frameTabFiles.Grid.Cells[AColumn, RowIndex] := AValue;
          if Assigned(frameTabFiles.Grid.OnSetEditText) then
          begin
            frameTabFiles.Grid.OnSetEditText(frameTabFiles.Grid, AColumn, RowIndex, AValue);
          end;
        end;
      end;
    end;
  finally
    frameTabFiles.Grid.EndUpdate;
  end;
end;

{ TUndoTabFiles }

constructor TUndoTabFiles.Create(var TabFiles: TTabFileCollection);
begin
  FOldTabFiles := TTabFileCollection.Create(nil);
  FOldTabFiles.Assign(frmGoPhast.PhastModel.SwrTabFiles);
  FNewTabFiles := TabFiles;
  TabFiles := nil;
end;

function TUndoTabFiles.Description: string;
begin
  result := 'change SWR tab files';
end;

destructor TUndoTabFiles.Destroy;
begin
  FOldTabFiles.Free;
  FNewTabFiles.Free;
  inherited;
end;

procedure TUndoTabFiles.DoCommand;
begin
  frmGoPhast.PhastModel.SwrTabFiles := FNewTabFiles;

end;

procedure TUndoTabFiles.Undo;
begin
  frmGoPhast.PhastModel.SwrTabFiles := FOldTabFiles;
end;

end.
