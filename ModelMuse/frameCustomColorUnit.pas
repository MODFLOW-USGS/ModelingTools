unit frameCustomColorUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, ArgusDataEntry, Grids, RbwDataGrid4, frameDisplayLimitUnit,
  SsButtonEd, RbwStringTreeCombo, StdCtrls, ComCtrls, JvExComCtrls, JvUpDown,
  JvExControls, JvxSlider, Mask, JvExMask, JvSpin, JvExStdCtrls,
  VirtualTrees, DataSetUnit, LegendUnit, RbwParser, ClassificationUnit,
  EdgeDisplayUnit, SubscriptionUnit, GrayTabs;

type
  TframeCustomColor = class(TFrame)
    pcChoices: TPageControl;
    tabSelection: TTabSheet;
    // @name displays "Data set or boundary condition".
    lblDataSet: TLabel;
    // @name displays "Color scheme".
    lblColorScheme: TLabel;
    // @name labels @link(seCycles).
    lblCycles: TLabel;
    // @name is used to give a preview of the color schemes listed in
    // @link(comboColorScheme).
    // See @link(pbColorSchemePaint).
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    lblComment: TLabel;
    // @name displays a list of the different methods for converting
    // a value in the @link(TDataArray) used for coloring the @link(TPhastGrid)
    // to a color.
    comboColorScheme: TComboBox;
    seCycles: TJvSpinEdit;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    cbLogTransform: TCheckBox;
    udDataSets: TJvUpDown;
    rgUpdateLimitChoice: TRadioGroup;
    virttreecomboDataSets: TRbwStringTreeCombo;
    tabFilters: TTabSheet;
    // @name displays "Lower limit".
    lblLowerLimit: TLabel;
    lblUpperLimit: TLabel;
    lblValuesToIgnore: TLabel;
    lblNumberOfValuesToIgnore: TLabel;
    lblEpsilon: TLabel;
    // @name is used to set the maximum value used to color the grid.
    frameCheck3DMax: TframeDisplayLimit;
    // @name is used to set the minimum value used to color the grid.
    frameCheck3DMin: TframeDisplayLimit;
    cbActiveOnly: TCheckBox;
    rdgValuesToIgnore: TRbwDataGrid4;
    seNumberOfValuesToIgnore: TJvSpinEdit;
    rdeEpsilon: TRbwDataEntry;
    tabLegend: TTabSheet;
    imLegend: TImage;
    pnlLegend: TPanel;
    lblMethod: TLabel;
    lblColorLegendRows: TLabel;
    comboMethod: TComboBox;
    seLegendRows: TJvSpinEdit;
    rdgLegend: TRbwDataGrid4;
    timerLegend: TTimer;
    reComment: TRichEdit;
    btnColorSchemes: TButton;
    btnFont: TButton;
    dlgFontLegend: TFontDialog;
    splColor: TSplitter;
    procedure seNumberOfValuesToIgnoreChange(Sender: TObject);
    procedure seLegendRowsChange(Sender: TObject);
    // @name gives a preview of the color scheme
    // selected in @link(comboColorScheme).
    procedure pbColorSchemePaint(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure seCyclesChange(Sender: TObject);
    procedure seCyclesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure jsColorExponentChange(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure virttreecomboDataSetsTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure udDataSetsChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure rdgValuesToIgnoreEndUpdate(Sender: TObject);
    procedure comboMethodChange(Sender: TObject);
    procedure rdgLegendEndUpdate(Sender: TObject);
    procedure rdgLegendSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLegendStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure FrameResize(Sender: TObject);
    procedure timerLegendTimer(Sender: TObject);
    procedure btnColorSchemesClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure virttreecomboDataSetsTreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    // @name is implemented as a TObjectList.
    FDataSetDummyObjects: TList;
    FSelectedVirtNode: PVirtualNode;
    FUpdatingLegend: Boolean;
    FStartTime: TDateTime;
    FLegendFont: TFont;
    FFontAssigned: Boolean;
    procedure UpdateLegendAfterDelay;
    function GetLegendDataSource: TObserver;
    procedure SetLegendDataSource(const Value: TObserver);
    { Private declarations }
  protected
    FStoredLegend: TLegend;
    FLegend: TLegend;
    // @name stores a list of the @link(TDataArray)s or boundary conditions
    // that can be displayed on the front view of the model.  The Objects
    // property of @name contains either the corresponding @link(TDataArray)
    // or the corresponding @link(TPhastTimeList).
    FFrontItems: TStringList;
    // @name stores a list of the @link(TDataArray)s or boundary conditions
    // that can be displayed on the side view of the model.  The Objects
    // property of @name contains either the corresponding @link(TDataArray)
    // or the corresponding @link(TPhastTimeList).
    FSideItems: TStringList;
    // @name stores a list of the @link(TDataArray)s or boundary conditions
    // that can be displayed on the top view of the model.  The Objects
    // property of @name contains either the corresponding @link(TDataArray)
    // or the corresponding @link(TPhastTimeList).
    FTopItems: TStringList;
    // @name fills @link(virttreecomboDataSets) with data about the things
    // that can be used to color the grid.
    // @name retrieves the data about the @link(TDataArray)
    // used to color the @link(TPhastGrid).
    function GetSelectedArray: TDataArray; virtual; abstract;
    procedure GetDataSets;
    procedure StoreDataSetsInLists;
    procedure FinalizeList(List: TStringList);
    procedure SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RetrieveSelectedObject(var AnObject: TObject);
    procedure AssignLimits(DataType: TRbwDataType; Limits: TColoringLimits);
    procedure ReadLimits(DataType: TRbwDataType; Limits: TColoringLimits);
    procedure UpdateLegend;
    procedure Loaded; override;
    function CanColorDataSet(DataArray: TDataArray): boolean; virtual;
    procedure GetData; virtual;

  public
    property LegendDataSource: TObserver read GetLegendDataSource
      write SetLegendDataSource;
    procedure ResetTreeText;
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetData; virtual; abstract;
    procedure UpdateColorSchemes;
    property LegendFont: TFont read FLegendFont;
    { Public declarations }
  end;

resourcestring
  StrBoundaryConditions = 'Boundary Conditions, Observations, and Other Features';

implementation

uses
  GoPhastTypes, frmGoPhastUnit, frmCustomGoPhastUnit, PhastModelUnit,
  ModelMuseUtilities, Clipbrd, Contnrs, frmColorSchemesUnit;

resourcestring
  StrValuesToIgnore = 'Values to ignore';
  StrValues = 'Values';
  StrNone = 'none';

{$R *.dfm}

const
  OneSecond = 1/24/3600;

{ TframeCustomColor }

procedure TframeCustomColor.AssignLimits(DataType: TRbwDataType;
  Limits: TColoringLimits);
var
  IntegerValue: Integer;
  SkipRealItem: TSkipReal;
  RealValue: Double;
  SkipIndex: Integer;
  SkipIntegerItem: TSkipInteger;
begin
  if TryStrToFloat(rdeEpsilon.Text, RealValue) then
  begin
    Limits.Epsilon := RealValue
  end;
  Limits.LowerLimit := frameCheck3DMin.Limit;
  Limits.UpperLimit := frameCheck3DMax.Limit;
  Limits.ActiveOnly := cbActiveOnly.Checked;
  case DataType of
    rdtDouble:
      begin
        Limits.RealValuesToSkip.Clear;
        for SkipIndex := 1 to seNumberOfValuesToIgnore.AsInteger do
        begin
          if TryStrToFloat(rdgValuesToIgnore.
            Cells[0, SkipIndex], RealValue) then
          begin
            SkipRealItem := Limits.RealValuesToSkip.Add as TSkipReal;
            SkipRealItem.RealValue := RealValue;
          end;
        end;
        Limits.LogTransform := cbLogTransform.Checked;
      end;
    rdtInteger:
      begin
        Limits.IntegerValuesToSkip.Clear;
        for SkipIndex := 1 to seNumberOfValuesToIgnore.AsInteger do
        begin
          if TryStrToInt(rdgValuesToIgnore.
            Cells[0, SkipIndex], IntegerValue) then
          begin
            SkipIntegerItem := Limits.IntegerValuesToSkip.Add as TSkipInteger;
            SkipIntegerItem.IntegerValue := IntegerValue;
          end;
        end;
        Limits.LogTransform := False;
      end;
    rdtBoolean:
      begin
        Limits.LogTransform := False;
      end;
    rdtString:
      begin
        Limits.StringValuesToSkip.Clear;
        for SkipIndex := 1 to seNumberOfValuesToIgnore.AsInteger do
        begin
          Limits.StringValuesToSkip.Add(rdgValuesToIgnore.Cells[0, SkipIndex]);
        end;
        Limits.LogTransform := False;
      end;
  else
    Assert(False);
  end;
  Limits.Update;
end;

procedure TframeCustomColor.btnColorSchemesClick(Sender: TObject);
begin
  ShowAForm(TfrmColorSchemes)
end;

procedure TframeCustomColor.btnFontClick(Sender: TObject);
begin
  dlgFontLegend.Font := FLegendFont;
  if dlgFontLegend.Execute then
  begin
    FLegendFont.Assign(dlgFontLegend.Font);
    UpdateLegend
  end;
end;

function TframeCustomColor.CanColorDataSet(DataArray: TDataArray): boolean;
begin
  result := False;
  case DataArray.EvaluatedAt of
    eaBlocks: result := True;
    eaNodes: result := frmGoPhast.PhastModel.ModelSelection
      in [msPhast, msSutra22, msSutra30];
    else Assert(False);
  end;
end;

procedure TframeCustomColor.comboColorSchemeChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeCustomColor.comboMethodChange(Sender: TObject);
begin
  rdgLegend.Enabled := comboMethod.ItemIndex = 1;
  seLegendRows.Enabled := rdgLegend.Enabled;
  if rdgLegend.Enabled then
  begin
    rdgLegend.Color := clWindow;
  end
  else
  begin
    rdgLegend.Color := clBtnFace;
  end;
  UpdateLegend;
end;

constructor TframeCustomColor.Create(AOwner: TComponent);
begin
  inherited;
  FLegendFont := TFont.Create
end;

destructor TframeCustomColor.Destroy;
begin
  FLegendFont.Free;
  FTopItems.Free;
  FFrontItems.Free;
  FSideItems.Free;
  FDataSetDummyObjects.Free;
  FStoredLegend.Free;
  inherited;
end;

procedure TframeCustomColor.FinalizeList(List: TStringList);
begin
  List.Sort;
  List.Sorted := False;
  List.Insert(0, StrNone);
end;

procedure TframeCustomColor.FrameResize(Sender: TObject);
begin
  UpdateLegend;
  if pnlLegend.Left > splColor.Left then
  begin
    pnlLegend.Left := splColor.Left;
  end;
end;

procedure TframeCustomColor.GetData;
var
  index: Integer;
begin
  if not FFontAssigned then
  begin
    FFontAssigned := True;
    FLegendFont.Assign(Font);
  end;
  if FLegend <> nil then
  begin
    comboMethod.ItemIndex := Ord(FLegend.ValueAssignmentMethod)-1;
    seLegendRows.Enabled := FLegend.ValueAssignmentMethod = vamManual;
    if FLegend.ValueAssignmentMethod = vamManual then
    begin
      seLegendRows.AsInteger := FLegend.Values.Count;
      seLegendRows.OnChange(nil);
      case FLegend.Values.DataType of
        rdtDouble:
          begin
            for index := 0 to FLegend.Values.Count - 1 do
            begin
              // FloatToStr doesn't round properly when compiled
              // with 64 bit precision.
              rdgLegend.Cells[0,index+1] :=
                FloatToStrF(FLegend.Values.RealValues[Index], ffGeneral, 14, 0)
//              FloatToStr(FLegend.Values.RealValues[index]);
            end;
          end;
        rdtInteger:
          begin
            for index := 0 to FLegend.Values.Count - 1 do
            begin
              rdgLegend.Cells[0,index+1] := IntToStr(FLegend.Values.IntValues[index]);
            end;
          end;
        rdtBoolean:
          begin
            for index := 0 to FLegend.Values.Count - 1 do
            begin
              if FLegend.Values.BooleanValues[index] then
              begin
                rdgLegend.Cells[0,index+1] := 'True';
              end
              else
              begin
                rdgLegend.Cells[0,index+1] := 'False';
              end;
            end;
          end;
        rdtString:
          begin
            for index := 0 to FLegend.Values.Count - 1 do
            begin
              rdgLegend.Cells[0,index+1] := FLegend.Values.StringValues[index];
            end;
          end;
      end;
    end;
  end;
end;

procedure TframeCustomColor.GetDataSets;
begin
  FillVirtualStringTreeWithDataSets(virttreecomboDataSets.Tree,
    FDataSetDummyObjects, GetSelectedArray, CanColorDataSet);
end;

function TframeCustomColor.GetLegendDataSource: TObserver;
begin
  if FLegend = nil then
  begin
    result := nil
  end
  else
  begin
    result := FLegend.ValueSource;
  end;
end;

procedure TframeCustomColor.jsColorExponentChange(Sender: TObject);
begin
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TframeCustomColor.Loaded;
begin
  inherited;
  reComment.DoubleBuffered := False;

  FSelectedVirtNode := nil;
  FDataSetDummyObjects := TObjectList.Create;

//  Constraints.MinHeight := Height;
//  Constraints.MinWidth := Width;
  FTopItems := TStringList.Create;
  FFrontItems := TStringList.Create;
  FSideItems := TStringList.Create;
  frameCheck3DMax.Limit.DefaultBooleanLimitValue := True;
  udDataSets.Max := High(SmallInt);
  udDataSets.Min := Low(SmallInt);

  rdgValuesToIgnore.Cells[0,0] := StrValuesToIgnore;
  rdgValuesToIgnore.FixedRows := 1;

  rdgLegend.Cells[0,0] := StrValues;

  pcChoices.ActivePageIndex := 0;

  udDataSets.Left := virttreecomboDataSets.Left + virttreecomboDataSets.Width;
  udDataSets.Top := virttreecomboDataSets.Top;
  udDataSets.Height := virttreecomboDataSets.Height;

end;

procedure TframeCustomColor.pbColorSchemePaint(Sender: TObject);
var
  X: integer;
  Fraction: Real;
  AColor: TColor;
  ColorAdjustmentFactor: Real;
begin
  if comboColorScheme.ItemIndex >= 0 then
  begin
    for X := 0 to pbColorScheme.Width - 1 do
    begin
      Fraction := 1 - X / pbColorScheme.Width;
      ColorAdjustmentFactor := seColorExponent.Value;

      AColor := FracAndSchemeToColor(comboColorScheme.ItemIndex,
        Fraction, ColorAdjustmentFactor, seCycles.AsInteger);

      with pbColorScheme.Canvas do
      begin
        Pen.Color := AColor;
        MoveTo(X, 0);
        LineTo(X, pbColorScheme.Height - 1);
      end;
    end;
  end;
end;

procedure TframeCustomColor.rdgLegendEndUpdate(Sender: TObject);
begin
  if seLegendRows <> nil then
  begin
    seLegendRows.AsInteger := rdgLegend.RowCount -1;
  end;
end;

procedure TframeCustomColor.rdgLegendSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  UpdateLegendAfterDelay;
end;

procedure TframeCustomColor.rdgLegendStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  UpdateLegendAfterDelay;
end;

procedure TframeCustomColor.rdgValuesToIgnoreEndUpdate(Sender: TObject);
var
  NewCount: integer;
begin
  if seNumberOfValuesToIgnore <> nil then
  begin
    NewCount := rdgValuesToIgnore.RowCount-1;
    if (NewCount = 1) and (rdgValuesToIgnore.Cells[0,1] = '') then
    begin
      NewCount := 0;
    end;
    seNumberOfValuesToIgnore.AsInteger := NewCount;
  end;
end;

procedure TframeCustomColor.ReadLimits(DataType: TRbwDataType;
  Limits: TColoringLimits);
var
  SkipIntegerItem: TSkipInteger;
  SkipRealItem: TSkipReal;
  SkipIndex: Integer;
begin
  rdeEpsilon.Text := FloatToStr(Limits.Epsilon);
  frameCheck3DMin.Limit := Limits.LowerLimit;
  frameCheck3DMax.Limit := Limits.UpperLimit;
  frameCheck3DMin.DataType := DataType;
  frameCheck3DMax.DataType := DataType;
  cbActiveOnly.Checked := Limits.ActiveOnly;
  case DataType of
    rdtDouble:
      begin
        rdgValuesToIgnore.Columns[0].Format := rcf4Real;
        rdgValuesToIgnore.BeginUpdate;
        try
          seNumberOfValuesToIgnore.AsInteger := Limits.RealValuesToSkip.Count;
          seNumberOfValuesToIgnoreChange(nil);
          for SkipIndex := 0 to Limits.RealValuesToSkip.Count - 1 do
          begin
            SkipRealItem := Limits.RealValuesToSkip.
              Items[SkipIndex] as TSkipReal;
            rdgValuesToIgnore.Cells[0, SkipIndex + 1] :=
              FloatToStr(SkipRealItem.RealValue);
          end;
          cbLogTransform.Checked := Limits.LogTransform;
        finally
          rdgValuesToIgnore.EndUpdate;
        end;
      end;
    rdtInteger:
      begin
        rdgValuesToIgnore.Columns[0].Format := rcf4Integer;
        rdgValuesToIgnore.BeginUpdate;
        try
          seNumberOfValuesToIgnore.AsInteger := Limits.IntegerValuesToSkip.Count;
          seNumberOfValuesToIgnoreChange(nil);
          for SkipIndex := 0 to Limits.IntegerValuesToSkip.Count - 1 do
          begin
            SkipIntegerItem := Limits.IntegerValuesToSkip.
              Items[SkipIndex] as TSkipInteger;
            rdgValuesToIgnore.Cells[0, SkipIndex + 1] :=
              IntToStr(SkipIntegerItem.IntegerValue);
          end;
          cbLogTransform.Checked := False;
        finally
          rdgValuesToIgnore.EndUpdate;
        end;
      end;
    rdtBoolean:
      begin
        cbLogTransform.Checked := False;
      end;
    rdtString:
      begin
        rdgValuesToIgnore.Columns[0].Format := rcf4String;
        rdgValuesToIgnore.BeginUpdate;
        try
          seNumberOfValuesToIgnore.AsInteger := Limits.StringValuesToSkip.Count;
          seNumberOfValuesToIgnoreChange(nil);
          for SkipIndex := 0 to Limits.StringValuesToSkip.Count - 1 do
          begin
            rdgValuesToIgnore.Cells[0, SkipIndex + 1] :=
              Limits.StringValuesToSkip[SkipIndex];
          end;
          cbLogTransform.Checked := False;
        finally
          rdgValuesToIgnore.EndUpdate;
        end;
      end;
  end;
end;

procedure TframeCustomColor.ResetTreeText;
begin
  UpdateTreeComboText(SelectedVirtNode, virttreecomboDataSets);
end;

procedure TframeCustomColor.RetrieveSelectedObject(var AnObject: TObject);
var
  NodeData: PClassificationNodeData;
begin
  if SelectedVirtNode = nil then
  begin
    AnObject := nil;
  end
  else
  begin
    NodeData := virttreecomboDataSets.Tree.GetNodeData(SelectedVirtNode);
    if Assigned(NodeData) and Assigned(NodeData.ClassificationObject) then
    begin
      if NodeData.ClassificationObject is TBoundaryClassification then
      begin
        AnObject := TBoundaryClassification(NodeData.ClassificationObject).ClassifiedObject;
      end
      else if NodeData.ClassificationObject is TDataSetClassification then
      begin
        AnObject := TDataSetClassification(NodeData.ClassificationObject).DataArray;
      end
      else
      begin
        AnObject := nil;
      end;
    end
    else
    begin
      AnObject := nil;
    end;
  end;
end;

procedure TframeCustomColor.seColorExponentChange(Sender: TObject);
begin
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate
end;

procedure TframeCustomColor.seCyclesChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeCustomColor.seCyclesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    ClipBoard.AsText := seCycles.SelText;
  end;
  if ((Key = Ord('V')) or (Key = Ord('V'))) and (ssCtrl in Shift) then
  begin
    seCycles.SelText := ClipBoard.AsText;
  end;
end;

procedure TframeCustomColor.seLegendRowsChange(Sender: TObject);
begin
  if rdgLegend.RowCount <> seLegendRows.AsInteger + 1 then
  begin
    rdgLegend.RowCount := seLegendRows.AsInteger + 1;
    UpdateLegendAfterDelay;
  end;
end;

procedure TframeCustomColor.seNumberOfValuesToIgnoreChange(Sender: TObject);
var
  NewCount: Integer;
begin
  inherited;
  NewCount := seNumberOfValuesToIgnore.AsInteger;
  if NewCount = 0 then
  begin
    rdgValuesToIgnore.RowCount := 2;
    rdgValuesToIgnore.Color := clBtnFace;
    rdgValuesToIgnore.Cells[0,1] := '';
  end
  else
  begin
    rdgValuesToIgnore.RowCount := NewCount + 1;
    rdgValuesToIgnore.Color := clWindow;
  end;
end;

procedure TframeCustomColor.SetLegendDataSource(const Value: TObserver);
begin
  if FLegend <> nil then
  begin
    FLegend.ValueSource := Value;
  end;
end;

procedure TframeCustomColor.SetSelectedNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SelectOnlyLeaves(Node, virttreecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TframeCustomColor.StoreDataSetsInLists;
var
  DataSet: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    case DataSet.Orientation of
      dsoTop:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoFront:
        begin
          FFrontItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoSide:
        begin
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
      dso3D:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
          FFrontItems.AddObject(DataSet.Name, DataSet);
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TframeCustomColor.timerLegendTimer(Sender: TObject);
begin
  if Now - FStartTime > OneSecond then
  begin
    timerLegend.Enabled := False;
    UpdateLegend;
  end;
end;

procedure TframeCustomColor.udDataSetsChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
var
  NewSelection: PVirtualNode;
begin
  inherited;
  NewSelection := nil;
  if SelectedVirtNode <> nil then
  begin
    case Direction of
      updNone: ; // do nothing
      updUp:
        begin
          NewSelection := SelectedVirtNode.PrevSibling;
        end;
      updDown:
        begin
          NewSelection := SelectedVirtNode.NextSibling;
        end;
      else Assert(False);
    end;
  end;
  if NewSelection = nil then
  begin
    Beep;
  end
  else
  begin
    virttreecomboDataSets.Tree.Selected[NewSelection] := True;
    SetSelectedNode(virttreecomboDataSets.Tree, NewSelection);
    SetData;
  end;
  udDataSets.ControlStyle := udDataSets.ControlStyle - [csCaptureMouse];

end;

procedure TframeCustomColor.UpdateColorSchemes;
//var
//  StoredIndex: integer;
//  StoredName: string;
//  index: Integer;
//  NewIndex: Integer;
begin
  UpdateColorScheme(comboColorScheme, pbColorScheme);
//  StoredIndex := comboColorScheme.ItemIndex;
//  StoredName := comboColorScheme.Text;
//  comboColorScheme.Items.Clear;
//  comboColorScheme.Items.Add(StrRainbow);
//  comboColorScheme.Items.Add(StrGreenToMagenta);
//  comboColorScheme.Items.Add(StrBlueToRed);
//  comboColorScheme.Items.Add(StrBlueToDarkOrange);
//  comboColorScheme.Items.Add(StrBlueToGreen);
//  comboColorScheme.Items.Add(StrBrownToBlue);
//  comboColorScheme.Items.Add(StrBlueToGray);
//  comboColorScheme.Items.Add(StrBlueToOrange);
//  comboColorScheme.Items.Add(StrBlueToOrangeRed);
//  comboColorScheme.Items.Add(StrLightBlueToDarkB);
//  comboColorScheme.Items.Add(StrModifiedSpectralSc);
//  comboColorScheme.Items.Add(StrSteppedSequential);
//
//  for index := 0 to frmGoPhast.PhastModel.ColorSchemes.Count - 1 do
//  begin
//    comboColorScheme.Items.Add(frmGoPhast.PhastModel.ColorSchemes[index].Name);
//  end;
//  NewIndex := comboColorScheme.Items.IndexOf(StoredName);
//  if (NewIndex < 0) then
//  begin
//    if StoredIndex < comboColorScheme.Items.Count then
//    begin
//      NewIndex := StoredIndex;
//    end
//    else
//    begin
//      NewIndex := 0
//    end;
//  end;
//  comboColorScheme.ItemIndex := NewIndex;
//  pbColorScheme.Invalidate;
end;

procedure TframeCustomColor.UpdateLegend;
var
  BitMap: TBitmap;
  Index: Integer;
  DummyRect: TRect;
begin
  if FUpdatingLegend or (csDestroying in ComponentState)
    or (frmGoPhast = nil) or (frmGoPhast.PhastModel = nil)
    or frmGoPhast.PhastModel.Clearing
    or (csDestroying in frmGoPhast.PhastModel.ComponentState)
    or (FLegend = nil) then
  begin
    Exit;
  end;

  FUpdatingLegend := True;
  try
    tabLegend.TabVisible := LegendDataSource <> nil;
    if tabLegend.TabVisible then
    begin
      if FStoredLegend <> nil then
      begin
        FLegend.Assign(FStoredLegend);
        Exit;
      end;
      FLegend.ValueAssignmentMethod :=
        TValueAssignmentMethod(comboMethod.ItemIndex + 1);
      rdgLegend.BeginUpdate;
      try
        case FLegend.ValueAssignmentMethod of
          vamNoLegend: Exit;
          vamAutomatic:
            begin
              if not FLegend.ValueSource.UpToDate then
              begin
                Exit;
              end;
              FLegend.AutoAssignValues;
              case FLegend.Values.DataType of
                rdtDouble: rdgLegend.Columns[0].Format := rcf4Real;
                rdtInteger: rdgLegend.Columns[0].Format := rcf4Integer;
                rdtBoolean: rdgLegend.Columns[0].Format := rcf4Boolean;
                rdtString: rdgLegend.Columns[0].Format := rcf4String;
                else Assert(False);
              end;
              seLegendRows.AsInteger := FLegend.Values.Count;
              seLegendRowsChange(nil);
              for Index := 0 to FLegend.Values.Count - 1 do
              begin
                case FLegend.Values.DataType of
                  rdtDouble:
                    begin
                    // FloatToStr doesn't round properly when compiled
                    // with 64 bit precision.
                      rdgLegend.Cells[0,Index + 1] :=
                        FloatToStrF(FLegend.Values.RealValues[Index],
                          ffGeneral, 14, 0);
                    end;
                  rdtInteger:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        IntToStr(FLegend.Values.IntValues[Index]);
                    end;
                  rdtBoolean:
                    begin
                      rdgLegend.Cells[0,Index + 1] := '';
                      rdgLegend.Checked[0,Index + 1] :=
                        FLegend.Values.BooleanValues[Index];
                    end;
                  rdtString:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        FLegend.Values.StringValues[Index];
                    end;
                  else Assert(False);
                end;
              end;
            end;
          vamManual:
            begin
              FLegend.Values.Count := seLegendRows.AsInteger;
              for Index := 0 to FLegend.Values.Count - 1 do
              begin
                case FLegend.Values.DataType of
                  rdtDouble:
                    begin
                      if cbLogTransform.Checked then
                      begin
                        FLegend.Values.RealValues[Index] :=
                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 0);
                      end
                      else
                      begin
                        FLegend.Values.RealValues[Index] :=
                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 0);
                      end;
                    end;
                  rdtInteger:
                    begin
                      FLegend.Values.IntValues[Index] :=
                        StrToIntDef(rdgLegend.Cells[0,Index + 1], 0);
                    end;
                  rdtBoolean:
                    begin
                      FLegend.Values.BooleanValues[Index] :=
                        rdgLegend.Checked[0,Index + 1];
                    end;
                  rdtString:
                    begin
                      FLegend.Values.StringValues[Index] :=
                        rdgLegend.Cells[0,Index + 1];
                    end;
                  else Assert(False);
                end;
              end;

            end;
          else Assert(False);
        end;
      finally
        rdgLegend.EndUpdate;
      end;

      if FLegend.AssignFractions then
      begin
        BitMap := TBitMap.Create;
        try
          BitMap.Canvas.Font := FLegendFont;
          BitMap.Width := imLegend.Width;
          BitMap.Height := imLegend.Height;
          FLegend.Draw(BitMap.Canvas, 10, 10, DummyRect, FLegendFont);
          imLegend.Picture.Assign(BitMap);
        finally
          BitMap.Free;
        end;
      end;
    end;
  finally
    FUpdatingLegend := False;
  end;
end;

procedure TframeCustomColor.UpdateLegendAfterDelay;
begin
  FStartTime := Now;
  timerLegend.Enabled := True;
end;

procedure TframeCustomColor.virttreecomboDataSetsChange(Sender: TObject);
begin
  ResetTreeText;
end;

procedure TframeCustomColor.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SetSelectedNode(Sender, Node);
end;

procedure TframeCustomColor.virttreecomboDataSetsTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TframeCustomColor.virttreecomboDataSetsTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TframeCustomColor.virttreecomboDataSetsTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
  Node: PVirtualNode;
begin
  virttreecomboDataSets.Tree.GetHitTestInfoAt(X, Y, False, HitInfo);
  if (hiOnItemLabel in HitInfo.HitPositions) then
  begin
    Node := virttreecomboDataSets.Tree.GetNodeAt(X, Y);
    if virttreecomboDataSets.Tree.HasChildren[Node]  then
    begin
      virttreecomboDataSets.Tree.Expanded[Node] :=
        not virttreecomboDataSets.Tree.Expanded[Node];
    end;
  end;

end;

end.
