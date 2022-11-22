{@abstract(The main purpose of @name is to define @link(TfrmStartUp)
  which is used to specify the grid for a new model or open an
  existing model.)}
unit frmStartUpUnit;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ExtCtrls,
  ComCtrls, Buttons, ArgusDataEntry, RbwEdit, Grids, RbwDataGrid4,
  frameInitialGridPositionUnit, frameGridUnit, Vcl.Mask, JvExMask, JvSpin,
  JvExComCtrls, JvMonthCalendar, JvExControls, JvCalendar, JvDateTimePicker,
  GrayTabs;
type
  TStandardChoices = (scNewModflow, scNewPhast, scNewSutra,
    scNewFootprint,
    scExisting, scImportModflow);

  TStartUpPages = (supModelChoice, supGeoRef, supPhastGrid, supModflowGrid,
    supSutraMesh, supFootprintGrid);

  {@abstract(@name is used to specify the grid for a new model or open an
    existing model.)}
  TfrmStartUp = class(TfrmCustomGoPhast)
    btnDontCreateGrid: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnNextClick).
    btnNext: TBitBtn;
    // @name: TGroupBox;
    // @name groups together the controls for creating an initial grid.
    gbInitialGrid: TGroupBox;
    // @name: TLabel;
    // @name displays "Number of nodes in X (column) direction".
    lblNumNodesX: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in Y (row) direction".
    lblNumNodesY: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in Z (layer) direction".
    lblNumNodesZ: TLabel;
    // @name: TLabel;
    // @name displays "Distance between X nodes".
    lblXDist: TLabel;
    // @name: TLabel;
    // @name displays "Distance between Y nodes".
    lblYDist: TLabel;
    // @name: TLabel;
    // @name displays "Distance between Z nodes".
    lblZDist: TLabel;
    // @name: TPageControl;
    // @name holds @link(tabModelChoice) and @link(tabInitialGrid).
    pcStartup: TPageControl;
    // @name: TPanel;
    // @name holds the buttons at the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TRbwDataEntry;
    // @name is used to specify the default column width
    // (@link(TCustomModelGrid.ColumnWidth)).
    rdeColWidth: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the default layer height
    // (@link(TPhastGrid.LayerThickness)).
    rdeLayerHeight: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the number of column
    // boundaries in the @link(TPhastGrid).
    rdeNCol: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the number of layer
    // boundaries in the @link(TPhastGrid).
    rdeNLay: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the number of row
    // boundaries in the @link(TPhastGrid).
    rdeNRow: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the default row width
    // (@link(TCustomModelGrid.RowWidth)).
    rdeRowWidth: TRbwDataEntry;
    // @name: TRadioGroup;
    // @name is used to choose to open an existing model or create a new one.
    rgChoice: TRadioGroup;
    // @name: TTabSheet;
    // @name holds the controls for creating an initial grid.
    tabInitialGrid: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(rgChoice).
    tabModelChoice: TTabSheet;
    // @name is used to display help on this @classname.
    btnHelp: TBitBtn;
    tabInitialModflowGrid: TTabSheet;
    gbInitialGridModflow: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    rdeModflowRowWidth: TRbwDataEntry;
    rdeModflowColWidth: TRbwDataEntry;
    rdeModflowLayerCount: TRbwDataEntry;
    rdeModflowRowCount: TRbwDataEntry;
    rdeModflowColumnCount: TRbwDataEntry;
    rdgInitialLayers: TRbwDataGrid4;
    frameInitialGridPosition: TframeInitialGridPosition;
    tabInitialSutraMesh: TTabSheet;
    rgMeshType: TRadioGroup;
    frameModelLayers: TframeGrid;
    lblLayerGroups: TLabel;
    rgTransport: TRadioGroup;
    rdgLocation: TRbwDataGrid4;
    lblModelPosition: TLabel;
    rdeMinimumThickness: TRbwDataEntry;
    lblMinimumThickness: TLabel;
    rgSaturation: TRadioGroup;
    tabInitialGridFootprint: TTabSheet;
    gbFootprint: TGroupBox;
    rdeColumnCountFootprint: TRbwDataEntry;
    rdeRowCountFootprint: TRbwDataEntry;
    lblRowCountFootprint: TLabel;
    lblColumnCountFootprint: TLabel;
    rdeCellSizeFootprint: TRbwDataEntry;
    lblCellSizeFootprint: TLabel;
    tabGeoRef: TTabSheet;
    lblSimStartDate: TLabel;
    jvtmdtSimStartTime: TJvTimeEdit;
    lblSimStartTime: TLabel;
    mmoModelDescription: TMemo;
    rgProjectionType: TRadioGroup;
    lbledtProjection: TLabeledEdit;
    grpGeoRef: TGroupBox;
    lblModelDescription: TLabel;
    calSimStartDate: TJvDateTimePicker;
    comboLengthUnit: TComboBox;
    comboTimeUnit: TComboBox;
    lblLengthUnit: TLabel;
    lblTimeUnit: TLabel;
    pnlModflowChoice: TPanel;
    lblModflowSelection: TLabel;
    comboModflowSelection: TComboBox;
    // @name sets the vertical exaggeration of the model
    // but does not set up the grid.
    procedure btnDontCreateGridClick(Sender: TObject);
    // If @link(tabModelChoice) is the active page, @name either opens
    // a new model or advances to @link(tabInitialGrid) depending on the
    // choice in @link(rgChoice). @br
    // If @link(tabInitialGrid) is the active page, @name sets up the
    // grid using the values displayed on @link(tabInitialGrid) by
    // calling @link(SetUpPhastGrid).
    // If @link(tabInitialModflowGrid) is the active page, @name sets up the
    // grid using the values displayed on @link(tabInitialModflowGrid) by
    // calling @link(SetUpModflowGrid).
    procedure btnNextClick(Sender: TObject);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name sets the HelpKeyword property of @link(btnHelp) to the
    // active page in @link(pcStartup).
    procedure FormShow(Sender: TObject);
    procedure pcStartupChange(Sender: TObject);
    procedure rdeModflowLayerCountChange(Sender: TObject);
    procedure rdgInitialLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgInitialLayersEndUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure frameModelLayersseNumberChange(Sender: TObject);
    procedure frameModelLayersGridEndUpdate(Sender: TObject);
    procedure frameModelLayerssbDeleteClick(Sender: TObject);
    procedure frameModelLayerssbInsertClick(Sender: TObject);
    procedure frameModelLayerssbAddClick(Sender: TObject);
    procedure rgMeshTypeClick(Sender: TObject);
    procedure frameModelLayersGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure rdgLocationBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rgTransportClick(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    // @name sets up the
    // @link(TPhastGrid) using the values displayed on @link(tabInitialGrid).
    function SetUpPhastGrid: Boolean;
    // Set the vertical exaggeration.
    procedure SetExaggeration;
    function SetUpModflowGrid : Boolean;
//    procedure InitializeView(ModelXWidth, ModelYWidth, ModelHeight: Real);
    procedure SetUpModflowLayers(ColCount, RowCount: Integer;
      out ModelHeight: Real);
    procedure SetUpSutraModel;
    function SetUpFootprintGrid: Boolean;
    procedure SetUpGeoRefAndModelDescription;
    procedure SetModflowChoice;
  protected
    procedure Loaded; override;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses Math, Contnrs, frmGoPhastUnit, GoPhastTypes, frmGoToUnit, DataSetUnit,
  RbwParser, LayerStructureUnit, PhastModelUnit, ModelMuseUtilities,
  SutraMeshUnit, SutraOptionsUnit, RealListUnit, ZoomBox2, GeoRefUnit;

resourcestring
  StrInitialGrid = 'Initial Grid';
  StrFinish = 'Finish';
  StrLayerGroupName = 'Layer group name';
  StrBottomElevation = 'Bottom elevation';
  StrUpperAquifer = 'Upper Aquifer';
  StrMiddleAquifer = 'Middle Aquifer';
  StrLowerAquifer = 'Lower Aquifer';
  StrInitialModelArea = 'Initial Model Area';
  StrLower = 'Lower';
  StrHigher = 'Higher';
  StrX = 'X';
  StrY = 'Y';
  StrZ = 'Z';
  StrGeoReferenceAndMo = 'Geo Reference and Model Description';
  StrGeoReference = 'Geo Reference';
  StrPhastYouMustSpecifyPos = 'You must specify positive values for the '
  + 'distance between nodes in the X, Y, and Z directions.';
  StrModflowYouMustSpecifyPos = 'You must specify positive values for the wi' +
  'dths for all columns and rows.';
  StrFootprintYouMustSpecifyPos = 'You must specify positive values for the ' +
  'cell size.';
  StrGridOriginUpperL = 'Grid origin: Upper left corner';
  StrGridOriginLowerL = 'Grid origin: Lower left corner';
  StrTheNumberOfCellI = 'The number of cell in you model exceeds the number ' +
  'of cells that can be represent by a 32-bit integer (%d). You need to use ' +
  'a smaller number of cells';


{$R *.dfm}

//Const DefaultItemCount = 4;

procedure TfrmStartUp.btnNextClick(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  case TStartUpPages(pcStartup.ActivePageIndex) of
    supModelChoice:
      begin
        Assert(rgChoice.ItemIndex >= 0);
        if rgChoice.ItemIndex <= Ord(High(TStandardChoices)) then
        begin
          case TStandardChoices(rgChoice.ItemIndex) of
            scNewModflow, scNewPhast, scNewSutra, scNewFootprint:
              begin
                case TStandardChoices(rgChoice.ItemIndex) of
                  scNewModflow:
                    begin
                      comboLengthUnit.Style := csDropDownList;
                      comboTimeUnit.Style := csDropDownList;
                      AssignModflowLengthUnitStringsToPicklist(comboLengthUnit.Items);
                      AssignModflowTimeUnitStringsToPicklist(comboTimeUnit.Items);
                      comboLengthUnit.ItemIndex := 2;
                      comboTimeUnit.ItemIndex := 1;
                    end;
                  scNewPhast:
                    begin
                      comboLengthUnit.Style := csDropDownList;
                      comboTimeUnit.Style := csDropDownList;
                      AssignTypicalLengthUnitStringsToPicklist(comboLengthUnit.Items);
                      AssignTypicalTimeUnitStringsToPicklist(comboTimeUnit.Items);
                      comboLengthUnit.ItemIndex := 5;
                      comboTimeUnit.ItemIndex := 0;
                    end;
                  scNewSutra, scNewFootprint:
                    begin
                      comboLengthUnit.Style := csDropDown;
                      comboTimeUnit.Style := csDropDown;
                      AssignTypicalLengthUnitStringsToPicklist(comboLengthUnit.Items);
                      AssignTypicalTimeUnitStringsToPicklist(comboTimeUnit.Items);
                      comboLengthUnit.ItemIndex := 5;
                      comboTimeUnit.ItemIndex := 0;
                    end;
                  else
                    Assert(False);
                end;

                comboLengthUnit.Visible :=
                   TStandardChoices(rgChoice.ItemIndex) in
                   [scNewModflow, scNewSutra, scNewFootprint] ;
                comboTimeUnit.Visible := comboLengthUnit.Visible;
                lblLengthUnit.Visible := comboLengthUnit.Visible;
                lblTimeUnit.Visible := comboTimeUnit.Visible;

                mmoModelDescription.Visible :=
                  TStandardChoices(rgChoice.ItemIndex) = scNewModflow;
                lblModelDescription.Visible := mmoModelDescription.Visible;

                if mmoModelDescription.Visible then
                begin
                  grpGeoRef.Caption := StrGeoReferenceAndMo;
                end
                else
                begin
                  grpGeoRef.Caption := StrGeoReference;
                end;

                pcStartup.ActivePageIndex := Ord(supGeoRef);
              end;
            scExisting: // The user has chosen to open an existing model.
              begin
                if frmGoPhast.odOpenDialog.Execute then
                begin
                  ModalResult := mrOK;
                  frmGoPhast.OpenAFile(frmGoPhast.odOpenDialog.FileName);
                end;
        //              frmGoPhast.acFileOpenExecute(nil);
              end;
            scImportModflow:
              begin
                // import a model
                Hide;
                ModalResult := mrOK;
                frmGoPhast.acFileNewModflowModelExecute(frmGoPhast.miModflow2005Model)
              end
            else
              begin
                Assert(False);
              end;
          end;
        end
        else
        begin
          ModalResult := mrOK;
          FileName := frmGoPhast.
            MostRecentlyUsed.FileNames[rgChoice.ItemIndex-Ord(High(TStandardChoices))-1];
          frmGoPhast.OpenAFile(FileName);
        end;
      end;
    supGeoRef:
      begin
        Assert(rgChoice.ItemIndex >= 0);
        Assert( rgChoice.ItemIndex <= Ord(High(TStandardChoices)));
        begin

          case TStandardChoices(rgChoice.ItemIndex) of
            scNewModflow, scNewPhast: // The user has chosen to create a new model. Go to the next page.
              begin
                Caption := StrInitialGrid;
                btnNext.Caption := StrFinish;
                btnDontCreateGrid.Visible := True;
                btnDontCreateGrid.Left := btnNext.Left - btnDontCreateGrid.Width - 8;
                btnHelp.Left := btnDontCreateGrid.Left - btnHelp.Width - 8;
                if TStandardChoices(rgChoice.ItemIndex) = scNewModflow then
                begin
                  // new MODFLOW model.
                  frameInitialGridPosition.Parent := gbInitialGridModflow;
                  pcStartup.ActivePageIndex := Ord(supModflowGrid);
                  frameInitialGridPosition.lblGridOrigin.Caption := StrGridOriginUpperL;
                end
                else
                begin
                  // new PHAST model
                  frameInitialGridPosition.Parent := gbInitialGrid;
                  frmGoPhast.ModelSelection := msPhast;
                  pcStartup.ActivePageIndex := Ord(supPhastGrid);
                  frameInitialGridPosition.lblGridOrigin.Caption := StrGridOriginLowerL;
                end;
                frmGoPhast.UpdateModelSelection;
              end;
            scNewSutra:
              begin
                Caption := StrInitialModelArea;
                btnNext.Caption := StrFinish;
                pcStartup.ActivePageIndex := Ord(supSutraMesh);
                rgMeshTypeClick(nil);
              end;
            scNewFootprint:
              begin
                btnNext.Caption := StrFinish;
                btnDontCreateGrid.Visible := True;
                frameInitialGridPosition.Parent := gbFootprint;
                frmGoPhast.ModelSelection := msfootprint;
                frameInitialGridPosition.rdeExaggeration.Visible := False;
                frameInitialGridPosition.lblVerticalExaggeration.Visible := False;
                frameInitialGridPosition.rdeZ.Visible := False;
                frameInitialGridPosition.lblOriginZ.Visible := False;
                pcStartup.ActivePageIndex := Ord(supFootprintGrid);
              end;
          else
            Assert(False);
          end;
//        end
//        else
//        begin
//              Assert(rgChoice.ItemIndex >= DefaultItemCount);
//          ModalResult := mrOK;
//          FileName := frmGoPhast.
//            MostRecentlyUsed.FileNames[rgChoice.ItemIndex-Ord(High(TStandardChoices))-1];
//          frmGoPhast.OpenAFile(FileName);
        end;
        pcStartupChange(nil);
      end;
    supPhastGrid: // The user is creating a new model. Create the initial grid.
      begin
        SetUpGeoRefAndModelDescription;
        if SetUpPhastGrid then
        begin
          ModalResult := mrOK;
        end;
      end;
    supModflowGrid:  // The user is creating a new MODFLOW model.
      begin
        SetUpGeoRefAndModelDescription;
        if SetUpModflowGrid then
        begin
          ModalResult := mrOK;
        end;
        frmGoPhast.PhastModel.ModflowOptions.LengthUnit := comboLengthUnit.ItemIndex;
        frmGoPhast.PhastModel.ModflowOptions.TimeUnit := comboTimeUnit.ItemIndex;
      end;
    supSutraMesh:  // The user is creating a new SUTRA model.
      begin
        SetUpGeoRefAndModelDescription;
        SetUpSutraModel;
        ModalResult := mrOK;
      end;
    supFootprintGrid: // new footprint model
      begin
        SetUpGeoRefAndModelDescription;
        if SetUpFootprintGrid then
        begin
          ModalResult := mrOK;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmStartUp.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmStartUp.SetUpSutraModel;
var
  PhastModel: TPhastModel;
  LayerStructure: TSutraLayerStructure;
  LayerGroup: TSutraLayerGroup;
  RowIndex: Integer;
  AName: string;
  Grid: TRbwDataGrid4;
  Values: TRealList;
  NewDataArray: TDataArray;
  AValue: double;
  LayerIndex: Integer;
  XLeft: double;
  XRight: double;
  YBottom: double;
  YTop: double;
  ModelXWidth: Double;
  ZTop: Double;
  ZBottom: Double;
  ZHeight: Double;
  FrameRatio: double;
  E: Integer;
  D: Integer;
  ModelYWidth: Double;
  Exaggeration: double;
  TopZoomBox: TQRbwZoomBox2;
  FrontZoomBox: TQRbwZoomBox2;
  FrameHeight: double;
  MinThickness: double;
begin

  PhastModel := frmGoPhast.PhastModel;
  PhastModel.SutraMesh.MeshType := TMeshType(rgMeshType.ItemIndex);
  if PhastModel.SutraMesh.MeshType = mtProfile then
  begin
    PhastModel.SutraOptions.GravityY := -9.81;
  end;
  frmGoPhast.splitHoriz.Minimized := PhastModel.SutraMesh.MeshType <> mt3D;
  PhastModel.SutraOptions.TransportChoice :=
    TTransportChoice(rgTransport.ItemIndex);
  PhastModel.SutraOptions.SaturationChoice :=
    TSaturationChoice(rgSaturation.ItemIndex);

{$IFDEF SUTRA4}
  PhastModel.ModelSelection := msSutra40;
{$ELSE}
  PhastModel.ModelSelection := msSutra30;
{$ENDIF}
  Values:= TRealList.Create;
  try
    if PhastModel.SutraMesh.MeshType = mt3D then
    begin
      MinThickness := StrToFloat(rdeMinimumThickness.Text);
      LayerStructure := TSutraLayerStructure.Create(nil);
      try
        LayerStructure.Assign(frmGoPhast.PhastModel.SutraLayerStructure);
        while LayerStructure.Count > 1 do
        begin
          LayerStructure.Delete(LayerStructure.Count-1);
        end;
        if LayerStructure.Count = 0 then
        begin
          LayerStructure.Add;
          LayerGroup := LayerStructure.LayerGroups[0];
          LayerGroup.AquiferName := kSUTRAMeshTop;
        end;

        Grid := frameModelLayers.Grid;
        if TryStrToFloat(Grid.Cells[1,1], AValue) then
        begin
          Values.Add(AValue);
        end
        else
        begin
          Values.Add(0);
        end;

        for RowIndex := 2 to Grid.RowCount - 1 do
        begin
          AName := Trim(Grid.Cells[0,RowIndex]);
          if (AName <> '') and TryStrToFloat(Grid.Cells[1,RowIndex], AValue) then
          begin
            LayerStructure.Add;
            LayerGroup := LayerStructure.LayerGroups[LayerStructure.Count-1];
            LayerGroup.AquiferName := GenerateNewName(AName);
            Values.Add(AValue);
          end;
        end;

        PhastModel.SutraLayerStructure := LayerStructure;

        for LayerIndex := 0 to PhastModel.SutraLayerStructure.Count - 1 do
        begin
          LayerGroup := PhastModel.SutraLayerStructure.LayerGroups[LayerIndex];
          LayerGroup.MinThickness := MinThickness;
          NewDataArray := PhastModel.DataArrayManager.
            GetDataSetByName(LayerGroup.DataArrayName);
          Assert(NewDataArray <> nil);
          NewDataArray.Formula := FortranFloatToStr(Values[LayerIndex]);
        end;

      finally
        LayerStructure.Free;
      end;
    end;
    XLeft := StrToFloatDef(rdgLocation.Cells[1,1], 0);
    XRight := StrToFloatDef(rdgLocation.Cells[2,1], 0);
    YBottom := StrToFloatDef(rdgLocation.Cells[1,2], 0);
    YTop := StrToFloatDef(rdgLocation.Cells[2,2], 0);

//    PhastModel.ModelSelection := msSutra22;
    if (XLeft < XRight) and (YBottom < YTop) then
    begin
      ModelXWidth := XRight-XLeft;
      ModelYWidth := YTop-YBottom;
      TopZoomBox := frmGoPhast.frameTopView.ZoomBox;
      TopZoomBox.Magnification := 0.9 *
        Min(TopZoomBox.Width / ModelXWidth,
        TopZoomBox.Height / ModelYWidth);
      FrontZoomBox := frmGoPhast.frameFrontView.ZoomBox;
      FrontZoomBox.Magnification := TopZoomBox.Magnification;
      SetTopPosition((XLeft + XRight)/2, (YBottom + YTop)/2);
      if PhastModel.SutraMesh.MeshType = mt3D then
      begin
        ZTop := Values[0];
        ZBottom := Values[Values.Count-1];
        if ZTop > ZBottom then
        begin
          ModelXWidth := FrontZoomBox.X(FrontZoomBox.ClientWidth-1)
            - FrontZoomBox.X(0);
          ZHeight := ZTop-ZBottom;
          Exaggeration := ModelXWidth / ZHeight;
          if ((FrontZoomBox.Height > 10)
            and (FrontZoomBox.Width > 10)) then
          begin
            FrameRatio := FrontZoomBox.Width
              / FrontZoomBox.Height;
            Exaggeration := Exaggeration/FrameRatio;
          end
          else
          begin
            Exaggeration := Exaggeration/3;
          end;
          E := Floor(Log10(Exaggeration));
          D := ceil(Exaggeration*Power(10, -E));
          Exaggeration := D*Power(10, E);
          frmGoPhast.UpdateVerticalExaggeration(Exaggeration);
          FrameHeight := FrontZoomBox.Y(0)
            - FrontZoomBox.Y(FrontZoomBox.ClientHeight-1);
          if FrameHeight < (ZTop - ZBottom) then
          begin
            FrontZoomBox.Magnification := 0.8 * FrontZoomBox.Magnification
              * FrameHeight/(ZTop - ZBottom);
            TopZoomBox.Magnification := FrontZoomBox.Magnification;
            SetTopPosition((XLeft + XRight)/2, (YBottom + YTop)/2);
          end;
          SetFrontPosition((XLeft + XRight)/2, (ZTop + ZBottom)/2);
        end;
      end
      else if PhastModel.SutraMesh.MeshType = mtProfile then
      begin
        if YTop > YBottom then
        begin
          ModelXWidth := TopZoomBox.X(TopZoomBox.ClientWidth-1)
            - TopZoomBox.X(0);
          ZHeight := YTop-YBottom;
          Exaggeration := ModelXWidth / ZHeight;
          if ((TopZoomBox.Height > 10)
            and (TopZoomBox.Width > 10)) then
          begin
            FrameRatio := TopZoomBox.Width
              / TopZoomBox.Height;
            Exaggeration := Exaggeration/FrameRatio;
          end
          else
          begin
            Exaggeration := Exaggeration/3;
          end;
          E := Floor(Log10(Exaggeration));
          D := ceil(Exaggeration*Power(10, -E));
          Exaggeration := D*Power(10, E);
          frmGoPhast.UpdateVerticalExaggeration(Exaggeration);

          TopZoomBox.Magnification := 0.9 *
            Min(TopZoomBox.Width / ModelXWidth,
            TopZoomBox.Height / (ModelYWidth*Exaggeration));
          FrontZoomBox.Magnification := TopZoomBox.Magnification;


          SetTopPosition((XLeft + XRight)/2, (YBottom + YTop)/2);
        end;
      end;
    end;
  finally
    Values.Free;
  end;
end;

procedure TfrmStartUp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if ModalResult <> mrOK then
  begin
    Application.Terminate;
  end;
end;

procedure TfrmStartUp.FormCreate(Sender: TObject);
var
  Index: integer;
  FileName: string;
  Control: TControl;
  Grid: TRbwDataGrid4;
begin
  inherited;

  jvtmdtSimStartTime.Time := 0;

  SetAppearance;
  Caption := StrModelName;
  Assert(Ord(High(TStandardChoices))+1 = rgChoice.Items.Count);
  for Index := 0 to pcStartup.PageCount - 1 do
  begin
    pcStartup.Pages[Index].TabVisible := False;
  end;

  pcStartup.ActivePageIndex := Ord(supModelChoice);
  for Index := 0 to frmGoPhast.MostRecentlyUsed.FileNames.Count -1 do
  begin
    FileName := frmGoPhast.MostRecentlyUsed.FileNames[Index];
    rgChoice.Items.Add(ExtractFileName(FileName) + '    (' + FileName + ')');

    // This will cause TCustomRadioGroup.UpdateButtons to be called.
    rgChoice.WordWrap := not rgChoice.WordWrap;
    rgChoice.WordWrap := not rgChoice.WordWrap;

    rgChoice.Handle;
//    rgChoice.Items.Add(ExtractFileName(FileName));
//    Control := rgChoice.Controls[rgChoice.ControlCount-1];
    Control := rgChoice.Buttons[rgChoice.ControlCount-1];
    Control.Hint := FileName;
    Control.ShowHint := True;
  end;
  if rgChoice.Items.Count > Ord(High(TStandardChoices))+1 then
  begin
    rgChoice.ItemIndex := Ord(High(TStandardChoices))+1;
  end;

  rdgInitialLayers.Cells[0,0] := StrLayerGroupName;
  rdgInitialLayers.Cells[1,0] := StrBottomElevation;
  rdgInitialLayers.Cells[0,1] := StrModelTop;
  rdgInitialLayers.Cells[1,1] := '0';
  rdgInitialLayers.Cells[0,2] := StrUpperAquifer;
  rdgInitialLayers.Cells[1,2] := '-10';
  rdgInitialLayers.Cells[0,3] := StrMiddleAquifer;
  rdgInitialLayers.Cells[1,3] := '-20';
  rdgInitialLayers.Cells[0,4] := StrLowerAquifer;
  rdgInitialLayers.Cells[1,4] := '-30';

  rdgInitialLayers.Col := 1;

  Grid := frameModelLayers.Grid;
  Grid.Cells[0,0] := StrLayerGroupName;
  Grid.Cells[1,0] := StrBottomElevation;
  Grid.Cells[0,1] := StrSUTRAMeshTop;
  Grid.Cells[1,1] := '0';
  Grid.Cells[0,2] := StrUpperAquifer;
  Grid.Cells[1,2] := '-10';
  Grid.Cells[0,3] := StrMiddleAquifer;
  Grid.Cells[1,3] := '-20';
  Grid.Cells[0,4] := StrLowerAquifer;
  Grid.Cells[1,4] := '-30';
  Grid.Row := 2;

  rdgLocation.Cells[1,0] := StrLower;
  rdgLocation.Cells[2,0] := StrHigher;
  rdgLocation.Cells[0,1] := StrX;
  rdgLocation.Cells[0,2] := StrY;
  rdgLocation.Cells[0,3] := StrZ;

  rdgLocation.Cells[1,1] := '0';
  rdgLocation.Cells[1,2] := '0';
  rdgLocation.Cells[2,1] := '10000';
  rdgLocation.Cells[2,2] := '10000';

end;

procedure TfrmStartUp.SetExaggeration;
begin
  // Set the vertical exaggeration.
  if Trim(frameInitialGridPosition.rdeExaggeration.Text) = '' then
  begin
    frmGoPhast.PhastModel.Exaggeration := frmGoPhast.DefaultVE;
  end
  else
  begin
    frmGoPhast.PhastModel.Exaggeration :=
      StrToFloat(frameInitialGridPosition.rdeExaggeration.Text);
  end;
end;

Type
  TLayerStorage = class
    Name: string;
    Elevation: real;
  end;

function TfrmStartUp.SetUpFootprintGrid: Boolean;
var
  Dimension: TOneDRealArray;
  Index: integer;
  XOrigin, YOrigin: double;
  Angle: double;
  XStart, YStart: double;
  OriginAngle: double;
  OriginDistance: double;
  ColWidth, RowWidth: double;
  ColCount, RowCount: integer;
//  ModelYWidth: Real;
//  ModelXWidth: Real;
//  ModelHeight: Real;
begin
  // get some initial data.
  result := False;
  XOrigin := StrToFloat(frameInitialGridPosition.rdeX.Text);
  YOrigin := StrToFloat(frameInitialGridPosition.rdeY.Text);
  Angle := DegToRad(StrToFloat(frameInitialGridPosition.rdeAngle.Text));
  OriginDistance := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

  // Determine where the starting positions for the grid in the top view.
  if OriginDistance <> 0 then
  begin
    OriginAngle := ArcTan2(YOrigin, XOrigin);
    XStart := Cos(Angle - OriginAngle) * OriginDistance;
    YStart := -Sin(Angle - OriginAngle) * OriginDistance;
  end
  else
  begin
    XStart := 0;
    YStart := 0;
  end;

  frmGoPhast.FootPrintGrid.GridAngle := Angle;

  ColWidth := StrToFloat(rdeCellSizeFootprint.Text);
  ColCount := StrToInt(rdeColumnCountFootprint.Text);
  RowWidth := ColWidth;
  RowCount := StrToInt(rdeRowCountFootprint.Text);
  if (ColWidth <= 0) then
  begin
    Beep;
    MessageDlg(StrFootprintYouMustSpecifyPos, mtError, [mbOK], 0);
    Exit;
  end;

  // Set up the columns.
  SetLength(Dimension, ColCount+1);
  for Index := 0 to ColCount do
  begin
    Dimension[Index] := Index * ColWidth + XStart;
  end;
  frmGoPhast.FootPrintGrid.ColumnPositions := Dimension;

  // Set up the rows.
  SetLength(Dimension, RowCount+1);
  for Index := 0 to RowCount do
  begin
    Dimension[Index] := YStart- Index * RowWidth;
  end;
  frmGoPhast.FootPrintGrid.RowPositions := Dimension;
//  frmGoPhast.PhastModel.ClearNameChangeWarnings;

//  ModelXWidth := ColCount * ColWidth;
//  ModelYWidth := RowCount * RowWidth;

  // Set the selected layer.
  frmGoPhast.FootPrintGrid.SelectedLayer := 0;
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;
//  ModelHeight := 1;
  SetExaggeration;
  frmGoPhast.RestoreDefault2DView1Click(nil);
//  InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
  result := True;
end;

procedure TfrmStartUp.SetUpGeoRefAndModelDescription;
var
  GeoRef: TGeoRef;
begin
  GeoRef := frmGoPhast.PhastModel.GeoRef;
  GeoRef.StartDate := calSimStartDate.Date;
  GeoRef.StartTime := jvtmdtSimStartTime.Time;
  if rgProjectionType.ItemIndex >= 0 then
  begin
    GeoRef.ProjectionType := TProjectionType(rgProjectionType.ItemIndex);
  end;
  GeoRef.Projection := lbledtProjection.Text;
  if ComboLengthUnit.Visible then
  begin
    GeoRef.OtherLengthUnits := ComboLengthUnit.Text;
  end;
  if ComboTimeUnit.Visible then
  begin
    GeoRef.OtherTimeUnits := ComboTimeUnit.Text;
  end;
  if mmoModelDescription.Visible then
  begin
    frmGoPhast.PhastModel.ModflowOptions.Description :=
      mmoModelDescription.Lines;
  end;
end;

function TfrmStartUp.SetUpModflowGrid: Boolean;
var
  Dimension: TOneDRealArray;
  Index: integer;
  XOrigin, YOrigin: double;
  Angle: double;
  XStart, YStart: double;
  OriginAngle: double;
  OriginDistance: double;
  ColWidth, RowWidth: double;
  ColCount, RowCount: integer;
//  ModelYWidth: Real;
//  ModelXWidth: Real;
  ModelHeight: Real;
  LayerCount: Integer;
  CellCount: Integer;
  MsInitialHead: TDataArray;
begin
  SetModflowChoice;

  result := False;
  // get some initial data.
  XOrigin := StrToFloat(frameInitialGridPosition.rdeX.Text);
  YOrigin := StrToFloat(frameInitialGridPosition.rdeY.Text);
  Angle := DegToRad(StrToFloat(frameInitialGridPosition.rdeAngle.Text));
  OriginDistance := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

  // Determine where the starting positions for the grid in the top view.
  if OriginDistance <> 0 then
  begin
    OriginAngle := ArcTan2(YOrigin, XOrigin);
    XStart := Cos(Angle - OriginAngle) * OriginDistance;
    YStart := -Sin(Angle - OriginAngle) * OriginDistance;
  end
  else
  begin
    XStart := 0;
    YStart := 0;
  end;

  frmGoPhast.ModflowGrid.GridAngle := Angle;

  ColWidth := StrToFloat(rdeModflowColWidth.Text);
  ColCount := StrToInt(rdeModflowColumnCount.Text);
  RowWidth := StrToFloat(rdeModflowRowWidth.Text);
  RowCount := StrToInt(rdeModflowRowCount.Text);
  LayerCount := StrToInt(rdeModflowLayerCount.Text);
  try
    CellCount := ColCount * RowCount * LayerCount;
  except on EIntOverflow do
    begin
      Beep;
      MessageDlg(Format(StrTheNumberOfCellI, [MAXINT]), mtError, [mbOK], 0);
      Exit;
    end;
  end;
  if (ColWidth <= 0) or (RowWidth <= 0) then
  begin
    Beep;
    MessageDlg(StrModflowYouMustSpecifyPos, mtError, [mbOK], 0);
    Exit;
  end;

  // Set up the columns.
  SetLength(Dimension, ColCount+1);
  for Index := 0 to ColCount do
  begin
    Dimension[Index] := Index * ColWidth + XStart;
  end;
  frmGoPhast.ModflowGrid.ColumnPositions := Dimension;

  // Set up the rows.
  SetLength(Dimension, RowCount+1);
  for Index := 0 to RowCount do
  begin
    Dimension[Index] := YStart- Index * RowWidth;
  end;
  frmGoPhast.ModflowGrid.RowPositions := Dimension;
  try
    SetUpModflowLayers(ColCount, RowCount, ModelHeight);
  except on E: EOutOfMemory do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
    end;
  end;
//  frmGoPhast.PhastModel.ClearNameChangeWarnings;

//  ModelXWidth := ColCount * ColWidth;
//  ModelYWidth := RowCount * RowWidth;

  // Set the selected layer.
  frmGoPhast.ModflowGrid.SelectedLayer := 0;
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;

  MsInitialHead := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(rsModflow_Initial_Head);
  MsInitialHead.Formula := kModelTop;

  SetExaggeration;
  frmGoPhast.RestoreDefault2DView1Click(nil);
//  InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
  result := True;
end;

//procedure TfrmStartUp.InitializeView(ModelXWidth, ModelYWidth, ModelHeight: Real);
//begin
//  SetExaggeration;
//
//  frmGoPhast.InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
//end;

procedure TfrmStartUp.Loaded;
begin
  inherited;
//{$IFNDEF SUTRA}
//  rgChoice.Items.Delete(2);
//{$ENDIF}
end;

procedure TfrmStartUp.SetModflowChoice;
begin
          {$IFDEF OWHMV2}
          // fix this
          Assert(False);
          {$ENDIF}
  if rgChoice.ItemIndex = Ord(scNewModflow) then
  begin
    case comboModflowSelection.ItemIndex of
      0:
        begin
          // MODFLOW 6
          frmGoPhast.ModelSelection := msModflow2015;
        end;
      1:
        begin
          //MODFLOW-2005
          frmGoPhast.ModelSelection := msModflow;
        end;
      2:
        begin
          //MODFLOW-NWT
          frmGoPhast.ModelSelection := msModflowNWT;
        end;
      3:
        begin
          //MODFLOW-OWHM
          frmGoPhast.ModelSelection := msModflowFmp;
        end;
      4:
        begin
          //MODFLOW-CFP
          frmGoPhast.ModelSelection := msModflowCfp;
        end;
      5:
        begin
          //MODFLOW-LGR
          frmGoPhast.ModelSelection := msModflowLGR2;
        end;
    end;
  end;
end;

procedure TfrmStartUp.SetUpModflowLayers(ColCount, RowCount: Integer;
  out ModelHeight: Real);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewLayerElevations: TThreeDRealArray;
  LayerCount: Integer;
  NewDataArray: TDataArray;
  LayerGroup: TLayerGroup;
  LayerStorage: TLayerStorage;
  Value: Real;
  Layers: TList;

//  LayerIndex: Integer;
  LayerName: string;
  LayerNames: TStringList;
  Root: string;
  index: Integer;
begin
  ModelHeight := 1;
  // Set up the layers.
  Layers := TObjectList.Create;
  LayerNames := TStringList.Create;
  try
    LayerNames.Sorted := true;
    LayerNames.CaseSensitive := False;
    for LayerIndex := 2 to rdgInitialLayers.RowCount - 1 do
    begin
      try
        Value := FortranStrToFloatDef(Trim(rdgInitialLayers.Cells[1, LayerIndex]), 0);
        LayerName := Trim(rdgInitialLayers.Cells[0, LayerIndex]);
        if LayerName = '' then
        begin
          Continue;
        end;
        if LayerNames.IndexOf(LayerName) >= 0 then
        begin
          Root := LayerName;
          index := 1;
          repeat
            LayerName := Root + IntToStr(index);
            Inc(index);
          until (LayerNames.IndexOf(LayerName) < 0);
        end;
        LayerNames.Add(LayerName);
        LayerStorage := TLayerStorage.Create;
        Layers.Add(LayerStorage);
        LayerStorage.Name := LayerName;
        LayerStorage.Elevation := Value;
      except
        on E: EConvertError do
        begin
        end;
      end;
      // ignore
    end;
    Value := FortranStrToFloatDef(Trim(rdgInitialLayers.Cells[1, 1]), 0);
    LayerGroup := frmGoPhast.PhastModel.LayerStructure.Add as TLayerGroup;
    LayerGroup.AquiferName := kModelTop;
    NewDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(
      LayerGroup.DataArrayName);
    Assert(NewDataArray <> nil);
//    NewDataArray := frmGoPhast.PhastModel.DataSets[DataArrayIndex];
    NewDataArray.Formula := FortranFloatToStr(Value);
    LayerCount := Layers.Count + 1;
    SetLength(NewLayerElevations, ColCount, RowCount, LayerCount);
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        NewLayerElevations[ColIndex, RowIndex, 0] := Value;
      end;
    end;
    for LayerIndex := 0 to Layers.Count - 1 do
    begin
      LayerStorage := Layers[LayerIndex];
      LayerGroup := frmGoPhast.PhastModel.LayerStructure.Add as TLayerGroup;
      LayerGroup.AquiferName := LayerStorage.Name;
      NewDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerGroup.DataArrayName);
      Assert(NewDataArray <> nil);
//      NewDataArray := frmGoPhast.PhastModel.DataSets[DataArrayIndex];
      NewDataArray.Formula := FortranFloatToStr(LayerStorage.Elevation);
      for ColIndex := 0 to ColCount - 1 do
      begin
        for RowIndex := 0 to RowCount - 1 do
        begin
          NewLayerElevations[ColIndex, RowIndex, LayerIndex + 1] := LayerStorage.Elevation;
        end;
      end;
      if LayerIndex = Layers.Count - 1 then
      begin
        ModelHeight := Value - LayerStorage.Elevation;
      end;
    end;
  finally
    Layers.Free;
    LayerNames.Free;
  end;
  frmGoPhast.ModflowGrid.LayerElevations := NewLayerElevations;
  frmGoPhast.PhastModel.LayerStructure.AssignAssociatedInputDataSets;
//  frmGoPhast.PhastModel.ClearNameChangeWarnings;
end;

function TfrmStartUp.SetUpPhastGrid: Boolean;
var
  Dimension: TOneDRealArray;
  Index: integer;
  XOrigin, YOrigin: double;
  Angle: double;
  XStart, YStart, ZStart: double;
  OriginAngle: double;
  OriginDistance: double;
  ColWidth, RowWidth, LayerHeight: double;
  ColCount, RowCount, LayerCount: integer;
//  ModelXWidth: Real;
//  ModelYWidth: Real;
//  ModelHeight: Real;
begin
  result := False;
  // get some initial data.
  XOrigin := StrToFloat(frameInitialGridPosition.rdeX.Text);
  YOrigin := StrToFloat(frameInitialGridPosition.rdeY.Text);
  Angle := DegToRad(StrToFloat(frameInitialGridPosition.rdeAngle.Text));
  OriginDistance := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

  // Determine where the starting positions for the grid in the top view.
  if OriginDistance <> 0 then
  begin
    OriginAngle := ArcTan2(YOrigin, XOrigin);
    XStart := Cos(Angle - OriginAngle) * OriginDistance;
    YStart := -Sin(Angle - OriginAngle) * OriginDistance;
  end
  else
  begin
    XStart := 0;
    YStart := 0;
  end;
  ZStart := StrToFloat(frameInitialGridPosition.rdeZ.Text);

  frmGoPhast.PhastGrid.GridAngle := Angle;

  ColWidth := StrToFloat(rdeColWidth.Text);
  ColCount := StrToInt(rdeNCol.Text);
  RowWidth := StrToFloat(rdeRowWidth.Text);
  RowCount := StrToInt(rdeNRow.Text);
  LayerHeight := StrToFloat(rdeLayerHeight.Text);
  LayerCount := StrToInt(rdeNLay.Text);

  if (ColWidth <= 0) or (RowWidth <= 0) or (LayerHeight <= 0) then
  begin
    Beep;
    MessageDlg(StrPhastYouMustSpecifyPos, mtError, [mbOK], 0);
    Exit;
  end;

  // Set up the columns.
  SetLength(Dimension, ColCount);
  for Index := 0 to ColCount - 1 do
  begin
    Dimension[Index] := Index * ColWidth + XStart;
  end;
  frmGoPhast.PhastGrid.ColumnPositions := Dimension;

  // Set up the rows.
  SetLength(Dimension, RowCount);
  for Index := 0 to RowCount - 1 do
  begin
    Dimension[Index] := Index * RowWidth + YStart;
  end;
  frmGoPhast.PhastGrid.RowPositions := Dimension;

  // Set up the layers.
  SetLength(Dimension, LayerCount);
  for Index := 0 to LayerCount - 1 do
  begin
    Dimension[Index] := Index * LayerHeight + ZStart;
  end;
  frmGoPhast.PhastGrid.LayerElevations := Dimension;

  // Set the selected layer.
  frmGoPhast.PhastGrid.SelectedLayer := LayerCount - 1;

//  ModelXWidth := ColCount * ColWidth;
//  ModelYWidth := RowCount * RowWidth;
//  ModelHeight := LayerCount * LayerHeight;

//  InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
  SetExaggeration;
  frmGoPhast.RestoreDefault2DView1Click(nil);
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;

  result := True;
end;

procedure TfrmStartUp.pcStartupChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := pcStartup.ActivePage.HelpKeyword;

end;

procedure TfrmStartUp.rdeModflowLayerCountChange(Sender: TObject);
var
  Value: integer;
  RowIndex: Integer;
  Depth: double;
begin
  inherited;
  if rdgInitialLayers = nil then Exit;
  try
    if Trim(rdeModflowLayerCount.Text) <> '' then
    begin
      Value  := StrToInt(Trim(rdeModflowLayerCount.Text));
      if Value > 0 then
      begin
        rdgInitialLayers.RowCount := Value+2;
      end;
    end;
  except on EConvertError do
    begin
      // ignore
    end;
  end;
  for RowIndex := 2 to rdgInitialLayers.RowCount - 1 do
  begin
    if rdgInitialLayers.Cells[0,RowIndex] = '' then
    begin
      rdgInitialLayers.Cells[0,RowIndex] := Format('Layer%d', [RowIndex-1]);
    end;
    if (rdgInitialLayers.Cells[1,RowIndex] = '')
      and TryStrToFloat(rdgInitialLayers.Cells[1,RowIndex-1], Depth) then
    begin
      rdgInitialLayers.Cells[1,RowIndex] := FloatToStr(Depth-10);
    end;
  end;
end;

procedure TfrmStartUp.rdgInitialLayersEndUpdate(Sender: TObject);
begin
  inherited;
  if rdeModflowLayerCount <> nil then
  begin
    rdeModflowLayerCount.Text := IntToStr(rdgInitialLayers.RowCount-2);
  end;
end;

procedure TfrmStartUp.rdgInitialLayersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelect := (ARow <> 1) or (ACol <> 0);
end;

procedure TfrmStartUp.rdgLocationBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  LowerValue: Extended;
  HigherValue: Extended;
begin
  inherited;
  if (ACol >= rdgLocation.FixedCols) and (ARow >= rdgLocation.FixedRows) then
  begin
    LowerValue := StrToFloatDef(rdgLocation.Cells[1,ARow], 0);
    HigherValue := StrToFloatDef(rdgLocation.Cells[2,ARow], 0);
    if LowerValue > HigherValue then
    begin
      rdgLocation.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmStartUp.rgMeshTypeClick(Sender: TObject);
begin
  inherited;
  frameModelLayers.Enabled := TMeshType(rgMeshType.ItemIndex) = mt3D;
  rdeMinimumThickness.Enabled := TMeshType(rgMeshType.ItemIndex) = mt3D;
end;

procedure TfrmStartUp.rgTransportClick(Sender: TObject);
var
  TransportChoice: TTransportChoice;
begin
  inherited;
  TransportChoice := TTransportChoice(rgTransport.ItemIndex);
  rgSaturation.Enabled := TransportChoice in [tcSolute, tcEnergy, tcFreezing];
  if not rgSaturation.Enabled then
  begin
    rgSaturation.ItemIndex := 0;
  end;

end;

procedure TfrmStartUp.FormShow(Sender: TObject);
begin
  inherited;
  HelpKeyword := pcStartup.ActivePage.HelpKeyword;
end;

procedure TfrmStartUp.frameModelLayersGridEndUpdate(Sender: TObject);
begin
  inherited;
  if (frameModelLayers <> nil) and (frameModelLayers.seNumber <> nil) then
  begin
    frameModelLayers.seNumber.AsInteger := frameModelLayers.Grid.RowCount -2;
  end;
//  frameModelLayers.GridEndUpdate(Sender);

end;

procedure TfrmStartUp.frameModelLayersGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ACol = 0) and (ARow = 1) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmStartUp.frameModelLayerssbAddClick(Sender: TObject);
begin
  inherited;
  frameModelLayers.sbAddClick(Sender);

end;

procedure TfrmStartUp.frameModelLayerssbDeleteClick(Sender: TObject);
begin
  inherited;
  if frameModelLayers.Grid.SelectedRow >= frameModelLayers.Grid.FixedRows+1  then
  begin
//    if Grid.RowCount > Grid.FixedRows + 1 then
//    begin
      frameModelLayers.Grid.DeleteRow(frameModelLayers.Grid.SelectedRow);
      frameModelLayersGridEndUpdate(nil);
//    end
//    else
//    begin
//      ClearSelectedRow;
//      seNumber.AsInteger := seNumber.AsInteger -1;
//    end;
  end;
//  frameModelLayers.sbDeleteClick(Sender);

end;

procedure TfrmStartUp.frameModelLayerssbInsertClick(Sender: TObject);
begin
  inherited;
  if frameModelLayers.Grid.SelectedRow >= frameModelLayers.Grid.FixedRows+1  then
  begin
    frameModelLayers.Grid.InsertRow(frameModelLayers.Grid.SelectedRow);
    frameModelLayers.ClearSelectedRow;
    frameModelLayersGridEndUpdate(nil);
  end;
//  frameModelLayers.sbInsertClick(Sender);

end;

procedure TfrmStartUp.frameModelLayersseNumberChange(Sender: TObject);
begin
  inherited;
  frameModelLayers.Grid.RowCount := Max(3, frameModelLayers.seNumber.AsInteger+2);
  frameModelLayers.sbDelete.Enabled := frameModelLayers.seNumber.AsInteger > 0;
//  frameModelLayers.seNumberChange(Sender);

end;

procedure TfrmStartUp.btnDontCreateGridClick(Sender: TObject);
var
  ModelHeight: Real;
begin
  inherited;
  SetModflowChoice;
  SetExaggeration;

  SetUpGeoRefAndModelDescription;
  if frmGoPhast.ModelSelection in ModflowSelection then
  begin
    SetUpModflowLayers(0, 0, ModelHeight);
    frmGoPhast.PhastModel.ModflowOptions.LengthUnit := comboLengthUnit.ItemIndex;
    frmGoPhast.PhastModel.ModflowOptions.TimeUnit := comboTimeUnit.ItemIndex;
  end;
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;
  frmGoPhast.SynchronizeViews(vdTop);
  frmGoPhast.AdjustScales;
  ModalResult := mrOK;
end;

end.

