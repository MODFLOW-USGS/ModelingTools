unit frmExportImageUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, ExtCtrls, Buttons, Grids,
  Mask, JvExMask, JvSpin, JvExStdCtrls, JvCombobox, VirtualTrees,
  DataSetUnit, RbwDataGrid4, Contnrs, DrawTextUnit,
  InPlaceEditUnit, Types, LegendUnit, GR32, RbwRuler, ExtDlgs, ComCtrls,
  DisplaySettingsUnit, JvExExtCtrls, JvNetscapeSplitter, Menus,
  JvToolEdit, JvExButtons, JvBitBtn, PhastModelUnit;

type
  TfrmExportImage = class(TfrmCustomGoPhast)
    pnlControls: TPanel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnClose: TBitBtn;
    btnSaveSettings: TButton;
    lblSavedSettings: TLabel;
    comboSavedSettings: TComboBox;
    fdTextFont: TFontDialog;
    spdSaveImage: TSavePictureDialog;
    pdPrintImage: TPrintDialog;
    scrollBoxPreview: TScrollBox;
    imagePreview: TImage;
    timerDrawImageDelay: TTimer;
    btnRefresh: TBitBtn;
    btnManageSettings: TButton;
    pmChangeStates: TPopupMenu;
    miCheckSelected: TMenuItem;
    UncheckSelected1: TMenuItem;
    JvNetscapeSplitter2: TJvNetscapeSplitter;
    btnSaveImage1: TJvBitBtn;
    JvBitBtn1: TJvBitBtn;
    CatPanelGroup: TCategoryPanelGroup;
    cpView: TCategoryPanel;
    cpText: TCategoryPanel;
    cpAnimation: TCategoryPanel;
    Panel3: TPanel;
    lblSelectedView: TLabel;
    lblImageHeight: TLabel;
    lblImageWidth: TLabel;
    comboView: TComboBox;
    seImageHeight: TJvSpinEdit;
    seImageWidth: TJvSpinEdit;
    cbShowColoredGridLines: TCheckBox;
    cbColorLegend: TCheckBox;
    cbContourLegend: TCheckBox;
    cbHorizontalScale: TCheckBox;
    cbVerticalScale: TCheckBox;
    pnlText: TPanel;
    lblTitle: TLabel;
    sbText: TSpeedButton;
    sbSelect: TSpeedButton;
    memoTitle: TMemo;
    btnTitleFont: TButton;
    btnFont: TButton;
    pnlAnimation: TPanel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    Panel1: TPanel;
    rgDisplayChoice: TRadioGroup;
    btnPreview: TButton;
    btnStop: TButton;
    btnSaveMultipleImages: TBitBtn;
    rdgDataSets: TRbwDataGrid4;
    vstDataSets: TVirtualStringTree;
    cbHeadObsLegend: TCheckBox;
    btnScaleFont: TButton;
    btnCopySettings: TBitBtn;
    btnPasteSettings: TBitBtn;
    cbEndpoints: TCheckBox;
    edColorLegendTitle: TEdit;
    lblColorLegendTitle: TLabel;
    lblContourLegendTitle: TLabel;
    edContourLegendTitle: TEdit;
    procedure FormCreate(Sender: TObject); override;
    procedure seImageHeightChange(Sender: TObject);
    procedure seImageWidthChange(Sender: TObject);
    procedure btnTitleFontClick(Sender: TObject);
    procedure memoTitleChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure imagePreviewDblClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure imagePreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imagePreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbColorLegendClick(Sender: TObject);
    procedure cbContourLegendClick(Sender: TObject);
    procedure cbShowColoredGridLinesClick(Sender: TObject);
    procedure comboViewChange(Sender: TObject);
    procedure cbHorizontalScaleClick(Sender: TObject);
    procedure cbVerticalScaleClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure timerDrawImageDelayTimer(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure comboSavedSettingsCloseUp(Sender: TObject);
    procedure comboSavedSettingsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnManageSettingsClick(Sender: TObject);
    procedure comboSavedSettingsDropDown(Sender: TObject);
    procedure spdSaveImageTypeChange(Sender: TObject);
    procedure vstDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstDataSetsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstDataSetsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure miCheckSelectedClick(Sender: TObject);
    procedure UncheckSelected1Click(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnSaveMultipleImagesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure JvBitBtn1Click(Sender: TObject);
    procedure vstDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure cpAnimationExpand(Sender: TObject);
    procedure cpViewExpand(Sender: TObject);
    procedure cpTextExpand(Sender: TObject);
    procedure cbHeadObsLegendClick(Sender: TObject);
    procedure btnScaleFontClick(Sender: TObject);
    procedure btnCopySettingsClick(Sender: TObject);
    procedure btnPasteSettingsClick(Sender: TObject);
    procedure cbEndpointsClick(Sender: TObject);
    procedure edContourLegendTitleChange(Sender: TObject);
    procedure edColorLegendTitleChange(Sender: TObject);
  private
    FShouldDraw: Boolean;
    FTextItems: TList;
    FDoubleClicked: Boolean;
    FDefaultFont: TFont;
    FSelectingItem: Boolean;
    FSelectedItem: TDrawItem;
    FChangingFont: Boolean;
    FInPlaceEdit: TRbwInplaceEdit;
    FStartX: Integer;
    FStartY: Integer;
    FPriorX: Integer;
    FPriorY: Integer;
    FModelImage: TBitMap32;
    FChangeTime: TDateTime;
    FShouldChange: Boolean;
    FQuerySaveSettings: boolean;
    FCanDraw: Boolean;
    FDataSetDummyObjects: TList;
    FShouldStop: Boolean;
    FRightOffset: Integer;
    FRunning: Boolean;
    FScaleFont: TFont;
    procedure GetData;
    procedure DrawImageAfterDelay;
    procedure DrawTitle(DrawingRect: TRect; ACanvas: TCanvas;
      out TitleRect: TRect);
    procedure DrawTextItems(ACanvas: TCanvas);
    procedure DefaultFontChanged(Sender: TObject);
    procedure SetSelectedItem(const Value: TDrawItem);
    procedure DrawContourLegend(ACanvas: TCanvas; const LegendY: Integer;
      out ContourRect: TRect);
//    function CanColorDataSet(DataArray: TDataArray): boolean;
    procedure GetDataSets;
    procedure DrawHeadObsLegend(ACanvas: TCanvas; const LegendY: Integer;
      out HeadObsRect: TRect);
    procedure ApplyCrossSectionSettings(AModel: TCustomModel;
      ASetting: TDisplaySettingsItem);
    property SelectedItem: TDrawItem read FSelectedItem write SetSelectedItem;
    procedure SelectItemToDrag(X: Integer; Y: Integer);
    function DragItem(X, Y: Integer): Boolean;
    function FinishEditingExistingItem: Boolean;
    procedure AddItem;
    procedure ItemChanged(Sender: TObject);
    function CreateInplaceEditForExistingItem(X, Y: Integer): Boolean;
    procedure ResizeInplaceEdit(Sender: TObject);
    procedure CreateInplaceEditForNewItem(X: Integer; Y: Integer);
    procedure DrawColorLegend(ACanvas: TCanvas; const LegendY: Integer;
      out ColorRect: TRect);
    procedure DrawEndpointLegend(ACanvas: TCanvas; const LegendY: Integer;
      out EndpointRect: TRect);
    procedure DrawModel(DrawingRect: TRect; ACanvas: TCanvas;
      FilterIndex: integer; Factor: double);
    procedure DrawHorizontalScale(var ACanvas: TCanvas; var DrawingRect: TRect);
    procedure DrawVerticalScale(var ACanvas: TCanvas; var DrawingRect: TRect);
    procedure DrawBackgroundImages(BitMap32OrCanvas: TPersistent; Width, Height: integer);
    procedure DrawOnCanvas(CanvasWidth, CanvasHeight: Integer;
      ACanvas: TCanvas; FilterIndex: integer; Factor: double);
    procedure DrawBackground(ACanvas: TCanvas;
      CanvasHeight, CanvasWidth: Integer);
    // @name draws the items outside the frame for the model.
    // After @name is called, DrawingRect is the area within which
    // the model should be drawn. However, for side views, the area
    // may need to be rotated 90 degrees.
    procedure DrawOutsideItems(CanvasHeight, CanvasWidth: Integer;
      var DrawingRect: TRect; ACanvas: TCanvas);
    procedure DrawImage;
    procedure SaveContourSettings(
      ContourDisplaySettings: TContourDisplaySettings);
    procedure SaveColorDisplaySettings(
      ColorDisplaySettings: TColorDisplaySettings);
    procedure SaveSettings;
    procedure ApplySettings;
    procedure ApplyContourDisplaySettings(
      ContourDisplaySettings: TContourDisplaySettings);
    procedure ApplyColorDisplaySettings(
      ColorDisplaySettings: TColorDisplaySettings);
    procedure UpdateModelColors;
    procedure ExpandText(Sender: TObject; var TextToDraw: string);
    procedure SaveImage(FilterIndex: Integer; FileName: string);
    procedure SetStateOfMultipleNodes(BaseNode: PVirtualNode;
      NewState: TCheckState);
    procedure CollapseOtherPanels(Sender: TObject);
    procedure ApplyMacro(CommentLines: TStringList; CommentSearchKey: string;
      TextSearchKey: string; var TextToDraw: string);
    procedure AssignASetting(PhastModel: TPhastModel; ASetting: TDisplaySettingsItem);
    procedure ApplyASetting(ASetting: TDisplaySettingsItem; PhastModel: TPhastModel);
    procedure AssignEndpointSettings(ASetting: TDisplaySettingsItem);
    procedure ApplyEndPointSettings(ASetting: TDisplaySettingsItem);
    { Private declarations }
  public
    { Public declarations }
  end;

var frmExportImage: TfrmExportImage = nil;  

implementation

uses
  EdgeDisplayUnit, Math, JclStrings, GoPhastTypes, frmGoPhastUnit,
  AbstractGridUnit, BigCanvasMethods, ScreenObjectUnit,
  CompressedImageUnit, frameViewUnit, frmGoToUnit,
  UndoItems, frmManageSettingsUnit,
  UndoItemsScreenObjects, ClassificationUnit, frmProgressUnit,
  frmErrorsAndWarningsUnit, Clipbrd, RbwParser, frmDisplayDataUnit,
  pngimage, jpeg, VectorDisplayUnit, ModflowHeadObsResults,
  Generics.Collections, DrawMeshTypesUnit;

resourcestring
  StrProgress = 'Progress';
  StrSHasTheWrongTyp = '%s has the wrong type of data. Only data sets with r' +
  'eal numbers can be contoured.';
  StrDisplayingS = 'Displaying %s';
  StrYouMustEnterANam = 'You must enter a name for these settings to save th' +
  'em.  Type the name in the "Saved settings" box.';
  StrDoYouWantToSave = 'Do you want to save the current image settings.';
  StrTheSImageCanNot = 'The %s image can not be shown at this magnification.' +
  ' When the magnification is reduced, it will be displayed again.';
  StrDataSets = 'Data Sets';
  StrTheClipboardConten = 'The clipboard contents were not a valid image set' +
  'ting.';
  StrDefaultSettingPrefix = 'Setting_';
  StrTheSideViewCanNo = 'The side view can not be visualized in this model t' +
  'ype.';
  StrUnableToSaveFile = 'Unable to save file. The error message was "%s".';

const
  StrSP = '%SP';
  StrTS = '%TS';
  StrET = '%ET';
  StrTrS = '%TrS';
  VectorGraphFilterIndex = 5;
  BmpFilterIndex = 2;

{$R *.dfm}

procedure TfrmExportImage.AddItem;
var
  Item: TDrawItem;
  ARect: TRect;
begin
  if Trim(FInPlaceEdit.Text) <> '' then
  begin
    Item := TDrawItem.Create;
    Item.OnChange := ItemChanged;
    Item.OnDraw := ExpandText;
    FTextItems.Add(Item);
    ARect := Item.Rect;
    ARect.Left := FPriorX + scrollBoxPreview.HorzScrollBar.Position;
    ARect.Top := FPriorY + scrollBoxPreview.VertScrollBar.Position;
    Item.Rect := ARect;
    Item.Text := FInPlaceEdit.Text;
    Item.Font.Assign(FDefaultFont);
    Item.Selected := True;
    Item.Editing := False;
    SelectedItem := Item;
    FShouldDraw := True;
    FQuerySaveSettings := True;
  end;
end;

procedure TfrmExportImage.btnPasteSettingsClick(Sender: TObject);
var
  ClipboardText: string;
  ClipStream: TStringStream;
  DisplaySettings: TDisplaySettingsComponent;
  MemStream: TMemoryStream;
  SettingIndex: Integer;
  ASetting: TDisplaySettingsItem;
  PhastModel: TPhastModel;
  ExistingDisplaySettings: TDisplaySettingsCollection;
  NameIndex: Integer;
  ExistingSetting: TDisplaySettingsItem;
  TestName: string;
begin
  inherited;
  ClipboardText := ClipBoard.AsText;
  PhastModel := frmGoPhast.PhastModel;
  ExistingDisplaySettings := PhastModel.DisplaySettings;

  try
    ClipStream := TStringStream.Create(ClipboardText);
    try
      ClipStream.Position := 0;
      MemStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(ClipStream, MemStream);
        DisplaySettings := TDisplaySettingsComponent.Create(nil);
        try
          MemStream.Position := 0;
          MemStream.ReadComponent(DisplaySettings);
//          ExistingSetting := nil;
          for SettingIndex := 0 to DisplaySettings.Settings.Count - 1 do
          begin
            ASetting := DisplaySettings.Settings.Items[SettingIndex]
              as TDisplaySettingsItem;
            NameIndex := 1;
            while ASetting.Name = '' do
            begin
              TestName := StrDefaultSettingPrefix + IntToStr(NameIndex);
              if ExistingDisplaySettings.GetItemByName(TestName) = nil then
              begin
                ASetting.Name := TestName;
              end
              else
              begin
                Inc(NameIndex);
              end;
            end;
            ExistingSetting := ExistingDisplaySettings.GetItemByName(ASetting.Name);
            if ExistingSetting <> nil then
            begin
              ExistingSetting.Assign(ASetting);
              comboSavedSettings.ItemIndex := comboSavedSettings.Items.IndexOf(ASetting.Name);
            end
            else
            begin
              ASetting.Collection := ExistingDisplaySettings;
              ExistingSetting := ASetting;
              comboSavedSettings.ItemIndex :=
                comboSavedSettings.Items.AddObject(ExistingSetting.Name,
                ExistingSetting);
            end;
            ApplyASetting(ExistingSetting, PhastModel);
          end;
          SaveSettings;
          FreeAndNil(FModelImage);
          DrawImage;
        finally
          DisplaySettings.Free;
        end;
      finally
        MemStream.Free;
      end;
    finally
      ClipStream.Free;
    end;
  except on EParserError do
    begin
      Beep;
      MessageDlg(StrTheClipboardConten, mtError, [mbOK], 0);
    end;

  end;


  end;

procedure TfrmExportImage.btnPreviewClick(Sender: TObject);
const
  HalfSecond = 1/24/3600/2;
var
  Index: Integer;
  DataSetList: TList;
  DataArray: TDataArray;
  PriorDataArray: TDataArray;
  CurrentTime: TDateTime;
  Legend: TLegend;
  RootName: string;
  FileName: string;
  Extension: string;
  ImageIndex: Integer;
  PadIndex: integer;
  Grid: TCustomModelGrid;
  Mesh: IDrawMesh;
//  Mesh: TSutraMesh3D;
begin
  inherited;
  FShouldStop := False;
  FRunning := True;
  DataSetList := TList.Create;
  Legend := TLegend.Create(nil);
  Grid := frmGoPhast.Grid;
  Mesh := frmGoPhast.PhastModel.DrawMesh;
  try
    for Index := 1 to rdgDataSets.RowCount - 1 do
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.
        GetDataSetByName(rdgDataSets.Cells[1, Index]);
      if DataArray <> nil then
      begin
        DataSetList.Add(DataArray);
      end;
    end;

    if Sender = btnSaveMultipleImages then
    begin
      Extension := ExtractFileExt(spdSaveImage.FileName);
      RootName := ChangeFileExt(spdSaveImage.FileName, '');
    end
    else
    begin
      Extension := '';
      RootName := '';
    end;

    PriorDataArray := nil;
    frmProgressMM.Caption := StrProgress;
    frmProgressMM.Show;
    for Index := 0 to DataSetList.Count - 1 do
    begin
      CurrentTime := Now;
      DataArray := DataSetList[Index];
      if (rgDisplayChoice.ItemIndex = 1)
        and (DataArray.DataType <> rdtDouble) then
      begin
        Beep;
        MessageDlg(Format(StrSHasTheWrongTyp, [DataArray.Name]),
          mtError, [mbOK], 0);
        Exit;
      end;
      frmProgressMM.Caption := Format(StrDisplayingS, [DataArray.Name]);
      Application.ProcessMessages;
      if Grid <> nil then
      begin
        case rgDisplayChoice.ItemIndex of
          0:
            begin
              if Index > 0 then
              begin
                DataArray.Limits := PriorDataArray.Limits;
                Legend.Assign(frmGoPhast.PhastModel.ColorLegend);
              end;
              case DataArray.Orientation of
                dsoTop:
                  begin
                    Grid.TopDataSet := DataArray;
                  end;
                dsoFront:
                  begin
                    Grid.FrontDataSet := DataArray;
                  end;
                dsoSide:
                  begin
                    Grid.SideDataSet := DataArray;
                  end;
                dso3D:
                  begin
                    Grid.TopDataSet := DataArray;
                    Grid.FrontDataSet := DataArray;
                    Grid.SideDataSet := DataArray;
                    frmGoPhast.PhastModel.ThreeDDataSet := DataArray;
                  end;
                else
                  Assert(False);
              end;
            end;
          1:
            begin
              if Index > 0 then
              begin
                DataArray.ContourLimits := PriorDataArray.ContourLimits;
                DataArray.Contours := PriorDataArray.Contours;
                if DataArray.Contours = nil then
                begin
                  Continue;
                end;
                DataArray.Contours.SpecifyContours :=
                  PriorDataArray.Contours.SpecifyContours;
                Legend.Assign(frmGoPhast.PhastModel.ContourLegend);
              end;
              case DataArray.Orientation of
                dsoTop:
                  begin
                    Grid.TopContourDataSet := DataArray;
                  end;
                dsoFront:
                  begin
                    Grid.FrontContourDataSet := DataArray;
                  end;
                dsoSide:
                  begin
                    Grid.SideContourDataSet := DataArray;
                  end;
                dso3D:
                  begin
                    Grid.TopContourDataSet := DataArray;
                    Grid.FrontContourDataSet := DataArray;
                    Grid.SideContourDataSet := DataArray;
                    Grid.ThreeDContourDataSet := DataArray;
                  end;
                else
                  Assert(False);
              end;
            end;
          else
            Assert(False);
        end;
        Grid.GridChanged;
      end
      else
      begin
        Assert(Mesh <> nil);
        case rgDisplayChoice.ItemIndex of
          0:
            begin
              if Index > 0 then
              begin
                DataArray.Limits := PriorDataArray.Limits;
                Legend.Assign(frmGoPhast.PhastModel.ColorLegend);
              end;
              case DataArray.Orientation of
                dsoTop:
                  begin
                    Mesh.TopDataSet := DataArray;
                  end;
                dsoFront:
                  begin
//                    Mesh.ThreeDDataSet := DataArray;
                  end;
                dsoSide:
                  begin
//                    Mesh.SideDataSet := DataArray;
                  end;
                dso3D:
                  begin
                    Mesh.TopDataSet := DataArray;
                    Mesh.ThreeDDataSet := DataArray;
                    frmGoPhast.PhastModel.ThreeDDataSet := DataArray;
//                    Mesh.SideDataSet := DataArray;
                  end;
                else
                  Assert(False);
              end;
            end;
          1:
            begin
              if Index > 0 then
              begin
                DataArray.ContourLimits := PriorDataArray.ContourLimits;
                DataArray.Contours := PriorDataArray.Contours;
                if DataArray.Contours = nil then
                begin
                  Continue;
                end;
                DataArray.Contours.SpecifyContours :=
                  PriorDataArray.Contours.SpecifyContours;
                DataArray.ContourInterval.Value := PriorDataArray.ContourInterval.Value;
                Legend.Assign(frmGoPhast.PhastModel.ContourLegend);
              end;
              case DataArray.Orientation of
                dsoTop:
                  begin
                    Mesh.TopContourDataSet := DataArray;
                  end;
                dsoFront:
                  begin
                    Mesh.ThreeDContourDataSet := DataArray;
                  end;
                dsoSide:
                  begin
//                    Mesh.SideContourDataSet := DataArray;
                  end;
                dso3D:
                  begin
                    Mesh.TopContourDataSet := DataArray;
                    Mesh.ThreeDContourDataSet := DataArray;
//                    Mesh.SideContourDataSet := DataArray;
                  end;
                else
                  Assert(False);
              end;
              Mesh.ThreeDContourDataSet := DataArray;
            end;
          else
            Assert(False);
        end;
        Mesh.MeshChanged;
      end;

      UpdateFrmDisplayData(True);
//      UpdateFrmContourData;
      Application.ProcessMessages;
      PriorDataArray := DataArray;
      if FShouldStop then
      begin
        break;
      end;
      if Index > 0 then
      begin
        case rgDisplayChoice.ItemIndex of
          0:
            begin
              frmGoPhast.PhastModel.ColorLegend.Assign(Legend);
            end;
          1:
            begin
              frmGoPhast.PhastModel.ContourLegend.Assign(Legend);
            end;
        end;
      end;
      while frmGoPhast.frameTopView.Drawing
        or frmGoPhast.frameFrontView.Drawing
        or frmGoPhast.frameSideView.Drawing
        or frmGoPhast.frameTopView.PaintingNeeded
        or frmGoPhast.frameFrontView.PaintingNeeded
        or frmGoPhast.frameSideView.PaintingNeeded
        do
      begin
      end;
      if Index = 0 then
      begin
        case rgDisplayChoice.ItemIndex of
          0:
            begin
              Legend.Assign(frmGoPhast.PhastModel.ColorLegend);
            end;
          1:
            begin
              Legend.Assign(frmGoPhast.PhastModel.ContourLegend);
            end;
        end;
      end;

      FreeAndNil(FModelImage);
      DrawImage;

      if Sender = btnPreview then
      begin
        while Now - CurrentTime < HalfSecond do
        begin
          Application.ProcessMessages;
        end;
      end
      else if Sender = btnSaveMultipleImages then
      begin
        ImageIndex := Index + 1;
        FileName := RootName;
        for PadIndex := Trunc(Log10(ImageIndex)) to
          Trunc(Log10(DataSetList.Count+1))-1 do
        begin
          FileName := FileName + '0';
        end;
        FileName := FileName + IntToStr(ImageIndex);
        FileName := ChangeFileExt(FileName, Extension);
        SaveImage(spdSaveImage.FilterIndex, FileName);
      end
      else
      begin
        Assert(False);
      end;
      if FShouldStop then
      begin
        break;
      end;
    end;
  finally
    frmProgressMM.Hide;
    DataSetList.Free;
    Legend.Free;
    FRunning := False;
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

procedure TfrmExportImage.btnCloseClick(Sender: TObject);
begin
  FShouldStop := True;
  inherited;
end;

procedure TfrmExportImage.btnCopySettingsClick(Sender: TObject);
var
  PhastModel: TPhastModel;
  ModifiedDisplaySettings: TDisplaySettingsComponent;
  MemStream: TMemoryStream;
  ClipStream: TStringStream;
  ASetting: TDisplaySettingsItem;
begin
  inherited;
  PhastModel := frmGoPhast.PhastModel;
  ModifiedDisplaySettings := TDisplaySettingsComponent.Create(nil);
  try
    ASetting := ModifiedDisplaySettings.Settings.Add as TDisplaySettingsItem;
    if comboSavedSettings.Text <> comboSavedSettings.Items[0] then
    begin
      ASetting.Name := comboSavedSettings.Text;
    end;
    AssignASetting(PhastModel, ASetting);

    MemStream := TMemoryStream.Create;
    ClipStream := TStringStream.Create('');
    try
      MemStream.WriteComponent(ModifiedDisplaySettings);
      MemStream.Position := 0;
      ClipStream.Position := 0;
      ObjectBinaryToText(MemStream, ClipStream);
      ClipStream.Position := 0;
      Clipboard.AsText := ClipStream.ReadString(ClipStream.Size);
    finally
      ClipStream.Free;
      MemStream.Free;
    end;

  finally
    ModifiedDisplaySettings.Free;
  end;
end;

procedure TfrmExportImage.btnFontClick(Sender: TObject);
begin
  inherited;
  fdTextFont.Font := FDefaultFont;
  if fdTextFont.Execute then
  begin
    FDefaultFont.Assign(fdTextFont.Font);
    FQuerySaveSettings := True;
  end;
end;

procedure TfrmExportImage.btnManageSettingsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageSettings);
  GetData;
end;

procedure TfrmExportImage.btnSaveMultipleImagesClick(Sender: TObject);
var
  OldIndex: Integer;
begin
  inherited;
  OldIndex := spdSaveImage.FilterIndex;
  try
    spdSaveImage.FilterIndex := 2;
    if spdSaveImage.Execute then
    begin
      btnPreviewClick(Sender);
    end;
  finally
    spdSaveImage.FilterIndex := OldIndex;
  end;
end;

procedure TfrmExportImage.btnSaveImageClick(Sender: TObject);
begin
  inherited;
  if spdSaveImage.Execute then
  begin
    try
      SaveImage(spdSaveImage.FilterIndex, spdSaveImage.FileName);
    except
      on E: EFCreateError do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TfrmExportImage.btnSaveSettingsClick(Sender: TObject);
begin
  inherited;
  if (comboSavedSettings.ItemIndex = 0) or (comboSavedSettings.Text = '') then
  begin
    Beep;
    MessageDlg(StrYouMustEnterANam, mtError, [mbOK], 0);
  end
  else
  begin
    SaveSettings;
  end;
end;

procedure TfrmExportImage.btnScaleFontClick(Sender: TObject);
begin
  inherited;
  fdTextFont.Font := FScaleFont;
  if fdTextFont.Execute then
  begin
    FScaleFont.Assign(fdTextFont.Font);
    FQuerySaveSettings := True;
    DrawImage;
  end;
end;

procedure TfrmExportImage.btnStopClick(Sender: TObject);
begin
  inherited;
  FShouldStop := True;
end;

procedure TfrmExportImage.btnTitleFontClick(Sender: TObject);
begin
  inherited;
  fdTextFont.Font := memoTitle.Font;
  if fdTextFont.Execute then
  begin
    memoTitle.Font := fdTextFont.Font;
    DrawImage;
    FQuerySaveSettings := True;
  end;
end;

procedure TfrmExportImage.cbColorLegendClick(Sender: TObject);
begin
  inherited;
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cbContourLegendClick(Sender: TObject);
begin
  inherited;
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cbEndpointsClick(Sender: TObject);
begin
  inherited;
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cbHeadObsLegendClick(Sender: TObject);
begin
  inherited;
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cbHorizontalScaleClick(Sender: TObject);
begin
  inherited;
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cbShowColoredGridLinesClick(Sender: TObject);
begin
  inherited;
  FreeAndNil(FModelImage);
  DrawImageAfterDelay;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cbVerticalScaleClick(Sender: TObject);
begin
  inherited;
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.miCheckSelectedClick(Sender: TObject);
begin
  inherited;
  vstDataSets.BeginUpdate;
  try
    SetStateOfMultipleNodes(vstDataSets.RootNode, csCheckedNormal);
  finally
    vstDataSets.EndUpdate;
  end;
end;

procedure TfrmExportImage.comboSavedSettingsChange(Sender: TObject);
begin
  inherited;
  if FShouldChange then
  begin
    ApplySettings;
    FreeAndNil(FModelImage);
    DrawImage;
  end;
  FShouldChange := False;
end;

procedure TfrmExportImage.comboSavedSettingsCloseUp(Sender: TObject);
begin
  inherited;
  FShouldChange := True;
end;

procedure TfrmExportImage.comboSavedSettingsDropDown(Sender: TObject);
begin
  inherited;
  if FQuerySaveSettings then
  begin
    FQuerySaveSettings := False;
    if (MessageDlg(StrDoYouWantToSave, mtConfirmation,
      [mbYes, mbNo], 0) = mrYes) then
    begin
      btnSaveSettingsClick(nil);
    end;
  end;
end;

procedure TfrmExportImage.comboViewChange(Sender: TObject);
var
  ViewDirection: TViewDirection;
begin
  inherited;
  FreeAndNil(FModelImage);
  ViewDirection := TViewDirection(comboView.ItemIndex);
  case ViewDirection of
    vdTop:
      begin
        seImageHeight.AsInteger := frmGoPhast.frameTopView.ZoomBox.Height;
        seImageWidth.AsInteger := frmGoPhast.frameTopView.ZoomBox.Width;
      end;
    vdFront:
      begin
        seImageHeight.AsInteger := frmGoPhast.frameFrontView.ZoomBox.Height;
        seImageWidth.AsInteger := frmGoPhast.frameFrontView.ZoomBox.Width;
      end;
    vdSide:
      begin
        if (frmGoPhast.ModelSelection in SutraSelection)
          or frmGoPhast.PhastModel.DisvUsed then
        begin
          Beep;
          MessageDlg(StrTheSideViewCanNo, mtWarning, [mbOK], 0);
          Exit;
        end;
        seImageHeight.AsInteger := frmGoPhast.frameSideView.ZoomBox.Width;
        seImageWidth.AsInteger := frmGoPhast.frameSideView.ZoomBox.Height;
      end;
  end;
  DrawImageAfterDelay;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.cpAnimationExpand(Sender: TObject);
begin
  inherited;
  CollapseOtherPanels(Sender);
end;

procedure TfrmExportImage.cpTextExpand(Sender: TObject);
begin
  inherited;
  CollapseOtherPanels(Sender);
end;

procedure TfrmExportImage.cpViewExpand(Sender: TObject);
begin
  inherited;
  CollapseOtherPanels(Sender);
end;

function TfrmExportImage.CreateInplaceEditForExistingItem(X,
  Y: Integer): Boolean;
var
  Index: Integer;
  Item: TDrawItem;
  CanSelect: boolean;
  EditText: string;
  ARect: TRect;
begin
  result := False;
  SelectedItem := nil;
  if sbText.Down then
  begin
    CanSelect := True;
    for Index := FTextItems.Count - 1 downto 0 do
    begin
      Item := FTextItems[Index];
      if CanSelect and GR32.PtInRect(Item.Rect, GR32.Point(X, Y)) then
      begin
        CanSelect := False;
        Item.Selected := True;
        SelectedItem := Item;
        if FDoubleClicked then
        begin
          Item.Editing := True;
          FDoubleClicked := False;
          FInPlaceEdit := TRbwInplaceEdit.Create(self);
          try
            EditText := Item.Text;
            ARect.Left := Item.Rect.Left + scrollBoxPreview.Left - 3
              - scrollBoxPreview.HorzScrollBar.Position;
            ARect.Top := Item.Rect.Top
              - scrollBoxPreview.VertScrollBar.Position;
            ARect.Right := Item.Rect.Left + 16;
            ARect.Bottom := Item.Rect.Top + 8;

            FInPlaceEdit.Parent := self;
            FInPlaceEdit.Font := FDefaultFont;
            FInPlaceEdit.SetBounds(ARect.Left, ARect.Top,
              ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
            FInPlaceEdit.OnChange := ResizeInplaceEdit;
            FInPlaceEdit.Text := EditText;
            FShouldDraw := True;
            FInPlaceEdit.SelectAll;
            FInPlaceEdit.SetFocus;
            FInPlaceEdit.MouseCapture := False;
            FInPlaceEdit.Run;
            SelectedItem := nil;
            if (FInPlaceEdit <> nil) and (FInPlaceEdit.ModalResult = mrOK) then
            begin
              if FInPlaceEdit.Text = '' then
              begin
                FTextItems.Remove(Item);
                FQuerySaveSettings := True;
              end
              else
              begin
                Item.Text := FInPlaceEdit.Text;
                Item.Editing := False;
                FQuerySaveSettings := True;
              end;
            end;
            Item.Editing := False;
            result := True;
          finally
            FreeAndNil(FInPlaceEdit);
          end;
        end
        else
        begin
          Item.Editing := False;
          result := True;
        end;
      end
      else
      begin
        Item.Selected := False;
        Item.Editing := False;
      end;
    end;
  end;
end;

procedure TfrmExportImage.CreateInplaceEditForNewItem(X, Y: Integer);
var
  Extent: TSize;
  EditText: string;
  ARect: TRect;
begin
  if FShouldDraw then
  begin
    DrawImage;
  end;
  if sbText.Down then
  begin
    if SelectedItem <> nil then
    begin
      SelectedItem.Selected := False;
      SelectedItem.Editing := False;
    end;
    SelectedItem := nil;
    Extent := Canvas.TextExtent(' ');
    FInPlaceEdit := TRbwInplaceEdit.Create(self);
    try
      FPriorX := X - scrollBoxPreview.HorzScrollBar.Position;
      FPriorY := Y - scrollBoxPreview.VertScrollBar.Position;
      EditText := ' ';
      ARect.Left := FPriorX + scrollBoxPreview.Left - 3;
      ARect.Top := FPriorY;
      ARect.Right := ARect.Left + Extent.cx + 16;
      ARect.Bottom := ARect.Top + Extent.cy + 8;

      FInPlaceEdit.Parent := self;
      FInPlaceEdit.Font := FDefaultFont;
      FInPlaceEdit.SetBounds(ARect.Left, ARect.Top,
        ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
      FInPlaceEdit.OnChange := ResizeInplaceEdit;
      FInPlaceEdit.Text := EditText;
      FInPlaceEdit.SelectAll;
      FInPlaceEdit.SetFocus;
      FInPlaceEdit.MouseCapture := True;
      FInPlaceEdit.Run;
      if (FInPlaceEdit <> nil) and (FInPlaceEdit.ModalResult = mrOK) then
      begin
        AddItem;
      end;
    finally
      FreeAndNil(FInPlaceEdit);
    end;
  end;
end;

procedure TfrmExportImage.DrawColorLegend(ACanvas: TCanvas;
  const LegendY: Integer; Out ColorRect: TRect);
var
  ViewDirection: TViewDirection;
  PhastModel: TPhastModel;
  Grid: TCustomModelGrid;
  ShowLegend: Boolean;
  Mesh: IDrawMesh;
begin
  ColorRect.Left := 0;
  ColorRect.Top := 0;
  ColorRect.BottomRight := ColorRect.TopLeft;

  if cbColorLegend.Checked then
  begin
    ViewDirection := TViewDirection(comboView.ItemIndex);
    PhastModel := frmGoPhast.PhastModel;
    Grid := PhastModel.Grid;
    Mesh := PhastModel.DrawMesh;
    ShowLegend := False;
    if Grid <> nil then
    begin
      case ViewDirection of
        vdTop:
          begin
            ShowLegend := (Grid.TopDataSet <> nil)
              or (PhastModel.EdgeDisplay <> nil);
          end;
        vdFront:
          begin
            ShowLegend := (Grid.FrontDataSet <> nil);
          end;
        vdSide:
          begin
            ShowLegend := (Grid.SideDataSet <> nil);
          end;
      else
        Assert(False);
      end;
    end
    else if Mesh <> nil then
    begin
      case ViewDirection of
        vdTop:
          begin
            ShowLegend := (Mesh.TopDataSet <> nil)
              or (PhastModel.EdgeDisplay <> nil);
          end;
        vdFront:
          begin
            ShowLegend := (Mesh.ThreeDDataSet <> nil);
          end;
        vdSide:
          begin
            ShowLegend := False
          end;
      else
        Assert(False);
      end;
    end;
    if ShowLegend then
    begin
      if frmDisplayData = nil then
      begin
        Application.CreateForm(TfrmDisplayData, frmDisplayData);
      end;
      UpdateFrmDisplayData(True);
      PhastModel.ColorLegend.Caption := edColorLegendTitle.Text;
      PhastModel.ColorLegend.Draw(ACanvas, 10, LegendY, ColorRect,
        frmDisplayData.frameColorGrid.LegendFont);
    end;
  end;
end;

procedure TfrmExportImage.DrawModel(DrawingRect: TRect; ACanvas: TCanvas;
  FilterIndex: integer; Factor: double);
var
  TempBMP: TBitmap;
  AScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  ViewDirection: TViewDirection;
  Orientation: TDataSetOrientation;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  VelocityVectors: TVectorCollection;
  VItem: TVectorItem;
  ModelMetaFile: TMetaFile;
  ModelMetaFileCanvas: TMetaFileCanvas;
  TransformMatrix: XFORM;
  MyRgn: HRGN;
  Mesh: IDrawMesh;
begin
  { TODO : Try to condense this. }
  ViewDirection := TViewDirection(comboView.ItemIndex);
  if FilterIndex <> VectorGraphFilterIndex then
  begin
    if FModelImage = nil then
    begin
      UpdateModelColors;
      FModelImage := TBitmap32.Create;
      FModelImage.Font := frmGoPhast.PhastModel.ContourFont;
      if ViewDirection = vdSide then
      begin
        FModelImage.Width := DrawingRect.Bottom - DrawingRect.Top;
        FModelImage.Height := DrawingRect.Right - DrawingRect.Left;
      end
      else
      begin
        FModelImage.Height := DrawingRect.Bottom - DrawingRect.Top;
        FModelImage.Width := DrawingRect.Right - DrawingRect.Left;
      end;
      DrawBigRectangle32(FModelImage, clBlack32, clWhite32, 1, 0, 0,
        FModelImage.Width - 1, FModelImage.Height - 1);

      DrawBackgroundImages(FModelImage, FModelImage.Width, FModelImage.Height);

      LocalModel := frmGoPhast.PhastModel;
      if LocalModel.Grid = nil then
      begin
        Mesh := LocalModel.DrawMesh;
        if Mesh <> nil then
        begin
          Mesh.Draw(FModelImage, ViewDirection);
        end;
      end
      else
      begin
        LocalModel.Grid.DrawColoredGridLines := cbShowColoredGridLines.Checked;
        if LocalModel.LgrUsed then
        begin
          for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
            ChildModel.Grid.DrawColoredGridLines := cbShowColoredGridLines.Checked;
          end;
        end;
        try
          frmGoPhast.Grid.Draw(FModelImage, ViewDirection);
        finally
          frmGoPhast.Grid.DrawColoredGridLines := True;
          if LocalModel.LgrUsed then
          begin
            for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
            begin
              ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
              ChildModel.Grid.DrawColoredGridLines := True;
            end;
          end;
        end;
      end;

      if ViewDirection = vdTop then
      begin
        LocalModel.DrawPilotPoints(FModelImage);
      end;


      for ScreenObjectIndex := 0 to
        frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
        AScreenObject.Draw(FModelImage, ViewDirection);
      end;

      Orientation := dsoTop;
      case ViewDirection of
        vdTop: Orientation := dsoTop;
        vdFront: Orientation := dsoFront;
        vdSide: Orientation := dsoSide;
        else Assert(False);
      end;

      if (LocalModel.ModelSelection in ModflowSelection)
        and (ViewDirection = vdTop) then
      begin
        LocalModel.DrawHeadObservations(FModelImage, frmGoPhast.frameTopView.ZoomBox);
        LocalModel.DrawPestPointObservations(FModelImage, frmGoPhast.frameTopView.ZoomBox);
        LocalModel.DrawSfrStreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
        LocalModel.DrawStrStreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
        LocalModel.DrawSfrMf6StreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
        LocalModel.DrawSwrReachConnections(FModelImage, frmGoPhast.frameTopView.ZoomBox);
          if LocalModel.LgrUsed then
        begin
          for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
            ChildModel.DrawHeadObservations(FModelImage, frmGoPhast.frameTopView.ZoomBox);
            ChildModel.DrawPestPointObservations(FModelImage, frmGoPhast.frameTopView.ZoomBox);
            ChildModel.DrawSfrStreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
            ChildModel.DrawStrStreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
            ChildModel.DrawSfrMf6StreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
            ChildModel.DrawSwrReachConnections(FModelImage, frmGoPhast.frameTopView.ZoomBox);
          end;
        end;
      end
      else if (LocalModel.ModelSelection in SutraSelection)
        and (ViewDirection = vdTop) then
      begin
        LocalModel.DrawPestPointObservations(FModelImage, frmGoPhast.frameTopView.ZoomBox);
      end;
      if (LocalModel.ModelSelection in ModflowSelection)
        {and not LocalModel.DisvUsed} then
      begin
        LocalModel.Pathlines.Draw(Orientation, FModelImage);
        LocalModel.EndPoints.Draw(Orientation, FModelImage);
        LocalModel.TimeSeries.Draw(Orientation, FModelImage);
        if LocalModel.LgrUsed then
        begin
          for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
            ChildModel.Pathlines.Draw(Orientation, FModelImage);
            ChildModel.EndPoints.Draw(Orientation, FModelImage);
            ChildModel.TimeSeries.Draw(Orientation, FModelImage);
          end;
        end;
      end;

      if LocalModel.ModelSelection in (SutraSelection + [msModflow2015]) then
      begin
        LocalModel.MaxVectors.PlotVectors2D(ViewDirection, FModelImage);
        LocalModel.MidVectors.PlotVectors2D(ViewDirection, FModelImage);
        LocalModel.MinVectors.PlotVectors2D(ViewDirection, FModelImage);
        VelocityVectors := LocalModel.VelocityVectors;
        if VelocityVectors.SelectedItem >= 0 then
        begin
          VItem := VelocityVectors.Items[VelocityVectors.SelectedItem] as TVectorItem;
          VItem.Vectors.PlotVectors2D(ViewDirection, FModelImage);
        end;
      end;

      if (LocalModel.ModelSelection in ModflowSelection)
        and (ViewDirection in [vdFront, vdSide]) then
      begin
        LocalModel.CrossSection.Draw(FModelImage, ViewDirection);
        if LocalModel.LgrUsed then
        begin
          for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
            ChildModel.CrossSection.Draw(FModelImage, ViewDirection);
          end;
        end;
      end;

      if ViewDirection = vdSide then
      begin
        FModelImage.Rotate90;
      end;
    end;
    TempBMP := TBitMap.Create;
    try
      TempBMP.Assign(FModelImage);
      ACanvas.Draw(DrawingRect.Left, DrawingRect.Top, TempBMP);
    finally
      TempBMP.Free;
    end;
  end
  else
  begin
    ModelMetaFile := TMetaFile.Create;
    try
      ModelMetaFile.MMWidth := Trunc((DrawingRect.Right-DrawingRect.Left)* Factor);
      ModelMetaFile.MMHeight := Trunc((DrawingRect.Bottom-DrawingRect.Top)* Factor);
      ModelMetaFileCanvas := TMetaFileCanvas.Create(ModelMetaFile, 0);
      try
        MyRgn := CreateRectRgn(0,0,DrawingRect.Right-DrawingRect.Left,DrawingRect.Bottom-DrawingRect.Top);
        SelectClipRgn(ModelMetaFileCanvas.Handle,MyRgn);
        try
          if ViewDirection = vdSide then
          begin
            SetGraphicsMode(ModelMetaFileCanvas.Handle, GM_ADVANCED);
            SetMapMode(ModelMetaFileCanvas.Handle, MM_TEXT);
            ZeroMemory(@TransformMatrix, SizeOf(TransformMatrix));
            TransformMatrix.eM11 := 0;  // Cos(Angle);
            TransformMatrix.eM12 := 1; // Sin(Angle);
            TransformMatrix.eM21 := -1;  // -Sin(Angle);
            TransformMatrix.eM22 := 0;  // Cos(Angle);
            TransformMatrix.eDx := DrawingRect.Right-DrawingRect.Left-1;
            TransformMatrix.eDy := 0;
            Assert(SetWorldTransform(ModelMetaFileCanvas.Handle, TransformMatrix));
          end;

          UpdateModelColors;
          ModelMetaFileCanvas.Font := frmGoPhast.PhastModel.ContourFont;
          ViewDirection := TViewDirection(comboView.ItemIndex);
          if ViewDirection = vdSide then
          begin
            DrawBigRectangle32(ModelMetaFileCanvas, clBlack32, clWhite32, 1, 0, 0,
              DrawingRect.Bottom - DrawingRect.Top - 1, DrawingRect.Right - DrawingRect.Left-1);
          end
          else
          begin
            DrawBigRectangle32(ModelMetaFileCanvas, clBlack32, clWhite32, 1, 0, 0,
              DrawingRect.Right - DrawingRect.Left - 1, DrawingRect.Bottom - DrawingRect.Top - 1);
          end;

          DrawBackgroundImages(ModelMetaFileCanvas,
            ModelMetaFile.MMWidth, ModelMetaFile.MMHeight);

          LocalModel := frmGoPhast.PhastModel;
          if LocalModel.Grid = nil then
          begin
            Mesh := LocalModel.DrawMesh;
            if Mesh <> nil then
            begin
              Mesh.Draw(ModelMetaFileCanvas, ViewDirection);
            end;
          end
          else
          begin
            LocalModel.Grid.DrawColoredGridLines := cbShowColoredGridLines.Checked;
            if LocalModel.LgrUsed then
            begin
              for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
              begin
                ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
                ChildModel.Grid.DrawColoredGridLines := cbShowColoredGridLines.Checked;
              end;
            end;
            try
              frmGoPhast.Grid.Draw(ModelMetaFileCanvas, ViewDirection);
            finally
              frmGoPhast.Grid.DrawColoredGridLines := True;
              if LocalModel.LgrUsed then
              begin
                for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
                begin
                  ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
                  ChildModel.Grid.DrawColoredGridLines := True;
                end;
              end;
            end;
          end;

          for ScreenObjectIndex := 0 to
            frmGoPhast.PhastModel.ScreenObjectCount - 1 do
          begin
            AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
            AScreenObject.Draw(ModelMetaFileCanvas, ViewDirection);
          end;

          Orientation := dsoTop;
          case ViewDirection of
            vdTop: Orientation := dsoTop;
            vdFront: Orientation := dsoFront;
            vdSide: Orientation := dsoSide;
            else Assert(False);
          end;

          if (LocalModel.ModelSelection in ModflowSelection)
            and (ViewDirection = vdTop) then
          begin
            LocalModel.DrawHeadObservations(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
            LocalModel.DrawPestPointObservations(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
            LocalModel.DrawSfrStreamLinkages(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
            LocalModel.DrawStrStreamLinkages(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
            LocalModel.DrawSfrMf6StreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
            LocalModel.DrawSwrReachConnections(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
              if LocalModel.LgrUsed then
            begin
              for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
              begin
                ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
                ChildModel.DrawHeadObservations(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
                ChildModel.DrawPestPointObservations(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
                ChildModel.DrawSfrStreamLinkages(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
                ChildModel.DrawStrStreamLinkages(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
                ChildModel.DrawSfrMf6StreamLinkages(FModelImage, frmGoPhast.frameTopView.ZoomBox);
                ChildModel.DrawSwrReachConnections(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
              end;
            end;
          end
          else if (LocalModel.ModelSelection in SutraSelection)
            and (ViewDirection = vdTop) then
          begin
            LocalModel.DrawPestPointObservations(ModelMetaFileCanvas, frmGoPhast.frameTopView.ZoomBox);
          end;
          if (LocalModel.ModelSelection in ModflowSelection) then
          begin
            LocalModel.Pathlines.Draw(Orientation, ModelMetaFileCanvas);
            LocalModel.EndPoints.Draw(Orientation, ModelMetaFileCanvas);
            LocalModel.TimeSeries.Draw(Orientation, ModelMetaFileCanvas);
            if LocalModel.LgrUsed then
            begin
              for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
              begin
                ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
                ChildModel.Pathlines.Draw(Orientation, ModelMetaFileCanvas);
                ChildModel.EndPoints.Draw(Orientation, ModelMetaFileCanvas);
                ChildModel.TimeSeries.Draw(Orientation, ModelMetaFileCanvas);
              end;
            end;
          end;

          if LocalModel.ModelSelection in SutraSelection then
          begin
            LocalModel.MaxVectors.PlotVectors2D(ViewDirection, ModelMetaFileCanvas);
            LocalModel.MidVectors.PlotVectors2D(ViewDirection, ModelMetaFileCanvas);
            LocalModel.MinVectors.PlotVectors2D(ViewDirection, ModelMetaFileCanvas);
            VelocityVectors := LocalModel.VelocityVectors;
            if VelocityVectors.SelectedItem >= 0 then
            begin
              VItem := VelocityVectors.Items[VelocityVectors.SelectedItem] as TVectorItem;
              VItem.Vectors.PlotVectors2D(ViewDirection, ModelMetaFileCanvas);
            end;
          end;

          if (LocalModel.ModelSelection in ModflowSelection)
            and (ViewDirection in [vdFront, vdSide]) then
          begin
            LocalModel.CrossSection.Draw(ModelMetaFileCanvas, ViewDirection);
            if LocalModel.LgrUsed then
            begin
              for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
              begin
                ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
                ChildModel.CrossSection.Draw(ModelMetaFileCanvas, ViewDirection);
              end;
            end;
          end;

        finally
          SelectClipRgn(ModelMetaFileCanvas.Handle, HRGN(nil));
          DeleteObject(MyRgn);
        end;
      finally
        ModelMetaFileCanvas.Free;
      end;

      ACanvas.Draw(DrawingRect.Left, DrawingRect.Top, ModelMetaFile);
    finally
      ModelMetaFile.Free;
    end;
  end;

end;

procedure TfrmExportImage.DrawHorizontalScale(var ACanvas: TCanvas;
  var DrawingRect: TRect);
const
  LeftHorizontalRulerOffset = 20;
var
  RulerRect: TRect;
  HRuler: TRulerPainter;
  ViewDirection: TViewDirection;
  Ruler: TRbwRuler;
  RightHorizontalRulerOffset: integer;
  TextHeight: Integer;
//  RulerLinePosition: Integer;
begin
  if cbHorizontalScale.Checked then
  begin
    RightHorizontalRulerOffset := 20;
    if cbVerticalScale.Checked then
    begin
      Inc(RightHorizontalRulerOffset, 60);
    end;
    ACanvas.Font := FScaleFont;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Style := psSolid;
    HRuler := TRulerPainter.Create(nil);
    TextHeight := ACanvas.TextHeight('0');
    try
      ViewDirection := TViewDirection(comboView.ItemIndex);
      Ruler := nil;
      case ViewDirection of
        vdTop:
          Ruler := frmGoPhast.frameTopView.rulHorizontal;
        vdFront:
          Ruler := frmGoPhast.frameFrontView.rulHorizontal;
        vdSide:
          Ruler := frmGoPhast.frameSideView.rulVertical;
      else
        Assert(False);
      end;
      HRuler.Assign(Ruler.Painter);
      HRuler.RulerPosition := rpBottom;
      HRuler.RulerStart := sTopLeft;

      HRuler.RulerEnds.Lower := LeftHorizontalRulerOffset;
      HRuler.RulerEnds.Upper := DrawingRect.Right - DrawingRect.Left
        - RightHorizontalRulerOffset;
      case ViewDirection of
        vdTop:
          begin
            HRuler.RulerValues.Lower := frmGoPhast.frameTopView.ZoomBox.X(
              LeftHorizontalRulerOffset);
            HRuler.RulerValues.Upper := frmGoPhast.frameTopView.ZoomBox.X(
              HRuler.RulerEnds.Upper);
          end;
        vdFront:
          begin
            HRuler.RulerValues.Lower := frmGoPhast.frameFrontView.ZoomBox.X(
              LeftHorizontalRulerOffset);
            HRuler.RulerValues.Upper := frmGoPhast.frameFrontView.ZoomBox.X(
              HRuler.RulerEnds.Upper);
          end;
        vdSide:
          begin
            HRuler.RulerValues.Lower := frmGoPhast.frameSideView.ZoomBox.Y(
              DrawingRect.Right - DrawingRect.Left - LeftHorizontalRulerOffset - FRightOffset);
            HRuler.RulerValues.Upper := frmGoPhast.frameSideView.ZoomBox.Y(
              RightHorizontalRulerOffset - FRightOffset);
          end;
      else
        Assert(False);
      end;
      HRuler.RulerMajorTickLength := Sign(Ruler.RulerMajorTickLength)*(TextHeight+2) div 2;
      HRuler.RulerMinorTickLength := HRuler.RulerMajorTickLength div 2;
      HRuler.RulerLinePosition := Ruler.RulerLinePosition - 20 + TextHeight;
      HRuler.DrawRuler(ACanvas, DrawingRect, RulerRect);
      DrawingRect.Bottom := RulerRect.Top - TextHeight;
    finally
      HRuler.Free;
    end;
  end;
end;

procedure TfrmExportImage.DrawVerticalScale(var ACanvas: TCanvas;
  var DrawingRect: TRect);
const
  UpperVerticalRulerOffset = 20;
  LowerVerticalRulerOffset = 20;
var
  Ruler: TRbwRuler;
  ViewDirection: TViewDirection;
  VRuler: TRulerPainter;
  RulerRect: TRect;
//  RulerPostion: Integer;
  TextHeight: integer;
begin
  if cbVerticalScale.Checked then
  begin
    ACanvas.Font := FScaleFont;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Font := FScaleFont;
    VRuler := TRulerPainter.Create(nil);
    try
      ViewDirection := TViewDirection(comboView.ItemIndex);
      Ruler := nil;
      case ViewDirection of
        vdTop:
          Ruler := frmGoPhast.frameTopView.rulVertical;
        vdFront:
          Ruler := frmGoPhast.frameFrontView.rulVertical;
        vdSide:
          Ruler := frmGoPhast.frameSideView.rulHorizontal;
      else
        Assert(False);
      end;
      TextHeight := ACanvas.TextHeight('0');
      VRuler.Assign(Ruler.Painter);
      VRuler.RulerPosition := rpRight;
      VRuler.RulerStart := sBottomRight;
      VRuler.RulerEnds.Upper := DrawingRect.Bottom - DrawingRect.Top
        - LowerVerticalRulerOffset;
      VRuler.RulerEnds.Lower := UpperVerticalRulerOffset;
      case ViewDirection of
        vdTop:
          begin
            VRuler.RulerValues.Lower := frmGoPhast.frameTopView.ZoomBox.Y(
              DrawingRect.Bottom - DrawingRect.Top - LowerVerticalRulerOffset);
            VRuler.RulerValues.Upper := frmGoPhast.frameTopView.ZoomBox.Y(
              UpperVerticalRulerOffset);
          end;
        vdFront:
          begin
            VRuler.RulerValues.Lower := frmGoPhast.frameFrontView.ZoomBox.Y(
              DrawingRect.Bottom - DrawingRect.Top - LowerVerticalRulerOffset);
            VRuler.RulerValues.Upper := frmGoPhast.frameFrontView.ZoomBox.Y(
              UpperVerticalRulerOffset);
          end;
        vdSide:
          begin
            VRuler.RulerValues.Lower := frmGoPhast.frameSideView.ZoomBox.X(
              DrawingRect.Bottom - DrawingRect.Top - LowerVerticalRulerOffset);
            VRuler.RulerValues.Upper := frmGoPhast.frameSideView.ZoomBox.X(
              UpperVerticalRulerOffset);
          end;
      else
        Assert(False);
      end;
      VRuler.RulerMajorTickLength := Sign(Ruler.RulerMajorTickLength)*(TextHeight+2) div 2;
      VRuler.RulerMinorTickLength := VRuler.RulerMajorTickLength div 2;
      VRuler.RulerLinePosition := Ruler.RulerLinePosition - 20 + TextHeight;
      VRuler.DrawRuler(ACanvas, DrawingRect, RulerRect);
      DrawingRect.Right := RulerRect.Left - TextHeight;
    finally
      VRuler.Free;
    end;
  end;
end;

procedure TfrmExportImage.edColorLegendTitleChange(Sender: TObject);
begin
  inherited;
  DrawImageAfterDelay;
end;

procedure TfrmExportImage.edContourLegendTitleChange(Sender: TObject);
begin
  inherited;
  DrawImageAfterDelay;
end;

procedure TfrmExportImage.ExpandText(Sender: TObject; var TextToDraw: string);
var
  PhastModel: TPhastModel;
  Grid: TCustomModelGrid;
  DataArray: TDataArray;
  CommentLines: TStringList;
  ColorDataArray: TDataArray;
  ContourDataArray: TDataArray;
  PrimaryArray: TDataArray;
  SecondaryArray: TDataArray;
  Mesh: IDrawMesh;
begin
  PhastModel := frmGoPhast.PhastModel;
  Grid := PhastModel.Grid;
  Mesh := PhastModel.DrawMesh;
//  DataArray := nil;
  ColorDataArray := nil;
  ContourDataArray := nil;
  if (Grid <> nil) then
  begin
    case TViewDirection(comboView.ItemIndex) of
      vdTop:
        begin
          ColorDataArray := Grid.TopDataSet;
          ContourDataArray := Grid.TopContourDataSet;
        end;
      vdFront:
        begin
          ColorDataArray := Grid.FrontDataSet;
          ContourDataArray := Grid.FrontContourDataSet;
        end;
      vdSide:
        begin
          ColorDataArray := Grid.SideDataSet;
          ContourDataArray := Grid.SideContourDataSet;
        end;
    else
      Assert(False);
    end;
  end
  else
  begin
    case TViewDirection(comboView.ItemIndex) of
      vdTop:
        begin
          ColorDataArray := Mesh.TopDataSet;
          ContourDataArray := Mesh.TopContourDataSet;
        end;
      vdFront:
        begin
          ColorDataArray := Mesh.ThreeDDataSet;
          ContourDataArray := Mesh.ThreeDContourDataSet;
        end;
      vdSide:
        begin
        end;
    else
      Assert(False);
    end;
  end;
  case rgDisplayChoice.ItemIndex of
    0:
      begin
        PrimaryArray := ColorDataArray;
        SecondaryArray := ContourDataArray;
      end;
    1:
      begin
        PrimaryArray := ContourDataArray;
        SecondaryArray := ColorDataArray;
      end;
    else
      begin
        PrimaryArray := nil;
        SecondaryArray := nil;
        Assert(False);
      end;
  end;
  DataArray := PrimaryArray;
  if DataArray = nil then
  begin
    DataArray := SecondaryArray;
  end;
  if DataArray <> nil then
  begin
    CommentLines := TStringList.Create;
    try
      CommentLines.Text := DataArray.Comment;

      ApplyMacro(CommentLines, StrStressPeriodLabel, StrSP, TextToDraw);
      ApplyMacro(CommentLines, StrTimeStepLabel, StrTS, TextToDraw);
      ApplyMacro(CommentLines, StrElapsedTimeLabel, StrET, TextToDraw);
      ApplyMacro(CommentLines, StrTransportStep, StrTrS, TextToDraw);
    finally
      CommentLines.Free;
    end;
  end;
end;

procedure TfrmExportImage.SaveImage(FilterIndex: Integer; FileName: string);
var
  VerticalSizeInches: Double;
  VerticalSize: Integer;
  DC: HDC;
  ABitMap: TBitmap;
  DrawingRect: TRect;
  MetaFileCanvas: TMetafileCanvas;
  CanvasHeight: Integer;
  CanvasWidth: Integer;
  MetaFile: TMetafile;
  Factor: double;
  RealPixelsPerInch: double;
  VerticalResolution: Integer;
  RealVerticalPixelsPerInch: double;
  HorizontalSize: Integer;
  HorizontalSizeInches: Double;
  HorizontalResolution: Integer;
  RealHorizontalPixelsPerInch: double;
  WidthToDraw: Integer;
  HeightToDraw: Integer;
  Png: TPngImage;
  Jpeg: TJPEGImage;
const
  MillimeterPerInchTimes100 = 2540;
begin
  case FilterIndex of
    1: // emf
      begin
        { TODO : Much of this can be combined with exporting a vector file. }

        // You would think that Screen.PixelsPerInch would give you
        // what you need here but no such luck.
        DC := GetDC(0);
        try
          VerticalSize := GetDeviceCaps(DC, VERTSIZE);
          VerticalSizeInches := VerticalSize / 25.4;
          VerticalResolution := GetDeviceCaps(DC, VERTRES);
          RealVerticalPixelsPerInch := (VerticalResolution / VerticalSizeInches);

          HorizontalSize := GetDeviceCaps(DC, HORZSIZE);
          HorizontalSizeInches := HorizontalSize / 25.4;
          HorizontalResolution := GetDeviceCaps(DC, HORZRES);
          RealHorizontalPixelsPerInch :=
            (HorizontalResolution / HorizontalSizeInches);

          RealPixelsPerInch :=
            Min(RealVerticalPixelsPerInch, RealHorizontalPixelsPerInch);
        finally
          ReleaseDC(0, DC);
        end;

        Factor := (MillimeterPerInchTimes100 / RealPixelsPerInch);

        MetaFile := TMetaFile.Create;
        try
          MetaFile.MMWidth := Trunc(seImageWidth.AsInteger * Factor);
          MetaFile.MMHeight := Trunc(seImageHeight.AsInteger * Factor);
          CanvasWidth := MetaFile.MMWidth;
          CanvasHeight := MetaFile.MMHeight;
          MetaFileCanvas := TMetaFileCanvas.Create(MetaFile, 0);
          try
            FRightOffset := 0;
            DrawOutsideItems(CanvasHeight, CanvasWidth,
              DrawingRect, MetaFileCanvas);
          finally
            MetaFileCanvas.Free;
          end;
        finally
          MetaFile.Free;
        end;
        MetaFile := TMetaFile.Create;
        try
          WidthToDraw := seImageWidth.AsInteger
            + ((CanvasWidth - DrawingRect.Right) + DrawingRect.Left);
          MetaFile.MMWidth := Trunc(WidthToDraw * Factor);
          HeightToDraw := seImageHeight.AsInteger
            + ((CanvasHeight - DrawingRect.Bottom) + DrawingRect.Top);
          MetaFile.MMHeight := Trunc(HeightToDraw * Factor);
          MetaFileCanvas := TMetaFileCanvas.Create(MetaFile, 0);
          try
            DrawOnCanvas(WidthToDraw, HeightToDraw, MetaFileCanvas, FilterIndex, Factor);
          finally
            MetaFileCanvas.Free;
          end;
          try
            MetaFile.SaveToFile(FileName);
          except on E: EWriteError do
            begin
              Beep;
              MessageDlg(Format(StrUnableToSaveFile, [E.message]), mtError, [mbOK], 0);
            end;
          end;
        finally
          MetaFile.Free;
        end;
      end;
    2:  // bmp
      begin
        ABitMap := TBitMap.Create;
        try
          ABitMap.Assign(imagePreview.Picture);
          try
            ABitMap.SaveToFile(FileName);
          except on E: EWriteError do
            begin
              Beep;
              MessageDlg(Format(StrUnableToSaveFile, [E.message]), mtError, [mbOK], 0);
            end;
          end;
        finally
          ABitMap.Free;
        end;
      end;
    3: // png
      begin
        Png := TPngImage.Create;
        try
          Png.Assign(imagePreview.Picture.Graphic);
          try
            Png.SaveToFile(FileName);
          except on E: EWriteError do
            begin
              Beep;
              MessageDlg(Format(StrUnableToSaveFile, [E.message]), mtError, [mbOK], 0);
            end;
          end;
        finally
          Png.Free;
        end;
      end;
    4: // jpg
      begin
        Jpeg := TJPEGImage.Create;
        try
          Jpeg.Assign(imagePreview.Picture.Graphic);
          try
            Jpeg.SaveToFile(FileName);
          except on E: EWriteError do
            begin
              Beep;
              MessageDlg(Format(StrUnableToSaveFile, [E.message]), mtError, [mbOK], 0);
            end;
          end;
        finally
          Jpeg.Free;
        end;
      end;
    VectorGraphFilterIndex: // vector graphic
      begin
        // You would think that Screen.PixelsPerInch would give you
        // what you need here but no such luck.
        DC := GetDC(0);
        try
          VerticalSize := GetDeviceCaps(DC, VERTSIZE);
          VerticalSizeInches := VerticalSize / 25.4;
          VerticalResolution := GetDeviceCaps(DC, VERTRES);
          RealVerticalPixelsPerInch := (VerticalResolution / VerticalSizeInches);

          HorizontalSize := GetDeviceCaps(DC, HORZSIZE);
          HorizontalSizeInches := HorizontalSize / 25.4;
          HorizontalResolution := GetDeviceCaps(DC, HORZRES);
          RealHorizontalPixelsPerInch :=
            (HorizontalResolution / HorizontalSizeInches);

          RealPixelsPerInch :=
            Min(RealVerticalPixelsPerInch, RealHorizontalPixelsPerInch);
        finally
          ReleaseDC(0, DC);
        end;

        Factor := (MillimeterPerInchTimes100 / RealPixelsPerInch);

        MetaFile := TMetaFile.Create;
        try
          MetaFile.MMWidth := Trunc(seImageWidth.AsInteger * Factor);
          MetaFile.MMHeight := Trunc(seImageHeight.AsInteger * Factor);
          CanvasWidth := MetaFile.MMWidth;
          CanvasHeight := MetaFile.MMHeight;
          MetaFileCanvas := TMetaFileCanvas.Create(MetaFile, 0);
          try
            FRightOffset := 0;
            DrawOutsideItems(CanvasHeight, CanvasWidth,
              DrawingRect, MetaFileCanvas);
          finally
            MetaFileCanvas.Free;
          end;
        finally
          MetaFile.Free;
        end;
        MetaFile := TMetaFile.Create;
        try
          WidthToDraw := seImageWidth.AsInteger
            + ((CanvasWidth - DrawingRect.Right) + DrawingRect.Left);
          MetaFile.MMWidth := Trunc(WidthToDraw * Factor);
          HeightToDraw := seImageHeight.AsInteger
            + ((CanvasHeight - DrawingRect.Bottom) + DrawingRect.Top);
          MetaFile.MMHeight := Trunc(HeightToDraw * Factor);
          MetaFileCanvas := TMetaFileCanvas.Create(MetaFile, 0);
          try
            DrawOnCanvas(WidthToDraw, HeightToDraw, MetaFileCanvas, FilterIndex, Factor);
          finally
            MetaFileCanvas.Free;
          end;
          try
            MetaFile.SaveToFile(FileName);
          except on E: EWriteError do
            begin
              Beep;
              MessageDlg(Format(StrUnableToSaveFile, [E.message]), mtError, [mbOK], 0);
            end;
          end;
        finally
          MetaFile.Free;
        end;
      end;
  end;
end;

procedure TfrmExportImage.DrawBackgroundImages(BitMap32OrCanvas: TPersistent; Width, Height: integer);
var
  ViewDirection: TViewDirection;
  BitmapIndex: Integer;
  Item: TCompressedBitmapItem;
begin
  ViewDirection := TViewDirection(comboView.ItemIndex);
  for BitmapIndex := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
  begin
    Item := frmGoPhast.PhastModel.Bitmaps.Items[BitmapIndex]
      as TCompressedBitmapItem;
    if Item.Visible and Item.CanShow
      and (Item.ViewDirection = ViewDirection) then
    begin
      try
        Item.DrawCompressedImage(BitMap32OrCanvas, Width, Height);
      except
        on EOutOfResources do
        begin
          Item.CanShow := False;
          if Item.DisplayMessage then
          begin
            MessageDlg( Format(StrTheSImageCanNot, [Item.Name]),
              mtInformation, [mbOK], 0);
            Item.DisplayMessage := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmExportImage.DrawOnCanvas(CanvasWidth, CanvasHeight: Integer;
  ACanvas: TCanvas; FilterIndex: integer; Factor: double);
var
  DrawingRect: TRect;
begin
  DrawBackground(ACanvas, CanvasHeight, CanvasWidth);

  DrawOutsideItems(CanvasHeight, CanvasWidth, DrawingRect, ACanvas);

  DrawModel(DrawingRect, ACanvas, FilterIndex, Factor);

  DrawTextItems(ACanvas);
end;

procedure TfrmExportImage.DrawBackground(ACanvas: TCanvas;
  CanvasHeight, CanvasWidth: Integer);
begin
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Style := psClear;
  ACanvas.Rectangle(0, 0, CanvasWidth, CanvasHeight);
end;

procedure TfrmExportImage.DrawOutsideItems(CanvasHeight, CanvasWidth: Integer;
  var DrawingRect: TRect; ACanvas: TCanvas);
var
  TitleRect: TRect;
  LegendY: Integer;
  ColorRect: TRect;
  ContourRect: TRect;
  HeadObsRect: TRect;
  StartRight: integer;
  EndpointRect: TRect;
  RHeight: integer;
begin
  ACanvas.Font := Font;
  LegendY := 60;
  DrawColorLegend(ACanvas, LegendY, ColorRect);

  RHeight := ColorRect.Bottom - ColorRect.Top;
  if RHeight > 0 then
  begin
    LegendY := LegendY + RHeight + 40;
  end;
  DrawContourLegend(ACanvas, LegendY, ContourRect);

  RHeight := ContourRect.Bottom - ContourRect.Top;
  if RHeight > 0 then
  begin
    LegendY := LegendY + RHeight + 40;
  end;
  DrawHeadObsLegend(ACanvas, LegendY, HeadObsRect);

  RHeight := HeadObsRect.Bottom - HeadObsRect.Top;
  if RHeight > 0 then
  begin
    LegendY := LegendY + RHeight + 40;
  end;
  DrawEndpointLegend(ACanvas, LegendY, EndpointRect);

  DrawingRect.Left := Max(ColorRect.Right, ContourRect.Right);
  DrawingRect.Left := Max(DrawingRect.Left, HeadObsRect.Right);
  DrawingRect.Left := Max(DrawingRect.Left, EndpointRect.Right);
  DrawingRect.Top := 0;
    DrawingRect.Right := CanvasWidth;
  DrawingRect.Bottom := CanvasHeight;

  DrawTitle(DrawingRect, ACanvas, TitleRect);
  DrawingRect.Top := TitleRect.Bottom;

  DrawHorizontalScale(ACanvas, DrawingRect);
  StartRight := DrawingRect.Right;
  DrawVerticalScale(ACanvas, DrawingRect);
  FRightOffset := StartRight - DrawingRect.Right;
end;

procedure TfrmExportImage.DrawImage;
var
  BitMap: TBitmap;
  CanvasWidth: Integer;
  CanvasHeight: Integer;
  DrawingRect: TRect;
  ViewDirection: TViewDirection;
begin
  if not FCanDraw then
  begin
    Exit;
  end;
  ViewDirection := TViewDirection(comboView.ItemIndex);
  if (ViewDirection = vdSide) and
    ((frmGoPhast.ModelSelection in SutraSelection)
    or frmGoPhast.PhastModel.DisvUsed) then
  begin
    Beep;
    MessageDlg(StrTheSideViewCanNo, mtWarning, [mbOK], 0);
    Exit;
  end;

  BitMap := TBitMap.Create;
  try
    BitMap.Width := seImageWidth.AsInteger;
    BitMap.Height := seImageHeight.AsInteger;
    CanvasWidth := BitMap.Width;
    CanvasHeight := BitMap.Height;
    FRightOffset := 0;
    DrawOutsideItems(CanvasHeight, CanvasWidth, DrawingRect, BitMap.Canvas);
  finally
    BitMap.Free;
  end;
  BitMap := TBitMap.Create;
  try
    BitMap.Width := CanvasWidth + (CanvasWidth - DrawingRect.Right)
      + DrawingRect.Left;
    BitMap.Height := CanvasHeight + (CanvasHeight - DrawingRect.Bottom)
      + DrawingRect.Top;
    CanvasWidth := BitMap.Width;
    CanvasHeight := BitMap.Height;
    DrawOnCanvas(CanvasWidth, CanvasHeight, BitMap.Canvas, BmpFilterIndex, 1);
    imagePreview.Height := CanvasHeight;
    imagePreview.Width := CanvasWidth;
    imagePreview.Picture.Assign(BitMap);
  finally
    BitMap.Free;
    Screen.Cursor := crDefault;
  end;
  FShouldDraw := False;
end;

procedure TfrmExportImage.SaveContourSettings(
  ContourDisplaySettings: TContourDisplaySettings);
var
  DataArray: TDataArray;
  Grid: TCustomModelGrid;
  Mesh: IDrawMesh;
begin
  Grid := frmGoPhast.PhastModel.Grid;
  Mesh := frmGoPhast.PhastModel.DrawMesh;
  DataArray := nil;


  if Grid <> nil then
  begin
    case TViewDirection(comboView.ItemIndex) of
      vdTop:
        begin
          DataArray := Grid.TopContourDataSet;
        end;
      vdFront:
        begin
          DataArray := Grid.FrontContourDataSet;
        end;
      vdSide:
        begin
          DataArray := Grid.SideContourDataSet;
        end;
    else
      Assert(False);
    end;
  end
  else
  begin
    Assert(Mesh <> nil);
    case TViewDirection(comboView.ItemIndex) of
      vdTop:
        begin
          DataArray := Mesh.TopContourDataSet;
        end;
      vdFront:
        begin
          DataArray := Mesh.ThreeDContourDataSet;
        end;
      vdSide:
        begin
          DataArray := nil;
        end;
    else
      Assert(False);
    end;
  end;
  if DataArray = nil then
  begin
    ContourDisplaySettings.DataSetName := '';
    ContourDisplaySettings.LegendVisible := False;
  end
  else
  begin
    ContourDisplaySettings.DataSetName := DataArray.Name;
    ContourDisplaySettings.LegendVisible := cbContourLegend.Checked;
    ContourDisplaySettings.Legend := frmGoPhast.PhastModel.ContourLegend;
    ContourDisplaySettings.Limits := DataArray.Limits;
    ContourDisplaySettings.ContourAlgorithm := DataArray.ContourAlg;
    ContourDisplaySettings.Contours := DataArray.Contours;
    ContourDisplaySettings.Caption := edContourLegendTitle.Text;
  end;
end;

procedure TfrmExportImage.SaveColorDisplaySettings(
  ColorDisplaySettings: TColorDisplaySettings);
var
  ATime: Double;
  TimeList: TCustomTimeList;
  DataArray: TDataArray;
  Grid: TCustomModelGrid;
  PhastModel: TPhastModel;
  Mesh: IDrawMesh;
begin
  PhastModel := frmGoPhast.PhastModel;
  Grid := frmGoPhast.PhastModel.Grid;
  Mesh := frmGoPhast.PhastModel.DrawMesh;
  TimeList := nil;
  DataArray := nil;
  ATime := 0;
  case TViewDirection(comboView.ItemIndex) of
    vdTop:
      begin
        if Assigned(Grid) then
        begin
          DataArray := Grid.TopDataSet;
        end
        else
        begin
          Assert(Assigned(Mesh));
          DataArray := Mesh.TopDataSet;
        end;
        TimeList := PhastModel.TopTimeList;
        ATime := PhastModel.TopDisplayTime;
      end;
    vdFront:
      begin
        if Assigned(Grid) then
        begin
          DataArray := Grid.FrontDataSet;
        end
        else
        begin
          Assert(Assigned(Mesh));
          DataArray := Mesh.ThreeDDataSet;
        end;
        TimeList := PhastModel.FrontTimeList;
        ATime := PhastModel.FrontDisplayTime;
      end;
    vdSide:
      begin
        if Assigned(Grid) then
        begin
          DataArray := Grid.SideDataSet;
        end
        else
        begin
          Assert(Assigned(Mesh));
          DataArray := Mesh.ThreeDDataSet;
        end;
        TimeList := PhastModel.SideTimeList;
        ATime := PhastModel.SideDisplayTime;
      end;
  else
    Assert(False);
  end;
  if DataArray = nil then
  begin
    ColorDisplaySettings.DataSetName := '';
    ColorDisplaySettings.LegendVisible := False;
  end
  else
  begin
    ColorDisplaySettings.DataSetName := DataArray.Name;
    ColorDisplaySettings.LegendVisible := cbColorLegend.Checked;
    ColorDisplaySettings.Legend := PhastModel.ColorLegend;
    ColorDisplaySettings.Limits := DataArray.Limits;
    ColorDisplaySettings.Caption := edColorLegendTitle.Text;
  end;
  if TimeList = nil then
  begin
    ColorDisplaySettings.TimeListName := '';
    ColorDisplaySettings.Time := 0;
  end
  else
  begin
    ColorDisplaySettings.TimeListName := TimeList.Name;
    ColorDisplaySettings.Time := ATime;
    ColorDisplaySettings.Limits := TimeList.Limits;
  end;
end;

procedure TfrmExportImage.ApplyCrossSectionSettings(AModel: TCustomModel;
  ASetting: TDisplaySettingsItem);
var
  DataArrayList: TDataArrayObjectList;
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
  LayersToUse: TList<Integer>;
  index: Integer;
  Colors: TList<TColor>;
//  ACrossSectItem: TDataArrayColor;
begin
  DataArrayList := TDataArrayObjectList.Create;
  Colors := TList<TColor>.Create;
  try
    DataArrayList.OwnsObjects := False;
    DataArrayList.Capacity := ASetting.CrossSectionDataSets.Count;
    Colors.Capacity := ASetting.CrossSectionColors.Count;
    for DataArrayIndex := 0 to ASetting.CrossSectionDataSets.Count - 1 do
    begin
      ADataArray := AModel.DataArrayManager.GetDataSetByName(
        ASetting.CrossSectionDataSets[DataArrayIndex]);
      if ADataArray <> nil then
      begin
//        ACrossSectItem := TDataArrayColor.Create;
//        ACrossSectItem.DataArray := ADataArray;
//        ACrossSectItem.Color := ASetting.CrossSectionColors[DataArrayIndex];
        DataArrayList.Add(ADataArray);
        Colors.Add(ASetting.CrossSectionColors[DataArrayIndex].Value);
      end;
    end;
    AModel.CrossSection.DataArrays := DataArrayList;
    AModel.CrossSection.Colors := Colors;
  finally
    DataArrayList.Free;
    Colors.Free;
  end;
  LayersToUse := TList<Integer>.Create;
  try
    LayersToUse.Capacity := ASetting.CrossSectionLayersToUse.Count;
    for index := 0 to ASetting.CrossSectionLayersToUse.Count - 1 do
    begin
      LayersToUse.Add(ASetting.CrossSectionLayersToUse[index].Value);
    end;
    AModel.CrossSection.LayersToUse := LayersToUse;
  finally
    LayersToUse.Free;
  end;
end;

procedure TfrmExportImage.ApplySettings;
var
  PhastModel: TPhastModel;
  DisplaySettings: TDisplaySettingsCollection;
  ASetting: TDisplaySettingsItem;
begin
  PhastModel := frmGoPhast.PhastModel;
  DisplaySettings := PhastModel.DisplaySettings;
//  SutraSettings := PhastModel.SutraSettings;
  ASetting := DisplaySettings.GetItemByName(comboSavedSettings.Text);
  if ASetting = nil then
  begin
    Exit;
  end;
  ApplyASetting(ASetting, PhastModel);

end;

procedure TfrmExportImage.btnRefreshClick(Sender: TObject);
begin
  inherited;
  FreeAndNil(FModelImage);
  DrawImage;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.SaveSettings;
var
  PhastModel: TPhastModel;
  ModifiedDisplaySettings: TDisplaySettingsCollection;
  ASetting: TDisplaySettingsItem;
  Undo: TUndoEditDisplaySettings;
begin
  PhastModel := frmGoPhast.PhastModel;
  ModifiedDisplaySettings := TDisplaySettingsCollection.Create(nil);
  try
    ModifiedDisplaySettings.Assign(PhastModel.DisplaySettings);
    ASetting := ModifiedDisplaySettings.GetItemByName(comboSavedSettings.Text);
    if ASetting = nil then
    begin
      ASetting := ModifiedDisplaySettings.Add as TDisplaySettingsItem;
      ASetting.Name := comboSavedSettings.Text;
      comboSavedSettings.Items.Add(ASetting.Name)
    end;
    AssignASetting(PhastModel, ASetting);
//    ASetting.CrossSectionLayersToUse := PhastModel.CrossSection.LayersToUse;

    Undo := TUndoEditDisplaySettings.Create(ModifiedDisplaySettings);
    frmGoPhast.UndoStack.Submit(Undo);
    FQuerySaveSettings := False;

  finally
    ModifiedDisplaySettings.Free;
    btnManageSettings.Enabled := frmGoPhast.PhastModel.DisplaySettings.Count > 0;
  end;
end;

procedure TfrmExportImage.ApplyContourDisplaySettings(
  ContourDisplaySettings: TContourDisplaySettings);
var
  DataArray: TDataArray;
  PhastModel: TPhastModel;
  Grid: TCustomModelGrid;
  Mesh: IDrawMesh;
begin
  PhastModel := frmGoPhast.PhastModel;
  edContourLegendTitle.Text := ContourDisplaySettings.Caption;
  if ContourDisplaySettings.DataSetName = '' then
  begin
    DataArray := nil;
  end
  else
  begin
    DataArray := PhastModel.DataArrayManager.GetDataSetByName(
      ContourDisplaySettings.DataSetName);
    if DataArray <> nil then
    begin
      DataArray.ContourAlg := ContourDisplaySettings.ContourAlgorithm;
      DataArray.Contours := ContourDisplaySettings.Contours;
    end;
  end;
  Grid := PhastModel.Grid;
  Mesh := PhastModel.DrawMesh;
  if DataArray = nil then
  begin

    if Assigned(Grid) then
    begin
      Grid.TopContourDataSet := nil;
      Grid.FrontContourDataSet := nil;
      Grid.SideContourDataSet := nil;
      Grid.ThreeDContourDataSet := nil;
    end
    else
    begin
      Assert(Assigned(Mesh));
      Mesh.TopContourDataSet := nil;
      Mesh.ThreeDContourDataSet := nil;
    end;
  end
  else
  begin
    DataArray.Initialize;

    if Assigned(Grid) then
    begin
      case DataArray.Orientation of
        dsoTop:
          begin
            Grid.TopContourDataSet := DataArray;
            Grid.FrontContourDataSet := nil;
            Grid.SideContourDataSet := nil;
          end;
        dsoFront:
          begin
            Grid.TopContourDataSet := nil;
            Grid.FrontContourDataSet := DataArray;
            Grid.SideContourDataSet := nil;
          end;
        dsoSide:
          begin
            Grid.TopContourDataSet := nil;
            Grid.FrontContourDataSet := nil;
            Grid.SideContourDataSet := DataArray;
          end;
        dso3D:
          begin
            Grid.TopContourDataSet := DataArray;
            Grid.FrontContourDataSet := DataArray;
            Grid.SideContourDataSet := DataArray;
          end;
      else
        Assert(false);
      end;
      Grid.ThreeDContourDataSet := DataArray;
    end
    else
    begin
      Assert(Assigned(Mesh));
      case DataArray.Orientation of
        dsoTop:
          begin
            Mesh.TopContourDataSet := DataArray;
            Mesh.ThreeDContourDataSet := nil;
          end;
        dsoFront:
          begin
            Mesh.TopContourDataSet := nil;
            Mesh.ThreeDContourDataSet := nil;
          end;
        dsoSide:
          begin
            Mesh.TopContourDataSet := nil;
            Mesh.ThreeDContourDataSet := nil;
          end;
        dso3D:
          begin
            Mesh.TopContourDataSet := DataArray;
            Mesh.ThreeDContourDataSet := DataArray;
          end;
      else
        Assert(false);
      end;
    end;

    PhastModel.ContourLegend.Assign(ContourDisplaySettings.Legend);
    PhastModel.ContourLegend.ValueSource := DataArray;
    PhastModel.ContourLegend.Contours := DataArray.Contours;
    PhastModel.ContourLegend.AssignFractions;
    DataArray.UpdateMinMaxValues;
  end;
  cbContourLegend.Checked := ContourDisplaySettings.LegendVisible;
  UpdateFrmDisplayData(True);
end;

procedure TfrmExportImage.ApplyColorDisplaySettings(
  ColorDisplaySettings: TColorDisplaySettings);
var
  TimeList: TCustomTimeList;
  TimeListIndex: Integer;
  ADataArray: TDataArray;
  DataArray: TDataArray;
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  edColorLegendTitle.Text := ColorDisplaySettings.Caption;
  if ColorDisplaySettings.TimeListName = '' then
  begin
    TimeList := nil;
  end
  else
  begin
    PhastModel.EdgeDisplay := nil;
    TimeList := PhastModel.GetTimeListByName(ColorDisplaySettings.TimeListName);
  end;
  if TimeList <> nil then
  begin
    TimeList.Limits := ColorDisplaySettings.Limits;
    PhastModel.ColorLegend.Assign(ColorDisplaySettings.Legend);
    if TimeList.UpToDate then
    begin
      for TimeListIndex := 0 to TimeList.Count - 1 do
      begin
        ADataArray := TimeList[TimeListIndex];
        ADataArray.Limits := ColorDisplaySettings.Limits;
      end;
    end
    else
    begin
      TimeList.Initialize;
    end;
    PhastModel.UpdateThreeDTimeDataSet(TimeList, ColorDisplaySettings.Time);
    ADataArray := PhastModel.ThreeDDataSet;
    case TimeList.Orientation of
      dsoTop:
        begin
          PhastModel.UpdateTopTimeDataSet(TimeList, Time);
          PhastModel.FrontDataSet := nil;
          PhastModel.FrontTimeList := nil;
          PhastModel.SideDataSet := nil;
          PhastModel.SideTimeList := nil;
        end;
      dsoFront:
        begin
          PhastModel.TopDataSet := nil;
          PhastModel.TopTimeList := nil;
          PhastModel.UpdateFrontTimeDataSet(TimeList, Time);
          PhastModel.SideDataSet := nil;
          PhastModel.SideTimeList := nil;
        end;
      dsoSide:
        begin
          PhastModel.TopDataSet := nil;
          PhastModel.TopTimeList := nil;
          PhastModel.FrontDataSet := nil;
          PhastModel.FrontTimeList := nil;
          PhastModel.UpdateSideTimeDataSet(TimeList, Time);
        end;
      dso3D:
        begin
          PhastModel.UpdateTopTimeDataSet(TimeList, Time);
          PhastModel.UpdateFrontTimeDataSet(TimeList, Time);
          PhastModel.UpdateSideTimeDataSet(TimeList, Time);
        end;
    end;
    PhastModel.ColorLegend.ValueSource := ADataArray;
    PhastModel.ColorLegend.AssignFractions;
  end
  else
  begin
    PhastModel.TopTimeList := nil;
    PhastModel.FrontTimeList := nil;
    PhastModel.SideTimeList := nil;
    PhastModel.ThreeDTimeList := nil;
    if ColorDisplaySettings.DataSetName = '' then
    begin
      DataArray := nil;
    end
    else
    begin
      PhastModel.EdgeDisplay := nil;
      DataArray := PhastModel.DataArrayManager.GetDataSetByName(
        ColorDisplaySettings.DataSetName);
    end;
    if DataArray = nil then
    begin
      PhastModel.ThreeDDataSet := nil;
      PhastModel.TopDataSet := nil;
      PhastModel.FrontDataSet := nil;
      PhastModel.SideDataSet := nil;
    end
    else
    begin
      DataArray.Initialize;
      PhastModel.ColorLegend.Assign(ColorDisplaySettings.Legend);
      DataArray.Limits := ColorDisplaySettings.Limits;
      PhastModel.ThreeDTimeList := nil;
      case DataArray.Orientation of
        dsoTop:
          begin
            PhastModel.TopDataSet := DataArray;
            PhastModel.FrontDataSet := nil;
            PhastModel.SideDataSet := nil;
          end;
        dsoFront:
          begin
            PhastModel.TopDataSet := nil;
            PhastModel.FrontDataSet := DataArray;
            PhastModel.SideDataSet := nil;
          end;
        dsoSide:
          begin
            PhastModel.TopDataSet := nil;
            PhastModel.FrontDataSet := nil;
            PhastModel.SideDataSet := DataArray;
          end;
        dso3D:
          begin
            PhastModel.TopDataSet := DataArray;
            PhastModel.FrontDataSet := DataArray;
            PhastModel.SideDataSet := DataArray;
            PhastModel.ThreeDDataSet := DataArray;
          end;
      else
        Assert(False);
      end;
      PhastModel.ColorLegend.ValueSource := DataArray;
      PhastModel.ColorLegend.AssignFractions;
      DataArray.UpdateMinMaxValues;
    end;
  end;
  cbColorLegend.Checked := ColorDisplaySettings.LegendVisible;
  UpdateFrmDisplayData(True);
end;

procedure TfrmExportImage.UncheckSelected1Click(Sender: TObject);
begin
  inherited;
  vstDataSets.BeginUpdate;
  try
    SetStateOfMultipleNodes(vstDataSets.RootNode, csUncheckedNormal);
  finally
    vstDataSets.EndUpdate;
  end;
end;

procedure TfrmExportImage.UpdateModelColors;
var
  PhastModel: TPhastModel;
  Is3DSelected: boolean;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Grid <> nil then
  begin
    Is3DSelected := (PhastModel.ThreeDDataSet <> nil);
    if frmGoPhast.Grid.NeedToRecalculateTopCellColors then
    begin
      frmGoPhast.Grid.ResetTopCellColors;
      frmGoPhast.Grid.UpdateCellColors(vdTop);
    end;
    if frmGoPhast.Grid.NeedToRecalculateFrontCellColors then
    begin
      frmGoPhast.Grid.ResetFrontCellColors;
      frmGoPhast.Grid.UpdateCellColors(vdFront);
    end;
    if frmGoPhast.Grid.NeedToRecalculateSideCellColors then
    begin
      frmGoPhast.Grid.ResetSideCellColors;
      frmGoPhast.Grid.UpdateCellColors(vdSide);
    end;
    frmGoPhast.acColoredGrid.Enabled :=
      (frmGoPhast.PhastModel.ThreeDDataSet <> nil)
      or (frmGoPhast.PhastModel.EdgeDisplay <> nil);
    if not frmGoPhast.acColoredGrid.Enabled then
    begin
      frmGoPhast.acColoredGrid.Checked := False;
      frmGoPhast.tb3DColors.Down := False;
    end;
    if frmGoPhast.acColoredGrid.Enabled and not Is3DSelected then
    begin
      frmGoPhast.acColoredGrid.Checked := True;
      frmGoPhast.tb3DColors.Down := True;
    end;
    frmGoPhast.PhastModel.DiscretizationChanged;
  end;
end;

procedure TfrmExportImage.vstDataSetsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PClassificationNodeData;
  DataArray: TDataArray;
  Column: TStrings;
  DA_Position: Integer;
  ChildIndex: Integer;
  ChildNode: PVirtualNode;
  ParentNode: PVirtualNode;
  NewState: TCheckState;
begin
  inherited;
  rdgDataSets.BeginUpdate;
  try
    Data := Sender.GetNodeData(Node);
    // Add or remove the selected data sets.
    if (Data <> nil) and (Data.ClassificationObject is TDataSetClassification) then
    begin
      DataArray := TDataSetClassification(Data.ClassificationObject).DataArray;
      Column := rdgDataSets.Cols[1];
      DA_Position := Column.IndexOf(DataArray.Name);
      if Node.CheckState = csCheckedNormal then
      begin
        if DA_Position < 0 then
        begin
          if rdgDataSets.Cells[1,rdgDataSets.RowCount-1] <> '' then
          begin
            rdgDataSets.RowCount := rdgDataSets.RowCount + 1;
          end;
          rdgDataSets.Cells[1,rdgDataSets.RowCount-1] := DataArray.Name;
          rdgDataSets.Objects[1,rdgDataSets.RowCount-1] := DataArray;
        end;
      end
      else
      begin
        if DA_Position > 0 then
        begin
          if rdgDataSets.RowCount = 2 then
          begin
            rdgDataSets.Cells[1,1] := '';
            rdgDataSets.Objects[1,1] := nil;
          end
          else
          begin
            rdgDataSets.DeleteRow(DA_Position);
          end;
        end;
      end;
    end;

    // Update the check state of the children
    if (Node.ChildCount > 0)
      and (Node.CheckState in [csCheckedNormal, csUncheckedNormal]) then
    begin
      Sender.BeginUpdate;
      try
        ChildNode := nil;
        for ChildIndex := 0 to Node.ChildCount - 1 do
        begin
          if ChildIndex = 0 then
          begin
            ChildNode := Sender.GetFirstChild(Node);
          end
          else
          begin
            ChildNode := Sender.GetNextSibling(ChildNode);
          end;
          if ChildNode.CheckState <> Node.CheckState then
          begin
            ChildNode.CheckState := Node.CheckState;

            // handle children
            vstDataSetsChecked(Sender, ChildNode);
          end;
        end;
      finally
        Sender.EndUpdate;
      end;
    end;

    // update the check state of the parent.
    if (Sender.NodeParent[Node] <> Sender.RootNode)
      and (Sender.NodeParent[Node] <> nil)
      and (Sender.NodeParent[Node].CheckState <> Node.CheckState) then
    begin
      Sender.BeginUpdate;
      try
        ParentNode := Sender.NodeParent[Node];
        ChildNode := nil;
        NewState := ParentNode.CheckState;
        for ChildIndex := 0 to ParentNode.ChildCount - 1 do
        begin
          if ChildIndex = 0 then
          begin
            ChildNode := Sender.GetFirstChild(ParentNode);
            NewState := ChildNode.CheckState;
          end
          else
          begin
            ChildNode := Sender.GetNextSibling(ChildNode);
            if NewState <> ChildNode.CheckState then
            begin
              NewState := csMixedNormal;
              break;
            end;
          end;
        end;
        if ParentNode.CheckState <> NewState then
        begin
          ParentNode.CheckState := NewState;
          // propagate check state to grandparent
          vstDataSetsChecked(Sender, ParentNode);
        end;
      finally
        Sender.EndUpdate;
      end;
    end;
  finally
    rdgDataSets.EndUpdate
  end;
end;

procedure TfrmExportImage.vstDataSetsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmExportImage.vstDataSetsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmExportImage.vstDataSetsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  inherited;
  if Node.ChildCount > 0 then
  begin
    Node.CheckType:=ctTriStateCheckBox;
  end
  else
  begin
    Node.CheckType:=ctCheckBox;
  end;

end;

//function TfrmExportImage.CanColorDataSet(DataArray: TDataArray): boolean;
//begin
//  result := False;
//  case DataArray.EvaluatedAt of
//    eaBlocks: result := True;
//    eaNodes: result := frmGoPhast.PhastModel.ModelSelection in
//      [msPhast, msSutra22, msSutra30];
//    else Assert(False);
//  end;
//end;

procedure TfrmExportImage.GetDataSets;
begin
  vstDataSets.Clear;
  FillVirtualStringTreeWithDataSets(vstDataSets,
    FDataSetDummyObjects, nil, CanDisplayDataSet);
end;

procedure TfrmExportImage.DrawHeadObsLegend(ACanvas: TCanvas;
  const LegendY: Integer; out HeadObsRect: TRect);
const
  Offset = 8;
var
  ViewDirection: TViewDirection;
  PhastModel: TPhastModel;
  MaxResid: Double;
  ChildIndex: Integer;
  AChild: TChildModel;
  MaxSymbolSize: Integer;
  ResidString: string;
  Extent: TSize;
  TextLeft: Integer;
  TextTop: Integer;
  PriorMaxSymbolSize: Integer;
  SymbolLeft: Integer;
  AColor: TColor;
  ObsImported: Boolean;
  YCoord: integer;
begin
  YCoord := LegendY;
  HeadObsRect.Left := 0;
  HeadObsRect.Top := 0;
  HeadObsRect.BottomRight := HeadObsRect.TopLeft;

  ViewDirection := TViewDirection(comboView.ItemIndex);
  if cbHeadObsLegend.Checked and (ViewDirection = vdTop) then
  begin
    PhastModel := frmGoPhast.PhastModel;
    ObsImported := PhastModel.HeadObsResults.Count > 0;
    PhastModel.HeadObsResults.CalculateMaxResidual(frmGoPhast.PhastModel);
    MaxResid := PhastModel.HeadObsResults.MaxResidual;
    if frmGoPhast.PhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        AChild := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        ObsImported := ObsImported or (AChild.HeadObsResults.Count > 0);
        AChild.HeadObsResults.CalculateMaxResidual(AChild);
        if MaxResid < AChild.HeadObsResults.MaxResidual then
        begin
          MaxResid := AChild.HeadObsResults.MaxResidual;
        end;
      end;
    end;

    if not ObsImported then
    begin
      Exit;
    end;

    MaxSymbolSize := PhastModel.HeadObsResults.MaxSymbolSize;

    AColor := ACanvas.Brush.Color;
    try
      ACanvas.Brush.Color := PhastModel.HeadObsResults.PositiveColor;
      ACanvas.Ellipse(Offset,YCoord,MaxSymbolSize+Offset,YCoord+MaxSymbolSize);
    finally
      ACanvas.Brush.Color := AColor;
    end;

    ResidString := FloatToStr(MaxResid);
    Extent := ACanvas.TextExtent(ResidString);
    TextLeft := MaxSymbolSize+2*Offset;
    TextTop :=  YCoord + (MaxSymbolSize-Extent.cy) div 2;
    ACanvas.TextOut(TextLeft, TextTop, ResidString);
    HeadObsRect.Right := TextLeft + Extent.cx;

    YCoord := YCoord+MaxSymbolSize + Offset;

    PriorMaxSymbolSize := MaxSymbolSize;
    MaxSymbolSize := Round(Sqrt(Sqr(MaxSymbolSize/2)/2)*2);
    SymbolLeft := Offset + (PriorMaxSymbolSize - MaxSymbolSize) div 2;
    AColor := ACanvas.Brush.Color;
    try
      ACanvas.Brush.Color := PhastModel.HeadObsResults.NegativeColor;
      ACanvas.Ellipse(SymbolLeft,YCoord,MaxSymbolSize+SymbolLeft,YCoord+MaxSymbolSize);
    finally
      ACanvas.Brush.Color := AColor;
    end;

    ResidString := FloatToStr(-MaxResid/2);
    Extent := ACanvas.TextExtent(ResidString);
    TextLeft := PriorMaxSymbolSize+2*Offset;
    TextTop :=  YCoord + (MaxSymbolSize-Extent.cy) div 2;
    ACanvas.TextOut(TextLeft, TextTop, ResidString);
    HeadObsRect.Right := Max(HeadObsRect.Right, TextLeft + Extent.cx);
    HeadObsRect.Right := HeadObsRect.Right + Offset;

//    YCoord := YCoord+MaxSymbolSize + Offset;
  end;
end;

procedure TfrmExportImage.DrawContourLegend(ACanvas: TCanvas;
  const LegendY: Integer; out ContourRect: TRect);
var
  ViewDirection: TViewDirection;
  PhastModel: TPhastModel;
  Grid: TCustomModelGrid;
  ShowLegend: Boolean;
  Mesh: IDrawMesh;
begin
//  YCoord := LegendY;
  ContourRect.Left := 0;
  ContourRect.Top := 0;
  ContourRect.BottomRight := ContourRect.TopLeft;

  if cbContourLegend.Checked then
  begin
    ViewDirection := TViewDirection(comboView.ItemIndex);
    PhastModel := frmGoPhast.PhastModel;
    Grid := PhastModel.Grid;
    Mesh := PhastModel.DrawMesh;
    ShowLegend := False;
    if Grid <> nil then
    begin
      case ViewDirection of
        vdTop:
          begin
            ShowLegend := (Grid.TopContourDataSet <> nil);
          end;
        vdFront:
          begin
            ShowLegend := (Grid.FrontContourDataSet <> nil);
          end;
        vdSide:
          begin
            ShowLegend := (Grid.SideContourDataSet <> nil);
          end;
      else
        Assert(False);
      end;
    end
    else if Mesh <> nil then
    begin
      case ViewDirection of
        vdTop:
          begin
            ShowLegend := (Mesh.TopContourDataSet <> nil);
          end;
        vdFront:
          begin
            ShowLegend := (Mesh.ThreeDContourDataSet <> nil);
          end;
        vdSide:
          begin
            ShowLegend := False;
          end;
      else
        Assert(False);
      end;
    end;
    if ShowLegend then
    begin
      if frmDisplayData = nil then
      begin
        Application.CreateForm(TfrmDisplayData, frmDisplayData);
      end;
      UpdateFrmDisplayData(True);
      PhastModel.ContourLegend.Caption := edContourLegendTitle.Text;
      PhastModel.ContourLegend.Draw(ACanvas, 10, LegendY, ContourRect,
        frmDisplayData.frameContourData.LegendFont);
//      YCoord := ContourRect.Bottom + 40;
    end;
  end;
end;

procedure TfrmExportImage.DrawEndpointLegend(ACanvas: TCanvas;
  const LegendY: Integer; out EndpointRect: TRect);
var
//  ViewDirection: TViewDirection;
  PhastModel: TPhastModel;
  Grid: TCustomModelGrid;
  ShowLegend: Boolean;
//  Mesh: TSutraMesh3D;
begin
  EndpointRect.Left := 0;
  EndpointRect.Top := 0;
  EndpointRect.BottomRight := EndpointRect.TopLeft;

  if cbEndpoints.Checked then
  begin
//    ViewDirection := TViewDirection(comboView.ItemIndex);
    PhastModel := frmGoPhast.PhastModel;
    Grid := PhastModel.Grid;
//    Mesh := PhastModel.Mesh;
    ShowLegend := False;
    if Grid <> nil then
    begin
      ShowLegend := PhastModel.EndPoints.Visible
        and PhastModel.EndPoints.HasData;
    end;
    if ShowLegend then
    begin
      if frmDisplayData = nil then
      begin
        Application.CreateForm(TfrmDisplayData, frmDisplayData);
      end;
      UpdateFrmDisplayData(True);
      PhastModel.EndPointLegend.Draw(ACanvas, 10, LegendY, EndpointRect,
        frmDisplayData.frameModpathEndpointDisplay1.LegendFont);
    end;
  end;
end;

procedure TfrmExportImage.DefaultFontChanged(Sender: TObject);
begin
  if not FSelectingItem and Assigned(FDefaultFont)
    and Assigned(SelectedItem) then
  begin
    SelectedItem.Font := FDefaultFont;
    FQuerySaveSettings := True;
    DrawImage;
  end;

  if Assigned(FInPlaceEdit) then
  begin
    FInPlaceEdit.Font := FDefaultFont;
  end;

  if FChangingFont or not Assigned(FDefaultFont) then
  begin
    Exit;
  end;

  FChangingFont := True;
  try
    if (SelectedItem <> nil) and not FSelectingItem then
    begin
      SelectedItem.Font := FDefaultFont;
      FQuerySaveSettings := True;
      DrawImage;
    end;
  finally
    FChangingFont := False;
  end;
end;

function TfrmExportImage.DragItem(X, Y: Integer): Boolean;
var
  ARect: TRect;
begin
  result := False;
  if sbSelect.Down and Assigned(FSelectedItem)
    and ((Abs(FStartX - X) > 4) or (Abs(FStartY - Y) > 4)) then
  begin
    ARect := FSelectedItem.Rect;
    GR32.OffSetRect(ARect, X - FStartX, Y - FStartY);
    FSelectedItem.Rect := ARect;
    result := True;
    FQuerySaveSettings := True;
    FShouldDraw := True;
  end;
end;

procedure TfrmExportImage.DrawImageAfterDelay;
begin
  Screen.Cursor := crHourGlass;
  FChangeTime := Now;
  timerDrawImageDelay.Enabled := True;

end;

procedure TfrmExportImage.DrawTitle(DrawingRect: TRect; ACanvas: TCanvas;
  out TitleRect: TRect);
var
  X: Integer;
  Y: Integer;
  Extent: TSize;
  ALine: string;
  Index: Integer;
  ARect: TRect;
begin
  TitleRect.Left := 0;
  TitleRect.Top := 0;
  TitleRect.BottomRight := TitleRect.TopLeft;

  ACanvas.Font := memoTitle.Font;
  Y := 20;
  for Index := 0 to memoTitle.Lines.Count - 1 do
  begin
    ALine := memoTitle.Lines[Index];
    ExpandText(self, ALine);
    if ALine = '' then
    begin
      Extent := ACanvas.TextExtent(' ');
    end
    else
    begin
      Extent := ACanvas.TextExtent(ALine);
    end;
    X := (DrawingRect.Right - Extent.cx) div 2;
    ARect.Left := X;
    ARect.Top := Y;
    ARect.Right := X + Extent.cx;
    ARect.Bottom := Y + Extent.cy;
    ACanvas.TextOut(X, Y, ALine);
    Y := Y + Extent.cy;
    GR32.UnionRect(TitleRect, TitleRect, ARect);
  end;
  if TitleRect.Bottom > 0 then
  begin
    Inc(TitleRect.Bottom, 20);
  end;
end;

procedure TfrmExportImage.DrawTextItems(ACanvas: TCanvas);
var
  Index: Integer;
  Item: TDrawItem;
begin
  for Index := 0 to FTextItems.Count - 1 do
  begin
    Item := FTextItems[Index];
    Item.Draw(ACanvas);
  end;
end;

function TfrmExportImage.FinishEditingExistingItem: Boolean;
begin
  result := False;
  if Assigned(FInPlaceEdit) then
  begin
    if (FInPlaceEdit.ModalResult = mrOK) then
    begin
      if SelectedItem = nil then
      begin
        AddItem;
      end
      else
      begin
        if FInPlaceEdit.Text = '' then
        begin
          FTextItems.Remove(SelectedItem);
          FQuerySaveSettings := True;
        end
        else
        begin
          SelectedItem.Text := FInPlaceEdit.Text;
          SelectedItem.Editing := False;
          SelectedItem.Selected := False;
          FQuerySaveSettings := True;
        end;
        SelectedItem := nil;
        FShouldDraw := True;
        result := True;
      end;
    end
    else
    begin
      if SelectedItem <> nil then
      begin
        SelectedItem.Editing := False;
        SelectedItem.Selected := False;
      end;
      FShouldDraw := True;
    end;
    FreeAndNil(FInPlaceEdit);
  end;
end;

procedure TfrmExportImage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FShouldStop := True;
  if FRunning then
  begin
    Application.ProcessMessages;
  end;
  if FQuerySaveSettings then
  begin
    FQuerySaveSettings := False;
    if (MessageDlg(StrDoYouWantToSave,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      btnSaveSettingsClick(nil);
      Action := caNone;
      Exit;
    end;
  end;
  frmExportImage := nil;
//  Release;
end;

procedure TfrmExportImage.FormCreate(Sender: TObject);
begin
  inherited;
  FScaleFont := TFont.Create;
  FScaleFont.Assign(Font);

  rdgDataSets.Cells[1,0] := StrDataSets;
  FDataSetDummyObjects := TObjectList.Create;
  FCanDraw := False;

  cpView.Collapsed := False;
  cpText.Collapsed := True;
  cpAnimation.Collapsed := True;

//  ocSettings.ActivePage := opView;

  FTextItems:= TObjectList.Create;

  FDefaultFont := TFont.Create;
  FDefaultFont.OnChange :=  DefaultFontChanged;
  FDefaultFont.Assign(Font);

  timerDrawImageDelay.Interval := 1;

  GetData;

  FCanDraw := True;
  comboViewChange(nil);
  FQuerySaveSettings := False;

  StrNone
end;

procedure TfrmExportImage.FormDestroy(Sender: TObject);
begin
  inherited;
  FDataSetDummyObjects.Free;
  FDefaultFont.Free;
  FTextItems.Free;
  FModelImage.Free;
  FScaleFont.Free;
end;

procedure TfrmExportImage.GetData;
var
  Index: Integer;
  ASetting: TDisplaySettingsItem;
  SettingsList: TStringList;
  CurrentText: string;
  ComboIndex: Integer;
begin
  CurrentText := comboSavedSettings.Text;

  edColorLegendTitle.Text := frmGoPhast.PhastModel.ColorLegend.Caption;
  edContourLegendTitle.Text := frmGoPhast.PhastModel.ContourLegend.Caption;

  SettingsList := TStringList.Create;
  try
    SettingsList.Capacity := frmGoPhast.PhastModel.DisplaySettings.Count+1;
    for Index := 0 to frmGoPhast.PhastModel.DisplaySettings.Count - 1 do
    begin
      ASetting := frmGoPhast.PhastModel.DisplaySettings.Items[Index]
        as TDisplaySettingsItem;
      SettingsList.Add(ASetting.Name);
    end;
    SettingsList.CaseSensitive := False;
    SettingsList.Sorted := True;
    SettingsList.Sorted := False;
    SettingsList.Insert(0, '(none)');
    comboSavedSettings.Items.Assign(SettingsList);
  finally
    SettingsList.Free;
  end;
  ComboIndex := comboSavedSettings.Items.IndexOf(CurrentText);
  if ComboIndex < 0 then
  begin
    ComboIndex := 0;
  end;
  comboSavedSettings.ItemIndex := ComboIndex;
  btnManageSettings.Enabled := frmGoPhast.PhastModel.DisplaySettings.Count > 0;

  GetDataSets;

  if frmGoPhast.Grid = nil then
  begin
    rgDisplayChoice.ItemIndex := 0;
  end
  else if frmGoPhast.PhastModel.ThreeDDataSet <> nil then
  begin
    rgDisplayChoice.ItemIndex := 0;
  end
  else
  if frmGoPhast.Grid.ThreeDContourDataSet <> nil then
  begin
    rgDisplayChoice.ItemIndex := 1;
  end
  else
  begin
    rgDisplayChoice.ItemIndex := 1;
  end;
end;

procedure TfrmExportImage.imagePreviewDblClick(Sender: TObject);
begin
  inherited;
  FDoubleClicked := True;
end;

procedure TfrmExportImage.imagePreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SelectItemToDrag(X, Y);

  if FShouldDraw then
  begin
    DrawImage;
  end;
end;

procedure TfrmExportImage.imagePreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FShouldDraw := False;
  try
    if DragItem(X, Y) then
    begin
      Exit;
    end;

    if FinishEditingExistingItem then
    begin
      Exit;
    end;

    if CreateInplaceEditForExistingItem(X, Y) then
    begin
      Exit;
    end;

    CreateInplaceEditForNewItem(X, Y);
  finally
    if FShouldDraw then
    begin
      DrawImage;
    end;
  end;
end;

procedure TfrmExportImage.ItemChanged(Sender: TObject);
begin
  FShouldDraw := True;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.JvBitBtn1Click(Sender: TObject);
var
  ABitMap: TBitmap;
  RetryCount: Integer;
begin
  inherited;
  ABitMap := TBitMap.Create;
  try
    RetryCount := 0;
    repeat
      ABitMap.Assign(imagePreview.Picture);
      try
        Clipboard.Assign(ABitMap);
        break;
      except on E: EClipboardException do
        begin
          Inc(RetryCount);
          if RetryCount < 3 then
          begin
            Sleep(RetryCount * 100)
          end
          else
          begin
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    until RetryCount >= 3;
  finally
    ABitMap.Free;
  end;
end;

procedure TfrmExportImage.memoTitleChange(Sender: TObject);
begin
  inherited;
  DrawImageAfterDelay;
end;

procedure TfrmExportImage.ResizeInplaceEdit(Sender: TObject);
var
  Extent: TSize;
  Lines: TStringList;
  LineIndex: Integer;
  ARect: TRect;
  MaskEdit: TCustomMaskEdit;
begin
  if SelectedItem <> nil then
  begin
    Assert(SelectedItem.Selected);
    Assert(SelectedItem.Editing);
    DrawImage;
  end;
  MaskEdit := Sender as TCustomMaskEdit;
  if Assigned(MaskEdit) then
  begin
    ARect.Left := MaskEdit.Left;
    ARect.Top := MaskEdit.Top;
    Lines := TStringList.Create;
    try
      imagePreview.Canvas.Font := FDefaultFont;
      Lines.Text := MaskEdit.Text;
      if StrRight(MaskEdit.Text, 2) = sLineBreak then
      begin
        Lines.Add(' ')
      end;

      for LineIndex := 0 to Lines.Count - 1 do
      begin
        Extent := imagePreview.Canvas.TextExtent(Lines[LineIndex]+' ');
        if LineIndex = 0 then
        begin
          ARect.Right := ARect.Left + Extent.cx;
        end
        else
        begin
          ARect.Right := Max(ARect.Right,
            ARect.Left + Extent.cx);
        end;
        ARect.Bottom := ARect.Top + Extent.cy*(LineIndex+1);
      end;
    finally
      Lines.Free;
    end;

    MaskEdit.SetBounds(
      MaskEdit.Left,
      MaskEdit.Top,
      ARect.Right - ARect.Left + 16,
      ARect.Bottom - ARect.Top + 8);
  end;
end;

procedure TfrmExportImage.seImageHeightChange(Sender: TObject);
begin
  inherited;
  FreeAndNil(FModelImage);
  DrawImageAfterDelay;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.seImageWidthChange(Sender: TObject);
begin
  inherited;
  FreeAndNil(FModelImage);
  DrawImageAfterDelay;
  FQuerySaveSettings := True;
end;

procedure TfrmExportImage.SelectItemToDrag(X, Y: Integer);
var
  Index: Integer;
  Item: TDrawItem;
  CanSelect: Boolean;
begin
  FSelectedItem := nil;
  if sbSelect.Down then
  begin
    CanSelect := True;
    for Index := FTextItems.Count - 1 downto 0 do
    begin
      Item := FTextItems[Index];
      if CanSelect and GR32.PtInRect(Item.Rect, GR32.Point(X, Y)) then
      begin
        CanSelect := False;
        FSelectedItem := Item;
        FSelectedItem.Selected := True;
        FStartX := X;
        FStartY := Y;
      end
      else
      begin
        Item.Selected := False;
      end;
    end;
  end;
end;

procedure TfrmExportImage.SetSelectedItem(const Value: TDrawItem);
begin
  FSelectedItem := Value;
  if FSelectedItem <> nil then
  begin
    FSelectingItem := True;
    try
      FDefaultFont.Assign(FSelectedItem.Font);
    finally
      FSelectingItem := False;
    end;
  end;
end;

procedure TfrmExportImage.SetStateOfMultipleNodes(BaseNode: PVirtualNode;
  NewState: TCheckState);
begin
  rdgDataSets.BeginUpdate;
  try
    UpdateStringTreeViewCheckedState(vstDataSets, BaseNode, NewState);
  finally
    rdgDataSets.EndUpdate;
  end;
end;

procedure TfrmExportImage.CollapseOtherPanels(Sender: TObject);
var
  CatPanel: TCategoryPanel;
  Index: Integer;
  APanel: TCategoryPanel;
begin
  APanel := nil;
  for Index:=0 to CatPanelGroup.Panels.Count -1 do
  begin
    CatPanel := CatPanelGroup.Panels[Index];
    if CatPanel = Sender then
    begin
      APanel := CatPanel;
    end
    else
    begin
      CatPanel.Collapsed := True;
    end;
  end;

  if CatPanelGroup.VertScrollBar.Visible and (APanel <> nil) then
  begin
    CatPanelGroup.ScrollInView(APanel);
    CatPanelGroup.VertScrollBar.Visible := False;
  end;
end;

procedure TfrmExportImage.ApplyMacro(CommentLines: TStringList;
  CommentSearchKey: string; TextSearchKey: string; var TextToDraw: string);
var
  SearchPosition: Integer;
  ReplacementText: string;
   Index: Integer;
begin
  // See if one of the search terms such as  '%SP' is in the text that
  // will be displayed.
  SearchPosition := Pos(TextSearchKey, TextToDraw);
  if SearchPosition > 0 then
  begin
    // The search term has been found.
    // Set the default value for the text that will replace TextSearchKey.
    ReplacementText := '?';

    // Search each line of the comment for a line that begins with
    // CommentSearchKey.
    // One example of CommentSearchKey is 'Stress Period: ';
    for Index := 0 to CommentLines.Count - 1 do
    begin
      if Pos(CommentSearchKey, CommentLines[Index]) = 1 then
      begin
        // The CommentSearchKey has been found.
        // Set ReplacementText to the remainder of the line containing
        // CommentSearchKey
        ReplacementText := Trim(Copy(CommentLines[Index],
          Length(CommentSearchKey), MAXINT));
        break;
      end;
    end;
    // Replace TextSearchKey with ReplacementText.
    TextToDraw := StringReplace(TextToDraw, TextSearchKey,
      ReplacementText, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TfrmExportImage.AssignASetting(PhastModel: TPhastModel; ASetting: TDisplaySettingsItem);
var
  AScreenObject: TScreenObject;
  DrawItem: TDrawItem;
  TextItem: TTextItem;
  DataArrayIndex: Integer;
  Frame: TframeView;
  Index: Integer;
begin
  ASetting.ViewToDisplay := TViewDirection(comboView.ItemIndex);
  Frame := nil;
  case ASetting.ViewToDisplay of
    vdTop:
      begin
        Frame := frmGoPhast.frameTopView;
      end;
    vdFront:
      begin
        Frame := frmGoPhast.frameFrontView;
      end;
    vdSide:
      begin
        Frame := frmGoPhast.frameSideView;
      end;
  else
    Assert(False);
  end;
  ASetting.Magnification := Frame.ZoomBox.Magnification;
  ASetting.ReferencePointX := Frame.ZoomBox.X(0);
  ASetting.ReferencePointY := Frame.ZoomBox.Y(0);
  ASetting.VerticalExaggeration := PhastModel.Exaggeration;
  if ASetting.ViewToDisplay = vdSide then
  begin
    ASetting.HorizontalRuler.Assign(Frame.rulVertical);
    ASetting.VerticalRuler.Assign(Frame.rulHorizontal);
  end
  else
  begin
    ASetting.HorizontalRuler.Assign(Frame.rulHorizontal);
    ASetting.VerticalRuler.Assign(Frame.rulVertical);
  end;
  ASetting.HorizontalRuler.Visible := cbHorizontalScale.Checked;
  ASetting.VerticalRuler.Visible := cbVerticalScale.Checked;
  ASetting.ImageHeight := seImageHeight.AsInteger;
  ASetting.ImageWidth := seImageWidth.AsInteger;
  ASetting.ShowColoredGridLines := cbShowColoredGridLines.Checked;
  if PhastModel.Grid <> nil then
  begin
    ASetting.GridDisplayChoice := PhastModel.Grid.GridLineDrawingChoice;
  end
  else
  begin
    ASetting.GridDisplayChoice := gldcAll;
  end;
  AssignEndpointSettings(ASetting);

  ASetting.ModpathPathLineSettings := PhastModel.PathLines;
  ASetting.ModpathTimeSeriesSettings := PhastModel.TimeSeries;
  SaveContourSettings(ASetting.ContourDisplaySettings);
  SaveColorDisplaySettings(ASetting.ColorDisplaySettings);
  ASetting.EdgeDisplaySettings.Assign(PhastModel.EdgeDisplay);
  while ASetting.AdditionalText.Count > FTextItems.Count do
  begin
    ASetting.AdditionalText.Delete(ASetting.AdditionalText.Count - 1);
  end;
  while ASetting.AdditionalText.Count < FTextItems.Count do
  begin
    ASetting.AdditionalText.Add;
  end;
  for Index := 0 to FTextItems.Count - 1 do
  begin
    DrawItem := FTextItems[Index];
    TextItem := ASetting.AdditionalText.Items[Index] as TTextItem;
    TextItem.Assign(DrawItem);
  end;
  ASetting.Title.Text := memoTitle.Lines.Text;
  ASetting.Title.Font := memoTitle.Font;
  ASetting.VisibleObjects.Clear;
  ASetting.VisibleObjects.Capacity := PhastModel.ScreenObjectCount;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.Visible and (not AScreenObject.Deleted) then
    begin
      ASetting.VisibleObjects.Add(AScreenObject.Name);
    end;
  end;
  ASetting.VisibleObjects.Sorted := True;
  ASetting.ContourFont := PhastModel.ContourFont;
  ASetting.LabelContours := PhastModel.ShowContourLabels;
  ASetting.SfrStreamLinkPlot := PhastModel.SfrStreamLinkPlot;
  ASetting.SutraSettings.Assign(PhastModel.SutraMesh);
  ASetting.MaxVectors := PhastModel.MaxVectors;
  ASetting.MidVectors := PhastModel.MidVectors;
  ASetting.MinVectors := PhastModel.MinVectors;
  ASetting.VelocityVectors := PhastModel.VelocityVectors;
  ASetting.ShowHeadObsLegend := cbHeadObsLegend.Checked;
  ASetting.CrossSectionDataSets.Clear;
  ASetting.CrossSectionColors.Clear;
  ASetting.CrossSectionDataSets.Capacity := PhastModel.CrossSection.DataArrays.Count;
  ASetting.CrossSectionColors.Capacity := PhastModel.CrossSection.DataArrays.Count;
  for DataArrayIndex := 0 to PhastModel.CrossSection.DataArrays.Count - 1 do
  begin
    ASetting.CrossSectionDataSets.Add(PhastModel.CrossSection.DataArrays[DataArrayIndex].Name);
    ASetting.CrossSectionColors.Add.Value := PhastModel.CrossSection.Colors[DataArrayIndex];
  end;
  ASetting.CrossSectionLayersToUse.Clear;
  ASetting.CrossSectionLayersToUse.Capacity := PhastModel.CrossSection.LayersToUse.Count;
  for Index := 0 to PhastModel.CrossSection.LayersToUse.Count - 1 do
  begin
    ASetting.CrossSectionLayersToUse.Add.Value := PhastModel.CrossSection.LayersToUse[Index];
  end;
  ASetting.ShowPilotPoints := PhastModel.PestProperties.ShowPilotPoints;
end;

procedure TfrmExportImage.ApplyASetting(ASetting: TDisplaySettingsItem; PhastModel: TPhastModel);
var
  UndoShowHideObjects: TUndoShowHideScreenObject;
  AScreenObject: TScreenObject;
  Frame: TframeView;
  NewVisibility: Boolean;
  TextItem: TTextItem;
  ChildIndex: Integer;
  VisibilityChanged: Boolean;
  DrawItem: TDrawItem;
  Index: Integer;
  SelectedVelocityDescription: string;
begin
  comboView.ItemIndex := Ord(ASetting.ViewToDisplay);
  PhastModel.PestProperties.ShowPilotPoints := ASetting.ShowPilotPoints;
  PhastModel.Exaggeration := ASetting.VerticalExaggeration;
  PhastModel.SutraMesh.Assign(ASetting.SutraSettings);
  PhastModel.MaxVectors := ASetting.MaxVectors;
  PhastModel.MidVectors := ASetting.MidVectors;
  PhastModel.MinVectors := ASetting.MinVectors;
  PhastModel.VelocityVectors := ASetting.VelocityVectors;
  if ASetting.VelocityVectors.SelectedItem >= 0 then
  begin
    SelectedVelocityDescription := (ASetting.VelocityVectors.Items[
      ASetting.VelocityVectors.SelectedItem] as TVectorItem).Description;
    PhastModel.VelocityVectors.SetItemByName(SelectedVelocityDescription);
  end
  else
  begin
    PhastModel.VelocityVectors.SelectedItem := -1;
  end;
  frmGoPhast.frameTopView.ZoomBox.Magnification := ASetting.Magnification;
  frmGoPhast.frameFrontView.ZoomBox.Magnification := ASetting.Magnification;
  frmGoPhast.frameSideView.ZoomBox.Magnification := ASetting.Magnification;
  case ASetting.ViewToDisplay of
    vdTop:
      begin
        SetTopCornerPosition(ASetting.ReferencePointX, ASetting.ReferencePointY);
      end;
    vdFront:
      begin
        SetFrontCornerPosition(ASetting.ReferencePointX, ASetting.ReferencePointY);
      end;
    vdSide:
      begin
        SetSideCornerPosition(ASetting.ReferencePointY, ASetting.ReferencePointX);
      end;
  else
    Assert(false);
  end;
  Frame := nil;
  case ASetting.ViewToDisplay of
    vdTop:
      begin
        Frame := frmGoPhast.frameTopView;
      end;
    vdFront:
      begin
        Frame := frmGoPhast.frameFrontView;
      end;
    vdSide:
      begin
        Frame := frmGoPhast.frameSideView;
      end;
  else
    Assert(False);
  end;
  if ASetting.ViewToDisplay = vdSide then
  begin
    Frame.rulVertical.Assign(ASetting.HorizontalRuler);
    Frame.rulHorizontal.Assign(ASetting.VerticalRuler);
  end
  else
  begin
    Frame.rulVertical.Assign(ASetting.VerticalRuler);
    Frame.rulHorizontal.Assign(ASetting.HorizontalRuler);
  end;
  cbHorizontalScale.Checked := ASetting.HorizontalRuler.Visible;
  cbVerticalScale.Checked := ASetting.VerticalRuler.Visible;
  cbHeadObsLegend.Checked := ASetting.ShowHeadObsLegend;
  seImageHeight.AsInteger := ASetting.ImageHeight;
  seImageWidth.AsInteger := ASetting.ImageWidth;
  cbShowColoredGridLines.Checked := ASetting.ShowColoredGridLines;
  if PhastModel.Grid <> nil then
  begin
    case ASetting.GridDisplayChoice of
      gldcAll:
        begin
          frmGoPhast.acShowAllGridLines.Checked := True;
        end;
      gldcExterior:
        begin
          frmGoPhast.acShowExteriorGridLines.Checked := True;
        end;
      gldcActive:
        begin
          frmGoPhast.acShowActiveGridLines.Checked := True;
        end;
      gldcActiveEdge:
        begin
          frmGoPhast.acShowActiveEdge.Checked := True;
        end;
    else
      Assert(false);
    end;
    frmGoPhast.SetGridLineDrawingChoice(nil);
  end;
  UndoShowHideObjects := TUndoShowHideScreenObject.Create;
  try
    VisibilityChanged := False;
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index];
      NewVisibility := (not AScreenObject.Deleted)
        and (ASetting.VisibleObjects.IndexOf(AScreenObject.Name) >= 0);
      if AScreenObject.Visible <> NewVisibility then
      begin
        VisibilityChanged := True;
      end;
      AScreenObject.Visible := NewVisibility;
    end;
    UndoShowHideObjects.SetPostSelection;
    if VisibilityChanged then
    begin
      frmGoPhast.UndoStack.Submit(UndoShowHideObjects);
    end
    else
    begin
      UndoShowHideObjects.Free;
    end;
  except
    UndoShowHideObjects.Free;
    raise ;
  end;
  ApplyEndPointSettings(ASetting);
  PhastModel.PathLines.Assign(ASetting.ModpathPathLineSettings);
  PhastModel.TimeSeries.Assign(ASetting.ModpathTimeSeriesSettings);
  PhastModel.ShowContourLabels := ASetting.LabelContours;
  PhastModel.ContourFont := ASetting.ContourFont;
  ApplyCrossSectionSettings(PhastModel, ASetting);
  if PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ApplyCrossSectionSettings(PhastModel.ChildModels[ChildIndex].ChildModel, ASetting);
    end;
  end;
  ApplyContourDisplaySettings(ASetting.ContourDisplaySettings);
  ApplyColorDisplaySettings(ASetting.ColorDisplaySettings);
  if ASetting.EdgeDisplaySettings.Visible then
  begin
    PhastModel.EdgeDisplay := PhastModel.HfbDisplayer;
    PhastModel.EdgeDisplay.Assign(ASetting.EdgeDisplaySettings);
  end
  else
  begin
    PhastModel.EdgeDisplay := nil;
  end;
  UpdateFrmDisplayData(True);
  PhastModel.SfrStreamLinkPlot := ASetting.SfrStreamLinkPlot;
  FTextItems.Clear;
  for Index := 0 to ASetting.AdditionalText.Count - 1 do
  begin
    DrawItem := TDrawItem.Create;
    DrawItem.OnDraw := ExpandText;
    TextItem := ASetting.AdditionalText.Items[Index] as TTextItem;
    DrawItem.Assign(TextItem);
    FTextItems.Add(DrawItem);
  end;
  memoTitle.Lines.Text := ASetting.Title.Text;
  memoTitle.Font := ASetting.Title.Font;
  UpdateModelColors;
  FQuerySaveSettings := False;
end;

procedure TfrmExportImage.AssignEndpointSettings(ASetting: TDisplaySettingsItem);
begin
  ASetting.ModpathEndPointSettings := frmGoPhast.PhastModel.EndPoints;
  ASetting.ModpathEndPointSettings.LegendVisible := cbEndpoints.Checked;
end;

procedure TfrmExportImage.ApplyEndPointSettings(ASetting: TDisplaySettingsItem);
begin
  cbEndpoints.Checked := ASetting.ModpathEndPointSettings.LegendVisible;
  frmGoPhast.PhastModel.EndPoints.Assign(ASetting.ModpathEndPointSettings);
end;


procedure TfrmExportImage.spdSaveImageTypeChange(Sender: TObject);
begin
  inherited;
  case spdSaveImage.FilterIndex of
    1: spdSaveImage.DefaultExt := '.emf';
    2: spdSaveImage.DefaultExt := '.bmp';
    3: spdSaveImage.DefaultExt := '.png';
    4: spdSaveImage.DefaultExt := '.jpg';
    VectorGraphFilterIndex: spdSaveImage.DefaultExt := '.emf';
    else Assert(False);
  end;
  if spdSaveImage.FileName <> '' then
  begin
    spdSaveImage.FileName := ChangeFileExt(spdSaveImage.FileName,
      spdSaveImage.DefaultExt);
  end;
end;

procedure TfrmExportImage.timerDrawImageDelayTimer(Sender: TObject);
const
  OneSecond = 1/24/3600;
begin
  inherited;

  if (Now - FChangeTime > OneSecond)
    or (timerDrawImageDelay.Interval <> 100) then
  begin
    timerDrawImageDelay.Enabled := False;
    timerDrawImageDelay.Interval := 100;
    DrawImage;
  end;
end;

end.
