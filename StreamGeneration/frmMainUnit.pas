unit frmMainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvToolEdit, Vcl.ComCtrls, JvSpin, RasterValuesAlongSegmentsUnit,
  GenericRasterUnit, ClippedSurferRasterUnit, System.Types,
  System.Generics.Collections, ArgusDataEntry, PitRemovalUnit, Vcl.Menus,
  Xml.VerySimple;

type
  TExtractMethod = (emFromOriginalRaster, emFromIntermediateFiles, emToIntermediate);

  TfrmMain = class(TForm)
    pb1: TProgressBar;
    dlgOpenSelectExcludeAreas: TOpenDialog;
    pgcMethods: TPageControl;
    tabExtractStreams: TTabSheet;
    grpInput: TGroupBox;
    lblInput: TLabel;
    lblClipShapeFile: TLabel;
    lblStartingPoints: TLabel;
    lblMinimumPixels: TLabel;
    lblExcludeAreas: TLabel;
    fedInput: TJvFilenameEdit;
    fedClipShapeFile: TJvFilenameEdit;
    fedStartingPoints: TJvFilenameEdit;
    seMinimumPixels: TJvSpinEdit;
    memoExclude: TMemo;
    btnSelectExcludeAreas: TButton;
    grpOutput: TGroupBox;
    lblOutput: TLabel;
    lblStreamsOutput: TLabel;
    fedOutput: TJvFilenameEdit;
    fedStreamsOutput: TJvFilenameEdit;
    lblStatus: TLabel;
    tabAssignElevationsToShapes: TTabSheet;
    grpAssignElevationsInput: TGroupBox;
    lblStreamToModify: TLabel;
    fedStreamToModify: TJvFilenameEdit;
    lblSurferElevations: TLabel;
    lblSurferMeasuredValues: TLabel;
    grpAssignElevationsOutput: TGroupBox;
    fedModfiedStreams: TJvFilenameEdit;
    lblModfiedStreams: TLabel;
    btnModifyStreams: TButton;
    mmoSurferMeasuredValues: TMemo;
    mmoSurferElevations: TMemo;
    btn1: TButton;
    btn2: TButton;
    dlgOpenGridFiles: TOpenDialog;
    fedPitlessRaster: TJvFilenameEdit;
    lblPitlessRaster: TLabel;
    memo2: TMemo;
    lblFlowDirections: TLabel;
    fedFlowDirections: TJvFilenameEdit;
    grpIntermediate: TGroupBox;
    tabFilter: TTabSheet;
    fedUnfilteredStreams: TJvFilenameEdit;
    fedDomainBoundary: TJvFilenameEdit;
    fedFilteredStreams: TJvFilenameEdit;
    btnFilter: TButton;
    lblUnfilteredStreams: TLabel;
    lblDomainBoundary: TLabel;
    lblFilteredStreams: TLabel;
    rdeMaximumCarvingLength: TRbwDataEntry;
    cbMaximumCarvingLength: TCheckBox;
    lblMaximumCarvingLength: TLabel;
    lblMaximumCarvingDepth: TLabel;
    cbMaximumCarvingDepth: TCheckBox;
    rdeMaximumCarvingDepth: TRbwDataEntry;
    comboRasterMethod: TComboBox;
    btnExtractRaster: TButton;
    lblRasterExtractionMethod: TLabel;
    comboStreamMethod: TComboBox;
    lblStreamMethod: TLabel;
    btnExtractIntermediate: TButton;
    btnExtractStreams: TButton;
    mm1: TMainMenu;
    miFile1: TMenuItem;
    miOpen1: TMenuItem;
    miSave1: TMenuItem;
    miClose1: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    tsExperimental: TTabSheet;
    btnCoarsenDEM: TButton;
    odCoarsenDEM: TOpenDialog;
    btnSplitDEM: TButton;
    Button1: TButton;
    dlgSaveRandomDem: TSaveDialog;
    btnMergePitlessDEM: TButton;
    odMergePitlessDEM: TOpenDialog;
    procedure btnSelectExcludeAreasClick(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btnModifyStreamsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure cbMaximumCarvingLengthClick(Sender: TObject);
    procedure cbMaximumCarvingDepthClick(Sender: TObject);
    procedure btnExtractRasterClick(Sender: TObject);
    procedure btnExtractIntermediateClick(Sender: TObject);
    procedure btnExtractStreamsClick(Sender: TObject);
    procedure miClose1Click(Sender: TObject);
    procedure miOpen1Click(Sender: TObject);
    procedure miSave1Click(Sender: TObject);
    procedure btnCoarsenDEMClick(Sender: TObject);
    procedure btnSplitDEMClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnMergePitlessDEMClick(Sender: TObject);
    procedure fedInputChange(Sender: TObject);
  private
    procedure DoOnProgress(Sender: TObject; Position, MaxPosition: Integer);
    procedure WriteFlowDirections(FlowDirections: TFlowDirections;
      Raster: IRaster);
    procedure WritePitlessRaster(PitlessRaster: IRaster;
      Raster: IRaster);
    procedure WriteContributingArea(Streams: TStreamObjectList;
      Raster: IRaster; RunoffRaster: IRaster; const ExistingRasterFile: string);
    function AllFilesSpecified(Source: TExtractMethod): Boolean;
    procedure AssignClipping(ClipRaster: IClipRaster);
    procedure GetStartingPointsOutline(var StartingPointsOutline: TExcludeOutline);
    procedure ExtractPitsUsingFiles(RastInterface: IRasterFile;
      out PitlessRaster: IRasterFile; out FlowDirections: TFlowDirections);
    procedure ExtractPitsUsingMemory(RastInterface: IRaster;
      out PitlessRaster: IRaster; out FlowDirections: TFlowDirections);
    procedure ExtractAndWriteStreamsMemory(PitlessRaster: IRaster;
      RastInterface: IRaster; FlowDirections: TFlowDirections;
      const ExistingRasterFile: string);
    procedure ExtractAndWriteStreamsFile(PitlessRaster: IRasterFile;
      RastInterface: IRasterFile; FlowDirections: TFlowDirections;
      const ExistingRasterFile: string);
    procedure ReadFlowDirectionsFromFile(FlowDirections: TFlowDirections; FlowDirectionFileName: string);
    procedure AssignCritera(var CutCriteria: TCutCriteria);
    procedure SaveExtractStreams(ExtractStreamsNode: TXmlNode);
    procedure SaveAssignElevations(AssignElevations: TXmlNode);
    procedure SaveFilterStreams(FilterStreams: TXmlNode);
    procedure LoadExtractStreams(ExtractStreamsNode: TXmlNode);
    procedure LoadAssignElevations(AssignElevations: TXmlNode);
    procedure LoadFilterStreams(FilterStreams: TXmlNode);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ExtractStreamUnit,
  StreamExporterUnit,
  System.IOUtils, AssignElevationsToStreamsUnit, SurferGridFileReaderUnit,
  ShapefileUnit, XBase1, SubPolygonUnit, System.UITypes, CoarsenDemUnit,
  SurferGridFileWriterUnit, System.Math;

resourcestring
  StrStreamExtractorFil = 'Stream_Extractor_File';
  StrExtractStreams = 'Extract_Streams';
  StrAssignElevationsto = 'Assign_Elevations_to_Shapes';
  StrFilterStreams = 'Filter_Streams';
  StrInputSurferGridFi = 'Input_Surfer_Grid_File';
  StrIncludedAreaShape = 'Included_Area_Shape_File';
  StrOutflowAreaShapeF = 'Outflow_Area_Shape_File';
  StrMinimumNumberofGr = 'Minimum_Number_of_Grid_Points_Contributing_to_Stre' +
  'am';
  StrExcludeAreaShapefi = 'Exclude_Area_Shapefiles';
  StrAShapeFile = 'A_ShapeFile';
  StrPitlessSurferGrid = 'Pitless_Surfer_Grid_File';
  StrFlowDirectionsGrid = 'Flow_Directions_Grid_File';
  StrWatershedIDOutput = 'Watershed_ID_Output_Surfer_Grid_File';
  StrStreamsShapefile = 'Streams_Shapefile';
  StrRasterExtractionMe = 'Raster_Extraction_Method';
  StrStreamExtractionMe = 'Stream_Extraction_Method';
  StrMaximumCarvingLeng = 'Maximum_Carving_Length';
  StrMaximumCarvingDept = 'Maximum_Carving_Depth';
  StrSIsNotAStreamE = '%s is not a Stream Extractor file.';
  StrInputStreamShapefi = 'Input_Stream_Shapefile';
  StrSurferGridFilesof = 'Surfer_Grid_Files_of_Elevations';
  StrElevationFile = 'Elevation_File';
  StrSurferGridFilesofMeas = 'Surfer_Grid_Files_of_Measured_Values';
  StrMeasuredValueFile = 'Measured_Value_File';
  StrOutputStreamShapef = 'Output_Stream_Shapefile';
  StrUnfilteredStreams = 'Unfiltered_Streams';
  StrDomainBoundary = 'Domain_Boundary';
  StrFilteredStreams = 'Filtered_Streams';

const
  StrID = 'ID';
  StrDOWNSTREAM = 'DOWNSTREAM';

{$R *.dfm}

{
// Need to use FastMM4 to use this function.
// from http://stackoverflow.com/questions/437683/how-to-get-the-memory-used-by-a-delphi-program
function MemoryUsed: cardinal;
var
    st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
begin
    GetMemoryManagerState(st);
    result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
    for sb in st.SmallBlockTypeStates do begin
        result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
    end;
end;
}

function TfrmMain.AllFilesSpecified(Source: TExtractMethod): Boolean;
var
  AStringList: TStringList;
  FileIndex: Integer;
  AFileName: string;
begin
  AStringList := TStringList.Create;
  try
    if (Source = emFromOriginalRaster) and (not TFile.Exists(fedInput.FileName)) then
    begin
      AStringList.Add('The input Surfer grid file does not exist.');
    end;
    if fedPitlessRaster.FileName = '' then
    begin
      AStringList.Add('The pitless surfer grid file name has not been specified.');
    end;
    if fedFlowDirections.FileName = '' then
    begin
      AStringList.Add('The flow directions surfer grid file name has not been specified.');
    end;
    if (Source <> emToIntermediate) and (fedOutput.FileName = '') then
    begin
      AStringList.Add('The Watershed ID surfer grid file name has not been specified.');
    end;
    if (Source <> emToIntermediate) and (fedStreamsOutput.FileName = '') then
    begin
      AStringList.Add('The Streams Shapefile file name has not been specified.');
    end;
    result := AStringList.Count = 0;
    if not result then
    begin
      Beep;
      MessageDlg('The problems have been identified.' + sLineBreak
        + AStringList.Text, mtError, [mbOK], 0);
      Exit
    end;

    if fedClipShapeFile.FileName <> '' then
    begin
      if not  TFile.Exists(fedClipShapeFile.FileName)then
      begin
        AStringList.Add(fedClipShapeFile.FileName)
      end;
    end;
    if fedStartingPoints.FileName <> '' then
    begin
      if not  TFile.Exists(fedStartingPoints.FileName)then
      begin
        AStringList.Add(fedStartingPoints.FileName)
      end;
    end;
    for FileIndex := 0 to memoExclude.Lines.Count - 1 do
    begin
      AFileName := memoExclude.Lines[FileIndex];
      if AFileName <> '' then
      begin
        if not  TFile.Exists(AFileName)then
        begin
          AStringList.Add(AFileName)
        end;
      end;
    end;
    result := AStringList.Count = 0;
    if not result then
    begin
      Beep;
      MessageDlg('The following input files do not exist' + sLineBreak
        + AStringList.Text, mtError, [mbOK], 0);
    end;

  finally
    AStringList.Free;
  end;
end;

procedure TfrmMain.AssignClipping(ClipRaster: IClipRaster);
var
  AFileName: string;
  ExcludeIndex: Integer;
  AStringList: TStringList;
begin
  ClipRaster.IncludeShapeFileName := fedClipShapeFile.FileName;
  if memoExclude.Lines.Count > 0 then
  begin
    lblStatus.Caption := 'Storing Excluded Area';
    Application.ProcessMessages;
    AStringList := TStringList.Create;
    try
      for ExcludeIndex := 0 to memoExclude.Lines.Count - 1 do
      begin
        AFileName := memoExclude.Lines[ExcludeIndex];
        if TFile.Exists(AFileName) then
        begin
          AStringList.Add(AFileName);
        end;
      end;
      if AStringList.Count > 0 then
      begin
        ClipRaster.ExcludeShapeFileNames := AStringList;
      end;
    finally
      AStringList.Free;
    end;
  end;
end;

procedure TfrmMain.GetStartingPointsOutline(var StartingPointsOutline: TExcludeOutline);
var
  AStringList: TStringList;
begin
  StartingPointsOutline := nil;
  if TFile.Exists(fedStartingPoints.FileName) then
  begin
    AStringList := TStringList.Create;
    try
      AStringList.Add(fedStartingPoints.FileName);
      StartingPointsOutline := TExcludeOutline.Create;
      StartingPointsOutline.ExcludeShapeFiles(AStringList);
    finally
      AStringList.Free;
    end;
  end;
end;

procedure TfrmMain.LoadAssignElevations(AssignElevations: TXmlNode);
var
  ANode: TXmlNode;
  GroupNode: TXmlNode;
  ItemIndex: Integer;
  Nodes: TXmlNodeList;
begin
  ANode := AssignElevations.Find(StrInputStreamShapefi);
  if ANode <> nil then
  begin
    fedStreamToModify.FileName := ANode.Text;
  end;

  mmoSurferElevations.Lines.Clear;
  GroupNode := AssignElevations.Find(StrSurferGridFilesof);
  if GroupNode <> nil then
  begin
    Nodes := GroupNode.FindNodes(StrElevationFile);
    for ItemIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[ItemIndex];
      mmoSurferElevations.Lines.Add(ANode.Text);
    end;
  end;

  mmoSurferMeasuredValues.Lines.Clear;
  GroupNode := AssignElevations.Find(StrSurferGridFilesofMeas);
  if GroupNode <> nil then
  begin
    Nodes := GroupNode.FindNodes(StrMeasuredValueFile);
    for ItemIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[ItemIndex];
      mmoSurferMeasuredValues.Lines.Add(ANode.Text);
    end;
  end;

  ANode := AssignElevations.Find(StrOutputStreamShapef);
  if ANode <> nil then
  begin
    fedModfiedStreams.FileName := ANode.Text;
  end;
end;

procedure TfrmMain.LoadExtractStreams(ExtractStreamsNode: TXmlNode);
var
  ANode: TXmlNode;
  GroupNode: TXmlNode;
  Nodes: TXmlNodeList;
  ItemIndex: Integer;
begin
  ANode := ExtractStreamsNode.Find(StrInputSurferGridFi);
  if ANode <> nil then
  begin
    fedInput.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrIncludedAreaShape);
  if ANode <> nil then
  begin
    fedClipShapeFile.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrOutflowAreaShapeF);
  if ANode <> nil then
  begin
    fedStartingPoints.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrMinimumNumberofGr);
  if ANode <> nil then
  begin
    seMinimumPixels.AsInteger := ANode.Text.ToInteger;
  end;

  memoExclude.Lines.Clear;
  GroupNode := ExtractStreamsNode.Find(StrExcludeAreaShapefi);
  if GroupNode <> nil then
  begin
    Nodes := GroupNode.FindNodes(StrAShapeFile);
    for ItemIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[ItemIndex];
      memoExclude.Lines.Add(ANode.Text);
    end;
  end;

  ANode := ExtractStreamsNode.Find(StrPitlessSurferGrid);
  if ANode <> nil then
  begin
    fedPitlessRaster.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrFlowDirectionsGrid);
  if ANode <> nil then
  begin
    fedFlowDirections.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrWatershedIDOutput);
  if ANode <> nil then
  begin
    fedOutput.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrStreamsShapefile);
  if ANode <> nil then
  begin
    fedStreamsOutput.FileName := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrRasterExtractionMe);
  if ANode <> nil then
  begin
    comboRasterMethod.ItemIndex := ANode.Text.ToInteger;
  end;

  ANode := ExtractStreamsNode.Find(StrStreamExtractionMe);
  if ANode <> nil then
  begin
    comboStreamMethod.ItemIndex := ANode.Text.ToInteger;
  end;

  ANode := ExtractStreamsNode.Find(StrMaximumCarvingLeng);
  if ANode <> nil then
  begin
    rdeMaximumCarvingLength.Text := ANode.Text;
  end;

  ANode := ExtractStreamsNode.Find(StrMaximumCarvingDept);
  if ANode <> nil then
  begin
    rdeMaximumCarvingDepth.Text := ANode.Text;
  end;
end;

procedure TfrmMain.LoadFilterStreams(FilterStreams: TXmlNode);
var
  ANode: TXmlNode;
begin
  ANode := FilterStreams.Find(StrUnfilteredStreams);
  if ANode <> nil then
  begin
    fedUnfilteredStreams.FileName := ANode.Text;
  end;

  ANode := FilterStreams.Find(StrDomainBoundary);
  if ANode <> nil then
  begin
    fedDomainBoundary.FileName := ANode.Text;
  end;

  ANode := FilterStreams.Find(StrFilteredStreams);
  if ANode <> nil then
  begin
    fedFilteredStreams.FileName := ANode.Text;
  end;
end;

procedure TfrmMain.miClose1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.miOpen1Click(Sender: TObject);
var
  XmlFile: TXmlVerySimple;
  ExtractStreamsNode: TXmlNode;
  AssignElevations: TXmlNode;
  FilterStreams: TXmlNode;
begin
  if dlgOpen.Execute then
  begin
    XmlFile := TXmlVerySimple.Create;
    try
      XmlFile.LoadFromFile(dlgOpen.FileName);
      if XmlFile.DocumentElement.Name <> StrStreamExtractorFil then
      begin
        MessageDlg(Format(StrSIsNotAStreamE, [dlgOpen.FileName]), mtError, [mbOK], 0);
        Exit;
      end;

      ExtractStreamsNode := XmlFile.DocumentElement.Find(StrExtractStreams);
      if ExtractStreamsNode <> nil then
      begin
        LoadExtractStreams(ExtractStreamsNode);
      end;

      AssignElevations := XmlFile.DocumentElement.Find(StrAssignElevationsto);
      if AssignElevations <> nil then
      begin
        LoadAssignElevations(AssignElevations);
      end;

      FilterStreams := XmlFile.DocumentElement.Find(StrFilterStreams);
      if FilterStreams <> nil then
      begin
        LoadFilterStreams(FilterStreams);
      end;
    finally
      XmlFile.Free;
    end;
    dlgSave.FileName := dlgOpen.FileName;
  end;
end;

procedure TfrmMain.miSave1Click(Sender: TObject);
var
  XmlFile: TXmlVerySimple;
  ExtractStreamsNode: TXmlNode;
  AssignElevations: TXmlNode;
  FilterStreams: TXmlNode;
//  AName: string;
begin
  if dlgSave.Execute then
  begin
    XmlFile := TXmlVerySimple.Create;
    try
      XmlFile.AddChild(StrStreamExtractorFil);

      ExtractStreamsNode := XmlFile.DocumentElement.AddChild(StrExtractStreams);
      SaveExtractStreams(ExtractStreamsNode);

      AssignElevations := XmlFile.DocumentElement.AddChild(StrAssignElevationsto);
      SaveAssignElevations(AssignElevations);

      FilterStreams := XmlFile.DocumentElement.AddChild(StrFilterStreams);
      SaveFilterStreams(FilterStreams);

      XmlFile.SaveToFile(dlgSave.FileName);
    finally
      XmlFile.Free;
    end;
  end;
end;

procedure TfrmMain.AssignCritera(var CutCriteria: TCutCriteria);
begin
  CutCriteria.LimitLength := cbMaximumCarvingLength.Checked;
  CutCriteria.MaxLength := rdeMaximumCarvingLength.RealValue;
  CutCriteria.LimitCutDepth := cbMaximumCarvingDepth.Checked;
  CutCriteria.MaxCutDepth := rdeMaximumCarvingDepth.RealValue;
end;

procedure TfrmMain.SaveExtractStreams(ExtractStreamsNode: TXmlNode);
var
  ANode: TXmlNode;
  GroupNode: TXmlNode;
  ItemIndex: Integer;
  ALine: string;
begin
  ANode := ExtractStreamsNode.AddChild(StrInputSurferGridFi);
  ANode.Text := fedInput.FileName;

  ANode := ExtractStreamsNode.AddChild(StrIncludedAreaShape);
  ANode.Text := fedClipShapeFile.FileName;

  ANode := ExtractStreamsNode.AddChild(StrOutflowAreaShapeF);
  ANode.Text := fedStartingPoints.FileName;

  ANode := ExtractStreamsNode.AddChild(StrMinimumNumberofGr);
  ANode.Text := seMinimumPixels.AsInteger.ToString;

  GroupNode := ExtractStreamsNode.AddChild(StrExcludeAreaShapefi);
  for ItemIndex := 0 to memoExclude.Lines.Count - 1 do
  begin
    ALine := memoExclude.Lines[ItemIndex];
    if ALine <> '' then
    begin
      ANode := GroupNode.AddChild(StrAShapeFile);
      ANode.Text := ALine;
    end;
  end;

  ANode := ExtractStreamsNode.AddChild(StrPitlessSurferGrid);
  ANode.Text := fedPitlessRaster.FileName;

  ANode := ExtractStreamsNode.AddChild(StrFlowDirectionsGrid);
  ANode.Text := fedFlowDirections.FileName;

  ANode := ExtractStreamsNode.AddChild(StrWatershedIDOutput);
  ANode.Text := fedOutput.FileName;

  ANode := ExtractStreamsNode.AddChild(StrStreamsShapefile);
  ANode.Text := fedStreamsOutput.FileName;

  ANode := ExtractStreamsNode.AddChild(StrRasterExtractionMe);
  ANode.Text := comboRasterMethod.ItemIndex.ToString;

  ANode := ExtractStreamsNode.AddChild(StrStreamExtractionMe);
  ANode.Text := comboStreamMethod.ItemIndex.ToString;

  ANode := ExtractStreamsNode.AddChild(StrMaximumCarvingLeng);
  ANode.Text := rdeMaximumCarvingLength.Text;

  ANode := ExtractStreamsNode.AddChild(StrMaximumCarvingDept);
  ANode.Text := rdeMaximumCarvingDepth.Text;
end;

procedure TfrmMain.SaveAssignElevations(AssignElevations: TXmlNode);
var
  ANode: TXmlNode;
  GroupNode: TXmlNode;
  ItemIndex: Integer;
  ALine: string;
begin
  ANode := AssignElevations.AddChild(StrInputStreamShapefi);
  ANode.Text := fedStreamToModify.FileName;

  GroupNode := AssignElevations.AddChild(StrSurferGridFilesof);
  for ItemIndex := 0 to mmoSurferElevations.Lines.Count - 1 do
  begin
    ALine := mmoSurferElevations.Lines[ItemIndex];
    if ALine <> '' then
    begin
      ANode := GroupNode.AddChild(StrElevationFile);
      ANode.Text := ALine;
    end;
  end;

  GroupNode := AssignElevations.AddChild(StrSurferGridFilesofMeas);
  for ItemIndex := 0 to mmoSurferMeasuredValues.Lines.Count - 1 do
  begin
    ALine := mmoSurferMeasuredValues.Lines[ItemIndex];
    if ALine <> '' then
    begin
      ANode := GroupNode.AddChild(StrMeasuredValueFile);
      ANode.Text := ALine;
    end;
  end;

  ANode := AssignElevations.AddChild(StrOutputStreamShapef);
  ANode.Text := fedModfiedStreams.FileName;

end;

procedure TfrmMain.SaveFilterStreams(FilterStreams: TXmlNode);
var
  ANode: TXmlNode;
begin
  ANode := FilterStreams.AddChild(StrUnfilteredStreams);
  ANode.Text := fedUnfilteredStreams.FileName;

  ANode := FilterStreams.AddChild(StrDomainBoundary);
  ANode.Text := fedDomainBoundary.FileName;

  ANode := FilterStreams.AddChild(StrFilteredStreams);
  ANode.Text := fedFilteredStreams.FileName;
end;

procedure TfrmMain.ExtractPitsUsingFiles(RastInterface: IRasterFile;
  out PitlessRaster: IRasterFile; out FlowDirections: TFlowDirections);
var
  StartingPointsOutline: TExcludeOutline;
  PitRemover: TPitRemoverSurfGrid7;
  Changed: Boolean;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitI: IRaster;
  CutCriteria: TCutCriteria;
begin
  AssignCritera(CutCriteria);
  //    ReadSurfer7GrdFile(fedInput.FileName, Raster);
  try
    lblStatus.Caption := 'Identifying start points';
    Application.ProcessMessages;
    GetStartingPointsOutline(StartingPointsOutline);
    IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits,
      StartingPointsOutline, DoOnProgress);
    PitRemover := TPitRemoverSurfGrid7.Create;
    try
      PitRemover.OnProgress := DoOnProgress;
      lblStatus.Caption := 'removing pits';
      Application.ProcessMessages;
      PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitI, FlowDirections, CutCriteria, Changed);
      PitlessRaster := PitI as IRasterFile;
    finally
      PitRemover.Free;
    end;
  finally
    EdgeStartPoints.Free;
    StartingPointsOutline.Free;
    Pits.Free;
  end;
end;

procedure TfrmMain.btn1Click(Sender: TObject);
begin
  dlgOpenGridFiles.Files.Clear;
  if dlgOpenGridFiles.Execute then
  begin
    mmoSurferMeasuredValues.Lines.AddStrings(dlgOpenGridFiles.Files);
  end;

end;

procedure TfrmMain.btn2Click(Sender: TObject);
begin
  dlgOpenGridFiles.Files.Clear;
  if dlgOpenGridFiles.Execute then
  begin
    mmoSurferElevations.Lines.AddStrings(dlgOpenGridFiles.Files);
  end;
end;

procedure TfrmMain.btnCoarsenDEMClick(Sender: TObject);
var
  InputFile: TFileName;
  OutputFile: string;
  CoarsenInput: TCoarsenInput;
begin
  if odCoarsenDEM.Execute then
  begin
    InputFile := odCoarsenDEM.FileName;
    OutputFile := ChangeFileExt(InputFile, '');
    OutputFile := OutputFile + 'Out.grd';
    CoarsenInput.InputFileName := InputFile;
    CoarsenInput.OutputFileName := OutputFile;
    CoarsenInput.IncludeShapeFileName := '';
    CoarsenInput.ExcludeShapeFileNames := nil;
    CoarsenInput.CoarsenAmount := 32;
    CoarsenDem(CoarsenInput);

    Beep;
    ShowMessage('Done');
  end;
end;

procedure TfrmMain.btnExtractIntermediateClick(Sender: TObject);
var
  PitlessMemory: TClippedSurferRasterMemory;
  PitlessFile: TClippedSurferRaster;
  Pitless: TCustomSurferRaster7;
  PitlessInterfaceMemory: IRaster;
  PitlessInterfaceFile: IRasterFile;
  PitlessInterace: IClipRaster;
  FlowDirections: TFlowDirections;
  StartTime: TDateTime;
begin
  if not AllFilesSpecified(emFromIntermediateFiles) then
  begin
    Exit;
  end;
  OutputDebugString('SAMPLING ON');
  StartTime := Now;
  Screen.Cursor := crHourGlass;
  Pitless := nil;
  case comboStreamMethod.ItemIndex of
    0: // File
      begin
        PitlessFile := TClippedSurferRaster.Create(fedPitlessRaster.FileName);
        Pitless := PitlessFile;
        PitlessInterfaceFile := PitlessFile;
        PitlessInterace := PitlessFile;
      end;
    1: // Memory
      begin
        PitlessMemory := TClippedSurferRasterMemory.Create(fedPitlessRaster.FileName);
        Pitless := PitlessMemory;
        PitlessInterfaceMemory := PitlessMemory;
        PitlessInterace := PitlessMemory;
      end;
  end;
  try
    AssignClipping(PitlessInterace);

    FlowDirections := TFlowDirections.Create(Pitless.XCount, Pitless.YCount);
    try
      ReadFlowDirectionsFromFile(FlowDirections, fedFlowDirections.FileName);

    case comboStreamMethod.ItemIndex of
      0: // File
        begin
          ExtractAndWriteStreamsFile(PitlessInterfaceFile, PitlessInterfaceFile,
            FlowDirections, fedPitlessRaster.FileName);
        end;
      1: // Memory
        begin
          ExtractAndWriteStreamsMemory(PitlessInterfaceMemory, PitlessInterfaceMemory,
            FlowDirections, fedPitlessRaster.FileName);
        end;
    end;
    finally
      FlowDirections.Free;
    end;

  finally
    PitlessInterfaceMemory := nil;
    Screen.Cursor := crDefault;
    memo2.Lines.Add('time in seconds ' + FloatToStr((Now - StartTime)*3600*24));
    ShowMessage('Done');
    lblStatus.Caption := 'Done';
    pb1.Position := 0;
    OutputDebugString('SAMPLING OFF');
  end;

end;

type
  TConnection = record
    ID: Integer;
    Downstream: Integer;
  end;

  TConnectionArray = array of TConnection;


procedure TfrmMain.btnFilterClick(Sender: TObject);
var
  ErrorMessages: TStringList;
  UnfilteredShapes: TShapefileGeometryReader;
  FilteredShapes: TShapefileGeometryWriter;
  UnfilteredDataBase: TXBase;
  FilteredDataBase: TXBase;
  IncludeAreas: TObjectList<TSubPolygon>;
  DomainOutline: TShapefileGeometryReader;
  ShapeIndex: Integer;
  Locations: TRealPointArray;
  AShape: TShapeObject;
  PointIndex: Integer;
  APoint: TShapePoint;
  Fields: TStringList;
  ASubPolygon: TSubPolygon;
  DomainIndex: Integer;
  IncludePolygon: TSubPolygon;
  FieldIndex: Integer;
  Connections: TConnectionArray;
  DownstreamID: Integer;
  StartDownstreamID: Integer;
begin
  ErrorMessages := TStringList.Create;
  try
    if not TFile.Exists(fedUnfilteredStreams.FileName) then
    begin
      ErrorMessages.Add('The input unfilterd stream file has not been specified')
    end;
    if not TFile.Exists(fedDomainBoundary.FileName) then
    begin
      ErrorMessages.Add('The model domain input file has not been specified')
    end;
    if TFile.Exists(fedFilteredStreams.FileName) then
    begin
      ErrorMessages.Add('The output filterd stream file already exists')
    end;
    if ErrorMessages.Count > 0 then
    begin
      Beep;
      MessageDlg(ErrorMessages.Text, mtError, [mbOK], 0);
      Exit;
    end;
    ErrorMessages.Add(fedUnfilteredStreams.FileName);
    if ErrorMessages.IndexOf(fedDomainBoundary.FileName) >= 0 then
    begin
      Beep;
      MessageDlg(Format('%s is a duplicate file name',
        [fedDomainBoundary.FileName]), mtError, [mbOK], 0);
      Exit;
    end;
    ErrorMessages.Add(fedDomainBoundary.FileName);
    if ErrorMessages.IndexOf(fedFilteredStreams.FileName) >= 0 then
    begin
      Beep;
      MessageDlg(Format('%s is a duplicate file name',
        [fedFilteredStreams.FileName]), mtError, [mbOK], 0);
      Exit;
    end;
  finally
    ErrorMessages.Free;
  end;


  Screen.Cursor := crHourGlass;
  UnfilteredShapes := TShapefileGeometryReader.Create;
  UnfilteredDataBase := TXBase.Create(nil);
  FilteredDataBase := TXBase.Create(nil);
  IncludeAreas := TObjectList<TSubPolygon>.Create;
  try
    UnfilteredShapes.ReadFromFile(fedUnfilteredStreams.FileName,
      ChangeFileExt(fedUnfilteredStreams.FileName, '.shx'));
    if not UnfilteredShapes.FileHeader.ShapeType in [stPolyLine, stPolyLineZ, stPolyLineM] then
    begin
      Beep;
      MessageDlg('The unfiltered streams must consist of polygons', mtError, [mbOK], 0);
      Exit;
    end;

    FilteredShapes := TShapefileGeometryWriter.Create(
      UnfilteredShapes.FileHeader.ShapeType, False);
    try
      DomainOutline := TShapefileGeometryReader.Create();
      try
        DomainOutline.ReadFromFile(fedDomainBoundary.FileName,
          ChangeFileExt(fedDomainBoundary.FileName, '.shx'));
        if not DomainOutline.FileHeader.ShapeType in
          [stPolygon, stPolygonZ, stPolygonM] then
        begin
          Beep;
          MessageDlg('The domain outline must consist of polygons', mtError, [mbOK], 0);
          Exit;
        end;
        for ShapeIndex := 0 to DomainOutline.Count - 1 do
        begin
          AShape := DomainOutline[ShapeIndex];
          SetLength(Locations, AShape.FNumPoints + 1);
          for PointIndex := 0 to AShape.FNumPoints - 1 do
          begin
            APoint := AShape.FPoints[PointIndex];
            Locations[PointIndex].x := APoint.X;
            Locations[PointIndex].y := APoint.Y;
          end;
          Locations[AShape.FNumPoints] := Locations[0];
          IncludeAreas.Add(TSubPolygon.Create(Locations, AShape.FNumPoints+1, 0, 0));
        end;
      finally
        DomainOutline.Free;
      end;

      UnfilteredDataBase.FileName := ChangeFileExt(fedUnfilteredStreams.FileName, '.dbf');
      UnfilteredDataBase.Active := True;
      Fields := TStringList.Create;
      try
        UnfilteredDataBase.DBFExtractStruct(Fields, False);
        FilteredDataBase.DBFCreate(ChangeFileExt(fedFilteredStreams.FileName, '.dbf'), Fields);
        FilteredDataBase.FileName := ChangeFileExt(fedFilteredStreams.FileName, '.dbf');
        FilteredDataBase.Active := True;
      finally
        Fields.Free;
      end;
      FilteredDataBase.GotoBOF;
      UnfilteredDataBase.GotoBOF;

      Assert(UnfilteredShapes.Count = UnfilteredDataBase.RecordCount);
      if (UnfilteredDataBase.GetFieldNumberFromName(StrID) > 0)
        and (UnfilteredDataBase.GetFieldNumberFromName(StrDOWNSTREAM) > 0) then
      begin
        SetLength(Connections,UnfilteredDataBase.RecordCount);
      end;


      for ShapeIndex := 0 to UnfilteredShapes.Count - 1 do
      begin
        if Length(Connections) > 0 then
        begin
          Connections[ShapeIndex].ID := 0;
          Connections[ShapeIndex].Downstream := UnfilteredDataBase.GetFieldInt(StrDOWNSTREAM);
        end;
        AShape := UnfilteredShapes[ShapeIndex];
        SetLength(Locations, AShape.FNumPoints);
        for PointIndex := 0 to AShape.FNumPoints - 1 do
        begin
          APoint := AShape.FPoints[PointIndex];
          Locations[PointIndex].x := APoint.X;
          Locations[PointIndex].y := APoint.Y;
        end;
        ASubPolygon := TSubPolygon.Create(Locations, AShape.FNumPoints, 0, 0);
        try
          for DomainIndex := 0 to IncludeAreas.Count - 1 do
          begin
            IncludePolygon := IncludeAreas[DomainIndex];
            if IncludePolygon.IsPointInside(Locations[0].x, Locations[0].Y)
              or IncludePolygon.IsPointInside(Locations[
                AShape.FNumPoints-1].x, Locations[AShape.FNumPoints-1].Y)
              or ASubPolygon.Intersect(IncludePolygon) then
            begin
              FilteredShapes.AddShape(AShape);
              FilteredDataBase.AppendBlank;
              for FieldIndex := 1 to UnfilteredDataBase.FieldCount do
              begin
                FilteredDataBase.PutFieldByNumber(FieldIndex,
                  AnsiString(UnfilteredDataBase.GetFieldByNumber(FieldIndex)));
              end;
              FilteredDataBase.PostChanges;
              FilteredDataBase.GotoNext;

              if Length(Connections) > 0 then
              begin
                Connections[ShapeIndex].ID := UnfilteredDataBase.GetFieldInt(StrID);
              end;
              Break;
            end;
          end;
        finally
          ASubPolygon.Free;
        end;
        UnfilteredDataBase.GotoNext;
      end;

      FilteredDataBase.GotoBOF;
      for ShapeIndex := 0 to FilteredDataBase.RecordCount - 1 do
      begin
        DownstreamID := FilteredDataBase.GetFieldInt(StrDOWNSTREAM);
        StartDownstreamID := DownstreamID;
        if StartDownstreamID <> 0 then
        begin
          while (DownstreamID <> 0) and (Connections[DownstreamID-1].ID = 0) do
          begin
            DownstreamID := Connections[DownstreamID-1].Downstream;
          end;
        end;
        if StartDownstreamID <> DownstreamID then
        begin
          FilteredDataBase.UpdFieldInt(StrDOWNSTREAM, DownstreamID);
          FilteredDataBase.PostChanges;
        end;
        FilteredDataBase.GotoNext;
      end;


      FilteredShapes.WriteToFile(fedFilteredStreams.FileName,
        ChangeFileExt(fedFilteredStreams.FileName, '.shx'));
    finally
      FilteredShapes.Free;
    end;
  finally
    FilteredDataBase.Free;
    UnfilteredShapes.Free;
    UnfilteredDataBase.Free;
    IncludeAreas.Free;
    Screen.Cursor := crDefault;
  end;

  ShowMessage('Done');
end;

procedure TfrmMain.btnExtractRasterClick(Sender: TObject);
var
  MemoryRaster: TClippedSurferRasterMemory;
  FileRaster: TClippedSurferRaster;
  Raster: TCustomSurferRaster7;
  RastInterface: IRaster;
  RastInterfaceFile: IRasterFile;
  FlowDirections: TFlowDirections;
  PitlessRasterFile: IRasterFile;
  PitlessRaster: IRaster;
  StartTime: TDateTime;
begin
  if not AllFilesSpecified(emToIntermediate) then
  begin
    Exit;
  end;
  OutputDebugString('SAMPLING ON');
  FlowDirections := nil;
  StartTime := Now;
  Assert(FileExists(fedInput.FileName));

  Screen.Cursor := crHourGlass;

  Raster := nil;
  FileRaster := nil;
  MemoryRaster := nil;
  case comboRasterMethod.ItemIndex of
    0: // File
      begin
        FileRaster := TClippedSurferRaster.Create(fedInput.FileName);
        RastInterfaceFile := FileRaster;
        Raster := FileRaster;
      end;
    1: // Memory
      begin
        MemoryRaster := TClippedSurferRasterMemory.Create(fedInput.FileName);
        Raster := MemoryRaster;
      end;
  end;

  try
    RastInterface := Raster;

    case comboRasterMethod.ItemIndex of
      0: // File
        begin
          AssignClipping(FileRaster);
          ExtractPitsUsingFiles(RastInterfaceFile, PitlessRasterFile, FlowDirections);
          PitlessRaster := PitlessRasterFile;
        end;
      1: // Memory
        begin
          AssignClipping(MemoryRaster);
          ExtractPitsUsingMemory(RastInterface, PitlessRaster, FlowDirections);
        end;
    end;

    WritePitlessRaster(PitlessRaster, RastInterface);
    WriteFlowDirections(FlowDirections, RastInterface);
  finally
    FlowDirections.Free;
    RastInterface := nil;
    Screen.Cursor := crDefault;
    memo2.Lines.Add('time in seconds ' + FloatToStr((Now - StartTime)*3600*24));
    ShowMessage('Done');
    lblStatus.Caption := 'Done';
    pb1.Position := 0;
    OutputDebugString('SAMPLING OFF');
  end;

end;

procedure TfrmMain.btnExtractStreamsClick(Sender: TObject);
var
  MemoryRaster: TClippedSurferRasterMemory;
  FileRaster: TClippedSurferRaster;
  Raster: TCustomSurferRaster7;
  RastInterface: IRaster;
  RastInterfaceFile: IRasterFile;
  FlowDirections: TFlowDirections;
  PitlessRasterFile: IRasterFile;
  PitlessRaster: IRaster;
  StartTime: TDateTime;
  PitlessFile: TClippedSurferRaster;
  PitlessMemory: TClippedSurferRasterMemory;
begin
  if not AllFilesSpecified(emToIntermediate) then
  begin
    Exit;
  end;
  OutputDebugString('SAMPLING ON');
  FlowDirections := nil;
  StartTime := Now;
  Assert(FileExists(fedInput.FileName));

  Screen.Cursor := crHourGlass;

  Raster := nil;
  RastInterfaceFile := nil;
  FileRaster := nil;
  MemoryRaster := nil;
  PitlessRasterFile := nil;
  PitlessRaster := nil;
  case comboRasterMethod.ItemIndex of
    0: // File
      begin
        FileRaster := TClippedSurferRaster.Create(fedInput.FileName);
        RastInterfaceFile := FileRaster;
        Raster := FileRaster;
      end;
    1: // Memory
      begin
        MemoryRaster := TClippedSurferRasterMemory.Create(fedInput.FileName);
        Raster := MemoryRaster;
      end;
  end;

  try
    RastInterface := Raster;

    case comboRasterMethod.ItemIndex of
      0: // File
        begin
          AssignClipping(FileRaster);
          ExtractPitsUsingFiles(RastInterfaceFile, PitlessRasterFile, FlowDirections);
          PitlessRaster := PitlessRasterFile;
        end;
      1: // Memory
        begin
          AssignClipping(MemoryRaster);
          ExtractPitsUsingMemory(RastInterface, PitlessRaster, FlowDirections);
        end;
    end;

    WritePitlessRaster(PitlessRaster, RastInterface);
    WriteFlowDirections(FlowDirections, RastInterface);

    case comboStreamMethod.ItemIndex of
      0: // File
        begin
          if PitlessRasterFile = nil then
          begin
            PitlessFile := TClippedSurferRaster.Create(fedPitlessRaster.FileName);
//            Pitless := PitlessFile;
            PitlessRasterFile := PitlessFile;
            PitlessRaster := PitlessFile;
          end;
          ExtractAndWriteStreamsFile(PitlessRasterFile, PitlessRasterFile,
            FlowDirections, fedPitlessRaster.FileName);
        end;
      1: // Memory
        begin
          if PitlessRasterFile <> nil then
          begin
            PitlessMemory := TClippedSurferRasterMemory.Create(fedPitlessRaster.FileName);
            PitlessRaster := PitlessMemory;
          end;
          ExtractAndWriteStreamsMemory(PitlessRaster, PitlessRaster,
            FlowDirections, fedPitlessRaster.FileName);
        end;
    end;

  finally
    FlowDirections.Free;
    RastInterface := nil;
    PitlessRasterFile := nil;
    Screen.Cursor := crDefault;

    memo2.Lines.Add('time in seconds ' + FloatToStr((Now - StartTime)*3600*24));
    ShowMessage('Done');
    lblStatus.Caption := 'Done';
    pb1.Position := 0;
    OutputDebugString('SAMPLING OFF');
  end;

end;

Type
  TMergeRecord = record
    HigherPitlessGrid: TSurferRaster7File2;
    HigherFlowDir: TFlowDirections;
    LowerPitlessGrid: TSurferRaster7File2;
    LowerFlowDir: TFlowDirections;
  end;

procedure CheckMergeValidity(MergeRecord: TMergeRecord);
begin
  Assert(MergeRecord.HigherPitlessGrid <> nil);
  Assert(MergeRecord.HigherFlowDir <> nil);
  Assert(MergeRecord.LowerPitlessGrid <> nil);
  Assert(MergeRecord.LowerFlowDir <> nil);
  Assert(MergeRecord.HigherPitlessGrid.XCount = MergeRecord.HigherFlowDir.XCount);
  Assert(MergeRecord.HigherPitlessGrid.YCount = MergeRecord.HigherFlowDir.YCount);
  Assert(MergeRecord.HigherPitlessGrid.XCount = MergeRecord.LowerPitlessGrid.XCount);
  Assert(MergeRecord.HigherPitlessGrid.YCount = MergeRecord.LowerPitlessGrid.YCount);
  Assert(MergeRecord.HigherPitlessGrid.XCount = MergeRecord.LowerFlowDir.XCount);
  Assert(MergeRecord.HigherPitlessGrid.YCount = MergeRecord.LowerFlowDir.YCount);
  Assert(MergeRecord.HigherPitlessGrid.XSpacing = MergeRecord.LowerPitlessGrid.XSpacing);
  Assert(MergeRecord.HigherPitlessGrid.YSpacing = MergeRecord.LowerPitlessGrid.YSpacing);
end;

procedure MergeAdjacentRows(MergeRecord: TMergeRecord);
var
  XIndex: Integer;
  YIndex: Integer;
  OtherYIndex: Integer;
  XStart: Integer;
  XEnd: Integer;
  MatchIndex: Integer;
  InnerXIndex: Integer;
  MatchZ: Double;
  TestZ: Double;
  AFlowDirection: TFlowDirection;
begin
  CheckMergeValidity(MergeRecord);
  YIndex := 0;
  OtherYIndex := MergeRecord.LowerPitlessGrid.YCount-1;
  for XIndex := 0 to MergeRecord.HigherPitlessGrid.XCount - 1 do
  begin
    if (Not MergeRecord.HigherPitlessGrid.Ignore[XIndex,YIndex])
      and (MergeRecord.HigherFlowDir.Ordinals[XIndex,YIndex] = foMiddle) then
    begin
      XStart := Max(XIndex-1,0);
      XEnd :=   Min(XIndex+1,MergeRecord.HigherPitlessGrid.XCount - 1);
      MatchIndex := -1;
      MatchZ := -1e38;
      for InnerXIndex := XStart to XEnd do
      begin
        if Not MergeRecord.LowerPitlessGrid.Ignore[InnerXIndex,OtherYIndex] then
        begin
          if MatchIndex = -1 then
          begin
            MatchIndex := InnerXIndex;
            MatchZ := MergeRecord.LowerPitlessGrid.Z[InnerXIndex,OtherYIndex];
          end
          else
          begin
            TestZ := MergeRecord.LowerPitlessGrid.Z[InnerXIndex,OtherYIndex];
            if TestZ < MatchZ then
            begin
              MatchIndex := InnerXIndex;
              MatchZ := TestZ;
            end;
          end;
        end;

      end;
      if MatchIndex >= 0 then
      begin
        if MatchZ < MergeRecord.HigherPitlessGrid.Z[XIndex,YIndex] then
        begin
          AFlowDirection.DeltaY := -1;
          AFlowDirection.DeltaX := MatchIndex - XIndex;
          MergeRecord.HigherFlowDir.Items[XIndex,YIndex] := AFlowDirection;
          if MergeRecord.LowerFlowDir.Ordinals[MatchIndex,OtherYIndex] = foMiddle then
          begin
            // Pit in lower grid needs filling
          end;
        end
        else if MatchZ > MergeRecord.HigherPitlessGrid.Z[XIndex,YIndex] then
        begin
          AFlowDirection.DeltaY := 1;
          AFlowDirection.DeltaX := -(MatchIndex - XIndex);
          MergeRecord.LowerFlowDir.Items[MatchIndex,OtherYIndex] := AFlowDirection;
            // Pit in higher grid needs filling
        end
        else
        begin
          AFlowDirection.DeltaY := -1;
          AFlowDirection.DeltaX := MatchIndex - XIndex;
          MergeRecord.HigherFlowDir.Items[XIndex,YIndex] := AFlowDirection;
          if MergeRecord.LowerFlowDir.Ordinals[MatchIndex,OtherYIndex] = foMiddle then
          begin
            // Pit in lower grid needs filling
          end;
        end;
      end;
    end;
  end;

  OtherYIndex := 0;
  YIndex := MergeRecord.LowerPitlessGrid.YCount-1;
  for XIndex := 0 to MergeRecord.LowerPitlessGrid.XCount - 1 do
  begin
    if (Not MergeRecord.LowerPitlessGrid.Ignore[XIndex,YIndex])
      and (MergeRecord.LowerFlowDir.Ordinals[XIndex,YIndex] = foMiddle) then
    begin

    end;
  end;

end;

procedure MergeAdjacentColumns(MergeRecord: TMergeRecord);
begin
  CheckMergeValidity(MergeRecord);
end;

procedure TfrmMain.btnMergePitlessDEMClick(Sender: TObject);
const
  NumRows = 3;
  NumCols = 3;
var
  PitlessArray: array of array of TSurferRaster7File2;
  DirectionsArray: array of array of TFlowDirections;
  FileNames: TStringList;
  FileIndex: Integer;
  AfileName: string;
  BriefName: string;
  ANumber: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  RIndex: Integer;
  CIndex: Integer;
  MergeRecord: TMergeRecord;
  NewRaster: TSurferRaster7File2;
  NewFlowDirections: TFlowDirections;
begin
  if odMergePitlessDEM.Execute then
  begin
    FileNames := TStringList.Create;
    try
      FileNames.Assign(odMergePitlessDEM.Files);
      SetLength(PitlessArray, NumRows, NumCols);
      SetLength(DirectionsArray, NumRows, NumCols);
      for RIndex := 0 to 2 do
      begin
        for CIndex := 0 to 2 do
        begin
          PitlessArray[CIndex,RIndex] := nil;
          DirectionsArray[CIndex,RIndex] := nil;
        end;
      end;
      while FileNames.Count > 0 do
      begin
        FileIndex := Random(FileNames.Count);
        AfileName := FileNames[FileIndex];
        FileNames.Delete(FileIndex);
        BriefName := ExtractFileName(AfileName);
        ANumber := StrToInt(Copy(BriefName, 9, 1));
        case ANumber of
          1:
            begin
              RowIndex := 0;
              ColIndex := 0;
            end;
          2:
            begin
              RowIndex := 0;
              ColIndex := 1;
            end;
          3:
            begin
              RowIndex := 0;
              ColIndex := 2;
            end;
          4:
            begin
              RowIndex := 1;
              ColIndex := 0;
            end;
          5:
            begin
              RowIndex := 1;
              ColIndex := 1;
            end;
          6:
            begin
              RowIndex := 1;
              ColIndex := 2;
            end;
          7:
            begin
              RowIndex := 2;
              ColIndex := 0;
            end;
          8:
            begin
              RowIndex := 2;
              ColIndex := 1;
            end;
          9:
            begin
              RowIndex := 2;
              ColIndex := 2;
            end;
          else
            begin
              RowIndex := -1;
              ColIndex := -1;
              Assert(False);
            end;
        end;

        NewRaster := TSurferRaster7File2.Create(AfileName);
        PitlessArray[ColIndex,RowIndex] := NewRaster;
        AfileName := StringReplace(AfileName, 'Pitless', 'FlowDirection', []);
        NewFlowDirections := TFlowDirections.Create(NewRaster.XCount, NewRaster.YCount);
        ReadFlowDirectionsFromFile(NewFlowDirections, AfileName);
        DirectionsArray[ColIndex,RowIndex] := NewFlowDirections;

        if (RowIndex > 0) and (DirectionsArray[ColIndex,RowIndex-1] <> nil) then
        begin
          MergeRecord.HigherPitlessGrid := PitlessArray[ColIndex,RowIndex];
          MergeRecord.HigherFlowDir := DirectionsArray[ColIndex,RowIndex];
          MergeRecord.LowerPitlessGrid := PitlessArray[ColIndex,RowIndex-1];
          MergeRecord.LowerFlowDir := DirectionsArray[ColIndex,RowIndex-1];
          MergeAdjacentRows(MergeRecord);
        end;
        if (RowIndex < 2) and (DirectionsArray[ColIndex,RowIndex+1] <> nil) then
        begin
          MergeRecord.HigherPitlessGrid := PitlessArray[ColIndex,RowIndex+1];
          MergeRecord.HigherFlowDir := DirectionsArray[ColIndex,RowIndex+1];
          MergeRecord.LowerPitlessGrid := PitlessArray[ColIndex,RowIndex];
          MergeRecord.LowerFlowDir := DirectionsArray[ColIndex,RowIndex];
          MergeAdjacentRows(MergeRecord);
        end;
        if (ColIndex > 0) and (DirectionsArray[ColIndex-1,RowIndex] <> nil) then
        begin
          MergeRecord.HigherPitlessGrid := PitlessArray[ColIndex,RowIndex];
          MergeRecord.HigherFlowDir := DirectionsArray[ColIndex,RowIndex];
          MergeRecord.LowerPitlessGrid := PitlessArray[ColIndex-1,RowIndex];
          MergeRecord.LowerFlowDir := DirectionsArray[ColIndex-1,RowIndex];
          MergeAdjacentColumns(MergeRecord);
        end;
        if (ColIndex < 2) and (DirectionsArray[ColIndex+1,RowIndex] <> nil) then
        begin
          MergeRecord.HigherPitlessGrid := PitlessArray[ColIndex+1,RowIndex];
          MergeRecord.HigherFlowDir := DirectionsArray[ColIndex+1,RowIndex];
          MergeRecord.LowerPitlessGrid := PitlessArray[ColIndex,RowIndex];
          MergeRecord.LowerFlowDir := DirectionsArray[ColIndex,RowIndex];
          MergeAdjacentColumns(MergeRecord);
        end;
      end;
    finally
      for RIndex := 0 to 2 do
      begin
        for CIndex := 0 to 2 do
        begin
          PitlessArray[CIndex,RIndex].Free;
          DirectionsArray[CIndex,RIndex].Free;
        end;
      end;
      FileNames.Free;
    end;
  end;
end;

procedure TfrmMain.btnModifyStreamsClick(Sender: TObject);
var
  FileIndex: Integer;
  Converter: TElevationAssigner;
  Extensions: TStringList;
  ExtensionIndex: Integer;
  ExistingFileName: string;
  NewFileName: string;
begin
  if not TFile.Exists(fedStreamToModify.FileName) then
  begin
    Beep;
    MessageDlg('The input Shapefile does not exist.', mtError, [mbOK], 0);
    Exit;
  end;
  if TFile.Exists(fedModfiedStreams.FileName) then
  begin
    Beep;
    if MessageDlg('The output Shapefile already exists. Are you sure you want to overwrite it?.',
      mtWarning, [mbYes, mbNo], 0) <> mrYes then
    begin
      Exit;
    end;

  end;
  if mmoSurferElevations.Lines.Count = 0 then
  begin
    Beep;
    MessageDlg('No elevation grid files have been selected.', mtError, [mbOK], 0);
    Exit;
  end;
  for FileIndex := 0 to mmoSurferElevations.Lines.Count - 1 do
  begin
    if not TFile.Exists(mmoSurferElevations.Lines[FileIndex]) then
    begin
      Beep;
      MessageDlg(Format('The elevation file named %s does not exist',
        [mmoSurferElevations.Lines[FileIndex]]), mtWarning, [mbOK], 0);
      Exit;
    end;
  end;
{  if mmoSurferMeasuredValues.Lines.Count = 0 then
  begin
    Beep;
    if MessageDlg('No measured value grid files have been selected. Do you want to continue anyway?',
      mtError, [mbYes, mbNo], 0) <> mrYes then
    begin
      Exit;
    end;
  end;   }
  for FileIndex := 0 to mmoSurferMeasuredValues.Lines.Count - 1 do
  begin
    if not TFile.Exists(mmoSurferMeasuredValues.Lines[FileIndex]) then
    begin
      Beep;
      MessageDlg(Format('The measured file named %s does not exist',
        [mmoSurferMeasuredValues.Lines[FileIndex]]), mtWarning, [mbOK], 0);
      Exit;
    end;
  end;

  Extensions := TStringList.Create;
  try
    Extensions.Add('.dbf');
    Extensions.Add('.prj');
    Extensions.Add('.sbn');
    Extensions.Add('.sbx');
    Extensions.Add('.fbn');
    Extensions.Add('.fbx');
    Extensions.Add('.ain');
    Extensions.Add('.aih');
    Extensions.Add('.ixs');
    Extensions.Add('.mxs');
    Extensions.Add('.atx');
    Extensions.Add('.cpg');
    Extensions.Add('.qix');
    Extensions.Add('.shp.xml');

    for ExtensionIndex := 0 to Extensions.Count - 1 do
    begin
      ExistingFileName := ChangeFileExt(fedStreamToModify.FileName,
        Extensions[ExtensionIndex]);
      NewFileName := ChangeFileExt(fedModfiedStreams.FileName,
        Extensions[ExtensionIndex]);
      if TFile.Exists(ExistingFileName) then
      begin
        if TFile.Exists(NewFileName) then
        begin
          TFile.Delete(NewFileName);
        end;
        TFile.Copy(ExistingFileName, NewFileName);
      end;
    end;
  finally
    Extensions.Free;
  end;

  Screen.Cursor := crHourGlass;
  Converter := TElevationAssigner.Create;
  try
    Converter.InputShapeFile := fedStreamToModify.FileName;
    Converter.OutputShapeFile := fedModfiedStreams.FileName;
    Converter.ElevationGridFiles := mmoSurferElevations.Lines;
    Converter.MeasuredValueGridFiles := mmoSurferMeasuredValues.Lines;
    Converter.OnProgress := DoOnProgress;
    Converter.PerformConversion;
  finally
    Converter.Free;
    Screen.Cursor := crDefault;
  end;


  ShowMessage('Done');
end;

procedure TfrmMain.btnSelectExcludeAreasClick(Sender: TObject);
begin
  if dlgOpenSelectExcludeAreas.Execute then
  begin
    memoExclude.Lines.AddStrings(dlgOpenSelectExcludeAreas.Files);
  end;
end;

procedure TfrmMain.btnSplitDEMClick(Sender: TObject);
const
  SplitWidth = 32;
var
  Surfer7Grid: TSurferRaster7File2;
  ParentHeader: TGrid7Header;
  ChildHeader: TGrid7Header;
  RowIndex: Integer;
  ColIndex: Integer;
  Data: T2DDoubleArray;
  InnerRowIndex: Integer;
  InnerColIndex: Integer;
  RIndex: Integer;
  GridIndex: Integer;
  ParentRoot: String;
  CIndex: Integer;
begin
  if odCoarsenDEM.Execute then
  begin
    ParentRoot := ChangeFileExt(odCoarsenDEM.FileName, '') + '_';
    Surfer7Grid := TSurferRaster7File2.Create(odCoarsenDEM.FileName);
    try
      ParentHeader := Surfer7Grid.Header;
      ChildHeader := ParentHeader;
      ChildHeader.nRow := SplitWidth;
      ChildHeader.nCol := SplitWidth;
      SetLength(Data, SplitWidth, SplitWidth);
      GridIndex := 0;
      for RowIndex := 0 to ParentHeader.nRow div SplitWidth -1 do
      begin
        for ColIndex := 0 to ParentHeader.nCol div SplitWidth -1 do
        begin
          ChildHeader.xLL := ParentHeader.xLL + ColIndex*SplitWidth;
          ChildHeader.yLL := ParentHeader.yLL + RowIndex*SplitWidth;
          for InnerRowIndex := 0 to SplitWidth - 1 do
          begin
            RIndex := RowIndex*SplitWidth + InnerRowIndex;
            for InnerColIndex := 0 to SplitWidth - 1 do
            begin
              CIndex := ColIndex*SplitWidth + InnerColIndex;
              Data[InnerColIndex,InnerRowIndex] := Surfer7Grid.Z[CIndex,RIndex];
            end;
          end;
          Inc(GridIndex);

          WriteSurferGridFile(ParentRoot + IntToStr(GridIndex) + '.grd', ChildHeader, Data);
        end;
      end;
    finally
      Surfer7Grid.Free;
    end;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  Header: TGrid7Header;
  Data: T2DDoubleArray;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if dlgSaveRandomDem.Execute then
  begin
    Header.nRow := 32*3;
    Header.nCol := 32*3;
    Header.xLL := 0;
    Header.yLL := 0;
    Header.xSize := 1;
    Header.ySize := 1;
    Header.Rotation := 0;
    Header.BlankValue := 1e38;
    SetLength(Data, Header.nRow, Header.nCol);
    Randomize;
    for RowIndex := 0 to Header.nRow -1 do
    begin
      for ColIndex := 0 to Header.nCol -1 do
      begin
        Data[RowIndex,ColIndex] := Random(100);
      end;
    end;
    WriteSurferGridFile(dlgSaveRandomDem.FileName, Header, Data);
  end;
end;

procedure TfrmMain.cbMaximumCarvingDepthClick(Sender: TObject);
begin
  rdeMaximumCarvingDepth.Enabled := cbMaximumCarvingDepth.Checked;
end;

procedure TfrmMain.cbMaximumCarvingLengthClick(Sender: TObject);
begin
  rdeMaximumCarvingLength.Enabled := cbMaximumCarvingLength.Checked;
end;

procedure TfrmMain.DoOnProgress(Sender: TObject; Position,
  MaxPosition: Integer);
begin
  pb1.Max := MaxPosition;
  pb1.Position := Position;

end;

procedure TfrmMain.WriteFlowDirections(FlowDirections: TFlowDirections;
  Raster: IRaster);
var
  FlowDirectionsRaster: TSurferRaster7File2;
  YIndex: Integer;
  XIndex: Integer;
  Z: Double;
  Header: TGrid7Header;
  MinZ: Double;
  MaxZ: Double;
  FoundFirst: Boolean;
begin
  if fedFlowDirections.FileName <> '' then
  begin
    lblStatus.Caption := 'writing Flow Directions raster';
    if TFile.Exists(fedFlowDirections.FileName) then
    begin
      TFile.Delete(fedFlowDirections.FileName);
    end;
    TFile.Copy(fedInput.FileName, fedFlowDirections.FileName);
    FlowDirectionsRaster := TSurferRaster7File2.Create(fedFlowDirections.FileName);
    try
      Header := FlowDirectionsRaster.Header;
      FoundFirst := False;
      MinZ := 0;
      MaxZ := 0;
      for YIndex := 0 to FlowDirections.YCount - 1 do
      begin
        for XIndex := 0 to FlowDirections.XCount - 1 do
        begin
          if Raster.Ignore[XIndex, YIndex] then
          begin
            FlowDirectionsRaster.Z[XIndex, YIndex] := Header.BlankValue;
          end
          else
          begin
            Z := Ord(FlowDirections.Ordinals[XIndex, YIndex]);
            FlowDirectionsRaster.Z[XIndex, YIndex] := Z;
            if FoundFirst then
            begin
              if Z < MinZ then
              begin
                MinZ := Z;
              end;
              if Z > MaxZ then
              begin
                MaxZ := Z;
              end;
            end
            else
            begin
              MinZ := Z;
              MaxZ := Z;
              FoundFirst := True;
            end;
          end;
        end;
      end;
      Header.zMin := MinZ;
      Header.zMax := MaxZ;
      FlowDirectionsRaster.Header := Header;
    finally
      FlowDirectionsRaster.Free;
    end;
  end;
end;

procedure TfrmMain.ExtractPitsUsingMemory(RastInterface: IRaster;
  out PitlessRaster: IRaster; out FlowDirections: TFlowDirections);
var
  Changed: Boolean;
  PitRemover: TPitRemover;
  Pits: TPointList;
  EdgeStartPoints: TPointList;
//  PassNumber: Integer;
  StartingPointsOutline: TExcludeOutline;
//  AStringList: TStringList;
  CutCriteria: TCutCriteria;
begin
  AssignCritera(CutCriteria);
  try
    GetStartingPointsOutline(StartingPointsOutline);

    IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits,
      StartingPointsOutline, DoOnProgress);
    PitRemover := TPitRemover.Create;
    try
      PitRemover.OnProgress := DoOnProgress;
      lblStatus.Caption := 'removing pits';
      Application.ProcessMessages;
      PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitlessRaster,
        FlowDirections, CutCriteria, Changed);
//      PassNumber := 1;
    finally
      //          while Changed do
      //          begin
      //            Inc(PassNumber);
      //            RastInterface := PitlessRaster;
      //            EdgeStartPoints.Free;
      //            Pits.Free;
      //            PitlessRaster := nil;
      //            lblStatus.Caption := Format('Identifying start points - Pass %d', [PassNumber]);
      //            Application.ProcessMessages;
      //            IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, StartingPointsOutline, DoOnProgress);
      //            lblStatus.Caption := Format('removing pits - Pass %d', [PassNumber]);
      //            Application.ProcessMessages;
      //            PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitlessRaster,
      //              FlowDirections, Changed);
      //          end;
      PitRemover.Free;
    end;
  finally
    EdgeStartPoints.Free;
    Pits.Free;
    StartingPointsOutline.Free;
  end;
end;

procedure TfrmMain.fedInputChange(Sender: TObject);
var
  FileRoot: string;
begin
  FileRoot := ChangeFileExt(fedInput.FileName, '');
  fedPitlessRaster.FileName := FileRoot + 'Pitless.grd';
  fedFlowDirections.FileName := FileRoot + 'FlowDirection.grd';
  fedOutput.FileName := FileRoot + 'WatershedID.grd';
  fedStreamsOutput.FileName := FileRoot + 'Streams.shp';
end;

procedure TfrmMain.ExtractAndWriteStreamsMemory(PitlessRaster: IRaster;
  RastInterface: IRaster; FlowDirections: TFlowDirections;
  const ExistingRasterFile: string);
var
  MinArea: Integer;

  Streams: TStreamObjectList;
  RunoffRaster: IRaster;
  AccumulationRaster: IRaster;
begin
  MinArea := seMinimumPixels.AsInteger;
  try
    lblStatus.Caption := 'extracting streams';
    Application.ProcessMessages;
    ComputeAccumulationModified(PitlessRaster, FlowDirections,
      AccumulationRaster, MinArea, Streams, DoOnProgress);

    lblStatus.Caption := 'identifying contributing areas';
    Application.ProcessMessages;
    ExtractRunoffLocations(Streams, FlowDirections, PitlessRaster,
      RunoffRaster, DoOnProgress);

    WriteContributingArea(Streams, RastInterface, RunoffRaster, ExistingRasterFile);

    lblStatus.Caption := 'writing streams';
    Application.ProcessMessages;
    ExportStreams(Streams, PitlessRaster, AccumulationRaster,
      fedStreamsOutput.FileName);
  finally
    AccumulationRaster := nil;
    Streams.Free;
    PitlessRaster := nil;
  end;
end;

procedure TfrmMain.ExtractAndWriteStreamsFile(PitlessRaster: IRasterFile;
  RastInterface: IRasterFile; FlowDirections: TFlowDirections;
  const ExistingRasterFile: string);
var
  RunoffRaster: IRasterFile;
  AccumulationRaster: IRasterFile;
  Streams: TStreamObjectList;
  MinArea: Integer;
begin
  MinArea := seMinimumPixels.AsInteger;
  try
    lblStatus.Caption := 'extracting streams';
    Application.ProcessMessages;
    ComputeAccumulationSurfModified(PitlessRaster, FlowDirections,
      AccumulationRaster, MinArea, Streams, DoOnProgress);

    lblStatus.Caption := 'identifying contributing areas';
    Application.ProcessMessages;
    ExtractRunoffLocationsSurf(Streams, FlowDirections, PitlessRaster,
      RunoffRaster, DoOnProgress);

    WriteContributingArea(Streams, RastInterface, RunoffRaster, ExistingRasterFile);

    lblStatus.Caption := 'writing streams';
    Application.ProcessMessages;
    ExportStreams(Streams, PitlessRaster, AccumulationRaster,
      fedStreamsOutput.FileName);
  finally
    AccumulationRaster := nil;
    Streams.Free;
    PitlessRaster := nil;
  end;
end;

procedure TfrmMain.ReadFlowDirectionsFromFile(FlowDirections: TFlowDirections;
  FlowDirectionFileName: string);
var
  XIndex: Integer;
  FlowDirRaster: TSurferRaster7File2;
  YIndex: Integer;
begin
  lblStatus.Caption := 'reading flow directions';
  Application.ProcessMessages;
  FlowDirRaster := TSurferRaster7File2.Create(FlowDirectionFileName);
  try
    for YIndex := 0 to FlowDirRaster.YCount - 1 do
    begin
      for XIndex := 0 to FlowDirRaster.XCount - 1 do
      begin
        if FlowDirRaster.Ignore[XIndex, YIndex] then
        begin
          FlowDirections.Ordinals[XIndex, YIndex] := foMiddle;
        end
        else
        begin
          FlowDirections.Ordinals[XIndex, YIndex] :=
            TFlowOrdinal(Round(FlowDirRaster.Z[XIndex, YIndex]));
        end;
      end;
    end;
  finally
    FlowDirRaster.Free;
  end;
end;

procedure TfrmMain.WritePitlessRaster(PitlessRaster: IRaster;
  Raster: IRaster);
var
  PitLess: TSurferRaster7File2;
  XIndex: Integer;
  Header: TGrid7Header;
  Z: Double;
  YIndex: Integer;
  FoundFirst: Boolean;
  MinZ: Double;
  MaxZ: Double;
begin
  if fedPitlessRaster.FileName <> '' then
  begin
    lblStatus.Caption := 'writing pitless raster';
    if TFile.Exists(fedPitlessRaster.FileName) then
    begin
      TFile.Delete(fedPitlessRaster.FileName);
    end;
    TFile.Copy(fedInput.FileName, fedPitlessRaster.FileName);
    PitLess := TSurferRaster7File2.Create(fedPitlessRaster.FileName);
    try
      Header := PitLess.Header;
      FoundFirst := False;
      MinZ := 0;
      MaxZ := 0;
      for YIndex := 0 to PitlessRaster.YCount - 1 do
      begin
        for XIndex := 0 to PitlessRaster.XCount - 1 do
        begin
          if PitlessRaster.Ignore[XIndex, YIndex]
            or Raster.Ignore[XIndex, YIndex] then
          begin
            PitLess.Z[XIndex, YIndex] := Header.BlankValue;
          end
          else
          begin
            Z := PitlessRaster.Z[XIndex, YIndex];
            PitLess.Z[XIndex, YIndex] := Z;
            if FoundFirst then
            begin
              if Z < MinZ then
              begin
                MinZ := Z;
              end;
              if Z > MaxZ then
              begin
                MaxZ := Z;
              end;
            end
            else
            begin
              MinZ := Z;
              MaxZ := Z;
              FoundFirst := True;
            end;
          end;
        end;
      end;
      Header.zMin := MinZ;
      Header.zMax := MaxZ;
      PitLess.Header := Header;
    finally
      PitLess.Free;
    end;
  end;
end;

procedure TfrmMain.WriteContributingArea(Streams: TStreamObjectList;
  Raster: IRaster; RunoffRaster: IRaster; const ExistingRasterFile: string);
var
  Z: Double;
  XIndex: Integer;
  MinZ: Double;
  FoundFirst: Boolean;
  WaterShed: TSurferRaster7File2;
  Header: TGrid7Header;
  YIndex: Integer;
  MaxZ: Double;
begin
  lblStatus.Caption := 'writing contributing areas';
  Application.ProcessMessages;

  if TFile.Exists(fedOutput.FileName) then
  begin
    TFile.Delete(fedOutput.FileName);
  end;
  TFile.Copy(ExistingRasterFile, fedOutput.FileName);
  WaterShed := TSurferRaster7File2.Create(fedOutput.FileName);
  try
    Header := WaterShed.Header;
    if True then
    begin
      if Header.BlankValue <= Streams.Count then
      begin
        Header.BlankValue := Streams.Count + 1;
      end;
    end;
    FoundFirst := False;
    MinZ := 0;
    MaxZ := 0;
    for YIndex := 0 to RunoffRaster.YCount - 1 do
    begin
      for XIndex := 0 to RunoffRaster.XCount - 1 do
      begin
        if RunoffRaster.Ignore[XIndex, YIndex]
          or Raster.Ignore[XIndex, YIndex] then
        begin
          WaterShed.Z[XIndex, YIndex] := Header.BlankValue;
        end
        else
        begin
          Z := RunoffRaster.Z[XIndex, YIndex];
          WaterShed.Z[XIndex, YIndex] := Z;
          if FoundFirst then
          begin
            if Z < MinZ then
            begin
              MinZ := Z;
            end;
            if Z > MaxZ then
            begin
              MaxZ := Z;
            end;
          end
          else
          begin
            MinZ := Z;
            MaxZ := Z;
            FoundFirst := True;
          end;
        end;
      end;
    end;
    Header.zMin := MinZ;
    Header.zMax := MaxZ;
    WaterShed.Header := Header;
  finally
    WaterShed.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pgcMethods.ActivePageIndex := 0;
end;

end.
