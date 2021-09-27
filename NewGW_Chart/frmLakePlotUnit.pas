unit frmLakePlotUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, TeeProcs, TeEngine, Chart, StdCtrls, contnrs, Series,
  Buttons, ToolWin, ComCtrls, Strset, MyFormUnit, ReaderUnit, siComboBox,
  TeeEdit, VclTee.TeeGDIPlus;

type
  TfrmLakePlot = class(TMyForm)
    chrtLake: TChart;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    SaveImage1: TMenuItem;
    Exit1: TMenuItem;
    Format1: TMenuItem;
    About1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveData1: TMenuItem;
    Print1: TMenuItem;
    ToolBar1: TToolBar;
    Label2: TLabel;
    sbImage: TSpeedButton;
    sbFormat: TSpeedButton;
    sbSaveData: TSpeedButton;
    sbPrint: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    comboDataSource: TComboBox;
    StrSetModflowStrings: TStrSet;
    StrSetMoc3dStrings: TStrSet;
    PrintDialog1: TPrintDialog;
    Help1: TMenuItem;
    Help2: TMenuItem;
    comboGraphType: TsiComboBox;
    ChartEditor1: TChartEditor;
    ChartPreviewer1: TChartPreviewer;
    sbAdvancedFormat: TSpeedButton;
    Format2: TMenuItem;
    FormatAdvanced1: TMenuItem;
    PrintPreview1: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure comboGraphTypeChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure sbImageClick(Sender: TObject);
    procedure sbFormatClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure sbSaveDataClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure comboDataSourceChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure chrtLakeAfterDraw(Sender: TObject);
    procedure chrtLakeGetLegendPos(Sender: TCustomChart; Index: Integer;
      var X, Y, XColor: Integer);
    procedure chrtLakeGetLegendRect(Sender: TCustomChart; var Rect: TRect);
    procedure Help2Click(Sender: TObject);
    procedure sbAdvancedFormatClick(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
  private
    ExplanationVisible : boolean;
    LegendRect : TRect;
    ExplanationTop, ExplanationLeft : integer;
    function GetNextLine(SearchTerm: string; Start: integer;
      Listing : TStringList): integer; overload;
    function GetNextLine(SearchTerm1, SearchTerm2: string; Start: integer;
      Listing : TStringList): integer; overload;
    function GetStringBetween(AString, Before, After: String): string;
    function ReadStageVolume(Listing: TStringlist): integer;
    function ReadBudget(Listing: TStringlist; Start : integer): integer;
    procedure ReadBudgets(Listing: TStringlist; Start: integer);
    function DStrToFloat(AString: String): extended;
    Procedure FreeSeries;
    function GetColor(AColor: TColor): TColor;
    procedure CreateStageVolumeChart;
    procedure CreateLakeSummaryChart;
    procedure CreateLakeChart;
    Procedure SetAxesTitles;
    procedure WriteStageVolumeLines(Lines: TStringList);
    procedure WriteLakeSummaryLines(Lines: TStringList);
    procedure WriteLakeLines(Lines: TStringList);
    function ReadMocBudget(Listing: TStringlist; Start: integer): integer;
    procedure ReadMocBudgets(Listing: TStringlist);
    procedure CreateMocLakeVolumeTimeChart;
    procedure CreateMocLakeSoluteChart;
    procedure WriteMocLakeVolumeTime(Lines: TStringList);
    procedure WriteMocLakeSolute(Lines: TStringList);
    procedure ReadGages(Listing: TStringlist);
    procedure CreateGageTimeChart;
    procedure InitializeChart;
    procedure HideLegend;
    procedure UpdateDropDownWidth;
    { Private declarations }
  public
    StageVolumeList : TObjectList;
    TimeList : TObjectList;
    SeriesList : TObjectList;
    { Public declarations }
  end;

  TLakeData = class(TObject)
    LakeNumber: integer;
    Stage : double;
    Precipitation : double;
    Evaporation : double;
    Runoff : double;
    GW_Inflow : double;
    GW_Outflow : double;
    SW_Inflow : double;
    SW_Outflow : double;
    WaterUse : double;
    ConnectedLakeInflow : double;
    Volume : double;
    VolumeChange: double;
    SurfaceArea : double;
    StageChange : double;
    CumulativeStageChange : double;
    PercentDiscrepancy: Double;
  end;

  TLakesTime = class(TObject)
    LakeList : TObjectList;
    Time : double;
    ConnectedLakes : integer;
    DryLakeCells : integer;
    Area : double;
    constructor Create;
    Destructor Destroy; override;
  end;

  TStageVolume = class(TObject)
    Stage : double;
    Volume : double;
    Area: double;
  end;

  TMocLakeTime = class(TObject)
    LakeList : TObjectList;
    Time : double;
    constructor Create;
    Destructor Destroy; override;
  end;

  TMocLakeData = class(TObject)
    SoluteList : TObjectList;
    LakeVolume : double;
    constructor Create;
    Destructor Destroy; override;
  end;

  TMocLakeSoluteData = class(TObject)
    Concentration : double;
    PrecipitationMassIn : double;
    StreamMassIn : double;
    StreamMassOut : double;
    NetWithdrawal : double;
    RunoffMassIn : double;
    GW_MassIn : double;
    GW_MassOut : double;
    SoluteMassInLake : double;
    procedure Read(AString : string);
  end;

  TGageLakeTime = class(TObject)
    ValueList : TObjectList;
    Time : double;
    constructor Create;
    destructor Destroy; override;
  end;

  TGageData = Class(TObject)
    Value : double;
  end;

var
  frmLakePlot: TfrmLakePlot;

implementation

{$R *.DFM}

uses StrUtils, Math, frmFormatUnit, frmAboutUnit, frmModChartUnit;

function GetNextString(var AString : string) : string;
var
  Position : integer;
begin
  AString := Trim(AString);
  Position := Pos(' ', AString);
  if Position > 0 then
  begin
    result := Copy(AString, 1, Position-1);
    AString := Trim(Copy(AString, Position, MAXINT));
  end
  else
  begin
    result := AString;
    AString := '';
  end;

end;

function GetNCharacters(var AString : string; N : integer) : string;
begin
  result := Copy(AString, 1, N);
  AString := Copy(AString, N+1, MAXINT);
end;

function GetNextGageNumber(var AString : string) : string;
var
  SpacePos: integer;
  Index: integer;
  Start: integer;
  EndNumPos: integer;
begin
  result := AString;
  if Length(result) > 0 then
  begin
    if result[1] = ' ' then
    begin
      Start := MAXINT;
      for Index := 2 to Length(result) do
      begin
        if result[Index] <> ' ' then
        begin
          Start := Index;
          break;
        end;
      end;
    end
    else
    begin
      Start := 1;
    end;
    SpacePos := PosEx(' ', result, Start);
    if SpacePos = 0 then
    begin
      SpacePos := MAXINT;
    end;
    EndNumPos := PosEx('E', result, Start);
    if EndNumPos = 0 then
    begin
      EndNumPos := MAXINT;
    end
    else
    begin
      EndNumPos := EndNumPos + 4;;
    end;

    EndNumPos := Min(EndNumPos, SpacePos);
    result := Copy(result, 1, EndNumPos-1);
    AString := Copy(AString, EndNumPos, MAXINT);
  end;
end;

procedure TfrmLakePlot.Open1Click(Sender: TObject);
var
  Listing : TStringList;
  Index : integer;
begin
  If OpenDialog1.Execute then
  begin
    try
      ExplanationVisible := True;
      Screen.Cursor := crHourglass;
      StageVolumeList.Clear;
      TimeList.Clear;
      Listing := TStringList.Create;
      try
        Listing.LoadFromFile(OpenDialog1.FileName);
        if comboDataSource.ItemIndex = 0 then
        begin
          Index := ReadStageVolume(Listing);
          ReadBudgets(Listing, Index);
        end
        else if comboDataSource.ItemIndex = 1 then
        begin
          ReadMocBudgets(Listing);
        end
        else
        begin
          ReadGages(Listing);
        end;
      finally
        Listing.Free;
      end;
      comboGraphTypeChange(Sender);
      sbImage.Enabled := True;
      SaveImage1.Enabled := True;
      sbFormat.Enabled := True;
      sbAdvancedFormat.Enabled := True;
      Format1.Enabled := True;
      sbSaveData.Enabled := True;
      SaveData1.Enabled := True;
      Print1.Enabled := True;
      PrintPreview1.Enabled := True;
      sbPrint.Enabled := True;
    finally
      Screen.Cursor := crDefault;
    end;

  end;
end;

function TfrmLakePlot.GetNextLine(SearchTerm : string; Start : integer;
  Listing : TStringList) : integer;
var
  Index : integer;
begin
  result := -1;
  for Index := Start to Listing.Count -1 do
  begin
    if Pos(SearchTerm, Listing.Strings[Index])> 0 then
    begin
      result := Index;
      break;
    end;
  end;
end;

function TfrmLakePlot.GetStringBetween(AString, Before, After : String) : string;
begin
  result := AString;
  if Before <> '' then
  begin
    result := Copy(AString,
        Pos(Before,AString)+Length(Before),
        Length(AString));
  end;

  if After <> '' then
  begin
    result := Copy(result,1,Pos(After,result)-1);
  end;

  result := Trim(result);
end;


{ TLakesTime }

constructor TLakesTime.Create;
begin
  inherited;
  LakeList := TObjectList.Create;
end;

destructor TLakesTime.Destroy;
begin
  LakeList.Free;
  inherited;
end;

procedure TfrmLakePlot.FormCreate(Sender: TObject);
begin
  ExplanationVisible := False;
  StageVolumeList := TObjectList.Create;
  TimeList := TObjectList.Create;
  SeriesList := TObjectList.Create;
  comboDataSource.ItemIndex := 0;
  comboGraphType.Items.Assign(StrSetModflowStrings.Strings);
  UpdateDropDownWidth;
  comboGraphType.ItemIndex := 0;
  chrtLake.Title.Text.Clear;
  Constraints.MinHeight := Height div 2;
  Constraints.MinWidth := Width;

end;

procedure TfrmLakePlot.FormDestroy(Sender: TObject);
begin
  StageVolumeList.Free;
  TimeList.Free;
  SeriesList.Free;
end;

function TfrmLakePlot.ReadStageVolume(Listing: TStringlist) : integer;
const
  SearchTerm = 'STAGE/VOLUME RELATION FOR LAKE';
var
  index : integer;
  NoMoreLakes : boolean;
  AString : String;
  StageList : TObjectList;
  StageVolume : TStageVolume;
  Splitter: TStringList;
begin
  Splitter:= TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    result := -1;
    index := GetNextLine(SearchTerm,0,Listing) + 4;
    if index > 3 then
    begin
      repeat
        StageList := TObjectList.Create;
        StageVolumeList.Add(StageList);

        AString := Listing[Index];
        repeat
          Splitter.DelimitedText := AString;
          Assert(Splitter.Count in [2,3]);
          StageVolume := TStageVolume.Create;
          StageList.Add(StageVolume);
          StageVolume.Stage := DStrToFloat(Splitter[0]);
          StageVolume.Volume := DStrToFloat(Splitter[1]);
          if Splitter.Count = 3 then
          begin
            StageVolume.Area := DStrToFloat(Splitter[2]);
          end
          else
          begin
            StageVolume.Area := -1;
          end;

          Inc(Index);
          AString := Listing[Index];
        until AString = '';
        NoMoreLakes := (Pos(SearchTerm,Listing[Index+3]) = 0);
        if not NoMoreLakes then
        begin
          Inc(Index,7);
          AString := Listing[Index];
        end;
      until NoMoreLakes;
      result := index;
    end;
  finally
    Splitter.Free;
  end;    
end;

function TfrmLakePlot.ReadBudget(Listing: TStringlist;
  Start: integer): integer;
Const
  ConnectedLakes = 'NUMBER OF CONNECTED LAKES IS';
  TotalArea = 'TOTAL AREA =';
  DryCells = 'LAKE CELLS ARE DRY';
  DryCells2 = 'SECTIONS OF THE LAKE BOTTOM HAVE BECOME DRY';
  Time = 'TOTAL SIMULATION TIME';
var
  Index : integer;
  LakesTimeObject : TLakesTime;
  LakeData : TLakeData;
  AString : string;
  TimeIndex : integer;
  LineIndex : integer;
  Splitter: TStringList;
begin
  Splitter:= TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    LakesTimeObject := nil;
    result := -1;
    Index := GetNextLine(ConnectedLakes,Time,Start,Listing);
    if Index > -1 then
    begin
      if Pos(Time,Listing[Index]) > 0 then
      begin
        Start := Index -4;
        Index := -1;
        While Listing[Start] <> '' do
        begin
          Dec(Start);
        end;
      end;
    end;
    if Index > -1 then
    begin
      LakesTimeObject := TLakesTime.Create;
      TimeList.Add(LakesTimeObject);
      AString := Listing[Index];
      LakesTimeObject.ConnectedLakes := StrToInt(
        GetStringBetween(AString,ConnectedLakes,TotalArea));
      LakesTimeObject.Area := DStrToFloat(
        GetStringBetween(AString,TotalArea,''));
    end;

    if Index > -1 then
    begin
      Index := GetNextLine(DryCells,DryCells2,Index,Listing);
    end
    else
    begin
      Index := GetNextLine(DryCells,DryCells2,Start,Listing);
    end;
    if Index > -1 then
    begin
      if LakesTimeObject = nil then
      begin
        LakesTimeObject := TLakesTime.Create;
        TimeList.Add(LakesTimeObject);
      end;

      AString := Listing[Index];
      if Pos(DryCells,AString) > 0 then
      begin
        LakesTimeObject.DryLakeCells := StrToIntDef(
          GetStringBetween(AString,'',DryCells), -MAXINT);
        Inc(Index,3);
      end
      else
      begin
        Inc(Index);
        AString := Listing[Index];
        LakesTimeObject.DryLakeCells := StrToIntDef(
          GetStringBetween(AString,'LIE ABOVE THE FOLLOWING','AQUIFER CELLS'),
          -MAXINT);
        for LineIndex := Index + 1 to Listing.Count -1 do
        begin
          If Listing[LineIndex] = '' then
          begin
            Index := LineIndex + 2;
            break;
          end;
        end;

      end;

      AString := Listing[Index];
      LakesTimeObject.Time := DStrToFloat(
        GetStringBetween(AString,Time,''));

      Inc(Index,7);
      AString := Listing[Index];
      While AString <> '' do
      begin
        LakeData := TLakeData.Create;
        LakesTimeObject.LakeList.Add(LakeData);
        AString := StringReplace(AString, 'N/A (SS)', '0', [rfReplaceAll, rfIgnoreCase]);
        Splitter.DelimitedText := AString;
        Assert(Splitter.Count in [5,7]);
        if Splitter.Count = 7 then
        begin
          LakeData.LakeNumber := StrToInt(Splitter[0]);
          LakeData.Stage := DStrToFloat(Splitter[1]);
          LakeData.Volume :=  DStrToFloat(Splitter[2]);
          LakeData.VolumeChange :=  DStrToFloat(Splitter[3]);
          LakeData.Precipitation :=  DStrToFloat(Splitter[4]);
          LakeData.Evaporation :=  DStrToFloat(Splitter[5]);
          LakeData.Runoff :=  DStrToFloat(Splitter[6]);
        end
        else
        begin
          LakeData.LakeNumber := StrToInt(Splitter[0]);
          LakeData.Stage := DStrToFloat(Splitter[1]);
          LakeData.Volume :=  -MAXINT;
          LakeData.VolumeChange :=  -MAXINT;
          LakeData.Precipitation :=  DStrToFloat(Splitter[2]);
          LakeData.Evaporation :=  DStrToFloat(Splitter[3]);
          LakeData.Runoff :=  DStrToFloat(Splitter[4]);
        end;


//        GetNCharacters(AString,4);
//        LakeData.Stage := DStrToFloat(Trim(GetNCharacters(AString,11)));
//        LakeData.Precipitation := DStrToFloat(Trim(GetNCharacters(AString,12)));
//        LakeData.Evaporation := DStrToFloat(Trim(GetNCharacters(AString,12)));
//        LakeData.Runoff := DStrToFloat(Trim(GetNCharacters(AString,12)));
        Inc(Index);
        AString := Listing[Index];
      end;

      TimeIndex := 0;
      Inc(Index,3);
      AString := Listing[Index];
      While AString <> '' do
      begin
        LakeData := LakesTimeObject.LakeList.Items[TimeIndex] as TLakeData;
        Inc(TimeIndex);

        AString := StringReplace(AString, 'N/A (SS)', '0', [rfReplaceAll, rfIgnoreCase]);
        Splitter.DelimitedText := AString;
        Assert(Splitter.Count in [5,6]);

        Assert(LakeData.LakeNumber = StrToInt(Splitter[0]));
        LakeData.GW_Inflow := DStrToFloat(Splitter[1]);
        LakeData.GW_Outflow := DStrToFloat(Splitter[2]);
        LakeData.SW_Inflow := DStrToFloat(Splitter[3]);
        LakeData.SW_Outflow := DStrToFloat(Splitter[4]);
        if Splitter.Count = 6 then
        begin
          LakeData.WaterUse := DStrToFloat(Splitter[5]);
        end
        else
        begin
          LakeData.WaterUse := 0;
        end;                                          

//        GetNCharacters(AString,4);

//        LakeData.GW_Inflow := DStrToFloat(Trim(GetNCharacters(AString,12)));
//        LakeData.GW_Outflow := DStrToFloat(Trim(GetNCharacters(AString,12)));
//        LakeData.SW_Inflow := DStrToFloat(Trim(GetNCharacters(AString,12)));
//        LakeData.SW_Outflow := DStrToFloat(Trim(GetNCharacters(AString,12)));

        Inc(Index);
        AString := Listing[Index];
      end;

      TimeIndex := 0;
      Inc(Index,3);
      AString := Listing[Index];
      While Pos('---',AString) = 0 do
      begin
        LakeData := LakesTimeObject.LakeList.Items[TimeIndex] as TLakeData;
        Inc(TimeIndex);

        AString := StringReplace(AString, 'N/A (SS)', '0', [rfReplaceAll, rfIgnoreCase]);
        Splitter.DelimitedText := AString;
        Assert(Splitter.Count in [6,7]);

        if Splitter.Count = 6 then
        begin
          Assert(LakeData.LakeNumber = StrToInt(Splitter[0]));
          LakeData.ConnectedLakeInflow := DStrToFloat(Splitter[1]);
          LakeData.SurfaceArea := DStrToFloat(Splitter[2]);
          LakeData.StageChange := DStrToFloat(Splitter[3]);
          LakeData.CumulativeStageChange := DStrToFloat(Splitter[4]);
          LakeData.PercentDiscrepancy := DStrToFloat(Splitter[5]);
        end
        else
        begin
          Assert(LakeData.LakeNumber = StrToInt(Splitter[0]));
          LakeData.WaterUse := DStrToFloat(Splitter[1]);
          LakeData.ConnectedLakeInflow := DStrToFloat(Splitter[2]);
          LakeData.Volume := DStrToFloat(Splitter[3]);
          LakeData.SurfaceArea := DStrToFloat(Splitter[4]);
          LakeData.StageChange := DStrToFloat(Splitter[5]);
          LakeData.CumulativeStageChange := DStrToFloat(Splitter[6]);
          LakeData.PercentDiscrepancy := -MAXINT;
        end;

//        GetNCharacters(AString,4);
//
//        LakeData.WaterUse := DStrToFloat(Trim(GetNCharacters(AString,13)));
//        LakeData.ConnectedLakeInflow := DStrToFloat(Trim(GetNCharacters(AString,13)));
//        LakeData.Volume := DStrToFloat(Trim(GetNCharacters(AString,13)));
//        LakeData.SurfaceArea := DStrToFloat(Trim(GetNCharacters(AString,15)));
//        LakeData.StageChange := DStrToFloat(Trim(GetNCharacters(AString,13)));
//        LakeData.CumulativeStageChange := DStrToFloat(Trim(GetNCharacters(AString,13)));

        Inc(Index);
        AString := Listing[Index];
      end;
      result := index;
    end;
  finally
    Splitter.Free;
  end;    
end;

function TfrmLakePlot.ReadMocBudget(Listing: TStringlist;
  Start: integer): integer;
const
  SoluteBudget = 'SOLUTE BUDGETS FOR LAKES';
  ElapsedTime = 'ELAPSED TIME =';
var
  LakeNumber : integer;
  ALine : string;
  MocLakeTime : TMocLakeTime;
  MocLakeData : TMocLakeData;
  MocLakeSoluteData : TMocLakeSoluteData;
  Index : integer;
begin
  result := -1;
  Index := GetNextLine(SoluteBudget,Start,Listing);
  if Index > -1 then
  begin
    Inc(Index,4);
    MocLakeTime := TMocLakeTime.Create;
    TimeList.Add(MocLakeTime);

    while Listing[Index] <> '' do
    begin
      ALine := Listing[Index];
      LakeNumber := StrToInt(GetNCharacters(ALine,4)) -1;
      if LakeNumber < MocLakeTime.LakeList.Count then
      begin
        MocLakeData := MocLakeTime.LakeList[LakeNumber] as TMocLakeData;
      end
      else
      begin
        MocLakeData := TMocLakeData.Create;
        MocLakeTime.LakeList.Add(MocLakeData);
      end;
      MocLakeData.LakeVolume := InternationalStrToFloat(GetNCharacters(ALine,10));

      {SoluteNumber :=}{ StrToInt(}GetNCharacters(ALine,5){) -1};
{      if SoluteNumber < MocLakeData.SoluteList.Count then
      begin
        MocLakeSoluteData := MocLakeData.SoluteList[SoluteNumber]
          as TMocLakeSoluteData
      end
      else
      begin  }
        MocLakeSoluteData := TMocLakeSoluteData.Create;
        MocLakeData.SoluteList.Add(MocLakeSoluteData);
//      end;

      MocLakeSoluteData.Read(ALine);
      Inc(Index);

    end;

    Index := GetNextLine(ElapsedTime,Index,Listing);
    if Index > - 1 then
    begin
      MocLakeTime.Time := StrToFloat(GetStringBetween(Listing[Index],ElapsedTime,''));
      result := Index + 1;
    end
    else
    begin
      result := -1;
    end;

  end;
end;

procedure TfrmLakePlot.ReadGages(Listing: TStringlist);
Const
  DataStr = 'DATA';
  TimeStr = 'Time';
  LayerStr = 'LAYER';
var
  Index : integer;
  Start : integer;
  AString : String;
  GageLakeTime : TGageLakeTime;
  GageData : TGageData;
  Header: string;
  DataPos: integer;
  DataTypes: TStringList;
  HeaderIndex: integer;
  LayerPos: integer;
  LayerPresent: boolean;
begin
  Start := Listing.Count;
  comboGraphType.Items.Clear;
  for Index := 0 to Listing.Count -1 do
  begin
    DataPos := Pos(DataStr, Listing[Index]);
    if DataPos > 0 then
    begin
      Header := Listing[Index];
      Header := Copy(Header, DataPos + Length(DataStr), MAXINT);
      StringReplace(Header, '"', '', [rfReplaceAll, rfIgnoreCase]);
      DataPos := Pos(TimeStr, Header);
      if DataPos <= 0 then
      begin
        DataPos := Pos('TIME', Header);
      end;

      Assert(DataPos >= 1);

      LayerPos := Pos(LayerStr, Header);
      LayerPresent := (LayerPos >= 1) and (LayerPos < DataPos);

      Header := Copy(Header, DataPos + Length(TimeStr), MAXINT);

      // this doesn't handle multiple solutes.

      StringReplace(Header, 'M-P Flow', 'M-P-Flow', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Max. Rate', 'Max.-Rate', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Rate Diverted', 'Rate-Diverted', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Upstream Flow', 'Upstream-Flow', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'GW Head', 'GW-Head', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Stream Loss', 'Stream-Loss', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'GW Rech', 'GW-Rech', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Chnge UZ Stor.', 'Chnge-UZ-Stor.', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Vol. UZ Stor.', 'Vol.-UZ-Stor.', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Width Ave. Water Content', 'Width-Ave.-Water-Content', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Cell 1 Water Content', 'Cell-1-Water-Content', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Concentration of ', 'Concentration-of-', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Concentration-of- ', 'Concentration-of-', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Concentration-of- ', 'Concentration-of-', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, ' Solutes', '-Solutes', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Runoff  Concentration', 'Runoff-Concentration', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Runoff Concentration', 'Runoff-Concentration', [rfReplaceAll, rfIgnoreCase]);
      StringReplace(Header, 'Upstream Flow Concentration', 'Upstream-Flow-Concentration', [rfReplaceAll, rfIgnoreCase]);
      Header := Trim(Header);

      DataTypes := TStringList.Create;
      try
        DataTypes.Delimiter := ' ';
        DataTypes.DelimitedText := Header;

        HeaderIndex := DataTypes.IndexOf('');
        while HeaderIndex >= 0 do
        begin
          DataTypes.Delete(HeaderIndex);
          HeaderIndex := DataTypes.IndexOf('');
        end;

        for HeaderIndex := 0 to DataTypes.Count -1 do
        begin
          DataTypes[HeaderIndex] := DataTypes[HeaderIndex] + ' vs. Time'
        end;

        comboGraphType.Items.Assign(DataTypes);
      finally
        DataTypes.Free;
      end;

      Start := Index + 1;
      AString := Listing[Index];

      UpdateDropDownWidth;
      break;
    end;
  end;

  for Index := Start to Listing.Count -1 do
  begin
    AString := Listing[Index];
    if AString = '' then
    begin
      Continue;
    end;

    GetNCharacters(AString,4);

    GageLakeTime := TGageLakeTime.Create;
    TimeList.Add(GageLakeTime);

    if LayerPresent then
    begin
      GetNextGageNumber(AString);
    end;

//    GageLakeTime.Time := InternationalStrToFloat(GetNCharacters(AString,12));
//    GageLakeTime.Time := InternationalStrToFloat(GetNextString(AString));
    GageLakeTime.Time := InternationalStrToFloat(Trim(GetNextGageNumber(AString)));


    while Length(AString) > 0 do
    begin
      GageData := TGageData.Create;
      GageData.Value := InternationalStrToFloat(Trim(GetNextGageNumber(AString)));
      // The following doesn't work because there are sometimes
      // less than 12 characters per number.
//      GageData.Value := InternationalStrToFloat(Trim(GetNCharacters(AString,12)));
      // The following doesn't work because there may not be any spaces between
      // successive numbers (at least for lakes).
//      GageData.Value := InternationalStrToFloat(GetNextString(AString));
      GageLakeTime.ValueList.Add(GageData);
    end;

  end;

  if comboGraphType.Items.Count > 0 then
  begin
    comboGraphType.ItemIndex := 0;
  end;
  
end;

procedure TfrmLakePlot.ReadMocBudgets(Listing: TStringlist);
var
  Index : integer;
begin
  Index := 0;
  While Index > -1 do
  begin
    Index := ReadMocBudget(Listing, Index);
  end;
end;

procedure TfrmLakePlot.ReadBudgets(Listing: TStringlist;
  Start: integer);
var
  Index : integer;
begin
  Index := Start;
  While Index > -1 do
  begin
    Index := ReadBudget(Listing, Index);
  end;
end;

function TfrmLakePlot.DStrToFloat(AString : String): extended;
var
  DPosition : integer;
begin
  DPosition := Pos('D',AString);
  if DPosition = 0 then
  begin
    DPosition := Pos('d',AString);
  end;
  if DPosition > 0 then
  begin
    AString[DPosition] := 'E';
  end;
  result := InternationalStrToFloat(AString);
end;

procedure TfrmLakePlot.WriteStageVolumeLines(Lines : TStringList);
const
  VolStage = 'Volume'#9'Stage';
  StageArea = 'Stage'#9'Area';
  VolArea = 'Volume'#9'Area';
var
  LakeIndex : integer;
  StageIndex : integer;
  StageData : TStageVolume;
  StageList : TObjectList;
begin
  Lines.Clear;
  for LakeIndex := 0 to StageVolumeList.Count -1 do
  begin
    if LakeIndex > 0 then
    begin
      Lines.Add('');
    end;
    Lines.Add('Lake ' + IntToStr(LakeIndex+1));
    case comboGraphType.ItemIndex of
      0: Lines.Add(VolStage);
      1: Lines.Add(StageArea);
      2: Lines.Add(VolArea);
    else Assert(False);
    end;


    StageList := StageVolumeList.Items[LakeIndex] as TObjectList;

    for StageIndex := 0 to StageList.Count - 1 do
    begin
      StageData := StageList[StageIndex] as TStageVolume;
      case comboGraphType.ItemIndex of
        0: Lines.Add(FloatToStr(StageData.Volume) + #9 + FloatToStr(StageData.Stage));
        1: Lines.Add(FloatToStr(StageData.Stage) + #9 + FloatToStr(StageData.Area));
        2: Lines.Add(FloatToStr(StageData.Volume) + #9 + FloatToStr(StageData.Area));
      else Assert(False);
      end;
    end;
  end;
end;

procedure TfrmLakePlot.InitializeChart;
begin
  chrtLake.LeftAxis.Automatic := True;
  chrtLake.BottomAxis.Automatic := True;
end;

procedure TfrmLakePlot.HideLegend;
begin
  if chrtLake.SeriesList.Count < 2 then
  begin
    chrtLake.Legend.Visible := False;
  end;
end;

procedure TfrmLakePlot.CreateStageVolumeChart;
var
  ASeries : TLineSeries;
  LakeIndex : integer;
  AColor : TColor;
  StageIndex : integer;
  StageData : TStageVolume;
  StageList : TObjectList;
begin
  chrtLake.Legend.Visible := True;
  AColor := clAqua;
  for LakeIndex := 0 to StageVolumeList.Count -1 do
  begin
    StageList := StageVolumeList.Items[LakeIndex] as TObjectList;
    ASeries := TLineSeries.Create(self);
    ASeries.XValues.Order := loNone;
    SeriesList.Add(ASeries);
    ASeries.ParentChart := chrtLake;
    AColor := GetColor(AColor);
    ASeries.LinePen.Color := AColor;
    ASeries.AreaColor := AColor;
    ASeries.AreaLinesPen.Color := AColor;
    ASeries.SeriesColor := AColor;
    ASeries.Pointer.Visible := True;
    ASeries.Title := 'Lake ' + IntToStr(LakeIndex + 1);

    for StageIndex := 0 to StageList.Count - 1 do
    begin
      StageData := StageList[StageIndex] as TStageVolume;
      case comboGraphType.ItemIndex of
        0: ASeries.AddXY(StageData.Volume, StageData.Stage,'',clTeeColor);
        1: ASeries.AddXY(StageData.Area, StageData.Stage,'',clTeeColor);
        2: ASeries.AddXY(StageData.Area, StageData.Volume,'',clTeeColor);
      else Assert(False);
      end;


    end;
  end;
end;

procedure TfrmLakePlot.WriteMocLakeSolute(Lines : TStringList);
var
  TimeIndex : integer;
  LakeIndex : integer;
  MocLakeTime : TMocLakeTime;
  MocLakeData : TMocLakeData;
  MocLakeSoluteData : TMocLakeSoluteData;
  SoluteIndex : integer;
  AValue : double;
  AString : string;
begin
  if TimeList.Count > 0 then
  begin
    AString := 'Time';
    MocLakeTime := TimeList.Items[0] as TMocLakeTime;
    for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
    begin
      MocLakeData := MocLakeTime.LakeList[LakeIndex] as TMocLakeData;
      for SoluteIndex := 0 to MocLakeData.SoluteList.Count -1 do
      begin
        AString := AString + #9 + 'Lake ' + IntToStr(LakeIndex + 1)
          + '; Solute ' + IntToStr(SoluteIndex + 1);
      end;
    end;
    Lines.Add(AString);

    for TimeIndex := 0 to TimeList.Count -1 do
    begin
      MocLakeTime := TimeList.Items[TimeIndex] as TMocLakeTime;
      AString := FloatToStr(MocLakeTime.Time);
      for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
      begin
        MocLakeData := MocLakeTime.LakeList[LakeIndex] as TMocLakeData;
        for SoluteIndex := 0 to MocLakeData.SoluteList.Count -1 do
        begin
          MocLakeSoluteData := MocLakeData.SoluteList[SoluteIndex]
            as TMocLakeSoluteData;
          case comboGraphType.ItemIndex of
            1: AValue := MocLakeSoluteData.Concentration;
            2: AValue := MocLakeSoluteData.PrecipitationMassIn;
            3: AValue := MocLakeSoluteData.StreamMassIn;
            4: AValue := MocLakeSoluteData.StreamMassOut;
            5: AValue := MocLakeSoluteData.NetWithdrawal;
            6: AValue := MocLakeSoluteData.RunoffMassIn;
            7: AValue := MocLakeSoluteData.GW_MassIn;
            8: AValue := MocLakeSoluteData.GW_MassOut;
            9: AValue := MocLakeSoluteData.SoluteMassInLake;
          else
            begin
              Assert(False);
              AValue :=  0;
            end;
          end;
          AString := AString + #9 + FloatToStr(AValue);
        end;
      end;
      Lines.Add(AString);
    end;
  end;
end;

procedure TfrmLakePlot.CreateMocLakeSoluteChart;
var
  TimeIndex : integer;
  ASeries : TLineSeries;
  LakeIndex : integer;
  AColor : TColor;
  MocLakeTime : TMocLakeTime;
  MocLakeData : TMocLakeData;
  MocLakeSoluteData : TMocLakeSoluteData;
  SoluteIndex : integer;
  AValue : double;
  SeriesIndex : integer;
begin
  chrtLake.Legend.Visible := True;
  AColor := clAqua;
  if TimeList.Count > 0 then
  begin
    MocLakeTime := TimeList.Items[0] as TMocLakeTime;
    for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
    begin
      MocLakeData := MocLakeTime.LakeList[LakeIndex] as TMocLakeData;
      for SoluteIndex := 0 to MocLakeData.SoluteList.Count -1 do
      begin
        ASeries := TLineSeries.Create(self);
        ASeries.XValues.Order := loNone;
        SeriesList.Add(ASeries);
        ASeries.ParentChart := chrtLake;
        AColor := GetColor(AColor);
        ASeries.LinePen.Color := AColor;
        ASeries.AreaColor := AColor;
        ASeries.AreaLinesPen.Color := AColor;
        ASeries.SeriesColor := AColor;
        ASeries.Pointer.Visible := True;
        ASeries.Title := 'Lake ' + IntToStr(LakeIndex + 1)
          + '; Solute ' + IntToStr(SoluteIndex + 1);
      end;
    end;

    for TimeIndex := 0 to TimeList.Count -1 do
    begin
      MocLakeTime := TimeList.Items[TimeIndex] as TMocLakeTime;
      AColor := clAqua;
      SeriesIndex := -1;
      for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
      begin
        MocLakeData := MocLakeTime.LakeList[LakeIndex] as TMocLakeData;
        for SoluteIndex := 0 to MocLakeData.SoluteList.Count -1 do
        begin
          MocLakeSoluteData := MocLakeData.SoluteList[SoluteIndex]
            as TMocLakeSoluteData;
          AColor := GetColor(AColor);
          Inc(SeriesIndex);
          ASeries := SeriesList[SeriesIndex] as TLineSeries;
          case comboGraphType.ItemIndex of
            1: AValue := MocLakeSoluteData.Concentration;
            2: AValue := MocLakeSoluteData.PrecipitationMassIn;
            3: AValue := MocLakeSoluteData.StreamMassIn;
            4: AValue := MocLakeSoluteData.StreamMassOut;
            5: AValue := MocLakeSoluteData.NetWithdrawal;
            6: AValue := MocLakeSoluteData.RunoffMassIn;
            7: AValue := MocLakeSoluteData.GW_MassIn;
            8: AValue := MocLakeSoluteData.GW_MassOut;
            9: AValue := MocLakeSoluteData.SoluteMassInLake;
          else
            begin
              Assert(False);
              AValue :=  0;
            end;
          end;

          ASeries.AddXY(MocLakeTime.Time, AValue,'',clTeeColor);
        end;
      end;
    end;
  end;
end;

procedure TfrmLakePlot.CreateMocLakeVolumeTimeChart;
var
  TimeIndex : integer;
  ASeries : TLineSeries;
  LakeIndex : integer;
  AColor : TColor;
  MocLakeTime : TMocLakeTime;
  MocLakeData : TMocLakeData;
begin
  chrtLake.Legend.Visible := True;
  AColor := clAqua;
  if TimeList.Count > 0 then
  begin
    MocLakeTime := TimeList.Items[0] as TMocLakeTime;
    for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
    begin
      ASeries := TLineSeries.Create(self);
      ASeries.XValues.Order := loNone;
      SeriesList.Add(ASeries);
      ASeries.ParentChart := chrtLake;
      AColor := GetColor(AColor);
      ASeries.LinePen.Color := AColor;
      ASeries.AreaColor := AColor;
      ASeries.AreaLinesPen.Color := AColor;
      ASeries.SeriesColor := AColor;
      ASeries.Pointer.Visible := True;
      ASeries.Title := 'Lake ' + IntToStr(LakeIndex + 1);
    end;

    for TimeIndex := 0 to TimeList.Count -1 do
    begin
      MocLakeTime := TimeList.Items[TimeIndex] as TMocLakeTime;
      AColor := clAqua;
      for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
      begin
        MocLakeData := MocLakeTime.LakeList[LakeIndex] as TMocLakeData;
        AColor := GetColor(AColor);
        ASeries := SeriesList[LakeIndex] as TLineSeries;
        ASeries.AddXY(MocLakeTime.Time, MocLakeData.LakeVolume,'',clTeeColor);
      end;
    end;
  end;
end;

procedure TfrmLakePlot.CreateGageTimeChart;
var
  TimeIndex : integer;
  ASeries : TLineSeries;
//  LakeIndex : integer;
  AColor : TColor;
  GageTime : TGageLakeTime;
  GageData : TGageData;
begin
  chrtLake.Legend.Visible := False;
  AColor := clAqua;
  if TimeList.Count > 0 then
  begin
    ASeries := TLineSeries.Create(self);
    ASeries.XValues.Order := loNone;
    SeriesList.Add(ASeries);
    ASeries.ParentChart := chrtLake;
    AColor := GetColor(AColor);
    ASeries.LinePen.Color := AColor;
    ASeries.AreaColor := AColor;
    ASeries.AreaLinesPen.Color := AColor;
    ASeries.SeriesColor := AColor;
    ASeries.Pointer.Visible := True;
    ASeries.Title := 'Gage';

    for TimeIndex := 0 to TimeList.Count -1 do
    begin
      GageTime := TimeList.Items[TimeIndex] as TGageLakeTime;
//      AColor := clAqua;

      GageData := GageTime.ValueList[comboGraphType.ItemIndex] as TGageData;
      ASeries.AddXY(GageTime.Time, GageData.Value,'',clTeeColor);
    end;
  end;
end;


procedure TfrmLakePlot.WriteMocLakeVolumeTime(Lines : TStringList);
var
  TimeIndex : integer;
  LakeIndex : integer;
  MocLakeTime : TMocLakeTime;
  MocLakeData : TMocLakeData;
  AString : String;
begin
  AString := 'Time';
  if TimeList.Count > 0 then
  begin
    Lines.Add(comboGraphType.Text);
    MocLakeTime := TimeList.Items[0] as TMocLakeTime;
    for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
    begin
      AString := AString + #9 + 'Lake ' + IntToStr(LakeIndex + 1);
    end;
    Lines.Add(AString);

    for TimeIndex := 0 to TimeList.Count -1 do
    begin
      MocLakeTime := TimeList.Items[TimeIndex] as TMocLakeTime;
      AString := FloatToStr(MocLakeTime.Time);
      for LakeIndex := 0 to MocLakeTime.LakeList.Count -1 do
      begin
        MocLakeData := MocLakeTime.LakeList[LakeIndex] as TMocLakeData;
        AString := AString + #9 + FloatToStr(MocLakeData.LakeVolume);
      end;
      Lines.Add(AString);
    end;
  end;
end;

procedure TfrmLakePlot.WriteLakeSummaryLines(Lines : TStringList);
var
  TimeIndex : integer;
  LakesTimeObject : TLakesTime;
  AString : string;
begin
  Case comboGraphType.ItemIndex of
    3: Lines.Add('Time'#9'Connected Lakes');
    4: Lines.Add('Time'#9'Dry Lake Cells');
    5: Lines.Add('Time'#9'Total Lake Area');
    else Assert(False);
  end;
  Lines.Add(AString);

  for TimeIndex := 0 to TimeList.Count - 1 do
  begin
    LakesTimeObject := TimeList[TimeIndex] as TLakesTime;
    Case comboGraphType.ItemIndex of
      3: Lines.Add(FloatToStr(LakesTimeObject.Time)
        + #9 + IntToStr(LakesTimeObject.ConnectedLakes));
      4: Lines.Add(FloatToStr(LakesTimeObject.Time)
        + #9 + IntToStr(LakesTimeObject.DryLakeCells));
      5: Lines.Add(FloatToStr(LakesTimeObject.Time)
        + #9 + FloatToStr(LakesTimeObject.Area));
      else Assert(False);
    end;
  end;
end;

procedure TfrmLakePlot.CreateLakeSummaryChart;
var
  ASeries : TLineSeries;
  AColor : TColor;
  TimeIndex : integer;
  LakesTimeObject : TLakesTime;
  Y : double;
begin
  chrtLake.Legend.Visible := False;
  AColor := clBlack;
  ASeries := TLineSeries.Create(self);
  ASeries.XValues.Order := loNone;
  SeriesList.Add(ASeries);
  ASeries.ParentChart := chrtLake;
  ASeries.LinePen.Color := AColor;
  ASeries.AreaColor := AColor;
  ASeries.AreaLinesPen.Color := AColor;
  ASeries.SeriesColor := AColor;
  ASeries.Pointer.Visible := True;
  Case comboGraphType.ItemIndex of
    3: ASeries.Title := 'Connected Lakes';
    4: ASeries.Title := 'Dry Lake Cells';
    5: ASeries.Title := 'Total Lake Area';
    else Assert(False);
  end;

  for TimeIndex := 0 to TimeList.Count - 1 do
  begin
    LakesTimeObject := TimeList[TimeIndex] as TLakesTime;
    Case comboGraphType.ItemIndex of
      3: Y := LakesTimeObject.ConnectedLakes;
      4: Y := LakesTimeObject.DryLakeCells;
      5: Y := LakesTimeObject.Area;
    else
      begin
        Assert(False);
        Y :=  0;
      end;
    end;
    ASeries.AddXY(LakesTimeObject.Time, Y,'',clTeeColor);
  end;
end;

procedure TfrmLakePlot.WriteLakeLines(Lines : TStringList);
var
  LakeIndex : integer;
  TimeIndex : integer;
  LakesTimeObject : TLakesTime;
  LakeData : TLakeData;
  Y : double;
  AString : string;
begin
  case comboGraphType.ItemIndex of
    6 : AString := 'Stage';
    7 : AString := 'Precipitation';
    8 : AString := 'Evaporation';
    9 : AString := 'Runoff';
    10: AString := 'GW_Inflow';
    11: AString := 'GW_Outflow';
    12: AString := 'SW_Inflow';
    13: AString := 'SW_Outflow' ;
    14: AString := 'WaterUse' ;
    15: AString := 'ConnectedLakeInflow' ;
    16: AString := 'Volume' ;
    17: AString := 'SurfaceArea' ;
    18: AString := 'StageChange' ;
    19: AString := 'CumulativeStageChange' ;
    20: AString := 'VolumeChange' ;
    21: AString := 'PercentDiscrepancy' ;
  else Assert(False);
  end;
  Lines.Add(AString);
  AString := 'Time';
  if TimeList.Count > 0 then
  begin
    LakesTimeObject := TimeList[0] as TLakesTime;
    for LakeIndex := 0 to LakesTimeObject.LakeList.Count -1 do
    begin
      AString := AString + #9 + 'Lake ' + IntToStr(LakeIndex + 1);
    end;
    Lines.Add(AString);
  end;
  for TimeIndex := 0 to TimeList.Count - 1 do
  begin
    LakesTimeObject := TimeList[TimeIndex] as TLakesTime;
    AString := FloatToStr(LakesTimeObject.Time);
    for LakeIndex := 0 to LakesTimeObject.LakeList.Count -1 do
    begin
      LakeData := LakesTimeObject.LakeList.Items[LakeIndex] as TLakeData;
      case comboGraphType.ItemIndex of
        6 : Y := LakeData.Stage;
        7 : Y := LakeData.Precipitation;
        8 : Y := LakeData.Evaporation;
        9 : Y := LakeData.Runoff;
        10: Y := LakeData.GW_Inflow;
        11: Y := LakeData.GW_Outflow;
        12: Y := LakeData.SW_Inflow ;
        13: Y := LakeData.SW_Outflow ;
        14: Y := LakeData.WaterUse ;
        15: Y := LakeData.ConnectedLakeInflow ;
        16: Y := LakeData.Volume ;
        17: Y := LakeData.SurfaceArea ;
        18: Y := LakeData.StageChange ;
        19: Y := LakeData.CumulativeStageChange ;
        20: Y := LakeData.VolumeChange ;
        21: Y := LakeData.PercentDiscrepancy ;
      else
        begin
          Assert(False);
          Y :=  0;
        end;
      end;
      AString := AString + #9 + FloatToStr(Y);
    end;
    Lines.Add(AString);
  end;
end;

procedure TfrmLakePlot.CreateLakeChart;
var
  ASeries : TLineSeries;
  LakeIndex : integer;
  AColor : TColor;
  TimeIndex : integer;
  LakesTimeObject : TLakesTime;
  LakeData : TLakeData;
  Y : double;
  PreviousTime : double;
begin
  PreviousTime := 0;
  chrtLake.Legend.Visible := True;
  AColor := clAqua;
  if TimeList.Count > 0 then
  begin
    LakesTimeObject := TimeList[0] as TLakesTime;
    for LakeIndex := 0 to LakesTimeObject.LakeList.Count -1 do
    begin
      ASeries := TLineSeries.Create(self);
      ASeries.XValues.Order := loNone;
      SeriesList.Add(ASeries);
      ASeries.ParentChart := chrtLake;
      AColor := GetColor(AColor);
      ASeries.LinePen.Color := AColor;
      ASeries.AreaColor := AColor;
      ASeries.AreaLinesPen.Color := AColor;
      ASeries.SeriesColor := AColor;
      ASeries.Pointer.Visible := True;
      ASeries.Title := 'Lake ' + IntToStr(LakeIndex + 1);
    end;
  end;
  for TimeIndex := 0 to TimeList.Count - 1 do
  begin
    AColor := clAqua;
    LakesTimeObject := TimeList[TimeIndex] as TLakesTime;
    for LakeIndex := 0 to LakesTimeObject.LakeList.Count -1 do
    begin
      AColor := GetColor(AColor);
      LakeData := LakesTimeObject.LakeList.Items[LakeIndex] as TLakeData;
      ASeries := SeriesList[LakeIndex] as TLineSeries;
      case comboGraphType.ItemIndex of
        6 : Y := LakeData.Stage;
        7 : Y := LakeData.Precipitation;
        8 : Y := LakeData.Evaporation;
        9 : Y := LakeData.Runoff;
        10: Y := LakeData.GW_Inflow;
        11: Y := LakeData.GW_Outflow;
        12: Y := LakeData.SW_Inflow ;
        13: Y := LakeData.SW_Outflow ;
        14: Y := LakeData.WaterUse ;
        15: Y := LakeData.ConnectedLakeInflow ;
        16: Y := LakeData.Volume ;
        17: Y := LakeData.SurfaceArea ;
        18: Y := LakeData.StageChange ;
        19: Y := LakeData.CumulativeStageChange ;
        20: Y := LakeData.VolumeChange ;
        21: Y := LakeData.PercentDiscrepancy ;
      else
        begin
          Assert(False);
          Y :=  0;
        end;
      end;
      if (comboGraphType.ItemIndex > 4) and
        (comboGraphType.ItemIndex < 14) then
      begin
        Y := Y/(LakesTimeObject.Time-PreviousTime)
      end;
      ASeries.AddXY(LakesTimeObject.Time, Y,'',clTeeColor);
    end;
    PreviousTime := LakesTimeObject.Time;
  end;
end;



procedure TfrmLakePlot.FreeSeries;
begin
  chrtLake.RemoveAllSeries;
  SeriesList.Clear;
  chrtLake.Title.Text.Clear;
  chrtLake.LeftAxis.Title.Caption := '';
  chrtLake.BottomAxis.Title.Caption := '';
  chrtLake.Title.Text.Add(ExtractFileName(OpenDialog1.FileName));

end;

function TfrmLakePlot.GetColor(AColor : TColor) : TColor;
begin
  case AColor of
    clAqua     : Result := clBlack   ;
    clBlack    : Result := clBlue    ;
    clBlue     : Result := clDkGray  ;
    clDkGray   : Result := clFuchsia ;
    clFuchsia  : Result := clGreen    ;
    clGreen    : Result := clLime    ;
    clLime     : Result := clMaroon  ;
    clMaroon   : Result := clNavy    ;
    clNavy     : Result := clOlive   ;
    clOlive    : Result := clPurple  ;
    clPurple   : Result := clRed     ;
    clRed      : Result := clTeal  ;
    clTeal     : Result := clYellow   ;
    clYellow   : Result := clAqua    ;
    else
      result := clAqua;
  end;

end;

procedure TfrmLakePlot.comboGraphTypeChange(Sender: TObject);
begin
  FreeSeries;
  SetAxesTitles;
  InitializeChart;
  if comboDataSource.ItemIndex = 0 then
  begin
    case comboGraphType.ItemIndex of
      0,1,2: CreateStageVolumeChart;
      3,4,5: CreateLakeSummaryChart;
      6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21: CreateLakeChart;
    else Assert(False);
    end;
  end
  else if comboDataSource.ItemIndex = 1 then
  begin
    case comboGraphType.ItemIndex of
      0: CreateMocLakeVolumeTimeChart;
      1,2,3,4,5,6,7,8,9: CreateMocLakeSoluteChart;
    else Assert(False);
    end;
  end
  else
  begin
    CreateGageTimeChart;
  end;
  HideLegend;
end;

procedure TfrmLakePlot.SetAxesTitles;
begin
  if comboDataSource.ItemIndex = 0 then
  begin
    If comboGraphType.ItemIndex = 0 then
    begin
      chrtLake.BottomAxis.Title.Caption := UpperCase('Volume');
    end
    else if comboGraphType.ItemIndex in [1,2] then
    begin
      chrtLake.BottomAxis.Title.Caption := UpperCase('Area');
    end
    else
    begin
      chrtLake.BottomAxis.Title.Caption := UpperCase('Time');
    end;

    case comboGraphType.ItemIndex of
      0,1 : chrtLake.LeftAxis.Title.Caption := UpperCase('Stage');
      2: chrtLake.LeftAxis.Title.Caption := UpperCase('Volume');
      3 : chrtLake.LeftAxis.Title.Caption := UpperCase('Connected Lakes');
      4 : chrtLake.LeftAxis.Title.Caption := UpperCase('Dry Lake Cells');
      5 : chrtLake.LeftAxis.Title.Caption := UpperCase('Total Lake Area');
      6 : chrtLake.LeftAxis.Title.Caption := UpperCase('Stage');
      7 : chrtLake.LeftAxis.Title.Caption := UpperCase('Precipitation Rate');
      8 : chrtLake.LeftAxis.Title.Caption := UpperCase('Evaporation Rate');
      9 : chrtLake.LeftAxis.Title.Caption := UpperCase('Runoff Rate');
      10: chrtLake.LeftAxis.Title.Caption := UpperCase('Ground-Water Inflow Rate');
      11: chrtLake.LeftAxis.Title.Caption := UpperCase('Ground-Water Outflow Rate');
      12: chrtLake.LeftAxis.Title.Caption := UpperCase('Surface-Water Inflow Rate');
      13: chrtLake.LeftAxis.Title.Caption := UpperCase('Surface-Water Outflow Rate');
      14: chrtLake.LeftAxis.Title.Caption := UpperCase('Water Use Rate');
      15: chrtLake.LeftAxis.Title.Caption := UpperCase('Connected Lake Influx Rate');
      16: chrtLake.LeftAxis.Title.Caption := UpperCase('Volume');
      17: chrtLake.LeftAxis.Title.Caption := UpperCase('Surface Area');
      18: chrtLake.LeftAxis.Title.Caption := UpperCase('Stage Change');
      19: chrtLake.LeftAxis.Title.Caption := UpperCase('Cumulative Stage Change');
      20: chrtLake.LeftAxis.Title.Caption := UpperCase('Volume Change');
      21: chrtLake.LeftAxis.Title.Caption := UpperCase('Percent Discrepancy');
    else ;
      Assert(False);
    end;
  end
  else if comboDataSource.ItemIndex = 1 then
  begin
    chrtLake.BottomAxis.Title.Caption := UpperCase('Time');
    chrtLake.LeftAxis.Title.Caption := UpperCase(comboGraphType.Text);
{    case comboGraphType.ItemIndex of
      0 : chrtLake.LeftAxis.Title.Caption := 'Stage';
      1 : chrtLake.LeftAxis.Title.Caption := 'Connected Lakes';
      2 : chrtLake.LeftAxis.Title.Caption := 'Dry Lake Cells';
      3 : chrtLake.LeftAxis.Title.Caption := 'Total Lake Area';
      4 : chrtLake.LeftAxis.Title.Caption := 'Stage';
      5 : chrtLake.LeftAxis.Title.Caption := 'Precipitation';
      6 : chrtLake.LeftAxis.Title.Caption := 'Evaporation';
      7 : chrtLake.LeftAxis.Title.Caption := 'Runoff';
      8 : chrtLake.LeftAxis.Title.Caption := 'Groundwater Inflow';
      9 : chrtLake.LeftAxis.Title.Caption := 'Groundwater Outflow';
    else ;
      Assert(False);
    end;  }

  end
  else
  begin
    chrtLake.BottomAxis.Title.Caption := UpperCase('Time');
    chrtLake.LeftAxis.Title.Caption := UpperCase(Trim(Copy(comboGraphType.Text,1,Pos('vs',comboGraphType.Text)-1)));
{    case comboGraphType.ItemIndex of
      0 : chrtLake.LeftAxis.Title.Caption := 'Stage';
      1 : chrtLake.LeftAxis.Title.Caption := 'Connected Lakes';
      2 : chrtLake.LeftAxis.Title.Caption := 'Dry Lake Cells';
      3 : chrtLake.LeftAxis.Title.Caption := 'Total Lake Area';
      4 : chrtLake.LeftAxis.Title.Caption := 'Stage';
      5 : chrtLake.LeftAxis.Title.Caption := 'Precipitation';
      6 : chrtLake.LeftAxis.Title.Caption := 'Evaporation';
      7 : chrtLake.LeftAxis.Title.Caption := 'Runoff';
      8 : chrtLake.LeftAxis.Title.Caption := 'Groundwater Inflow';
      9 : chrtLake.LeftAxis.Title.Caption := 'Groundwater Outflow';
    else ;
      Assert(False);
    end;  }

  end;

end;

procedure TfrmLakePlot.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmLakePlot.sbImageClick(Sender: TObject);
var
  OldFilter : string;
  OldDefaultExtension : string;
  OldFilterIndex : integer;
begin
  OldFilter := SaveDialog1.Filter;
  OldDefaultExtension := SaveDialog1.DefaultExt;
  OldFilterIndex := SaveDialog1.FilterIndex;
  try
    SaveDialog1.Filter := 'Bitmap (*.bmp)|*.bmp|Enhanced Windows Metafile (*.emf)|*.emf|Windows Metafile (*.wmf)|*.wmf';
    SaveDialog1.DefaultExt := 'wmf';
    SaveDialog1.FilterIndex := 3;
    if SaveDialog1.Execute then
    begin
      if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.bmp' then
      begin
        chrtLake.SaveToBitmapFile(SaveDialog1.FileName);
      end
      else if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.emf' then
      begin
        chrtLake.SaveToMetafileEnh(SaveDialog1.FileName);
      end
      else if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.wmf' then
      begin
        chrtLake.SaveToMetafile(SaveDialog1.FileName);
      end;
    end;
  finally
    SaveDialog1.Filter := OldFilter;
    SaveDialog1.DefaultExt := OldDefaultExtension;
    SaveDialog1.FilterIndex := OldFilterIndex;
  end;

end;

procedure TfrmLakePlot.sbFormatClick(Sender: TObject);
begin
  frmFormat.GetData(chrtLake, 20);
  frmFormat.ShowModal;
end;

procedure TfrmLakePlot.About1Click(Sender: TObject);
begin
  frmAbout.showmodal;
end;

procedure TfrmLakePlot.sbSaveDataClick(Sender: TObject);
var
  Lines : TStringList;
begin
  If SaveDialog1.Execute then
  begin
    try
      Screen.Cursor := crHourglass;
      Lines := TStringList.Create;
      try
        if comboDataSource.ItemIndex = 0 then
        begin
          case comboGraphType.ItemIndex of
            0,1,2: WriteStageVolumeLines(Lines);
            3,4,5: WriteLakeSummaryLines(Lines);
            6..21: WriteLakeLines(Lines);
          else Assert(False);
          end;
        end
        else
        begin
          case comboGraphType.ItemIndex of
            0: WriteMocLakeVolumeTime(Lines);
            1,2,3,4,5,6,7,8,9: WriteMocLakeSolute(Lines);
          else Assert(False);
          end;
        end;
        Lines.SaveToFile(SaveDialog1.FileName);
      finally
        Lines.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;

  end;
end;

function TfrmLakePlot.GetNextLine(SearchTerm1, SearchTerm2: string;
  Start: integer; Listing: TStringList): integer;
var
  Index : integer;
begin
  result := -1;
  for Index := Start to Listing.Count -1 do
  begin
    if (Pos(SearchTerm1, Listing.Strings[Index])> 0)
      or (Pos(SearchTerm2, Listing.Strings[Index])> 0) then
    begin
      result := Index;
      break;
    end;
  end;
end;


procedure TfrmLakePlot.sbPrintClick(Sender: TObject);
var
  Colors : array of TColor;
  Index : integer;
  ASeries : TChartSeries;
  PrintInBW : boolean;
begin
  if PrintDialog1.Execute then
  begin
    PrintInBW := False;
    if SeriesList.Count > 0 then
    begin
      If MessageDlg('Do you wish to print in Black and White?', mtInformation,
        [mbYes, mbNo], 0) = mrYes then
        begin
          PrintInBW := True;
          SetLength(Colors, SeriesList.Count);
          for Index := 0 to SeriesList.Count -1 do
          begin
            ASeries := SeriesList[Index] as TLineSeries;
            Colors[Index] := ASeries.SeriesColor;
            ASeries.SeriesColor := clBlack;
          end;
        end;
    end;
    chrtLake.print;
    if PrintInBW then
    begin
      for Index := 0 to SeriesList.Count -1 do
      begin
        ASeries := SeriesList[Index] as TLineSeries;
        ASeries.SeriesColor := Colors[Index];
      end;
    end;
  end;
end;

procedure TfrmLakePlot.comboDataSourceChange(Sender: TObject);
begin
  FreeSeries;
  if comboDataSource.ItemIndex =0 then
  begin
    comboGraphType.Items.Assign(StrSetModflowStrings.Strings);
    UpdateDropDownWidth;
    OpenDialog1.Filter := 'Modflow Listing files (*.lst)|*.lst|All Files|*.*'
  end
  else if comboDataSource.ItemIndex =1 then
  begin
    comboGraphType.Items.Assign(StrSetMoc3dStrings.Strings);
    UpdateDropDownWidth;
    OpenDialog1.Filter := 'GWT or MOC3D Listing files (*.out)|*.out|All Files|*.*'
  end
  else
  begin
    comboGraphType.Items.Clear;
    comboGraphType.CWX := 0;
    OpenDialog1.Filter := 'Gage files (*.ggo; *.lakg*; *.sfrg*)|*.ggo;*.lakg*;*.sfrg*|All Files|*.*'
  end;
  if comboGraphType.Items.Count > 0 then
  begin
    comboGraphType.ItemIndex := 0;
  end;
end;

{ TMocLakeTime }

constructor TMocLakeTime.Create;
begin
  inherited;
  LakeList := TObjectList.Create;
end;

destructor TMocLakeTime.Destroy;
begin
  LakeList.Free;
  inherited;
end;

{ TMocLakeData }

constructor TMocLakeData.Create;
begin
  inherited;
  SoluteList := TObjectList.Create;
end;

destructor TMocLakeData.Destroy;
begin
  SoluteList.Free;
  inherited;
end;

{ TMocLakeSoluteData }

procedure TMocLakeSoluteData.Read(AString: string);
begin
  Concentration := InternationalStrToFloat(GetNCharacters(AString,11));
  PrecipitationMassIn := InternationalStrToFloat(GetNCharacters(AString,10));
  StreamMassIn := InternationalStrToFloat(GetNCharacters(AString,10));
  StreamMassOut := InternationalStrToFloat(GetNCharacters(AString,10));
  NetWithdrawal := InternationalStrToFloat(GetNCharacters(AString,10));
  RunoffMassIn := InternationalStrToFloat(GetNCharacters(AString,10));
  GW_MassIn := InternationalStrToFloat(GetNCharacters(AString,10));
  GW_MassOut := InternationalStrToFloat(GetNCharacters(AString,11));
  SoluteMassInLake := InternationalStrToFloat(GetNCharacters(AString,10));
end;

{ TGageLakeTime }

constructor TGageLakeTime.Create;
begin
  ValueList := TObjectList.Create;
end;

destructor TGageLakeTime.Destroy;
begin
  ValueList.Free;
end;

procedure TfrmLakePlot.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Visible then
  begin
    frmModChart.Close
  end;
end;

procedure TfrmLakePlot.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(frmModChart.mainMenuFormChoice);

end;

procedure TfrmLakePlot.chrtLakeAfterDraw(Sender: TObject);
var
  TempFont : TFont;
begin
  if ExplanationVisible and chrtLake.Legend.Visible then
  begin
    TempFont := TFont.Create;
    try
      TempFont.Assign(chrtLake.Canvas.Font);
      try
        chrtLake.Canvas.Font.Assign(chrtLake.Legend.Font);
        chrtLake.Canvas.TextOut(ExplanationLeft,ExplanationTop,
          'EXPLANATION');
      finally
        chrtLake.Canvas.Font.Assign(TempFont);
      end;
    finally
      TempFont.Free;
    end;
  end;
end;

procedure TfrmLakePlot.chrtLakeGetLegendPos(Sender: TCustomChart;
  Index: Integer; var X, Y, XColor: Integer);
var
  ExplanationHeight : integer;
  TempFont : TFont;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(chrtLake.Canvas.Font);
    try
      chrtLake.Canvas.Font.Assign(chrtLake.Legend.Font);
      ExplanationHeight := chrtLake.Canvas.TextHeight('EXPLANATION');
    finally
      chrtLake.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  Y := Y + ExplanationHeight;
end;

procedure TfrmLakePlot.chrtLakeGetLegendRect(Sender: TCustomChart;
  var Rect: TRect);
var
  ExplanationWidth : integer;
  ExplanationHeight : integer;
  TempFont : TFont;
begin
  LegendRect := Rect;
  ExplanationTop := Rect.Top + 5;
  ExplanationLeft := Rect.Left + 5;
  TempFont := TFont.Create;
  try
    TempFont.Assign(chrtLake.Canvas.Font);
    try
      chrtLake.Canvas.Font.Assign(chrtLake.Legend.Font);
      ExplanationWidth := chrtLake.Canvas.TextWidth('_EXPLANATION_');
      ExplanationHeight := chrtLake.Canvas.TextHeight('EXPLANATION');
    finally
      chrtLake.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  if Rect.Right - Rect.Left < ExplanationWidth then
  begin
    Rect.Right := Rect.Left + ExplanationWidth;
  end;
  Rect.Bottom := Rect.Bottom + ExplanationHeight;
end;

procedure TfrmLakePlot.Help2Click(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmLakePlot.UpdateDropDownWidth;
var
  Index : integer;
  NewWidth, TestWidth : integer;
begin
  NewWidth := -1;
  for Index := 0 to comboGraphType.Items.Count -1 do
  begin
    TestWidth := Canvas.TextWidth(comboGraphType.Items[Index]) + 30;
    if TestWidth > NewWidth then
    begin
      NewWidth := TestWidth;
    end;
  end;
  if NewWidth > comboGraphType.Width then
  begin
    comboGraphType.CWX := NewWidth - comboGraphType.Width
  end
  else
  begin
    comboGraphType.CWX := 0;
  end;
end;

procedure TfrmLakePlot.sbAdvancedFormatClick(Sender: TObject);
begin
  inherited;
  mHHelp.ChmFile := ChartHelpFileName;
  try
    ChartEditor1.Execute;
  finally
    mHHelp.ChmFile := HelpFileName;
  end;
end;

procedure TfrmLakePlot.PrintPreview1Click(Sender: TObject);
begin
  inherited;
  ChartPreviewer1.Execute;
end;

end.
