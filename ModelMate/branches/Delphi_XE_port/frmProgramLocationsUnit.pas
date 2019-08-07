unit frmProgramLocationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Mask, JvExMask,
  JvToolEdit, JvExControls, JvLinkLabel, JvExStdCtrls, JvHtControls,
  GlobalBasicData, GlobalTypesUnit, GlobalData, Utilities;

type
  TFormProgramLocations = class(TForm)
    fedUcode: TJvFilenameEdit;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    lblModflow: TLabel;
    JvHTLabel1: TJvHTLabel;
    fedModflow2005: TJvFilenameEdit;
    fedModflow2000: TJvFilenameEdit;
    Label1: TLabel;
    Label2: TLabel;
    JvHTLabel2: TJvHTLabel;
    JvHTLabel3: TJvHTLabel;
    fedGWChart: TJvFilenameEdit;
    JvHTLabel4: TJvHTLabel;
    Label3: TLabel;
    Label4: TLabel;
    JvHTLabel5: TJvHTLabel;
    fedResidAnalysis: TJvFilenameEdit;
    fedResidAnalysisAdv: TJvFilenameEdit;
    Label5: TLabel;
    JvHTLabel6: TJvHTLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure fedUcodeChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure fedModflow2005Change(Sender: TObject);
    procedure fedModflow2000Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fedGWChartChange(Sender: TObject);
    procedure fedResidAnalysisChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fedResidAnalysisAdvChange(Sender: TObject);
  private
    { Private declarations }
    UcodeLocation: TFileName;
    Modflow2005Location: TFileName;
    Modflow2000Location: TFileName;
    GWChartLocation: TFileName;
    ResidAnalysisLocation: TFileName;
    ResidAnalysisAdvLocation: TFileName;
    Procedure GetData;
    procedure SetData;
    procedure HighlightControls;
    procedure HighlightControl(FedTemp: TJvFilenameEdit; var OK: Boolean);
  public
    { Public declarations }
    LocalChange: boolean;
  end;

var
  FormProgramLocations: TFormProgramLocations;

implementation

{$R *.dfm}

{ TFormProgramLocations }

procedure TFormProgramLocations.btnHelpClick(Sender: TObject);
begin
  { TODO 3 -cHelp : Invoke help }
end;

procedure TFormProgramLocations.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  if LocalChange then
    begin
      GlobalProgramLocations.Ucode2005Location := UcodeLocation;
      GlobalProgramLocations.Modflow2005Location := Modflow2005Location;
      GlobalProgramLocations.Modflow2000Location := Modflow2000Location;
      GlobalProgramLocations.GWChartLocation := GWChartLocation;
      GlobalProgramLocations.ResidAnalysisLocation := ResidAnalysisLocation;
      GlobalProgramLocations.ResidAnalysisAdvLocation := ResidAnalysisAdvLocation;
//      ProjChanged := True;
    end;
end;

procedure TFormProgramLocations.fedGWChartChange(Sender: TObject);
var
  OK: boolean;
begin
  inherited;
  HighlightControl(fedGWChart,OK);
end;

procedure TFormProgramLocations.fedModflow2000Change(Sender: TObject);
var
  OK: boolean;
begin
  inherited;
  HighlightControl(fedModflow2000, OK);
end;

procedure TFormProgramLocations.fedModflow2005Change(Sender: TObject);
var
  OK: boolean;
begin
  inherited;
  HighlightControl(fedModflow2005, OK);
end;

procedure TFormProgramLocations.fedResidAnalysisAdvChange(Sender: TObject);
var
  OK: boolean;
begin
  inherited;
  HighlightControl(fedResidAnalysisAdv, OK);
end;

procedure TFormProgramLocations.fedResidAnalysisChange(Sender: TObject);
var
  OK: boolean;
begin
  inherited;
  HighlightControl(fedResidAnalysis, OK);
end;

procedure TFormProgramLocations.fedUcodeChange(Sender: TObject);
var
  OK: boolean;
begin
  inherited;
  HighlightControl(fedUcode, OK);
end;

procedure TFormProgramLocations.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormProgramLocations.FormShow(Sender: TObject);
begin
  UcodeLocation := GlobalProgramLocations.Ucode2005Location;
  Modflow2005Location := GlobalProgramLocations.Modflow2005Location;
  Modflow2000Location := GlobalProgramLocations.Modflow2000Location;
  GWChartLocation := GlobalProgramLocations.GWChartLocation;
  ResidAnalysisLocation := GlobalProgramLocations.ResidAnalysisLocation;
  ResidAnalysisAdvLocation := GlobalProgramLocations.ResidAnalysisAdvLocation;
  LocalChange := False;
  GetData;
  HighlightControls;
end;

procedure TFormProgramLocations.GetData;
begin
  fedUcode.FileName := GlobalProgramLocations.Ucode2005Location;
  fedModflow2005.FileName := GlobalProgramLocations.Modflow2005Location;
  fedModflow2000.FileName := GlobalProgramLocations.Modflow2000Location;
  fedGWChart.FileName := GlobalProgramLocations.GWChartLocation;
  fedResidAnalysis.FileName := GlobalProgramLocations.ResidAnalysisLocation;
  fedResidAnalysisAdv.FileName := GlobalProgramLocations.ResidAnalysisAdvLocation;
  //
  UcodeLocation := GlobalProgramLocations.Ucode2005Location;
  Modflow2005Location := GlobalProgramLocations.Modflow2005Location;
  Modflow2000Location := GlobalProgramLocations.Modflow2000Location;
  GWChartLocation := GlobalProgramLocations.GWChartLocation;
  ResidAnalysisLocation :=GlobalProgramLocations.ResidAnalysisLocation;
  ResidAnalysisAdvLocation := GlobalProgramLocations.ResidAnalysisAdvLocation;
end;

procedure TFormProgramLocations.SetData;
begin
  if fedUcode.FileName <> UcodeLocation then
    begin
      UcodeLocation := fedUcode.FileName;
      LocalChange := True;
    end;
  if fedModflow2005.FileName <> Modflow2005Location then
    begin
      Modflow2005Location := fedModflow2005.FileName;
      LocalChange := True;
    end;
  if fedModflow2000.FileName <> Modflow2000Location then
    begin
      Modflow2000Location := fedModflow2000.FileName;
      LocalChange := True;
    end;
  if fedGWChart.FileName <> GWChartLocation then
    begin
      GWChartLocation := fedGWChart.FileName;
      LocalChange := True;
    end;
  if fedResidAnalysis.FileName <> ResidAnalysisLocation then
    begin
      ResidAnalysisLocation := fedResidAnalysis.FileName;
      LocalChange := True;
    end;
  if fedResidAnalysisAdv.FileName <> ResidAnalysisAdvLocation then
    begin
      ResidAnalysisAdvLocation := fedResidAnalysisAdv.FileName;
      LocalChange := True;
    end;
end;

procedure TFormProgramLocations.HighlightControls;
var
  OK: Boolean;
  I: integer;
  FedTemp: TJvFilenameEdit;
begin
  OK := True;
  for I := 1 to 6 do
    begin
      case I of
        // pointer assignments
        1: FedTemp := fedUcode;
        2: FedTemp := fedModflow2005;
        3: FedTemp := fedModflow2000;
        4: FedTemp := fedGWChart;
        5: FedTemp := fedResidAnalysis;
        6: FedTemp := fedResidAnalysisAdv;
      else
        FedTemp := fedUcode;
      end;
      HighlightControl(FedTemp, OK);
    end;
end;

procedure TFormProgramLocations.HighlightControl(FedTemp: TJvFilenameEdit; var OK: Boolean);
var
  FileDoesExist: Boolean;
begin
  FileDoesExist := FileExists(FedTemp.FileName);
  if FileDoesExist then
    begin
      fedTemp.Color := clWindow;
      OK := False;
    end
  else
    begin
      fedTemp.Color := clRed;
      OK := False;
    end;
end;

end.
