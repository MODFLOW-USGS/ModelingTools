unit frameStreamLinkUnit;

interface

uses
  {$IF CompilerVersion >= 23} System.UITypes, {$IFEND} Windows, Messages,
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, PhastModelUnit, UndoItems, DisplaySettingsUnit, Mask,
  JvExMask, JvSpin;

type
  TStreamType = (stSFR, stSTR, stSFR_MF6);

  TframeStreamLink = class(TFrame)
    shpStreamColor: TShape;
    dlgLinkColor: TColorDialog;
    btnStreamColor: TButton;
    shpDiversionColor: TShape;
    btnDiversionColor: TButton;
    rgItemsToPlot: TRadioGroup;
    cbStreams: TCheckBox;
    cbPlotDiversions: TCheckBox;
    lblTimeToPlot: TLabel;
    comboTimeToPlot: TJvComboBox;
    cbPlotUnconnected: TCheckBox;
    btnUnconnectedColor: TButton;
    shpUnconnectedColor: TShape;
    seSquareSize: TJvSpinEdit;
    lblSquareSize: TLabel;
    cbBadConnection: TCheckBox;
    btnBadConnection: TButton;
    shpBadConnection: TShape;
    procedure btnStreamColorClick(Sender: TObject);
    procedure btnDiversionColorClick(Sender: TObject);
    procedure cbStreamsClick(Sender: TObject);
    procedure cbPlotDiversionsClick(Sender: TObject);
    procedure btnUnconnectedColorClick(Sender: TObject);
    procedure cbPlotUnconnectedClick(Sender: TObject);
    procedure cbBadConnectionClick(Sender: TObject);
    procedure btnBadConnectionClick(Sender: TObject);
  private
    FSfrStreamLinkPlot: TSfrStreamLinkPlot;
    FStreamType: TStreamType;
    procedure SetShapeColor(AShape: TShape);
    { Private declarations }
  public
    destructor Destroy; override;
    procedure GetData(StreamType: TStreamType);
    procedure SetData;
    { Public declarations }
  end;

  TUndoStreamLinks = class(TCustomUndo)
  private
    FOldSfrStreamLinkPlot: TSfrStreamLinkPlot;
    FNewSfrStreamLinkPlot: TSfrStreamLinkPlot;
    FStreamType: TStreamType;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var SfrStreamLinkPlot: TSfrStreamLinkPlot;
      StreamType: TStreamType);
    destructor Destroy; override;
  end;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrDisplayStreamLinks = 'display SFR stream links';
  StrDisplaySTRStreamLinks = 'display STR stream links';

{$R *.dfm}

procedure TframeStreamLink.btnBadConnectionClick(Sender: TObject);
begin
  SetShapeColor(shpBadConnection);
end;

procedure TframeStreamLink.btnDiversionColorClick(Sender: TObject);
begin
  SetShapeColor(shpDiversionColor);
end;

procedure TframeStreamLink.btnStreamColorClick(Sender: TObject);
begin
  SetShapeColor(shpStreamColor);
end;

procedure TframeStreamLink.btnUnconnectedColorClick(Sender: TObject);
begin
  SetShapeColor(shpUnconnectedColor);
end;

procedure TframeStreamLink.cbBadConnectionClick(Sender: TObject);
begin
  btnBadConnection.Enabled := cbBadConnection.Checked;
  shpBadConnection.Enabled := cbBadConnection.Checked;
end;

procedure TframeStreamLink.cbPlotDiversionsClick(Sender: TObject);
begin
  btnDiversionColor.Enabled := cbPlotDiversions.Checked;
  shpDiversionColor.Enabled := cbPlotDiversions.Checked;
end;

procedure TframeStreamLink.cbPlotUnconnectedClick(Sender: TObject);
begin
  btnUnconnectedColor.Enabled := cbPlotUnconnected.Checked;
  shpUnconnectedColor.Enabled := cbPlotUnconnected.Checked;
end;

procedure TframeStreamLink.cbStreamsClick(Sender: TObject);
begin
  btnStreamColor.Enabled := cbStreams.Checked;
  shpStreamColor.Enabled := cbStreams.Checked;
end;

destructor TframeStreamLink.Destroy;
begin
  FSfrStreamLinkPlot.Free;
  inherited;
end;

procedure TframeStreamLink.GetData(StreamType: TStreamType);
var
  EndTime: double;
begin
  Handle;
  FStreamType := StreamType;
  FSfrStreamLinkPlot.Free;
  FSfrStreamLinkPlot:= TSfrStreamLinkPlot.Create(nil);
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillStringsWithStartTimes(comboTimeToPlot.Items);
  EndTime := frmGoPhast.PhastModel.ModflowStressPeriods[
    frmGoPhast.PhastModel.ModflowStressPeriods.Count-1].EndTime;
  comboTimeToPlot.Items.Add(FloatToStr(EndTime));

  case FStreamType of
    stSFR: FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrStreamLinkPlot);
    stSTR: FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.StrStreamLinkPlot);
    stSFR_MF6: FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrMf6StreamLinkPlot);
  end;

  cbStreams.Checked := FSfrStreamLinkPlot.PlotStreamConnections;
  cbPlotDiversions.Checked := FSfrStreamLinkPlot.PlotDiversions;
  cbPlotUnconnected.Checked := FSfrStreamLinkPlot.PlotUnconnected;
  cbBadConnection.Checked := FSfrStreamLinkPlot.PlotBadConnection;
  shpStreamColor.Brush.Color := FSfrStreamLinkPlot.StreamColor;
  shpDiversionColor.Brush.Color := FSfrStreamLinkPlot.DiversionColor;
  shpUnconnectedColor.Brush.Color := FSfrStreamLinkPlot.UnconnectedColor;
  shpBadConnection.Brush.Color := FSfrStreamLinkPlot.BadConnectionColor;
  rgItemsToPlot.ItemIndex := Ord(FSfrStreamLinkPlot.StreamsToPlot);
  comboTimeToPlot.Text := FloatToStr(FSfrStreamLinkPlot.TimeToPlot);
  seSquareSize.AsInteger := FSfrStreamLinkPlot.SquareSize;
end;

procedure TframeStreamLink.SetData;
var
  Undo: TUndoStreamLinks;
begin
  if FSfrStreamLinkPlot = nil then
  begin
    FSfrStreamLinkPlot:= TSfrStreamLinkPlot.Create(nil);
    case FStreamType of
      stSFR: FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrStreamLinkPlot);
      stSTR: FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.StrStreamLinkPlot);
      stSFR_MF6: FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrMf6StreamLinkPlot);
    end;
  end;
  FSfrStreamLinkPlot.PlotStreamConnections := cbStreams.Checked;
  FSfrStreamLinkPlot.PlotDiversions := cbPlotDiversions.Checked;
  FSfrStreamLinkPlot.PlotUnconnected := cbPlotUnconnected.Checked;
  FSfrStreamLinkPlot.PlotBadConnection := cbBadConnection.Checked;
  FSfrStreamLinkPlot.StreamColor := shpStreamColor.Brush.Color;
  FSfrStreamLinkPlot.DiversionColor := shpDiversionColor.Brush.Color;
  FSfrStreamLinkPlot.UnconnectedColor := shpUnconnectedColor.Brush.Color;
  FSfrStreamLinkPlot.BadConnectionColor := shpBadConnection.Brush.Color;
  FSfrStreamLinkPlot.StreamsToPlot := TStreamsToPlot(rgItemsToPlot.ItemIndex);
  FSfrStreamLinkPlot.TimeToPlot := StrToFloat(comboTimeToPlot.Text);
  FSfrStreamLinkPlot.SquareSize := seSquareSize.AsInteger;
  Undo := TUndoStreamLinks.Create(FSfrStreamLinkPlot, FStreamType);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TframeStreamLink.SetShapeColor(AShape: TShape);
begin
  dlgLinkColor.Color := AShape.Brush.Color;
  if dlgLinkColor.Execute then
  begin
    AShape.Brush.Color := dlgLinkColor.Color;
  end;
end;

{ TUndoStreamLinks }

constructor TUndoStreamLinks.Create(var SfrStreamLinkPlot: TSfrStreamLinkPlot;
  StreamType: TStreamType);
begin
  inherited Create;
  FStreamType := StreamType;
  FNewSfrStreamLinkPlot := SfrStreamLinkPlot;
  SfrStreamLinkPlot := nil;
  FOldSfrStreamLinkPlot := TSfrStreamLinkPlot.Create(nil);
  case FStreamType of
    stSFR: FOldSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrStreamLinkPlot);
    stSTR: FOldSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.StrStreamLinkPlot);
    stSFR_MF6: FOldSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrMf6StreamLinkPlot);
  end;

end;

function TUndoStreamLinks.Description: string;
begin
  case FStreamType of
    stSFR, stSFR_MF6: result := StrDisplayStreamLinks;
    stSTR: result := StrDisplaySTRStreamLinks;
  end;

end;

destructor TUndoStreamLinks.Destroy;
begin
  FNewSfrStreamLinkPlot.Free;
  FOldSfrStreamLinkPlot.Free;
  inherited;
end;

procedure TUndoStreamLinks.DoCommand;
begin
  inherited;
  case FStreamType of
    stSFR: frmGoPhast.PhastModel.SfrStreamLinkPlot := FNewSfrStreamLinkPlot;
    stSTR: frmGoPhast.PhastModel.StrStreamLinkPlot := FNewSfrStreamLinkPlot;
    stSFR_MF6: frmGoPhast.PhastModel.SfrMf6StreamLinkPlot := FNewSfrStreamLinkPlot;
  end;
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
end;

procedure TUndoStreamLinks.Undo;
begin
  inherited;
  case FStreamType of
    stSFR: frmGoPhast.PhastModel.SfrStreamLinkPlot := FOldSfrStreamLinkPlot;
    stSTR: frmGoPhast.PhastModel.StrStreamLinkPlot := FOldSfrStreamLinkPlot;
    stSFR_MF6: frmGoPhast.PhastModel.SfrMf6StreamLinkPlot := FOldSfrStreamLinkPlot;
  end;
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
end;

end.
