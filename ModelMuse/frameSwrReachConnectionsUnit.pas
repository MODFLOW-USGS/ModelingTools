unit frameSwrReachConnectionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  DisplaySettingsUnit, UndoItems
  {$IF CompilerVersion >= 23}, System.UITypes {$IFEND};

type
  TframeSwrReachConnections = class(TFrame)
    cbReaches: TCheckBox;
    btnReachColor: TButton;
    shpReachColor: TShape;
    cbPlotUnconnected: TCheckBox;
    btnUnconnectedColor: TButton;
    shpUnconnectedColor: TShape;
    rgItemsToPlot: TRadioGroup;
    dlgLinkColor: TColorDialog;
    cbPlotStructures: TCheckBox;
    btnStructureColor: TButton;
    shpStructureColor: TShape;
    procedure btnReachColorClick(Sender: TObject);
    procedure btnUnconnectedColorClick(Sender: TObject);
    procedure cbReachesClick(Sender: TObject);
    procedure cbPlotUnconnectedClick(Sender: TObject);
    procedure btnStructureColorClick(Sender: TObject);
    procedure cbPlotStructuresClick(Sender: TObject);
  private
    procedure SetShapeColor(AShape: TShape);
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    { Public declarations }
  end;

  TUndoSwrReachConnections = class(TCustomUndo)
  private
    FOldSwrReachConnectionsPlot: TSwrReachConnectionsPlot;
    FNewSwrReachConnectionsPlot: TSwrReachConnectionsPlot;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var SwrReachConnectionsPlot: TSwrReachConnectionsPlot);
    destructor Destroy; override;
  end;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrDisplaySWRReachCo = 'display SWR reach connections';

{$R *.dfm}

{ TframeSwrReachConnections }

procedure TframeSwrReachConnections.btnStructureColorClick(Sender: TObject);
begin
  SetShapeColor(shpStructureColor);
end;

procedure TframeSwrReachConnections.btnUnconnectedColorClick(Sender: TObject);
begin
  SetShapeColor(shpUnconnectedColor);
end;

procedure TframeSwrReachConnections.cbPlotStructuresClick(Sender: TObject);
begin
  btnStructureColor.Enabled := cbPlotStructures.Checked;
  shpStructureColor.Enabled := cbPlotStructures.Checked;
end;

procedure TframeSwrReachConnections.cbPlotUnconnectedClick(Sender: TObject);
begin
  btnUnconnectedColor.Enabled := cbPlotUnconnected.Checked;
  shpUnconnectedColor.Enabled := cbPlotUnconnected.Checked;
end;

procedure TframeSwrReachConnections.cbReachesClick(Sender: TObject);
begin
  btnReachColor.Enabled := cbReaches.Checked;
  shpReachColor.Enabled := cbReaches.Checked;
end;

procedure TframeSwrReachConnections.btnReachColorClick(Sender: TObject);
begin
  SetShapeColor(shpReachColor);
end;

procedure TframeSwrReachConnections.GetData;
var
  SwrReachConnectionsPlot: TSwrReachConnectionsPlot;
begin
  SwrReachConnectionsPlot := frmGoPhast.PhastModel.SwrReachConnectionsPlot;
  cbReaches.Checked := SwrReachConnectionsPlot.PlotReachConnections;
  cbPlotUnconnected.Checked := SwrReachConnectionsPlot.PlotUnconnected;
  cbPlotStructures.Checked := SwrReachConnectionsPlot.PlotStructures;
  shpReachColor.Brush.Color := SwrReachConnectionsPlot.ConnectedColor;
  shpUnconnectedColor.Brush.Color := SwrReachConnectionsPlot.UnconnectedColor;
  shpStructureColor.Brush.Color := SwrReachConnectionsPlot.StructureColor;
  rgItemsToPlot.ItemIndex := Ord(SwrReachConnectionsPlot.ReachesToPlot);
end;

procedure TframeSwrReachConnections.SetData;
var
  Undo: TUndoSwrReachConnections;
  SwrReachConnectionsPlot: TSwrReachConnectionsPlot;
begin
  SwrReachConnectionsPlot := TSwrReachConnectionsPlot.Create(nil);
  try
    SwrReachConnectionsPlot.Assign(frmGoPhast.PhastModel.SwrReachConnectionsPlot);
    SwrReachConnectionsPlot.PlotReachConnections := cbReaches.Checked;
    SwrReachConnectionsPlot.PlotUnconnected := cbPlotUnconnected.Checked;
    SwrReachConnectionsPlot.PlotStructures := cbPlotStructures.Checked;
    SwrReachConnectionsPlot.ConnectedColor := shpReachColor.Brush.Color;
    SwrReachConnectionsPlot.UnconnectedColor := shpUnconnectedColor.Brush.Color;
    SwrReachConnectionsPlot.StructureColor := shpStructureColor.Brush.Color;
    SwrReachConnectionsPlot.ReachesToPlot := TStreamsToPlot(rgItemsToPlot.ItemIndex);

    Undo := TUndoSwrReachConnections.Create(SwrReachConnectionsPlot);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    SwrReachConnectionsPlot.Free;
  end;
end;

procedure TframeSwrReachConnections.SetShapeColor(AShape: TShape);
begin
  dlgLinkColor.Color := AShape.Brush.Color;
  if dlgLinkColor.Execute then
  begin
    AShape.Brush.Color := dlgLinkColor.Color;
  end;
end;

{ TUndoSwrReachConnections }

constructor TUndoSwrReachConnections.Create(
  var SwrReachConnectionsPlot: TSwrReachConnectionsPlot);
begin
  inherited Create;
  FNewSwrReachConnectionsPlot := SwrReachConnectionsPlot;
  SwrReachConnectionsPlot := nil;
  FOldSwrReachConnectionsPlot := TSwrReachConnectionsPlot.Create(nil);
  FOldSwrReachConnectionsPlot.Assign(frmGoPhast.PhastModel.SwrReachConnectionsPlot);
end;

function TUndoSwrReachConnections.Description: string;
begin
  result := StrDisplaySWRReachCo
end;

destructor TUndoSwrReachConnections.Destroy;
begin
  FNewSwrReachConnectionsPlot.Free;
  FOldSwrReachConnectionsPlot.Free;
  inherited;
end;

procedure TUndoSwrReachConnections.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SwrReachConnectionsPlot := FNewSwrReachConnectionsPlot;
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;

end;

procedure TUndoSwrReachConnections.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SwrReachConnectionsPlot := FOldSwrReachConnectionsPlot;
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
end;

end.
