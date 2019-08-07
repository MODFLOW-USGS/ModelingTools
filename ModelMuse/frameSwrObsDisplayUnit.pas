unit frameSwrObsDisplayUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Mask,
  JvExMask, JvToolEdit, ExtCtrls, Grids, RbwDataGrid4,
  Generics.Collections, PhastModelUnit, ScreenObjectUnit,
  ModflowSwrObsUnit, SwrReachObjectUnit, ReadSwrOutputUnit, JvSpin, JvTimer;

type
  TReachObsDisplay = class;

  TReachObsDisplayList = TList<TReachObsDisplay>;
  TReachObsDisplayObjectList = TObjectList<TReachObsDisplay>;

  TObsLink = class(TObject)
    ObsName: string;
    ObsType: TSwrObsType;
    ConnectedReach: Integer;
  end;

  TReachObsDisplay = class(TObject)
  private
    FReach: Integer;
    FLength: double;
    FNeighbors: TReachObsDisplayList;
    FObsLinks: TReachObsLinks;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TModelDisplay= class(TObject)
  private
    FModel: TCustomModel;
    FReachDisplayList: TReachObsDisplayObjectList;
    procedure UpdateReaches;
  public
    constructor Create(AModel: TCustomModel);
    destructor Destroy; override;
  end;

  TframeSwrObsDisplay = class(TFrame)
    pnlTop: TPanel;
    rdgTimes: TRbwDataGrid4;
    pbPlot: TPaintBox;
    fedObservationFile: TJvFilenameEdit;
    spl1: TSplitter;
    comboObservationType: TComboBox;
    btnGetSelectedReaches: TButton;
    seIncrement: TJvSpinEdit;
    btnAnimate: TButton;
    jvtmrAnimate: TJvTimer;
    lblAnimationInterval: TLabel;
    btnStopAnimation: TButton;
    lblObservationFile: TLabel;
    procedure fedObservationFileChange(Sender: TObject);
    procedure rdgTimesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure pbPlotPaint(Sender: TObject);
    procedure comboObservationTypeChange(Sender: TObject);
    procedure btnGetSelectedReachesClick(Sender: TObject);
    procedure btnAnimateClick(Sender: TObject);
    procedure jvtmrAnimateTimer(Sender: TObject);
    procedure btnStopAnimationClick(Sender: TObject);
  private
    FSwrObsData: TObsDataList;
    FModelDisplay: TModelDisplay;
    FModelDisplayList: TObjectList<TModelDisplay>;
    FAnimating: Boolean;
    FMinMaxFound: Boolean;
    FMinY: double;
    FMaxY: double;
    FReachesUpdated: Boolean;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData;
    procedure SetData;
    property ReachesUpdated: Boolean read FReachesUpdated;
    { Public declarations }
  end;

implementation

uses
  ModflowSwrWriterUnit, frmProgressUnit, frmGoPhastUnit,
  Generics.Defaults, xygraph;

resourcestring
  StrTime = 'Time';
  StrStopAnimation = 'Stop animation';
  StrResumeAnimation = 'Resume Animation';
  StrNoObjectsThatDefi = 'No objects that define reaches selected';
  StrNoObservationFile = 'No observation file has been selected';
  StrTheSelectedObserva = 'The selected SWR observation file does not exist.';
  StrNoObservationsOfT = 'No observations of the selected type were found fo' +
  'r the selected reaches.';

{$R *.dfm}

{ TReachObsDisplay }

constructor TReachObsDisplay.Create;
begin
  FNeighbors := TReachObsDisplayList.Create;
  FObsLinks := TReachObsLinks.Create;
end;

destructor TReachObsDisplay.Destroy;
begin
  FObsLinks.Free;
  FNeighbors.Free;
  inherited;
end;

{ TModelDisplay }

constructor TModelDisplay.Create(AModel: TCustomModel);
begin
  FModel := AModel;
  FReachDisplayList := TReachObsDisplayObjectList.Create;
end;

destructor TModelDisplay.Destroy;
begin
  FReachDisplayList.Free;
  inherited;
end;

procedure TModelDisplay.UpdateReaches;
var
  SwrWriter: TModflowSwrWriter;
  AReach: TReachObject;
  SelectedReaches: TList<TReachObject>;
  ReachIndex: Integer;
  ReachDisplay: TReachObsDisplay;
  NeighborIndex: Integer;
  NeighborReach: TReachObject;
  NeighborPosition: Integer;
  ObsIndex: Integer;
  ALink: TReachObsLink;
  NewLink: TReachObsLink;
  SortList1: TReachObsDisplayList;
  SortList2: TReachObsDisplayList;
  AReachDisplayObj: TReachObsDisplay;
  ANeighbor: TReachObsDisplay;
  Index: Integer;
  InsertPosition: Integer;
begin
  SwrWriter := TModflowSwrWriter.Create(FModel, etDisplay);
  try
    frmProgressMM.ShouldContinue := True;
    SwrWriter.UpdateReachNumberDisplay;
    FReachDisplayList.Clear;
    SelectedReaches := TList<TReachObject>.Create;
    try
      for ReachIndex := 0 to SwrWriter.ReachCount - 1 do
      begin
        AReach :=SwrWriter.Reaches[ReachIndex];
        if (AReach.FReachData.ScreenObject as TScreenObject).Selected then
        begin
          SelectedReaches.Add(AReach);
          ReachDisplay := TReachObsDisplay.Create;
          FReachDisplayList.Add(ReachDisplay);
          ReachDisplay.FReach := AReach.FReachData.Reach;
          ReachDisplay.FLength := AReach.FReachData.ReachLength;
          ReachDisplay.FObsLinks.Capacity := AReach.ObsLinks.Count;
          for ObsIndex := 0 to AReach.ObsLinks.Count - 1 do
          begin
            ALink := AReach.ObsLinks[ObsIndex];
            NewLink := TReachObsLink.Create;
            NewLink.Assign(ALink);
            ReachDisplay.FObsLinks.Add(NewLink);
          end;
        end;
      end;

      for ReachIndex := 0 to SelectedReaches.Count - 1 do
      begin
        AReach := SelectedReaches[ReachIndex];
        ReachDisplay := FReachDisplayList[ReachIndex];
        for NeighborIndex := 0 to AReach.Neighbors.Count - 1 do
        begin
          NeighborReach := AReach.Neighbors[NeighborIndex];
          NeighborPosition := SelectedReaches.IndexOf(NeighborReach);
          if NeighborPosition >= 0 then
          begin
            ReachDisplay.FNeighbors.Add(FReachDisplayList[NeighborPosition]);
          end;
        end;
      end;
    finally
      SelectedReaches.Free;
    end;
  finally
    SwrWriter.Free;
  end;

  SortList1 := TReachObsDisplayList.Create;
  SortList2 := TReachObsDisplayList.Create;
  try
    SortList1.AddRange(FReachDisplayList.ToArray);
    while SortList1.Count > 0 do
    begin
      AReachDisplayObj := SortList1.First;
      InsertPosition := SortList2.Add(AReachDisplayObj);
      SortList1.Delete(0);
      repeat
        NeighborPosition := -1;
        for Index := 0 to AReachDisplayObj.FNeighbors.Count - 1 do
        begin
          ANeighbor := AReachDisplayObj.FNeighbors[Index];
          NeighborPosition := SortList1.IndexOf(ANeighbor);
          if NeighborPosition >= 0 then
          begin
            SortList2.Add(ANeighbor);
            SortList1.Delete(NeighborPosition);
            AReachDisplayObj := ANeighbor;
            break;
          end;
        end;
      until NeighborPosition < 0;
      AReachDisplayObj := SortList2.First;
      repeat
        NeighborPosition := -1;
        for Index := 0 to AReachDisplayObj.FNeighbors.Count - 1 do
        begin
          ANeighbor := AReachDisplayObj.FNeighbors[Index];
          NeighborPosition := SortList1.IndexOf(ANeighbor);
          if NeighborPosition >= 0 then
          begin
            SortList2.Insert(InsertPosition, ANeighbor);
            SortList1.Delete(NeighborPosition);
            AReachDisplayObj := ANeighbor;
            break;
          end;
        end;
      until NeighborPosition < 0;
    end;
    FReachDisplayList.Sort(TComparer<TReachObsDisplay>.Construct(
      function(const L, R: TReachObsDisplay): Integer
        begin
          result := SortList2.IndexOf(L) - SortList2.IndexOf(R);
        end));
  finally
    SortList2.Free;
    SortList1.Free;
  end;

end;

procedure TframeSwrObsDisplay.btnAnimateClick(Sender: TObject);
begin
  rdgTimes.Row := 1;
  FAnimating := True;
  jvtmrAnimate.Interval := Round(seIncrement.Value*1000);
  FMinMaxFound := False;
  btnStopAnimation.Enabled := True;
  btnStopAnimation.Caption := StrStopAnimation;
  pbPlot.Invalidate;
end;

procedure TframeSwrObsDisplay.btnGetSelectedReachesClick(Sender: TObject);
begin
  GetData;
end;

procedure TframeSwrObsDisplay.btnStopAnimationClick(Sender: TObject);
begin
  FAnimating := not FAnimating;
  if FAnimating then
  begin
    btnStopAnimation.Caption := StrStopAnimation;
    pbPlot.Invalidate;
  end
  else
  begin
    btnStopAnimation.Caption := StrResumeAnimation;
  end;

end;

procedure TframeSwrObsDisplay.comboObservationTypeChange(Sender: TObject);
begin
  pbPlot.Invalidate;
end;

constructor TframeSwrObsDisplay.Create(AOwner: TComponent);
begin
  inherited;
  FSwrObsData := TObsDataList.Create;
  FModelDisplayList := TObjectList<TModelDisplay>.Create;
end;

destructor TframeSwrObsDisplay.Destroy;
begin
  FModelDisplayList.Free;
  FSwrObsData.Free;
  inherited;
end;

procedure TframeSwrObsDisplay.fedObservationFileChange(Sender: TObject);
var
  Extension: string;
  FileType: TSwrFileType;
  TimeIndex: Integer;
begin
  if FileExists(fedObservationFile.FileName) then
  begin
    Extension := ExtractFileExt(fedObservationFile.FileName);
    if AnsiSameText(Extension, StrSwrObsExt_A) then
    begin
      FileType := srtAscii;
    end
    else
    begin
      Assert(AnsiSameText(Extension, StrSwrObsExt_B));
      FileType := srtBinary;
    end;
    ReadSwrObsData(fedObservationFile.FileName, FileType, FSwrObsData);
    rdgTimes.BeginUpdate;
    try
      rdgTimes.RowCount := FSwrObsData.Count+1;
      for TimeIndex := 0 to FSwrObsData.Count - 1 do
      begin
        rdgTimes.RealValue[0,TimeIndex+1] := FSwrObsData[TimeIndex].Time;
      end;
    finally
      rdgTimes.EndUpdate;
    end;
  end
  else
  begin
    FSwrObsData.Clear;
  end;
  pbPlot.Invalidate;
end;

procedure TframeSwrObsDisplay.GetData;
var
  ModelDisplay: TModelDisplay;
begin
  FModelDisplay := nil;
  rdgTimes.Cells[0,0] := StrTime;
  FModelDisplayList.Clear;
  ModelDisplay := TModelDisplay.Create(frmGoPhast.PhastModel);
  FModelDisplayList.Add(ModelDisplay);
  ModelDisplay.UpdateReaches;
  FModelDisplay := ModelDisplay;
  pbPlot.Invalidate;
  FReachesUpdated := True;
end;

procedure TframeSwrObsDisplay.jvtmrAnimateTimer(Sender: TObject);
begin
  if FAnimating and (rdgTimes.Row < rdgTimes.RowCount -1) then
  begin
    rdgTimes.Row := rdgTimes.Row + 1;
    jvtmrAnimate.Enabled := False;
    pbPlot.Invalidate;
  end
  else
  begin
    FAnimating := False;
    btnStopAnimation.Enabled := (rdgTimes.Row < rdgTimes.RowCount -1);
    jvtmrAnimate.Enabled := False;
  end;
end;

procedure TframeSwrObsDisplay.pbPlotPaint(Sender: TObject);
var
  ReachIndex: Integer;
  XValues: TList<Double>;
  YValues: TList<Double>;
  AReach: TReachObsDisplay;
  ObsIndex: Integer;
  ObsLink: TReachObsLink;
  ObsType: TSwrObsType;
  Values: TObsData;
  PriorX: double;
  X: Double;
  Y: double;
  ObsPosition: Integer;
  First: boolean;
  Data: Tdatatype;
  index: Integer;
  PriorReach: TReachObsDisplay;
  ShouldPlotLine: TList<Boolean>;
  PlotLine: boolean;
  TimeIndex: Integer;
  MinY: Double;
  MaxY: Double;
  TextSize: TSize;
begin
  if FModelDisplay = nil then
  begin
    Exit;
  end;
  if FModelDisplay.FReachDisplayList.Count = 0 then
  begin
    TextSize := pbPlot.Canvas.TextExtent(StrNoObjectsThatDefi);
    pbPlot.Canvas.TextOut((pbPlot.Width - TextSize.cx) div 2,
      (pbPlot.Height - TextSize.cy) div 2, StrNoObjectsThatDefi);
  end
  else if fedObservationFile.FileName = '' then
  begin
    TextSize := pbPlot.Canvas.TextExtent(StrNoObservationFile);
    pbPlot.Canvas.TextOut((pbPlot.Width - TextSize.cx) div 2,
      (pbPlot.Height - TextSize.cy) div 2, StrNoObservationFile);
  end
  else if not FileExists(fedObservationFile.FileName) then
  begin
    TextSize := pbPlot.Canvas.TextExtent(StrTheSelectedObserva);
    pbPlot.Canvas.TextOut((pbPlot.Width - TextSize.cx) div 2,
      (pbPlot.Height - TextSize.cy) div 2, StrTheSelectedObserva);
  end
  else if (comboObservationType.ItemIndex >= 0) and (rdgTimes.Row >= 1)
    and (rdgTimes.Row-1 < FSwrObsData.Count) then
  begin
    ObsType := TSwrObsType(comboObservationType.ItemIndex);
    if not FMinMaxFound and FAnimating  then
    begin
      First := True;
      for ReachIndex := 0 to FModelDisplay.FReachDisplayList.Count - 1 do
      begin
        AReach := FModelDisplay.FReachDisplayList[ReachIndex];
        for ObsIndex := 0 to AReach.FObsLinks.Count -1 do
        begin
          ObsLink := AReach.FObsLinks[ObsIndex];
          if ObsType = ObsLink.ObsType then
          begin
            ObsPosition := FSwrObsData.IndexOfName(ObsLink.ObsName);
            if ObsPosition >= 0 then
            begin
              for TimeIndex := 0 to FSwrObsData.Count - 1 do
              begin
                Values := FSwrObsData[TimeIndex];
                Y := Values[ObsPosition];
                if First then
                begin
                  First := False;
                  FMinY := Y;
                  FMaxY := Y;
                end
                else
                begin
                  if Y < FMinY then
                  begin
                    FMinY := Y;
                  end
                  else if Y > FMaxY then
                  begin
                    FMaxY := Y;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      FMinMaxFound := True;
    end;
    if FAnimating then
    begin
      MinY := FMinY;
      MaxY := FMaxY;
    end
    else
    begin
      MinY := 0;
      MaxY := 0;
    end;
    XValues := TList<Double>.Create;
    YValues := TList<Double>.Create;
    ShouldPlotLine := TList<Boolean>.Create;
    try
      Values := FSwrObsData[rdgTimes.Row-1];
      PriorX := 0;
      First := True;
      PriorReach := nil;
      for ReachIndex := 0 to FModelDisplay.FReachDisplayList.Count - 1 do
      begin
        AReach := FModelDisplay.FReachDisplayList[ReachIndex];
        X := PriorX + AReach.FLength / 2;
        for ObsIndex := 0 to AReach.FObsLinks.Count -1 do
        begin
          ObsLink := AReach.FObsLinks[ObsIndex];
          if ObsType = ObsLink.ObsType then
          begin
            ObsPosition := FSwrObsData.IndexOfName(ObsLink.ObsName);
            if ObsPosition >= 0 then
            begin
              XValues.Add(X);
              Y := Values[ObsPosition];
              YValues.Add(Y);
              PlotLine := (PriorReach <> nil)
                and (PriorReach.FNeighbors.IndexOf(AReach) >= 0);
              ShouldPlotLine.Add(PlotLine);
              if not FAnimating then
              begin
                if First then
                begin
                  First := False;
                  MinY := Y;
                  MaxY := Y;
                end
                else
                begin
                  if Y < MinY then
                  begin
                    MinY := Y;
                  end
                  else if Y > MaxY then
                  begin
                    MaxY := Y;
                  end;
                end;
              end;
            end;
          end;
        end;
        PriorReach := AReach;
        PriorX := PriorX + AReach.FLength;
      end;

      if XValues.Count > 0 then
      begin
        xysetdataarray(Data, XValues.Count, 1);
        try
          xycleargraph(pbPlot,clWhite,clBlack,1);

          xystartgraph(0, 100, 0, 100, 50, 50, 50, 50, clipoff);

          xyxaxis(clBlack,0,PriorX,
            PriorX/10,0,'X',1,False,False,True, 2);

          xyyaxis(clBlack,MinY,MaxY,
            (MaxY-MinY)/10,0,comboObservationType.Text,5,False,False,True, 2);

          for index := 0 to XValues.Count - 1 do
          begin
            Data[index+1, 0] := XValues[index];
            Data[index+1, 1] := YValues[index];
          end;

          xysymbol(1,0,0);
          xyplotarray(data,2,1);

          for Index := 1 to XValues.Count - 1 do
          begin
            if ShouldPlotLine[index] then
            begin
              xymove(XValues[Index-1], YValues[Index-1]);
              xyDraw(XValues[Index], YValues[Index]);
            end;
          end;

          xyfinish;
        except on E: exception do
          begin
            TextSize := pbPlot.Canvas.TextExtent(e.message);
            pbPlot.Canvas.TextOut((pbPlot.Width - TextSize.cx) div 2,
              (pbPlot.Height - TextSize.cy) div 2, e.message);
            Exit;
          end;
        end;
      end
      else
      begin
        TextSize := pbPlot.Canvas.TextExtent(StrNoObservationsOfT);
        pbPlot.Canvas.TextOut((pbPlot.Width - TextSize.cx) div 2,
          (pbPlot.Height - TextSize.cy) div 2, StrNoObservationsOfT);
      end;
    finally
      ShouldPlotLine.Free;
      YValues.Free;
      XValues.Free;
    end;
    if FAnimating then
    begin
      jvtmrAnimate.Enabled := True;
    end;
  end;
end;

procedure TframeSwrObsDisplay.rdgTimesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ARow -1 < FSwrObsData.Count then
  begin
    pbPlot.Invalidate;
  end;
end;

procedure TframeSwrObsDisplay.SetData;
begin

end;

end.
