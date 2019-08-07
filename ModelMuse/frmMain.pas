unit frmMain;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QExtCtrls, QMenus, QTypes, QActnList, QStdActns,
  QImgList, QRbwRuler, Replacement;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlTopLeft: TPanel;
    pnlTopRight: TPanel;
    pnlLowerLeft: TPanel;
    pnlLowerRight: TPanel;
    splitHoriz: TSplitter;
    splitVertTop: TSplitter;
    splitVertBottom: TSplitter;
    ControlBar1: TControlBar;
    ToolBar3: TToolBar;
    ToolButton2: TToolButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Options1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    About1: TMenuItem;
    Exit1: TMenuItem;
    ActionList1: TActionList;
    ImageList1: TImageList;
    SelectAll1: TMenuItem;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    EditCut: TAction;
    EditPaste: TAction;
    EditSelectAll: TAction;
    EditCopy: TAction;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    FileNew: TAction;
    FileSave: TAction;
    FileSaveAs: TAction;
    FileOpen: TAction;
    FilePrint: TAction;
    FileExit: TAction;
    HelpContents1: THelpContents;
    HelpTopicSearch1: THelpTopicSearch;
    About2: TMenuItem;
    Navigate1: TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    Pan1: TMenuItem;
    MoveTo1: TMenuItem;
    Zoom1: TMenuItem;
    StatusBar1: TStatusBar;
    rulerTopNS: TRbwRuler;
    rulerSideHoriz: TRbwRuler;
    rulerFrontVertical: TRbwRuler;
    rulTopEW: TRbwRuler;
    rulerSideVertical: TRbwRuler;
    rulerFrontHoriz: TRbwRuler;
    ToolBar2: TToolBar;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tbZoomOut: TToolButton;
    tbZoomIn: TToolButton;
    tbPan: TToolButton;
    zbTop: TrbwZoomBox;
    zbSide: TrbwZoomBox;
    zbFront: TrbwZoomBox;
    procedure splitVertTopMoved(Sender: TObject);
    procedure splitVertBottomMoved(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure splitHorizMoved(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure zbTopPaintBoxMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure zbFrontPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure zbSidePaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure zbTopPaintBoxPaint(Sender: TObject);
    procedure zbSidePaintBoxPaint(Sender: TObject);
    procedure zbFrontPaintBoxPaint(Sender: TObject);
    procedure zbTopHorzScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbSideHorzScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbFrontHorzScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbTopVertScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbSideVertScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbFrontVertScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure zbTopPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure zbTopPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbPanClick(Sender: TObject);
    procedure tbZoomInClick(Sender: TObject);
    procedure tbZoomOutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure zbSidePaintBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure zbFrontPaintBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure zbFrontPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure zbSidePaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure AdjustHorizontalScales;
    procedure AdjustVerticalScales;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Qt;

const crHandFlat = 1;     { arbitrary positive number }
      crZoomIn = 2;
      crZoomOut = 3;

{$R *.dfm}
{$R CustomCursor.res} 

procedure TForm1.splitVertTopMoved(Sender: TObject);
begin
  pnlLowerRight.Width := pnlTopRight.Width;
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.splitVertBottomMoved(Sender: TObject);
begin
  pnlTopRight.Width := pnlLowerRight.Width;
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.AdjustHorizontalScales;
begin

  rulTopEW.RulerEnds.Lower := rulerTopNS.Width + 10;
  rulTopEW.RulerEnds.Upper := rulTopEW.Width-30;
  rulTopEW.RulerValues.Lower := zbTop.X(10 + zbTop.HorzScrollBar.Position);
  rulTopEW.RulerValues.Upper := zbTop.X(zbTop.HorzScrollBar.Width -12 + zbTop.HorzScrollBar.Position);

  rulerFrontHoriz.RulerEnds.Lower := rulTopEW.RulerEnds.Lower;
  rulerFrontHoriz.RulerEnds.Upper := rulTopEW.RulerEnds.Upper;
  rulerFrontHoriz.RulerValues.Lower := zbFront.X(10 + zbFront.HorzScrollBar.Position);
  rulerFrontHoriz.RulerValues.Upper := zbFront.X(zbFront.HorzScrollBar.Width -12 + zbFront.HorzScrollBar.Position);

  rulerSideVertical.RulerEnds.Upper := pnlTopRight.Width - rulerSideHoriz.Width -30;
  rulerSideVertical.RulerValues.Upper := zbSide.X(10 + zbSide.HorzScrollBar.Position);
  rulerSideVertical.RulerValues.Lower := zbSide.X(zbSide.HorzScrollBar.Width -12 + zbSide.HorzScrollBar.Position);


end;

procedure TForm1.FileNewExecute(Sender: TObject);
begin
//
end;

procedure TForm1.EditPasteExecute(Sender: TObject);
begin
 //
end;

procedure TForm1.AdjustVerticalScales;
var
  NewLower, NewUpper : double;
begin

  rulerTopNS.RulerEnds.Upper := pnlTopLeft.Height - rulTopEW.Height -30;
  NewLower := zbTop.Y(zbTop.VertScrollBar.Height -10 + zbTop.VertScrollBar.Position);
  NewUpper := zbTop.Y(12 + zbTop.VertScrollBar.Position);
  if NewLower < NewUpper then
  begin
    rulerTopNS.RulerValues.Lower := NewLower;
    rulerTopNS.RulerValues.Upper := NewUpper;
  end;

  rulerSideHoriz.RulerEnds.Upper := rulerTopNS.RulerEnds.Upper;
  NewLower := zbSide.Y(zbSide.VertScrollBar.Height -10 + zbSide.VertScrollBar.Position);
  NewUpper := zbSide.Y(12 + zbSide.VertScrollBar.Position);
  if NewLower < NewUpper then
  begin
    rulerSideHoriz.RulerValues.Lower := NewLower;
    rulerSideHoriz.RulerValues.Upper := NewUpper;
  end;

  rulerFrontVertical.RulerEnds.Upper := pnlLowerLeft.Height - rulerFrontHoriz.Height -30;
  NewLower := zbFront.Y(zbFront.VertScrollBar.Height -10 + zbFront.VertScrollBar.Position);
  NewUpper := zbFront.Y(12 + zbFront.VertScrollBar.Position);
  if NewLower < NewUpper then
  begin
    rulerFrontVertical.RulerValues.Lower := NewLower;
    rulerFrontVertical.RulerValues.Upper := NewUpper;
  end;
end;

procedure TForm1.splitHorizMoved(Sender: TObject);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  QBitmap : QBitmapH;
  QCursor : QCursorH;
begin
  with TBitmap.Create do
  begin
    try
      LoadFromResourceName(HInstance, 'CRHANDFLAT');
      QBitmap := QBitmap_create;
      try
        QBitmap_from_QPixmap(QBitmap, Handle);
        QCursor := QCursor_create(QBitmap, QBitmap, 24, 20);
      finally
        QBitmap_destroy(QBitmap);
      end;

    finally
      Free;
    end;
  end;
  Screen.Cursors[crHandFlat] := QCursor;

  with TBitmap.Create do
  begin
    try
      LoadFromResourceName(HInstance, 'CRZOOMIN');
      QBitmap := QBitmap_create;
      try
        QBitmap_from_QPixmap(QBitmap, Handle);
        QCursor := QCursor_create(QBitmap, QBitmap, 11, 11);
      finally
        QBitmap_destroy(QBitmap);
      end;

    finally
      Free;
    end;
  end;
  Screen.Cursors[crZoomIn] := QCursor;

  with TBitmap.Create do
  begin
    try
      LoadFromResourceName(HInstance, 'CRZOOMOUT');
      QBitmap := QBitmap_create;
      try
        QBitmap_from_QPixmap(QBitmap, Handle);
        QCursor := QCursor_create(QBitmap, QBitmap, 11, 11);
      finally
        QBitmap_destroy(QBitmap);
      end;

    finally
      Free;
    end;
  end;
  Screen.Cursors[crZoomOut] := QCursor;


  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbTopPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StatusBar1.Panels[0].Text := FloatToStr(zbTop.X(X)) + '; ' + FloatToStr(zbTop.Y(Y));
end;

procedure TForm1.zbFrontPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StatusBar1.Panels[0].Text := FloatToStr(zbFront.X(X)) + '; ' + FloatToStr(zbFront.Y(Y));
end;

procedure TForm1.zbSidePaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StatusBar1.Panels[0].Text := FloatToStr(zbSide.X(X)) + '; ' + FloatToStr(zbSide.Y(Y));
end;

procedure TForm1.zbTopPaintBoxPaint(Sender: TObject);
begin
  With zbTop.PaintBox do
  begin
    With Canvas do
    begin
      MoveTo(1,1);
      LineTo(Width-1,1);
      LineTo(Width-1,Height-1);
      LineTo(1,Height-1);
      LineTo(1,1);
      LineTo(Width-1,Height-1);
    end;
  end;
end;

procedure TForm1.zbSidePaintBoxPaint(Sender: TObject);
begin
  With zbSide.PaintBox do
  begin
    With Canvas do
    begin
      MoveTo(1,1);
      LineTo(Width-1,1);
      LineTo(Width-1,Height-1);
      LineTo(1,Height-1);
      LineTo(1,1);
      LineTo(Width-1,Height-1);
    end;
  end;
end;

procedure TForm1.zbFrontPaintBoxPaint(Sender: TObject);
begin
  With zbFront.PaintBox do
  begin
    With Canvas do
    begin
      MoveTo(1,1);
      LineTo(Width-1,1);
      LineTo(Width-1,Height-1);
      LineTo(1,Height-1);
      LineTo(1,1);
      LineTo(Width-1,Height-1);
    end;
  end;

end;

procedure TForm1.zbTopHorzScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbSideHorzScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbFrontHorzScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbTopVertScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbSideVertScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbFrontVertScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  AdjustHorizontalScales;
  AdjustVerticalScales;
end;

procedure TForm1.zbTopPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if tbZoomIn.Down then
  begin
    zbTop.BeginZoom(X, Y);
  end
  else if tbPan.Down then
  begin
    zbTop.Panning := True;
  end;

end;

procedure TForm1.zbTopPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if tbZoomOut.Down then
  begin
    zbTop.ZoomByAt(0.5, X, Y);
  end
  else if tbPan.Down then
  begin
    zbTop.Panning := False;
  end;

end;

procedure TForm1.tbPanClick(Sender: TObject);
begin
  if tbPan.Down then
  begin
    zbTop.PaintBox.Cursor := crHandFlat;
    zbFront.PaintBox.Cursor := crHandFlat;
    zbSide.PaintBox.Cursor := crHandFlat;
  end
  else
  begin
    zbTop.PaintBox.Cursor := crDefault;
    zbFront.PaintBox.Cursor := crDefault;
    zbSide.PaintBox.Cursor := crDefault;
  end;


end;

procedure TForm1.tbZoomInClick(Sender: TObject);
begin
  if tbZoomIn.Down then
  begin
    zbTop.PaintBox.Cursor := crZoomIn;
    zbFront.PaintBox.Cursor := crZoomIn;
    zbSide.PaintBox.Cursor := crZoomIn;
  end
  else
  begin
    zbTop.PaintBox.Cursor := crDefault;
    zbFront.PaintBox.Cursor := crDefault;
    zbSide.PaintBox.Cursor := crDefault;
  end;
end;

procedure TForm1.tbZoomOutClick(Sender: TObject);
begin
  if tbZoomOut.Down then
  begin
    zbTop.PaintBox.Cursor := crZoomOut;
    zbFront.PaintBox.Cursor := crZoomOut;
    zbSide.PaintBox.Cursor := crZoomOut;
  end
  else
  begin
    zbTop.PaintBox.Cursor := crDefault;
    zbFront.PaintBox.Cursor := crDefault;
    zbSide.PaintBox.Cursor := crDefault;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  QCursor_destroy(Screen.Cursors[crHandFlat]);
  QCursor_destroy(Screen.Cursors[crZoomIn]);
  QCursor_destroy(Screen.Cursors[crZoomOut]);

end;

procedure TForm1.zbSidePaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if tbZoomIn.Down then
  begin
    zbSide.BeginZoom(X, Y);
  end
  else if tbPan.Down then
  begin
    zbSide.Panning := True;
  end;
end;

procedure TForm1.zbFrontPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if tbZoomIn.Down then
  begin
    zbFront.BeginZoom(X, Y);
  end
  else if tbPan.Down then
  begin
    zbFront.Panning := True;
  end;
end;

procedure TForm1.zbFrontPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if tbZoomOut.Down then
  begin
    zbFront.ZoomByAt(0.5, X, Y);
  end
  else if tbPan.Down then
  begin
    zbFront.Panning := False;
  end;
end;

procedure TForm1.zbSidePaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if tbZoomOut.Down then
  begin
    zbSide.ZoomByAt(0.5, X, Y);
  end
  else if tbPan.Down then
  begin
    zbSide.Panning := False;
  end;
end;

end.
