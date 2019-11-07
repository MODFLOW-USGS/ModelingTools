
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

Unit OfficeHintDsgn;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, OfficeTypes, OfficeUtils, OfficeHint, ComCtrls,
  ImgList, Grids, {$IFDEF OVCL_D6} DesignEditors, DesignIntf {$ELSE} DsgnIntf {$ENDIF};

type

{ TfrmHintShape }

  TfrmHintShape = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    Preview: TPaintBox;
    Bevel2: TBevel;
    Images: TImageList;
    Shapes: TDrawGrid;
    PointsData: TMemo;
    procedure ShapesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure ShapesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FShape: TksoHintShape;
  end;

{ TksoHintShapeProperty }

  TksoHintShapeProperty = class(TPropertyEditor)
  private
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

var
  frmHintShape: TfrmHintShape;

implementation {===============================================================}

{$R *.DFM}

function GetToken(var S: string; Sep: String): string;
var
  P: byte;
begin
  Result := S;
  P := Pos(Sep, S);
  if P > 0 then
  begin
    SetLength(Result, P-1);
    Delete(S, 1, P);
  end
  else
    S := '';
  Trim(Result);
  Trim(S);
end;

{ TksoHintShapeProperty }

procedure TksoHintShapeProperty.Edit;
begin
  { Execute editor }
  frmHintShape := TfrmHintShape.Create(Application);
  try
    if frmHintShape.ShowModal = mrOk then
    begin
      TksoOfficeHint(GetComponent(0)).Shape := frmHintShape.FShape;
      Modified;
    end;
  finally
    frmHintShape.Free;
  end;
end;

function TksoHintShapeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog,paMultiSelect];
end;

function TksoHintShapeProperty.GetValue: string;
begin
  Result := '(Shape)';
end;

{ TfrmHintShape }

procedure TfrmHintShape.FormCreate(Sender: TObject);
var
  B: boolean;
begin
  FShape := TksoHintShape.Create;
  ShapesSelectCell(Self, 0, 0, B);
end;

procedure TfrmHintShape.FormDestroy(Sender: TObject);
begin
  FShape.Free;
end;

procedure TfrmHintShape.ShapesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  i: integer;
begin
  i := ARow*Shapes.ColCount + ACol;
  if (i >= 0) and (i < Images.Count) then
  with Rect do
  begin
    Shapes.Canvas.FillRect(Rect);
    Images.Draw(Shapes.Canvas, left + (right - left - Images.Width) div 2,
                top + (bottom - top - Images.Height) div 2, i, true);
  end;  
end;

procedure TfrmHintShape.ShapesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  i: integer;
  P: TksoShapePoint;
  S: string;
begin
  i := ARow*Shapes.ColCount + ACol;
  if (i >= 0) and (i < Images.Count) then
  begin
    S := PointsData.Lines[i];
    GetToken(S, ' ');
    { Extract points from string }
    FShape.Clear;
    try
      P.X := StrToInt(GetToken(S, ','));
      P.Y := StrToInt(GetToken(S, ','));
      FShape.TopLeft := Point(P.X, P.Y);
      P.X := StrToInt(GetToken(S, ','));
      P.Y := StrToInt(GetToken(S, ':'));
      FShape.BottomRight := Point(P.X, P.Y);
    except
    end;
    while S <> '' do
    begin
      try
        P.X := StrToInt(GetToken(S, ','));
        P.Y := StrToInt(GetToken(S, ' '));
        FShape.Add(P);
      except
        FShape.Clear;
      end;
    end;
    Preview.Invalidate;
  end;
end;

procedure TfrmHintShape.PreviewPaint(Sender: TObject);
begin
  { Draw preview }
  with Preview, Preview.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clInfoBk;
    Pen.Style := psSolid;
    Pen.Color := clInfoText;
    FShape.Draw(Preview.Canvas, 5, 20, Width-10, Height-40);
  end;
end;

end.
