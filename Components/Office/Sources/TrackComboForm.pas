
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit TrackComboForm;

interface

{$I OFFICEVER.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AxCtrls, StdCtrls, BaseCombo, BaseComboForm, ComCtrls;

type

  TksoTrackBar = class(TTrackBar)
  private
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
  end;

  TfrmTrackCombo = class(TfrmCustomCombo)
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    TrackBar: TksoTrackBar;
    procedure TrackBarChange(Sender: TObject);
  public
    { Public declarations }
    procedure AfterSetCombo; override;
  end;

var
  frmTrackCombo: TfrmTrackCombo;

implementation {===============================================================}

uses TrackCombo;

{$R *.DFM}

procedure TfrmTrackCombo.AfterSetCombo;
begin
  with (Combo as TksoTrackComboBox) do
  begin
    TrackBar.Frequency := Frequency;
    TrackBar.LineSize := LineSize;
    TrackBar.Max := Max;
    TrackBar.Min := Min;
    TrackBar.Orientation := Orientation;
    TrackBar.PageSize := PageSize;
    TrackBar.Position := Position;
    TrackBar.SliderVisible := SliderVisible;
    TrackBar.ThumbLength := ThumbLength;
    TrackBar.TickMarks := TickMarks;
    TrackBar.TickStyle := TickStyle;
    Self.Width := BarWidth + 4;
    Self.Height := BarHeight + 4;
  end;
end;

procedure TfrmTrackCombo.TrackBarChange(Sender: TObject);
begin
  (Combo as TksoTrackComboBox).Position := TrackBar.Position;
  Combo.Invalidate;
  Combo.Change;
end;

procedure TfrmTrackCombo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
    Key := 0;
  end;
end;

procedure TfrmTrackCombo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    TrackBarChange(Self);
    Close;
    Key := #0;
  end;
end;

procedure TfrmTrackCombo.FormCreate(Sender: TObject);
begin
  TrackBar := TksoTrackBar.Create(Self);
  TrackBar.Left := 2;
  TrackBar.Top := 2;
  TrackBar.Visible := true;
  TrackBar.Parent := Self;
  TrackBar.OnChange := TrackBarChange;
end;

{ TksoTrackBar }

procedure TksoTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Owner is TForm then
    (Owner as TForm).Close;
end;

end.
