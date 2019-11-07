
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit SpectrumComboForm;

interface

{$I OFFICEVER.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AxCtrls, StdCtrls, BaseCombo, BaseComboForm, ComCtrls;

type

  TfrmSpectrumCombo = class(TfrmCustomCombo)
    Sample: TPanel;
    SpectrumPanel: TPanel;
    Spectrum: TImage;
    procedure SpectrumMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpectrumMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterSetCombo; override;
  end;

var
  frmSpectrumCombo: TfrmSpectrumCombo;

implementation {===============================================================}

uses SpectrumCombo;

{$R *.DFM}

procedure TfrmSpectrumCombo.AfterSetCombo;
begin
  with (Combo as TksoSpectrumComboBox) do
  begin
    Self.Width := SpectrumPanel.Width + 4;
    Self.Height := Sample.Top + Sample.Height + 4;
  end;
end;

procedure TfrmSpectrumCombo.SpectrumMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Combo as TksoSpectrumComboBox) do
    ColorValue := Spectrum.Canvas.Pixels[X, Y];
  Close;
end;

procedure TfrmSpectrumCombo.SpectrumMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Sample.Color := Spectrum.Canvas.Pixels[X, Y];
end;

end.
