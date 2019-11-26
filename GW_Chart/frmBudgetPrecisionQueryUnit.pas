unit frmBudgetPrecisionQueryUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ReadModflowArrayUnit;

type
  TfrmBudgetPrecisionQuery = class(TForm)
    rgBudgetPrecision: TRadioGroup;
    pnlBottom: TPanel;
    btnClose: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBudgetPrecisionQuery: TfrmBudgetPrecisionQuery;

function QueryBudgetPrecision(FileStream: TFileStream): TModflowPrecision;

implementation

{$R *.dfm}

resourcestring
  StrUsuallyTheFileWil = 'Usually the file will be single precision so that ' +
  'is what we''ll try. However, if you encounter an error, the file may be ' +
  'double precision.';
  StrSinglePrecisionMea = 'Single precision means that each real number in t' +
  'he file is stored using 4 bytes. Double precision means that each real nu' +
  'mber is stored using 8 bytes. There are two versions of MODFLOW-2005 dist' +
  'ributed by the USGS. Mf2005.exe saves results in single precision format. ' +
  'Mf2005dbl.exe saves results in double precision format.';

function QueryBudgetPrecision(FileStream: TFileStream): TModflowPrecision;
begin
  try
    result := CheckBudgetPrecision(FileStream);
  except on EPrecisionReadError do
    begin
      frmBudgetPrecisionQuery := TfrmBudgetPrecisionQuery.Create(nil);
      try
        frmBudgetPrecisionQuery.ShowModal;
        case frmBudgetPrecisionQuery.rgBudgetPrecision.ItemIndex of
          0:
            begin
              result := mpSingle;
            end;
          1:
            begin
              result := mpDouble;
            end;
          2:
            begin
              MessageDlg(StrUsuallyTheFileWil, mtInformation, [mbOK], 0);
              result := mpSingle;
            end;
          3:
            begin
              MessageDlg(StrSinglePrecisionMea + StrUsuallyTheFileWil, mtInformation, [mbOK], 0);
              result := mpSingle;
            end;
          else
            begin
              result := mpSingle;
            end;
        end;
      finally
        frmBudgetPrecisionQuery.Free;
      end;
    end;
  end;
end;

end.
