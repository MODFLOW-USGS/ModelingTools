unit frmAncestorUnit;

interface

uses
  StdCtrls, Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.ConvUtils, System.StdConvs;

type
  TfrmAncestor = class(TForm)
  private
    { Private declarations }
  public
    function GetLengthUnits(ItemIndex: Integer): TConvType;
    // only feet and meters.
    function GetStandardLengthUnits(ItemIndex: Integer): TConvType;
    function GetAreaUnits(ItemIndex: Integer): TConvType;
    function GetVolumeUnits(ItemIndex: Integer): TConvType;
    { Public declarations }
  end;

var
  frmAncestor: TfrmAncestor;

implementation

uses
  frmGwMoundUnit;

{$R *.dfm}

{ TfrmAncestor }

function TfrmAncestor.GetAreaUnits(ItemIndex: Integer): TConvType;
begin
  result := auSquareMeters;
  case ItemIndex of
    0:
      result := auAcres;
    1:
      result := auSquareFeet;
    2:
      result := auSquareMeters;
  else
    Assert(False);
  end;
end;

function TfrmAncestor.GetLengthUnits(ItemIndex: Integer): TConvType;
begin
  result := duMeters;
  case ItemIndex of
    0:
      result := duInches;
    1:
      result := duFeet;
    2:
      result := duCentimeters;
    3:
      result := duMeters;
  else
    Assert(False);
  end;
end;

function TfrmAncestor.GetStandardLengthUnits(ItemIndex: Integer): TConvType;
begin
  result := duMeters;
  case ItemIndex of
    0:
      result := duFeet;
    1:
      result := duMeters;
  else
    Assert(False);
  end;
end;

function TfrmAncestor.GetVolumeUnits(ItemIndex: Integer): TConvType;
begin
  result := vuCubicMeters;
  case ItemIndex of
    0:
      result := vuCubicInches;
    1:
      result := vuCubicFeet;
    2:
      result := vuAcreFeet;
    3:
      result := vuCubicCentimeters;
    4:
      result := vuCubicMeters;
  else
    Assert(False);
  end;
end;

end.
