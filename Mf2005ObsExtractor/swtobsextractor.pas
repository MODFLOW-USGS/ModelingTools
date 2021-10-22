unit SwtObsExtractor;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, SubsidenceObsExtractor;

type

  { TSwtObsExtractor }

  TSwtObsExtractor = class(TSubsidenceObsExtractor)
  protected
    procedure Initialize3DObsTypes; override;

  end;

var
  SwtTypes: TStringList;

implementation

resourcestring
  rsSUBSIDENCE   = 'SUBSIDENCE';
  rsLAYERCOMPACT = 'LAYER COMPACTION';
  rsSYSTMCOMPACT = 'SYSTM COMPACTION';
  rsZDISPLACEMEN = 'Z DISPLACEMENT';
  rsPRECONSOLSTR = 'PRECONSOL STRESS';
  rsCHANGEINPCST = 'CHANGE IN PCSTRS';
  rsGEOSTATICSTR = 'GEOSTATIC STRESS';
  rsCHANGEINGSTR = 'CHANGE IN G-STRS';
  rsEFFECTIVESTR = 'EFFECTIVE STRESS';
  rsCHANGEINEFFS = 'CHANGE IN EFF-ST';
  rsVOIDRATIO    = 'VOID RATIO';
  rsTHICKNESS    = 'THICKNESS';
  rsCENTERELEVAT = 'CENTER ELEVATION';

procedure InitializeSwtTypes;
begin
  SwtTypes := TStringList.Create;
  SwtTypes.Add(rsSUBSIDENCE);
  SwtTypes.Add(rsLAYERCOMPACT);
  SwtTypes.Add(rsSYSTMCOMPACT);
  SwtTypes.Add(rsZDISPLACEMEN);
  SwtTypes.Add(rsPRECONSOLSTR);
  SwtTypes.Add(rsCHANGEINPCST);
  SwtTypes.Add(rsGEOSTATICSTR);
  SwtTypes.Add(rsCHANGEINGSTR);
  SwtTypes.Add(rsEFFECTIVESTR);
  SwtTypes.Add(rsCHANGEINEFFS);
  SwtTypes.Add(rsVOIDRATIO);
  SwtTypes.Add(rsTHICKNESS);
  SwtTypes.Add(rsCENTERELEVAT);
end;

{ TSwtObsExtractor }

procedure TSwtObsExtractor.Initialize3DObsTypes;
begin
  F3DObsTypes.Add(rsLAYERCOMPACT);
  F3DObsTypes.Add(rsSYSTMCOMPACT);
  F3DObsTypes.Add(rsZDISPLACEMEN);
  F3DObsTypes.Add(rsPRECONSOLSTR);
  F3DObsTypes.Add(rsCHANGEINPCST);
  F3DObsTypes.Add(rsGEOSTATICSTR);
  F3DObsTypes.Add(rsCHANGEINGSTR);
  F3DObsTypes.Add(rsEFFECTIVESTR);
  F3DObsTypes.Add(rsCHANGEINEFFS);
  F3DObsTypes.Add(rsVOIDRATIO);
  F3DObsTypes.Add(rsTHICKNESS);
  F3DObsTypes.Add(rsCENTERELEVAT);
end;

initialization
  InitializeSwtTypes;

finalization
  SwtTypes.Free;

end.

