unit InputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLocationID = record
    ID: string;
    APoint: TPoint2D;
  end;

  TLocationID = record
    ID: string;
    APoint: TPoint2D;
  end;

  TLocationList = specialize TList<TLocationID>;
  TLocationDictionary = specialize TDictionary<string, TLocationID>;

  TDerivedObs = class
    ID: string;
    Obsname: string;
    Time: double;
    Print: boolean;
    Value: double;
    TimeAssigned: Boolean;
  end;

  TDerivedObsList = specialize TList<TDerivedObs>;

  { TDerivedObsCompare }

  TDerivedObsCompare = class(specialize TComparer<TDerivedObs>)
    function Compare(constref Left, Right: TDerivedObs): Integer; override;
  end;

  { TDerivedObsObjectList }

  TDerivedObsObjectList = class(specialize TObjectList<TDerivedObs>)
  public
    procedure Sort;
  end;

  TDerivedObsDictionary = specialize TDictionary<string, TDerivedObs>;



implementation

end.

