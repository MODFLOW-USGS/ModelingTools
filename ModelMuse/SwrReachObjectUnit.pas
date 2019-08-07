unit SwrReachObjectUnit;

interface

uses
  Generics.Collections, ModflowSwrReachUnit, ModflowSwrObsUnit;

type
  TReachObsLink = class(TObject)
    ObsName: string;
    ObsType: TSwrObsType;
    procedure Assign(Link: TReachObsLink);
  end;

  TReachObsLinks = TObjectList<TReachObsLink>;

  // @name stores the data for one reach in the MODFLOW SWR process.
  TReachObject = class(TObject)
  private
    FNeighbors: TList<TReachObject>;
  public
    FReachData: TSwrReachRecord;
    ObsLinks: TReachObsLinks;
    constructor Create;
    destructor Destroy; override;
    property Neighbors: TList<TReachObject> read FNeighbors;
  end;

implementation

{ TReachObject }

constructor TReachObject.Create;
begin
  FNeighbors := TList<TReachObject>.Create;
  ObsLinks := TReachObsLinks.Create;
end;

destructor TReachObject.Destroy;
begin
  ObsLinks.Free;
  FNeighbors.Free;
  inherited;
end;

{ TReachObsLink }

procedure TReachObsLink.Assign(Link: TReachObsLink);
begin
  ObsName := Link.ObsName;
  ObsType := Link.ObsType;
end;

end.
