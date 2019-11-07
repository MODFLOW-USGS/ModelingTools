unit Strset;
{
  Copyright © 1997 WinWright Consulting
  Written by Wayne Niddery

  You may use this code freely in any project, commercial included, as long
  as the this entire comment section, including copyright and credit, remains 
  intact. You may redistribute this code to others, and/or a compiled version
  thereof, as freeware only.
}

interface

uses
  SysUtils, Windows, Messages, Classes, Controls;


type
  TStrSet = class(TComponent)
  private
    FStrList: TStrings;
    procedure SetStrList(Value: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Strings: TStrings read FStrList write SetStrList;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WinWright', [TStrSet]);
end;

constructor TStrSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrList := TStringList.Create;
end;

destructor TStrSet.Destroy;
begin
  FStrList.Free;
  inherited Destroy;
end;

procedure TStrSet.SetStrList(Value: TStrings);
begin
  FStrList.Assign(Value);
end;

end.
