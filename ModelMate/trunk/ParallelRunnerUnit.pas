unit ParallelRunnerUnit;

interface

  uses Classes, Math, SysUtils, Windows,
       GlobalBasicData, Utilities;

  type

    TParallelRunner = Class(TCollectionItem)
      // Class stores info related to one JUPITER parallel runner
      private
        // Fields
        fName: string;
        fDirectory: string;
        fExpectedRunTime: double; // in seconds
        fUse: boolean;
        procedure SetDirectory(const Value: string);
      public
        procedure Assign(Source: TPersistent); override;
        function AbsDirectory: string;
        function SameAs(Source: TPersistent): boolean;
      published
        // Properties
        property Name: string read fName write fName;
        property Directory: string read fDirectory write SetDirectory;
        property ExpectedRunTime: double read fExpectedRunTime write fExpectedRunTime;
        property Use: boolean read fUse write fUse;
    end;

    TParallelRunners = Class(TCollection)
      // Class stores data for all runners.
      private
        function GetItem(Index: integer): TParallelRunner;
        procedure SetItem(Index: integer; const Value: TParallelRunner);
      public
        property Items[I: integer]: TParallelRunner
                     read GetItem write SetItem; default;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);  override;
        function NumUsable: integer;
        function SameAs(Source: TPersistent): boolean;
    end;

implementation

{ TParallelRunner }

function TParallelRunner.AbsDirectory: string;
begin
  if IsUNC(Directory) then
    begin
      result := Directory;
    end
  else
    begin
      result := RelDirToAbsDir(ProjectDirectory, Directory);
    end;
end;

procedure TParallelRunner.Assign(Source: TPersistent);
var
  PRSource: TParallelRunner;
begin
  if Source is TParallelRunner then
    begin
      PRSource := Source as TParallelRunner;
      Name := PRSource.Name;
      Directory := PRSource.Directory;
      ExpectedRunTime := PRSource.ExpectedRunTime;
      Use := PRSource.Use;
    end
  else
    inherited;
end;

function TParallelRunner.SameAs(Source: TPersistent): boolean;
var
  PRSource: TParallelRunner;
begin
  result := False;
  if Source is TParallelRunner then
    begin
      result := True;
      PRSource := Source as TParallelRunner;
      if not AnsiSameStr(Name,PRSource.Name) then result := False;
      if not AnsiSameStr(Directory,PRSource.Directory) then result := False;
      if not SameValue(ExpectedRunTime,PRSource.ExpectedRunTime) then result := False;
      if not Use = PRSource.Use then result := False;
    end;
end;

procedure TParallelRunner.SetDirectory(const Value: string);
// Store relative path to directory Value unless Value is a UNC path.
begin
  fDirectory := RelativePath(Value);
end;

{ TParallelRunners }

procedure TParallelRunners.Assign(Source: TPersistent);
var
  I: integer;
  Item: TParallelRunners;
begin
  if Source is TParallelRunners then
    begin
      Item := Source as TParallelRunners;
      // Clear Items and dimension it to be the same size as Source.Items
      Clear;
      for I := 0 to Item.Count - 1 do
        begin
          Add;
          Items[I].Assign(Item.Items[I]); // Copy TParallelRunner objects
        end;
    end
  else
    inherited;
end;

constructor TParallelRunners.Create;
begin
  inherited Create(TParallelRunner);
end;

destructor TParallelRunners.Destroy;
begin
  self.Clear;
  inherited;
end;

function TParallelRunners.GetItem(Index: integer): TParallelRunner;
begin
  result := TParallelRunner(inherited GetItem(Index));
end;

function TParallelRunners.NumUsable: integer;
var
  I: integer;
begin
  result := 0;
  for I := 0 to self.Count - 1 do
    begin
      if Items[I].fUse then
        begin
          result := result + 1;
        end;
    end;
end;

function TParallelRunners.SameAs(Source: TPersistent): boolean;
var
  I: integer;
  Item: TParallelRunners;
begin
  result := True;
  if Source is TParallelRunners then
    begin
      Item := Source as TParallelRunners;
      if Count = Item.Count then
        begin
          for I := 0 to Item.Count - 1 do
            begin
              if not Items[I].SameAs(Item.Items[I]) then
                begin
                  result := False;
                  Break;
                end;
            end
        end
      else
        result := False;
    end;
end;

procedure TParallelRunners.SetItem(Index: integer;
  const Value: TParallelRunner);
begin
  inherited SetItem(Index, Value);
end;

end.
