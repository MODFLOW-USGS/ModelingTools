unit ParallelControlUnit;

interface

  uses Classes, Math, SysUtils;

  type

    TParallelControl = Class(TPersistent)
        // One instance defines data for a Parallel_Control input block
      private
        // Fields
        fParallel: boolean;
        fWait: double;
        fWaitRunners: double;
        fTimeOutFactor: double;
        fVerboseRunner: integer;
        fNumRunnersToUse: integer;
        fAutoStopRunners: boolean;
        fAutoStartLocalRunners: boolean;
        fAutoPopRunnerDirs: boolean;
      published
        // Properties
        property Parallel: boolean read fParallel write fParallel;
        property Wait: double read fWait write fWait;
        property WaitRunners: double read fWaitRunners write fWaitRunners;
        property TimeOutFactor: double read fTimeOutFactor write fTimeOutFactor;
        property VerboseRunner: integer read fVerboseRunner write fVerboseRunner;
        property NumRunnersToUse: integer read fNumRunnersToUse write fNumRunnersToUse;
        property AutoStopRunners: boolean read fAutoStopRunners write fAutoStopRunners;
        property AutoStartLocalRunners: boolean read fAutoStartLocalRunners
                                                write fAutoStartLocalRunners;
        property AutoPopRunnerDirs: boolean read fAutoPopRunnerDirs
                                            write fAutoPopRunnerDirs;
      public
        // Methods
        procedure Assign(Source: TPersistent); override;
        constructor Create;
        function SameAs(Source: TPersistent): boolean;
    end;

implementation

{ TParallelControl }

procedure TParallelControl.Assign(Source: TPersistent);
var
  PCSource: TParallelControl;
begin
  if Source is TParallelControl then
    begin
      PCSource := Source as TParallelControl;
      Parallel := PCSource.Parallel;
      Wait := PCSource.Wait;
      WaitRunners := PCSource.WaitRunners;
      TimeOutFactor := PCSource.TimeOutFactor;
      VerboseRunner := PCSource.VerboseRunner;
      NumRunnersToUse := PCSource.NumRunnersToUse;
      AutoStopRunners := PCSource.AutoStopRunners;
      AutoStartLocalRunners := PCSource.AutoStartLocalRunners;
      AutoPopRunnerDirs := PCSource.AutoPopRunnerDirs;
    end;
end;

constructor TParallelControl.Create;
begin
  inherited;
  fParallel := False;
  fWait := 0.001;
  fWaitRunners := 0.001;
  fTimeOutFactor := 3.0;
  fVerboseRunner := 3;
  fNumRunnersToUse := 0;
  fAutoStopRunners := True;
  fAutoStartLocalRunners := True;
  fAutoPopRunnerDirs := False;
end;

function TParallelControl.SameAs(Source: TPersistent): boolean;
var
  PCSource: TParallelControl;
begin
  result := False;
  if Source is TParallelControl then
    begin
      result := True;
      PCSource := Source as TParallelControl;
      if not Parallel = PCSource.Parallel then result := False;
      if not SameValue(Wait, PCSource.Wait) then result := False;
      if not SameValue(WaitRunners, PCSource.WaitRunners) then result := False;
      if not SameValue(TimeOutFactor, PCSource.TimeOutFactor) then result := False;
      if not SameValue(VerboseRunner, PCSource.VerboseRunner) then result := False;
      if not SameValue(NumRunnersToUse, PCSource.NumRunnersToUse) then result := False;
      if not AutoStopRunners = PCSource.AutoStopRunners then result := False;
      if not AutoStartLocalRunners = PCSource.AutoStartLocalRunners then result := False;
      if not AutoPopRunnerDirs = PCSource.AutoPopRunnerDirs then result := False;
    end;
end;

end.
