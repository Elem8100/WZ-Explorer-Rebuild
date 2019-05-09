unit DBThread;

interface

uses Windows, Classes, SysUtils, DatabaseConnection;

type
  TDBMethod = procedure(FDB: TDatabaseConnection);

  TDBThread = class(TThread)
  private
    FCore, FTag: Byte;
    FDB: TDatabaseConnection;
    FMethod: TDBMethod;
  public
    constructor Create(AMethod: TDBMethod; ACore, ATag: Byte);
  protected
    procedure Execute; override;
  end;

implementation

uses DBGeneral, Database;

{ TDBThread }

constructor TDBThread.Create(AMethod: TDBMethod; ACore, ATag: Byte);
begin
  FCore := ACore;
  FMethod := AMethod;
  FTag := ATag;
  FreeOnTerminate := True;

  inherited Create(False);
end;

procedure TDBThread.Execute;
begin
  SetThreadAffinityMask(Self.Handle, 1 shl FCore);
  FDB := GetConnection;
  try
    FDB.Con.StartTransaction;
    try
      FMethod(FDB);
      FDB.Con.Commit;
    except
      WriteLn(FTag, ': ' + Exception(ExceptObject).Message);
    end;
  finally
    FDB.Free;
    frmDatabase.OnDone(FTag);
  end;
end;

end.
