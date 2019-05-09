unit DatabaseConnection;

interface

uses SysUtils, ZConnection, ZAbstractRODataset, ZAbstractDataset, ZDataset, DB;

type
  TDatabaseConnection = class
  private
    FCon: TZConnection;

    procedure Init(DBName: string; NewDB: Boolean = False);
  public
    constructor Create(DBName: string; NewDB: Boolean = False);
    destructor Destroy; override;

    function GetQuery: TZQuery;

    property Con: TZConnection read FCon write FCon;
  end;

implementation

uses Database;

{ TDatabaseConnection }

constructor TDatabaseConnection.Create(DBName: string; NewDB: Boolean = False);
begin
  Init(DBName, NewDB);
end;

destructor TDatabaseConnection.Destroy;
begin
  // Close the connection
  FCon.Connected := False;
  FreeAndNil(FCon);

  inherited;
end;

procedure TDatabaseConnection.Init(DBName: string; NewDB: Boolean = False);
begin
  if frmDatabase.edtDBName.Text = '' then
    raise Exception.Create('Please enter a database name!');

  // Initialize the conenction with values from the settings
  FCon := TZConnection.Create(nil);
  FCon.Protocol := 'mysql-5';
  FCon.HostName := frmDatabase.edtSQLHost.Text;
  FCon.Port := frmDatabase.seSQLPort.Value;
  FCon.User := frmDatabase.edtSQLUser.Text;
  FCon.Password := frmDatabase.edtSQLPW.Text;

  FCon.Connected := True;

  if NewDB then
    with GetQuery do
    begin
      SQL.Text := 'DROP DATABASE IF EXISTS ' + DBName;
      ExecSQL;

      SQL.Text := 'CREATE DATABASE ' + DBName;
      ExecSQL;

      Free;
    end;

  FCon.Connected := False;

  FCon.Catalog := DBName;
  FCon.Database := DBName;

  FCon.Connected := True;
end;

function TDatabaseConnection.GetQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FCon;
end;

end.
