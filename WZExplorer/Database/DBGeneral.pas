unit DBGeneral;

interface

uses Classes, SysUtils, DatabaseConnection, ZDataset, WZArchive;

procedure AddFlag(var Flags: string; const NewFlag: string);
function DBStr(const S: string; Limit: Integer = 255): string;
function GetConnection(NewDB: Boolean = False): TDatabaseConnection;
function LoadArchive(const Name: string): TWZArchive;
procedure Init;
function NoIMG(const Name: string): Integer; inline;
procedure AddScriptTable(Q: TZQuery);
procedure ExecScriptQuery(Q: TZQuery; const SType, Script: string; Helper, ID: Integer);

const
  SCRIPT_QUERY = 'INSERT INTO scripts VALUES (:type, :helper, :id, :script)';

implementation

uses Database;

procedure AddFlag(var Flags: string; const NewFlag: string);
begin
  if Flags <> '' then
    Flags := Flags + ',' + NewFlag
  else
    Flags := NewFlag;
end;

function DBStr(const S: string; Limit: Integer = 255): string;
begin
  if Length(S) < 2 then
    Exit(S);

  if (S[1] > #255) or (S[2] > #255) then
    Result := '(untranslated)'
  else
    Result := S;

  if Length(Result) > Limit then
    Delete(Result, Limit + 1, 100);
end;

function GetConnection(NewDB: Boolean = False): TDatabaseConnection;
begin
  Result := TDatabaseConnection.Create(frmDatabase.edtDBName.Text, NewDB);
end;

function LoadArchive(const Name: string): TWZArchive;
var
  FN: string;
  LoadToMem: Boolean;
  F: TFileStream;
begin
  FN := IncludeTrailingPathDelimiter(frmDatabase.edtPath.Text) + Name;

  F := TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
  try
    LoadToMem := F.Size < 1024 * 1024 * 1024;
  finally
    F.Free;
  end;

  Result := TWZArchive.Create(FN, LoadToMem);
end;

function NoIMG(const Name: string): Integer; inline;
begin
  Result := StrToInt(ChangeFileExt(Name, ''));
end;

procedure Init;
var
  InfoQ: TZQuery;
begin
  with GetConnection(True) do
  try
    InfoQ := GetQuery;
    InfoQ.SQL.Text := 'CREATE TABLE `mcdb_info` (' +
    '`version` int(11) NOT NULL, ' +
    '`subversion` int(11) NOT NULL, ' +
    '`maple_locale` ENUM(''global'',''europe'',''korea'',''japan'',''vietnam'',''thailand'',''sea'',''brazil'',''china'',''taiwan'') NOT NULL, ' +
    '`test_server` tinyint(1) NOT NULL DEFAULT ''0'', ' +
    '`maple_version` smallint(6) NOT NULL ' +
    ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
    InfoQ.ExecSQL;
    InfoQ.SQL.Text := 'INSERT INTO mcdb_info VALUES (:maj, :min, :loc, DEFAULT, :mver)';
    InfoQ.ParamByName('maj').AsInteger := 4;
    InfoQ.ParamByName('min').AsInteger := 3;
    InfoQ.ParamByName('loc').AsString := 'global';
    with LoadArchive('Base.wz') do
    begin
      InfoQ.ParamByName('mver').AsInteger := Version;
      Free;
    end;
    InfoQ.ExecSQL;

    AddScriptTable(InfoQ);

    InfoQ.Free;
  finally
    Free;
  end;
end;

procedure AddScriptTable(Q: TZQuery);
begin
  Q.SQL.Text := 'CREATE TABLE `scripts` (' +
  '`script_type` enum(''npc'',''reactor'',''quest'',''item'',''map'',''map_enter'',''map_first_enter'') NOT NULL, ' +
  '`helper` tinyint(3) NOT NULL DEFAULT ''-1'' COMMENT ''Represents the quest state for quests, and the index of the script for NPCs (NPCs may have multiple scripts).'', ' +
  '`objectid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`script` varchar(40) NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`script_type`,`helper`,`objectid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Lists all the scripts that belong to NPCs/reactors/etc. ''';
  Q.ExecSQL;
end;

procedure ExecScriptQuery(Q: TZQuery; const SType, Script: string; Helper, ID: Integer);
begin
  Q.ParamByName('type').AsString := SType;
  Q.ParamByName('helper').AsInteger := Helper;
  Q.ParamByName('id').AsInteger := ID;
  Q.ParamByName('script').AsString := Script;
  Q.ExecSQL;
end;

end.
