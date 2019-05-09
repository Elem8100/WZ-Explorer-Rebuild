unit NPC;

interface

uses DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile, ZDataset;

procedure AddNPCTable(FDB: TDatabaseConnection);

implementation

procedure AddNPCTable(FDB: TDatabaseConnection);
var
  Q, ScriptQ: TZQuery;
  Data: TWZArchive;
  NPC: TWZFile;
  Flags, Script: string;
begin
  ScriptQ := FDB.GetQuery;
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `npc_data` (' +
  '`npcid` int(11) NOT NULL, ' +
  '`storage_cost` int(11) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''maple_tv'',''is_guild_rank'',''can_move'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`npcid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO npc_data VALUES (:id, :storcost, :flags)';
  ScriptQ.SQL.Text := SCRIPT_QUERY;

  Data := LoadArchive('Npc.wz');
  for NPC in Data.Root.Files do
    with Data.ParseFile(NPC) do
    begin
      Q.ParamByName('id').Value := NoIMG(NPC.Name);
      Q.ParamByName('storcost').AsInteger := Root.Get('info/trunkPut', 0);
      Flags := '';
      if Root.Get('info/guildRank', 0) = 1 then
        Flags := 'is_guild_rank'
      else if Root.Get('info/MapleTV', 0) = 1 then
        Flags := 'maple_tv'
      else if Root.Child['move'] <> nil then
        Flags := 'can_move';
      Q.ParamByName('flags').AsString := Flags;
      Q.ExecSQL;

      // Sources don't support more than one script anyway
      Script := Root.Get('info/script/0/script', '');
      if Script <> '' then
        ExecScriptQuery(ScriptQ, 'npc', Script, 0, NoIMG(NPC.Name));
      Free;
    end;
  Data.Free;
  Q.Free;
  ScriptQ.Free;
end;

end.
