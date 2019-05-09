unit Strings;

interface

uses DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile, ZDataset;

procedure AddStringsTable(FDB: TDatabaseConnection);

implementation

procedure AddStringsTable(FDB: TDatabaseConnection);
var
  Q: TZQuery;
  Data, Quests: TWZArchive;
  MapNames, MobNames, NPCNames, QuestNames, SkillNames, Dir, Iter: TWZIMGEntry;
  Equips, UseItems, Setup, Etc, Cash: TWZIMGEntry;
  S: string;

  function Untranslated: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to Length(S) do
      if Ord(S[i]) > 255 then
        Exit(True);
  end;

  procedure Add(const Names: TWZIMGEntry);
  var
    Iter: TWZIMGEntry;
  begin
    for Iter in Names.Children do
    begin
      try
        S := Iter.Get('name', '');
        if Untranslated then
          Continue;

        Q.ParamByName('id').AsString := Iter.Name;
        Q.ParamByName('label').AsString := S;
        Q.ExecSQL;
      except
        // Nexon'ed
      end;
    end;
  end;

begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `strings` (' +
  '`object_type` enum(''item'',''skill'',''map'',''mob'',''npc'',''quest'') NOT NULL, ' +
  '`objectid` int(11) NOT NULL, ' +
  '`label` varchar(255) NOT NULL, ' +
  'PRIMARY KEY (`object_type`,`objectid`), ' +
  'KEY `name` (`label`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Contains the string names for most mobs, items, etc.''';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO strings VALUES (:type, :id, :label)';

  Data := LoadArchive('String.wz');
  Quests := LoadArchive('Quest.wz');
  Equips := Data.ParseFile(TWZFile(Data.Root.Entry['Eqp.img'])).Root.Child['Eqp'];
  UseItems := Data.ParseFile(TWZFile(Data.Root.Entry['Consume.img'])).Root;
  Setup := Data.ParseFile(TWZFile(Data.Root.Entry['Ins.img'])).Root;
  Etc := Data.ParseFile(TWZFile(Data.Root.Entry['Etc.img'])).Root.Child['Etc'];
  Cash := Data.ParseFile(TWZFile(Data.Root.Entry['Cash.img'])).Root;
  SkillNames := Data.ParseFile(TWZFile(Data.Root.Entry['Skill.img'])).Root;
  MapNames := Data.ParseFile(TWZFile(Data.Root.Entry['Map.img'])).Root;
  MobNames := Data.ParseFile(TWZFile(Data.Root.Entry['Mob.img'])).Root;
  NPCNames := Data.ParseFile(TWZFile(Data.Root.Entry['Npc.img'])).Root;
  QuestNames := Quests.ParseFile(TWZFile(Quests.Root.Entry['QuestInfo.img'])).Root;

  Q.ParamByName('type').AsString := 'item';
  for Dir in Equips.Children do
    Add(Dir);
  Add(UseItems);
  Add(Setup);
  Add(Etc);
  Add(Cash);

  Q.ParamByName('type').AsString := 'skill';
  Add(SkillNames);

  Q.ParamByName('type').AsString := 'map';
  for Dir in MapNames.Children do
    for Iter in Dir.Children do
    begin
      S := Iter.Get('mapName', '');
      if Untranslated then
        Continue;

      Q.ParamByName('id').AsString := Iter.Name;
      if Iter.Get('streetName', '') <> '' then
        Q.ParamByName('label').AsString := Iter.Get('streetName', '') + ' - ' + S
      else
        Q.ParamByName('label').AsString := S;
      try
        Q.ExecSQL;
      except
        // "Oops, we were busy sucking cocks and duped some map strings!"
      end;
    end;

  Q.ParamByName('type').AsString := 'mob';
  Add(MobNames);

  Q.ParamByName('type').AsString := 'npc';
  Add(NPCNames);

  Q.ParamByName('type').AsString := 'quest';
  Add(QuestNames);

  Data.Free;
  Quests.Free;
  Q.Free;
end;

end.
