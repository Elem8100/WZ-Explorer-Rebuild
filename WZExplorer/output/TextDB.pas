unit TextDB;

interface

uses Classes, SysUtils, Generics.Collections, WZArchive, WZDirectory, WZIMGFile;

procedure DumpPortalDataToFile(const FileName: string);
procedure DumpMobDataToFile(const FileName: string);
procedure DumpMobNamesForLinosal(const FileName: string);
procedure DumpSkillsForLinosal(const FileName: string);

implementation

function NoIMG(const Name: string): Integer;
begin
  Result := StrToInt(ChangeFileExt(Name, ''));
end;

procedure DumpPortalDataToFile(const FileName: string);
var
  MapWZ, StringWZ: TWZArchive;
  Names: TDictionary<Integer, string>;
  S: string;
  ContinentDirs, MapDir: TWZDirectory;
  Map: TWZFile;
  Dir, Iter: TWZIMGEntry;
  Res: TStringList;
  MapID, MapTo: Integer;
begin
  StringWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\String.wz');
  with StringWZ.ParseFile(TWZFile(StringWZ.Root.Entry['Map.img'])) do
  begin
    Names := TDictionary<Integer, string>.Create;
    for Dir in Root.Children do
      for Iter in Dir.Children do
      begin
        S := StringReplace(Iter.Get('mapName', ''), #13#10, '', [rfReplaceAll]);

        if Iter.Get('streetName', '') <> '' then
          Names.AddOrSetValue(StrToInt(Iter.Name), Iter.Get('streetName', '') + ' - ' + S)
        else
          Names.AddOrSetValue(StrToInt(Iter.Name), S);
    end;
    Free;
  end;
  StringWZ.Free;

  MapWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\Map.wz');
  Res := TStringList.Create;
  try
    ContinentDirs := TWZDirectory(MapWZ.Root.Entry['Map']);
    for MapDir in ContinentDirs.SubDirs do
      for Map in MapDir.Files do
        with MapWZ.ParseFile(Map) do
        begin
          MapID := NoIMG(Map.Name);
          if Root.Child['portal'] <> nil then
            for Iter in Root.Child['portal'].Children do
            begin
              MapTo := Iter.Get('tm', -1);
              if Names.ContainsKey(MapID) then
              begin
                if (MapTo = 999999999) and (Iter.Get('script', '') <> '') then
                  Res.Add(Format('%d|%s|%d|%d|%s|!%s',
                    [MapID, string(Iter.Get('pn', '')), Iter.Get('x', 0), Iter.Get('y', 0), Names[MapID], Iter.Get('script', '')]))
                else if (MapTo > -1) and (MapTo < 999999999) and (MapTo <> MapID) then
                begin
                  Res.Add(Format('%d|%s|%d|%d|%s|%d',
                    [MapID, string(Iter.Get('pn', '')), Iter.Get('x', 0), Iter.Get('y', 0), Names[MapID], MapTo]));
                end;
              end;
            end;

          Free;
        end;
    Res.SaveToFile(FileName);
  finally
    MapWZ.Free;
    Res.Free;
  end;
end;

procedure DumpMobDataToFile(const FileName: string);
var
  MobWZ: TWZArchive;
  Mob: TWZFile;
  Res: TStringList;
  MobID: Integer;
begin
  MobWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\Mob.wz');
  Res := TStringList.Create;
  try
    for Mob in MobWZ.Root.Files do
      with MobWZ.ParseFile(Mob) do
      begin
        MobID := NoIMG(Mob.Name);
        if Root.Child['info'] <> nil then
          Res.Add(Format('%d|%d|%d|%d|%d|%d', [MobID, Int32(Root.Get('info/maxHP', 0)), Int32(Root.Get('info/PADamage', 0)), Int32(Root.Get('info/PDRate', 0)), Int32(Root.Get('info/MADamage', 0)), Int32(Root.Get('info/MDRate', 0))]));

        Free;
      end;
    Res.SaveToFile(FileName);
  finally
    MobWZ.Free;
    Res.Free;
  end;
end;

procedure DumpMobNamesForLinosal(const FileName: string);
var
  StringWZ: TWZArchive;
  Names: TDictionary<Integer, string>;
  MobIter: TWZIMGEntry;

  MobWZ: TWZArchive;
  Mob: TWZFile;
  Res: TStringList;
  MobID: Integer;
begin
  Names := TDictionary<Integer, string>.Create;

  StringWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\String.wz');
  with StringWZ.ParseFile(TWZFile(StringWZ.Root.Entry['Mob.img'])) do
  begin
    for MobIter in Root.Children do
      Names.Add(StrToInt(MobIter.Name), MobIter.Get('name', ''));
    Free;
  end;
  StringWZ.Free;

  MobWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\Mob.wz');
  Res := TStringList.Create;
  try
    for Mob in MobWZ.Root.Files do
      with MobWZ.ParseFile(Mob) do
      begin
        if not CharInSet(Mob.Name[1], ['0'..'9']) then
          Continue;

        MobID := NoIMG(Mob.Name);
        if Root.Child['info'] <> nil then
          Res.Add(Format('%d|%s', [MobID, Names[MobID]]));

        Free;
      end;
    Res.SaveToFile(FileName);
  finally
    MobWZ.Free;
    Res.Free;
  end;

  Names.Free;
end;

procedure DumpSkillsForLinosal(const FileName: string);
var
  StringWZ: TWZArchive;
  Names: TDictionary<Integer, string>;
  Skill: TWZIMGEntry;

  SkillWZ: TWZArchive;
  Job: TWZFile;
  Res: TStringList;
  SkillID: Integer;
begin
  Names := TDictionary<Integer, string>.Create;

  StringWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\String.wz');
  with StringWZ.ParseFile(TWZFile(StringWZ.Root.Entry['Skill.img'])) do
  begin
    for Skill in Root.Children do
      if Skill.Child['name'] <> nil then
        Names.Add(StrToInt(Skill.Name), Skill.Get('name', ''));
    Free;
  end;
  StringWZ.Free;

  SkillWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\Skill.wz');
  Res := TStringList.Create;
  try
    for Job in SkillWZ.Root.Files do
    begin
      if not CharInSet(Job.Name[1], ['0'..'9']) then
        Continue;

      with SkillWZ.ParseFile(Job) do
      begin
        for Skill in Root.Child['skill'].Children do
        begin
          SkillID := NoIMG(Skill.Name);
          if (Skill.Child['common'] <> nil) and (Names.ContainsKey(SkillID)) then
            Res.Add(Format('%d|%s|%s|%s', [SkillID, Names[SkillID], string(Skill.Get('common/time', '0')), string(Skill.Get('common/cooltime', '0'))]));

        end;
        Free;
      end;
    end;
    Res.SaveToFile(FileName);
  finally
    SkillWZ.Free;
    Res.Free;
  end;

  Names.Free;
end;

end.
