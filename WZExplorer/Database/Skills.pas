unit Skills;

interface

uses SysUtils, StrUtils, System.Types, Math, Generics.Collections, {$IFNDEF NOCRC}CRC32,{$ENDIF}
     DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile, ZDataset;

procedure AddSkillTable(FDB: TDatabaseConnection);
procedure AddProfessionTables(FDB: TDatabaseConnection; Data: TWZArchive);

implementation

uses Database;

function BracketText(const S: string; Idx: Integer): string;
var
  C, IdxStart: Integer;
begin
  // S[1] = '('
  C := 0;
  Result := '';
  IdxStart := Idx + 1;
  for Idx := Idx to Length(S) do
  begin
    if S[Idx] = '(' then
      Inc(C)
    else if S[Idx] = ')' then
      Dec(C)
    else if S[Idx] = ' ' then
      Exit;

    if C = 0 then
    begin
      C := Idx - IdxStart;
      Result := Copy(S, IdxStart, C);
      Break;
    end;
  end;
end;

function Solve(f: string; x: Integer): Single;
type
  TCommaResolver = reference to function(Res: Single): Integer;
var
  a, b: Integer;
  c, Pre: string;
  Proc: TCommaResolver;

  function GetArgument(p: Integer; Left: Boolean): Integer;
  var
    i: Integer;
    s: string;
  begin
    i := p;
    if Left then
    begin
      repeat
        Dec(i)
      until (i = 0) or not CharInSet(f[i], ['0'..'9']);
      Inc(i);
      s := Copy(f, i, p - i);
    end
    else
    begin
      repeat
        Inc(i)
      until (i > Length(f)) or not CharInSet(f[i], ['0'..'9']);
      Dec(i);
      s := Copy(f, p + 1, i - p);
    end;
    Result := StrToInt(s);
  end;

begin
  if f = '' then
    Exit(0);

  f := StringReplace(f, 'x', IntToStr(x), [rfReplaceAll]);
  f := StringReplace(f, '+y', '', [rfReplaceAll]); // 65000003
  f := StringReplace(f, ' ', '', [rfReplaceAll]);  // 51121008...
  f := StringReplace(f, #13#10, '', [rfReplaceAll]); // fuck the duck before the duck your mother fuck
  if f[1] = '=' then // Nexon's parser fucks this up
    while CharInSet(f[1], ['0'..'9', '=']) do
      Delete(f, 1, 1);

  if f[1] = '+' then
    Delete(f, 1, 1);

  if Length(f) < 3 then
    Exit(StrToInt(f));

  if (PosEx('-', f, 2) > 0) and (Pos('+', f) > 0) then
    WriteLn('Untested math: ' + f);

  while Pos('(', f) > 0 do
  begin
    a := Pos('(', f);
    c := BracketText(f, a);
    if a > 1 then
      Pre := f[a - 1]
    else
      Pre := '0';

    case Pre[1] of
      'u': Proc := function(Res: Single): Integer begin Result := Ceil(Res) end;
      'd': Proc := function(Res: Single): Integer begin Result := Floor(Res) end;
      else
      begin
        Proc := function(Res: Single): Integer begin Result := Trunc(Res) end;
        Pre := '';
      end;
    end;

    f := StringReplace(f, Pre + '(' + c + ')', IntToStr(Proc(Solve(c, x))), [rfReplaceAll]);
  end;
  while Pos('*', f) > 0 do
  begin
    a := GetArgument(Pos('*', f), True);
    b := GetArgument(Pos('*', f), False);
    f := StringReplace(f, Format('%d*%d', [a, b]), IntToStr(a * b), [rfReplaceAll]);
  end;
  while Pos('/', f) > 0 do
  begin
    a := GetArgument(Pos('/', f), True);
    b := GetArgument(Pos('/', f), False);
    f := StringReplace(f, Format('%d/%d', [a, b]), FloatToStr(a / b), [rfReplaceAll]);
  end;
  while PosEx('-', f, 2) > 0 do   // -20-1
  begin
    a := GetArgument(PosEx('-', f, 2), True);
    if f[1] = '-' then
      a := -a;
    b := GetArgument(PosEx('-', f, 2), False);
    f := StringReplace(f, Format('%d-%d', [a, b]), IntToStr(a - b), [rfReplaceAll]);
  end;
  while Pos('+', f) > 0 do
  begin
    a := GetArgument(Pos('+', f), True);
    if f[1] = '-' then
      a := -a;
    b := GetArgument(Pos('+', f), False);
    f := StringReplace(f, Format('%d+%d', [a, b]), IntToStr(a + b), [rfReplaceAll]);
  end;
  Result := StrToFloat(f);
end;

procedure AddSkillTable(FDB: TDatabaseConnection);
var
  DQ, Q, PsdQ: TZQuery;
  Data: TWZArchive;
  Book: TWZFile;
  Skill, Level, Vec, Iter, Iter2: TWZIMGEntry;
  i, MaxLv: Integer;

  Infos: TDictionary<string, TPair<PPoint, PPoint>>;
  P: TPoint;
  CurLT, CurRB: PPoint;
  s: string;
  Check: TList<string>;

  procedure AddLevelData;
  var
    j: Integer;
  begin
    for j := 2 to Q.Params.Count - {$IFNDEF NOCRC}2{$ELSE}1{$ENDIF} do
    begin
      if (Length(Q.Params[j].Name) = 3) and CharInSet(Q.Params[j].Name[3], ['x', 'y']) then
      begin
        Vec := Level.Child[Copy(Q.Params[j].Name, 1, 2)];
        if Vec <> nil then
        begin
          case Q.Params[j].Name[3] of
            'x': Q.Params[j].AsInteger := EnsureRange(Vec.Vector.X, Low(Int16), High(Int16));
            'y': Q.Params[j].AsInteger := EnsureRange(Vec.Vector.Y, Low(Int16), High(Int16));
          end;
        end
        else
          Q.Params[j].AsInteger := 0;
      end
      else
        Q.Params[j].AsInteger := Trunc(Solve(Level.Get(Q.Params[j].Name, ''), i));
    end;
    {$IFNDEF NOCRC}
    Q.ParamByName('crc32').AsInteger := CRC32.CalcSkillCRC(Q, Data.Version, Level);
    {$ENDIF}
    try
      Q.ExecSQL;
    except
      Writeln('Skill ', Skill.Name, ': ', Exception(ExceptObject).Message);
    end;
  end;

begin
  DQ := FDB.GetQuery;
  DQ.SQL.Text := 'CREATE TABLE `skill_player_data` (' +
  '`skillid` int(11) NOT NULL, ' +
  '`flags` set(''invisible'',''psd'') NOT NULL DEFAULT '''', ' +
  '`weapon` smallint(6) NOT NULL, ' +
  '`sub_weapon` smallint(6) NOT NULL, ' +
  '`skill_type` enum(''normal'',''mastery'',''booster'',''final_attack'',''serial_attack'') NOT NULL, ' +
  'PRIMARY KEY (`skillid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  DQ.ExecSQL;

  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `skill_player_level_data` (' +
  '`skillid` int(11) NOT NULL,' +
  '`skill_level` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`mob_count` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`hit_count` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`range` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`buff_time` int(11) NOT NULL DEFAULT ''0'',' +
  '`buff_sub_time` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`mp_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`hp_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`fury_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`aran_combo_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`spiritual_power_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`supply_cost` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`damage` int(11) NOT NULL DEFAULT ''0'',' +
  '`fixed_damage` int(11) NOT NULL DEFAULT ''0'',' +
  '`critical_damage` tinyint(3) unsigned NOT NULL DEFAULT ''0'',' +
  '`self_destruction_damage` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`boss_damage` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`damage_tw2` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`damage_tw3` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`damage_tw4` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`mastery` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`optional_item_cost` int(11) NOT NULL DEFAULT ''0'',' +
  '`item_cost` int(11) NOT NULL DEFAULT ''0'',' +
  '`item_count` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`bullet_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`money_cost` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`u` int(11) NOT NULL DEFAULT ''0'',' +
  '`v` int(11) NOT NULL DEFAULT ''0'',' +
  '`w` int(11) NOT NULL DEFAULT ''0'',' +
  '`x` int(11) NOT NULL DEFAULT ''0'',' +
  '`y` int(11) NOT NULL DEFAULT ''0'',' +
  '`z` int(11) NOT NULL DEFAULT ''0'',' +
  '`speed` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`jump` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`str` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`weapon_atk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`weapon_def` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`magic_atk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`magic_def` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`accuracy` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`avoid` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`hp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`mp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`max_hp` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`max_mp` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`damage_rate` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`damage_over_time` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`dot_duration` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`dot_interval` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`dot_stack_count` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`enhanced_watk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`enhanced_wdef` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`enhanced_matk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`enhanced_mdef` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`enhanced_hp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`enhanced_mp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_watk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_wdef` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_matk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_mdef` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_hp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_mp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_hp_rate` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`indie_mp_rate` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`indie_accuracy` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_avoid` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_all_stats` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_damage_rate` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`indie_speed` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_jump` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`indie_booster` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`passive_str` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_dex` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_int` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_luk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_watk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_matk` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_hp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`passive_mp` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`prop` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`sub_prop` tinyint(3) NOT NULL DEFAULT ''0'',' +
  '`morph` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`ltx` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`lty` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`rbx` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`rby` smallint(6) NOT NULL DEFAULT ''0'',' +
  '`cooldown_time` int(11) NOT NULL DEFAULT ''0'',' +
  {$IFNDEF NOCRC}
  '`crc32` int(11) NOT NULL DEFAULT ''0'', ' +
  {$ENDIF}
  'PRIMARY KEY (`skillid`, `skill_level`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Contains all the player skill information.'';';
  Q.ExecSQL;

  PsdQ := FDB.GetQuery;
  PsdQ.SQL.Text := 'CREATE TABLE `skill_player_psd_skills` (' +
  '`skillid` int(11) NOT NULL, ' +
  '`psd_skill` int(11) NOT NULL, ' +
  'PRIMARY KEY (`skillid`,`psd_skill`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  PsdQ.ExecSQL;

  Q.SQL.Text := 'CREATE TABLE `skill_animation_data` (' +
  '`id` VARCHAR(35) BINARY NOT NULL, ' +
  '`animation` INT(10) UNSIGNED NOT NULL, ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'CREATE TABLE `character_afterimage_data` (' +
  '`id` VARCHAR(10) NOT NULL, ' +
  '`animation` VARCHAR(15) NOT NULL, ' +
  '`x_pos1` smallint(6) NOT NULL, ' +
  '`y_pos1` smallint(6) NOT NULL, ' +
  '`x_pos2` smallint(6) NOT NULL, ' +
  '`y_pos2` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`id`,`animation`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  DQ.SQL.Text := 'INSERT INTO skill_player_data VALUES (:id, :flags, :weapon, :subWeapon, :type)';
  Q.SQL.Text := 'INSERT INTO skill_player_level_data VALUES (:id, :lv, :mobCount, :attackCount, ' +
                ':range, :time, :subTime, :mpCon, :hpCon, :forceCon, :aranComboCon, :epCon, :powerCon, ' +
                ':damage, :fixdamage, :criticalDamage, :selfDestruction, :damageToBoss, ' +
                ':damageTW2, :damageTW3, :damageTW4, :mastery, :itemConsume, ' +
                ':itemCon, :itemConNo, :bulletCount, :moneyCon, :u, :v, :w, :x, :y, :z, :speed, :jump, ' +
                ':str, :pad, :pdd, :mad, :mdd, :acc, :eva, :hp, :mp, :mhpR, :mmpR, :damR, ' +
                ':dot, :dotTime, :dotInterval, :dotSuperpos, :epad, :epdd, :emad, :emdd, :emhp, :emmp, ' +
                ':indiePad, :indiePdd, :indieMad, :indieMdd, :indieMhp, :indieMmp, :indieMhpR, :indieMmpR, ' +
                ':indieAcc, :indieEva, :indieAllStat, :indieDamR, :indieSpeed, :indieJump, :indieBooster, ' +
                ':strX, :dexX, :intX, :lukX, :padX, :madX, :mhpX, :mmpX, ' +
                ':prop, :subProp, :morph, ' +
                ':ltx, :lty, :rbx, :rby, :cooltime'{$IFNDEF NOCRC} + ', :crc32'{$ENDIF} + ');';
  PsdQ.SQL.Text := 'INSERT INTO skill_player_psd_skills VALUES (:id, :psd)';
  Data := LoadArchive('Skill.wz');
  for Book in Data.Root.Files do
    if TryStrToInt(ChangeFileExt(Book.Name, ''), i) then
      with Data.ParseFile(Book) do
      begin
        for Skill in Root.Child['skill'].Children do
        begin
          if Skill.Name = 'pack_ignore' then
            Continue;

          DQ.ParamByName('id').AsString := Skill.Name;
          s := '';
          if Skill.Get('invisible', 0) = 1 then
            s := 'invisible';
          if Skill.Get('psd', 0) = 1 then
            AddFlag(s, 'psd');
          DQ.ParamByName('flags').AsString := s;
          DQ.ParamByName('weapon').AsInteger := Skill.Get('weapon', -1);
          DQ.ParamByName('subWeapon').AsInteger := Skill.Get('subWeapon', -1);
          case Skill.Get('skillType', -1) of
            1: DQ.ParamByName('type').AsString := 'mastery';
            2: DQ.ParamByName('type').AsString := 'booster';
            3: DQ.ParamByName('type').AsString := 'final_attack';
            4: DQ.ParamByName('type').AsString := 'serial_attack';
            else DQ.ParamByName('type').AsString := 'normal';
          end;
          DQ.ExecSQL;

          if Skill.Child['psdSkill'] <> nil then
          begin
            PsdQ.ParamByName('id').AsString := Skill.Name;
            for Iter in Skill.Child['psdSkill'].Children do
            begin
              PsdQ.ParamByName('psd').AsInteger := StrToInt(Iter.Name);
              PsdQ.ExecSQL;
            end;
          end;

          Q.ParamByName('id').AsString := Skill.Name;
          if Skill.Child['common'] <> nil then
          begin
            MaxLv := Max(Skill.Get('common/maxLevel', 0), Integer(Skill.Get('PVPcommon/maxLevel', 0)));
            if Skill.Get('combatOrders', 0) = 1 then
              Inc(MaxLv, 2);

            for i := 1 to MaxLv do
            begin
              Q.ParamByName('lv').AsInteger := i;
              Level := Skill.Child['common'];
              AddLevelData;
            end;
            Continue;
          end;

          for Level in Skill.Child['level'].Children do
          begin
            Q.ParamByName('lv').AsString := Level.Name;
            AddLevelData;
          end;
        end;
        Free;
      end;

  if Data.Root.Entry['Recipe_9200.img'] <> nil then
    AddProfessionTables(FDB, Data);

  Data.Free;
  Data := TWZArchive.Create(IncludeTrailingPathDelimiter(frmDatabase.edtPath.Text) + 'Character.wz');
  Q.SQL.Text := 'INSERT INTO skill_animation_data VALUES (:id, :ani)';

  i := 0;
  with Data.GetImgFile('00002000.img') do
  begin
    for Skill in Root.Children do
      if (Skill.Name <> 'info') and (Pos('pack_ignore', Skill.Name) = 0) then
      begin
        Q.ParamByName('id').AsString := Skill.Name;
        Q.ParamByName('ani').AsInteger := i;
        Q.ExecSQL;
        Inc(i);
      end;
  end;

  Q.SQL.Text := 'INSERT INTO character_afterimage_data VALUES (:id, :ani, :x1, :y1, :x2, :y2)';

  Infos := TDictionary<string, TPair<PPoint, PPoint>>.Create;
  Check := TList<string>.Create;
  for Book in TWZDirectory(Data.Root.Entry['Afterimage']).Files do
    with Data.ParseFile(Book) do
    begin
      for Iter in Root.Children do
        for Iter2 in Iter.Children do
        begin
          if Infos.ContainsKey(Iter2.Name) then
          begin
            CurLT := Infos[Iter2.Name].Key;
            CurRB := Infos[Iter2.Name].Value;

            if Iter2.Child['lt'] <> nil then
            begin
              P := Iter2.Child['lt'].Vector;
              if (P.X < CurLT^.X) then
                CurLT^.X := P.X;
              if (P.Y < CurLT^.Y) then
                CurLT^.Y := P.Y;
            end;
            if Iter2.Child['rb'] <> nil then
            begin
              P := Iter2.Child['rb'].Vector;
              if (P.X > CurRB^.X) then
                CurRB^.X := P.X;
              if (P.Y > CurRB^.Y) then
                CurRB^.Y := P.Y;
            end;
          end
          else
          begin
            if Assigned(Iter2.Child['lt']) and Assigned(Iter2.Child['rb']) then
            begin
              New(CurLT);
              CurLT^ := TPoint(Iter2.Child['lt'].Vector);
              New(CurRB);
              CurRB^ := TPoint(Iter2.Child['rb'].Vector);
              Infos.Add(Iter2.Name, TPair<PPoint, PPoint>.Create(CurLT, CurRB));
            end;
          end;
        end;

      for s in Infos.Keys do
      begin
        CurLT := Infos[s].Key;
        CurRB := Infos[s].Value;

        Q.ParamByName('ani').AsString := s;
        Q.ParamByName('x1').AsInteger := CurLT.X;
        Q.ParamByName('y1').AsInteger := CurLT.Y;
        Q.ParamByName('x2').AsInteger := CurRB.X;
        Q.ParamByName('y2').AsInteger := CurRB.Y;

        if (Length(s) > 2) and (s[Length(s) - 1] = 'D') then
        begin
          if not Check.Contains('katara ' + s) then
          begin
            Check.Add('katara ' + s);
            Q.ParamByName('id').AsString := 'katara';
            Q.ExecSQL;
          end;
        end
        else if (Pos('PoleArm', s) > 0) or (s = 'doubleSwing') or (s = 'tripleSwing') then
        begin
          if not Check.Contains('aran ' + s) then
          begin
            Check.Add('aran ' + s);
            Q.ParamByName('id').AsString := 'aran';
            Q.ExecSQL;
          end;
        end
        else
        begin
          Q.ParamByName('id').AsString := StringReplace(Book.Name, '.img', '', [rfReplaceAll]);
          Q.ExecSQL;
        end;
      end;

      Infos.Clear;
      Free;
    end;

  Check.Free;
  Infos.Free;
  Data.Free;
  Q.Free;
end;

procedure AddProfessionTables(FDB: TDatabaseConnection; Data: TWZArchive);
var
  Q, ReqQ, TargetQ: TZQuery;
  i: Integer;
  Recipe, Iter: TWZIMGEntry;
begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `profession_recipes` (' +
  '`recipeid` int(11) NOT NULL, ' +
  '`req_skill_level` tinyint(3) NOT NULL, ' +
  '`inc_skill_exp` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`inc_fatigue` tinyint(3) NOT NULL, ' +
  '`req_map_object_tag` int(11) NOT NULL DEFAULT ''0'', ' +
  '`need_open_item` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`recipeid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  ReqQ := FDB.GetQuery;
  ReqQ.SQL.Text := 'CREATE TABLE `profession_required_items` (' +
  '`recipeid` int(11) NOT NULL, ' +
  '`itemid` int(11) NOT NULL, ' +
  '`count` smallint(5) NOT NULL, ' +
  'PRIMARY KEY (`recipeid`, `itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  ReqQ.ExecSQL;

  TargetQ := FDB.GetQuery;
  TargetQ.SQL.Text := 'CREATE TABLE `profession_targets` (' +
  '`recipeid` int(11) NOT NULL, ' +
  '`itemid` int(11) NOT NULL, ' +
  '`count` smallint(5) NOT NULL, ' +
  '`probability` tinyint(3) NOT NULL DEFAULT ''100'', ' +
  'PRIMARY KEY (`recipeid`, `itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  TargetQ.ExecSQL;

  Q.SQL.Text := 'INSERT INTO profession_recipes VALUES (:id, :lv, :iexp, :fatigue, :tag, :noi)';
  ReqQ.SQL.Text := 'INSERT INTO profession_required_items VALUES (:id, :item, :count)';
  TargetQ.SQL.Text := 'INSERT INTO profession_targets VALUES (:id, :item, :count, :prob)';

  for i := 9200 to 9204 do
    with Data.GetImgFile(Format('Recipe_%d.img', [i])) do
    begin
      for Recipe in Root.Children do
      begin
        Q.ParamByName('id').AsString := Recipe.Name;
        Q.ParamByName('lv').AsInteger := Recipe.Get('reqSkillLevel', 0);
        Q.ParamByName('iexp').AsInteger := Recipe.Get('incSkillProficiency', 0);
        Q.ParamByName('fatigue').AsInteger := Recipe.Get('incFatigability', 0);
        Q.ParamByName('tag').AsString := StringReplace(Recipe.Get('reqMapObjectTag', '0'), 'production_', '', [rfReplaceAll]);
        Q.ParamByName('noi').AsInteger := Recipe.Get('needOpenItem', 0);
        Q.ExecSQL;

        for Iter in Recipe.Child['recipe'].Children do
        begin
          ReqQ.ParamByName('id').AsString := Recipe.Name;
          ReqQ.ParamByName('item').AsInteger := Iter.Get('item', 0);
          ReqQ.ParamByName('count').AsInteger := Iter.Get('count', 0);
          ReqQ.ExecSQL;
        end;

        for Iter in Recipe.Child['target'].Children do
        begin
          TargetQ.ParamByName('id').AsString := Recipe.Name;
          TargetQ.ParamByName('item').AsInteger := Iter.Get('item', 0);
          TargetQ.ParamByName('count').AsInteger := Iter.Get('count', 0);
          TargetQ.ParamByName('prob').AsInteger := Iter.Get('probWeight', 100);
          try
            TargetQ.ExecSQL;
          except
            // Duplicate item...
          end;
        end;
      end;

      Free;
    end;

  Q.Free;
  ReqQ.Free;
  TargetQ.Free;
end;

end.
