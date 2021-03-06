unit Life;

interface

uses SysUtils, Dialogs,
     DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile, ZDataset;

type
  TElementModifiers = record
    Ice, Fire, Poison, Lightning, Holy, Darkness, NonElemental: AnsiString;
  end;

procedure AddNPCTable(FDB: TDatabaseConnection);
procedure AddMobTables(FDB: TDatabaseConnection);
procedure AddReactorTables(FDB: TDatabaseConnection);

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
      Q.ParamByName('id').AsInteger := NoIMG(NPC.Name);
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

function DecodeMobElementAttr(const Attr: string): TElementModifiers;
const
  Effectiveness: array[0..3] of AnsiString = ('normal', 'immune', 'strong', 'weak');
var
  i: Integer;
begin
  // Initialize with "normal"
  Result.Ice := Effectiveness[0];
  Result.Fire := Effectiveness[0];
  Result.Poison := Effectiveness[0];
  Result.Lightning := Effectiveness[0];
  Result.Holy := Effectiveness[0];
  Result.NonElemental := Effectiveness[0];
  Result.Darkness := Effectiveness[0];

  if Attr = '' then
    Exit;

  for i := 1 to Length(Attr) div 2 do
  begin
    if Attr[i * 2 - 1] = 'I' then
      Result.Ice := Effectiveness[StrToInt(Attr[i * 2])]
    else if Attr[i * 2 - 1] = 'F' then
      Result.Fire := Effectiveness[StrToInt(Attr[i * 2])]
    else if Attr[i * 2 - 1] = 'S' then
      Result.Poison := Effectiveness[StrToInt(Attr[i * 2])]
    else if Attr[i * 2 - 1] = 'L' then
      Result.Lightning := Effectiveness[StrToInt(Attr[i * 2])]
    else if Attr[i * 2 - 1] = 'H' then
      Result.Holy := Effectiveness[StrToInt(Attr[i * 2])]
    else if Attr[i * 2 - 1] = 'P' then
      Result.NonElemental := Effectiveness[StrToInt(Attr[i * 2])]
    else if Attr[i * 2 - 1] = 'D' then
      Result.Darkness := Effectiveness[StrToInt(Attr[i * 2])]
    else
      ShowMessage('WARNING: Unknown ElemAttr: ' + Attr);
  end;
end;

function GetMobFlagData(Info: TWZIMGEntry): string;
begin
  Result := '';

  if Info.Get('boss', 0) = 1 then
    AddFlag(Result, 'boss');

  if Info.Get('firstAttack', 0) = 1 then
    AddFlag(Result, 'auto_aggro');

  if Info.Get('undead', 0) = 1 then
    AddFlag(Result, 'undead');

  if Info.Get('flySpeed', MAXINT) <> MAXINT then
    AddFlag(Result, 'flying');

  if Info.Get('publicReward', 0) = 1 then
    AddFlag(Result, 'public_reward');

  if Info.Get('explosiveReward', 0) = 1 then
    AddFlag(Result, 'explosive_reward');

  if Info.Get('bodyAttack', 1) = 0 then
  begin
    AddFlag(Result, 'cannot_damage_player');
    AddFlag(Result, 'player_cannot_damage');
  end;

  if Info.Get('notAttack', 0) = 1 then
    AddFlag(Result, 'cannot_damage_player');

  if Info.Get('invincible', 0) = 1 then
    AddFlag(Result, 'invincible');

  if Info.Get('damagedByMob', 0) = 1 then
    AddFlag(Result, 'friendly');

  if Info.Get('doNotRemove', 0) = 1 then
    AddFlag(Result, 'no_remove_on_death');

  if Info.Get('onlyNormalAttack', 0) = 1 then
    AddFlag(Result, 'damaged_by_normal_attacks_only');

  if Info.Get('partyBonusMob', 0) = 1 then
    AddFlag(Result, 'party_bonus_mob');
end;

procedure AddMobTables(FDB: TDatabaseConnection);
var
  BanQ, MobQ, AttQ, SkillQ, SumQ: TZQuery;
  Data: TWZArchive;
  Mob: TWZFile;
  MobInfo, MobData, SInfo, S: TWZIMGEntry;
  ElemData: TElementModifiers;
  F: string;
begin
  BanQ := FDB.GetQuery;
  BanQ.SQL.Text := 'CREATE TABLE skill_mob_banish_data (' +
  '`mobid` int(11) NOT NULL, ' +
  '`message` varchar(200) NOT NULL DEFAULT '''', ' +
  '`destination` int(11) NOT NULL DEFAULT ''-1'', ' +
  '`portal` varchar(20) NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`mobid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  BanQ.ExecSQL;

  MobQ := FDB.GetQuery;
  MobQ.SQL.Text := 'CREATE TABLE mob_data ( ' +
  '`mobid` int(11) NOT NULL, ' +
  '`mob_level` smallint(6) NOT NULL, ' +
  '`flags` set(''boss'',''undead'',''flying'',''friendly'',''public_reward'',''explosive_reward'',''invincible'',''auto_aggro'',''damaged_by_normal_attacks_only'',''no_remove_on_death'',''cannot_damage_player'',''player_cannot_damage'',''party_bonus_mob'') NOT NULL DEFAULT '''', ' +
  '`hp` int(11) unsigned NOT NULL, ' +
  '`final_hp` bigint(20) unsigned NOT NULL DEFAULT ''0'', ' +
  '`mp` int(11) unsigned NOT NULL, ' +
  '`hp_recovery` int(11) unsigned NOT NULL DEFAULT ''0'' COMMENT ''Mob regains this amount of HP every 10 seconds.'', ' +
  '`mp_recovery` int(11) unsigned NOT NULL DEFAULT ''0'' COMMENT ''Mob regains this amount of MP every 10 seconds.'', ' +
  '`explode_hp` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Mob blows up if mob does not die and has less HP than indicated.'', ' +
  '`experience` int(11) unsigned NOT NULL DEFAULT ''0'', ' +
  '`link` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Mob takes attack data from another mob. Not to be confused with skill data.'', ' +
  '`summon_type` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`knockback` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Amount of damage it takes to knock a monster back.'', ' +
  '`fixed_damage` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Mob only takes this amount of damage from any attack.'', ' +
  '`death_buff` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Mob "uses" this item ID on all players on the map when it dies.'', ' +
  '`death_after` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Mob automatically dies after this number of seconds.'', ' +
  '`traction` double(3,1) NOT NULL DEFAULT ''0.0'', ' +
  '`damaged_by_skill_only` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Mob is only damaged by a specific skill.'', ' +
  '`damaged_by_mob_only` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Friendly mob is only damaged by a specific mob.'', ' +
  '`hp_bar_color` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`hp_bar_bg_color` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`carnival_points` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`physical_attack` int(11) NOT NULL DEFAULT ''0'', ' +
  '`physical_defense` int(11) NOT NULL DEFAULT ''0'', ' +
  '`magical_attack` int(11) NOT NULL DEFAULT ''0'', ' +
  '`magical_defense` int(11) NOT NULL DEFAULT ''0'', ' +
  '`accuracy` int(11) NOT NULL DEFAULT ''0'', ' +
  '`avoidability` int(11) NOT NULL DEFAULT ''0'', ' +
  '`speed` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`chase_speed` smallint(6) NOT NULL DEFAULT ''0'' COMMENT ''Changes the speed of a mob when aggro''''d.'', ' +
  '`ice_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  '`fire_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  '`poison_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  '`lightning_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  '`holy_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  '`darkness_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  '`nonelemental_modifier` enum(''normal'',''immune'',''strong'',''weak'') NOT NULL, ' +
  'PRIMARY KEY (`mobid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  MobQ.ExecSQL;

  AttQ := FDB.GetQuery;
  AttQ.SQL.Text := 'CREATE TABLE `mob_attacks` ( ' +
  '`mobid` int(11) NOT NULL, ' +
  '`attackid` smallint(6) NOT NULL, ' +
  '`mp_cost` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`mp_burn` int(11) NOT NULL DEFAULT ''0'',' +
  '`mob_skillid` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`mob_skill_level` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`after_attack` smallint(6) NOT NULL DEFAULT ''-1'', ' +
  '`after_attack_count` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''deadly'',''only_after_attack'') NOT NULL DEFAULT '''' COMMENT ''''''deadly'''' is used for 1/1 attacks. Will bring both your HP and MP down to 1 from whatever they were at.'', ' +
  'PRIMARY KEY (`mobid`,`attackid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  AttQ.ExecSQL;

  SkillQ := FDB.GetQuery;
  SkillQ.SQL.Text := 'CREATE TABLE `mob_skills` ( ' +
  '`id` bigint(21) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`mobid` int(11) NOT NULL, ' +
  '`skillid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`skill_level` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`pre_skill_index` smallint(6) NOT NULL DEFAULT ''-1'', ' +
  '`pre_skill_count` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`after_attack` smallint(6) NOT NULL DEFAULT ''-1'', ' +
  '`after_attack_count` smallint(6) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`id`)' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  SkillQ.ExecSQL;

  SumQ := FDB.GetQuery;
  SumQ.SQL.Text := 'CREATE TABLE `mob_summons` ( ' +
  '`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`mobid` int(11) NOT NULL, ' +
  '`summonid` int(11) NOT NULL, ' +
  'PRIMARY KEY (`id`), ' +
  'KEY `mobid` (`mobid`) ' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  SumQ.ExecSQL;

  Data := LoadArchive('Mob.wz');
  BanQ.SQL.Text := 'INSERT INTO skill_mob_banish_data VALUES (:id, :msg, :dst, :prt)';
  MobQ.SQL.Text := 'INSERT INTO mob_data VALUES (:id, :lvl, :flags, :hp, :fhp, :mp, :hpr, ' +
  ':mpr, :ehp, :exp, :lnk, :st, :kb, :fd, :db, :da, :traction, :dbs, :dbm, :hpc, ' +
  ':hpbgc, :cp, :patt, :pdef, :matt, :mdef, :acc, :avo, :spd, :cspd, :imod, :fmod, ' +
  ':pmod, :lmod, :hmod, :dmod, :nmod)';
  AttQ.SQL.Text := 'INSERT INTO mob_attacks VALUES (:id, :aid, :mpc, :mpb, :sid, :lvl, :aa, :aac, :flags)';
  SkillQ.SQL.Text := 'INSERT INTO mob_skills VALUES (DEFAULT, :id, :sid, :lvl, :ps, :psc, :aa, :aac)';
  SumQ.SQL.Text := 'INSERT INTO mob_summons VALUES (DEFAULT, :id, :sid)';

  for Mob in Data.Root.Files do
  begin
    with Data.ParseFile(Mob) do
    begin
      MobInfo := Root.Child['info'];

      if not Assigned(MobInfo) then
      begin
        ShowMessage('WARNING: No mob-info for: ' + Mob.Name);
        Continue;
      end;

      MobData := MobInfo.Child['ban'];
      if Assigned(MobData) then
      begin
        BanQ.ParamByName('id').AsInteger := NoIMG(Mob.Name);
        BanQ.ParamByName('msg').AsString := DBStr(MobData.Get('banMsg', ''));
        BanQ.ParamByName('dst').AsInteger := MobData.Get('banMap/0/field', -1);
        BanQ.ParamByName('prt').AsString := MobData.Get('banMap/0/portal', '');
        try
          BanQ.ExecSQL;
        except
          // Nexon Europe: Let's put banMsg into portal instead -____-
        end;
      end;

      MobData := MobInfo.Child['skill'];
      if Assigned(MobData) then
      begin
        SkillQ.ParamByName('id').AsInteger := NoIMG(Mob.Name);

        for SInfo in MobData.Children do
        begin
          SkillQ.ParamByName('sid').AsInteger := SInfo.Get('skill', 0);
          SkillQ.ParamByName('lvl').AsInteger := SInfo.Get('level', 0);
          SkillQ.ParamByName('ps').AsInteger := SInfo.Get('preSkillIndex', -1);
          SkillQ.ParamByName('psc').AsInteger := SInfo.Get('preSkillCount', 0);
          SkillQ.ParamByName('aa').AsInteger := SInfo.Get('afterAttack', -1);
          SkillQ.ParamByName('aac').AsInteger := SInfo.Get('afterAttackCount', 0);
          SkillQ.ExecSQL;
        end;
      end;

      MobData := MobInfo.Child['revive'];
      if Assigned(MobData) then
      begin
        SumQ.ParamByName('id').AsInteger := NoIMG(Mob.Name);

        for SInfo in MobData.Children do
        begin
          SumQ.ParamByName('sid').AsInteger := SInfo.Data;
          SumQ.ExecSQL;
        end;
      end;

      with MobQ do
      begin
        ParamByName('id').AsInteger := NoIMG(Mob.Name);
        ParamByName('lvl').AsInteger := MobInfo.Get('level', 0);
        ParamByName('flags').AsString := GetMobFlagData(MobInfo);
        S := MobInfo.Get('maxHP');
        if (S <> nil) and ((S.DataType <> mdtString) or ((S.Data) <> '?')) then
          ParamByName('hp').AsInteger := MobInfo.Get('maxHP', 0)
        else
          ParamByName('hp').AsInteger := 0;
        try
          ParamByName('fhp').AsLargeInt := MobInfo.Get('finalmaxHP', 0);
        except
          ParamByName('fhp').AsInteger := 0; // random question mark
        end;
        ParamByName('mp').AsInteger := MobInfo.Get('maxMP', 0);
        ParamByName('hpr').AsInteger := MobInfo.Get('hpRecovery', 0);
        ParamByName('mpr').AsInteger := MobInfo.Get('mpRecovery', 0);
        ParamByName('ehp').AsInteger := MobInfo.Get('selfDestruction/hp', 0);
        ParamByName('exp').AsInteger := MobInfo.Get('exp', 0);
        ParamByName('lnk').AsInteger := MobInfo.Get('link', 0);
        ParamByName('st').AsInteger := MobInfo.Get('summonType', 0);
        try
          ParamByName('kb').AsInteger := MobInfo.Get('pushed', 0);
        except
          ParamByName('kb').AsInteger := 0; // europe...
        end;
        ParamByName('fd').AsInteger := MobInfo.Get('fixedDamage', 0);
        if (MobInfo.Get('buff') <> nil) and (MobInfo.Get('buff').DataType <> mdtProperty) then
          ParamByName('db').AsInteger := MobInfo.Get('buff', 0)
        else  // They're just using the same name for something else -.-
          ParamByName('db').AsInteger := 0;
        ParamByName('da').AsInteger := MobInfo.Get('removeAfter', 0);
        ParamByName('traction').AsFloat := MobInfo.Get('fs', 10.0);
        ParamByName('dbs').AsInteger := MobInfo.Get('damagedBySelectedSkill/0', 0);
        ParamByName('dbm').AsInteger := MobInfo.Get('damagedBySelectedMob/0', 0);
        ParamByName('hpc').AsInteger := MobInfo.Get('hpTagColor', 0);
        ParamByName('hpbgc').AsInteger := MobInfo.Get('hpTagBgcolor', 0);
        ParamByName('cp').AsInteger := MobInfo.Get('getCP', 0);
        ParamByName('patt').AsInteger := MobInfo.Get('PADamage', 0);
        ParamByName('pdef').AsInteger := MobInfo.Get('PDDamage', 0);
        ParamByName('matt').AsInteger := MobInfo.Get('MADamage', 0);
        ParamByName('mdef').AsInteger := MobInfo.Get('MDDamage', 0);
        ParamByName('acc').AsInteger := MobInfo.Get('acc', 0);
        ParamByName('avo').AsInteger := MobInfo.Get('eva', 0);
        ParamByName('spd').AsInteger := MobInfo.Get('speed', 0);
        ParamByName('cspd').AsInteger := MobInfo.Get('chaseSpeed', MobInfo.Get('speed', 0));

        ElemData := DecodeMobElementAttr(MobInfo.Get('elemAttr', ''));
        {$WARNINGS OFF}
        ParamByName('imod').AsString := ElemData.Ice;
        ParamByName('fmod').AsString := ElemData.Fire;
        ParamByName('pmod').AsString := ElemData.Poison;
        ParamByName('lmod').AsString := ElemData.Lightning;
        ParamByName('hmod').AsString := ElemData.Holy;
        ParamByName('dmod').AsString := ElemData.Darkness;
        ParamByName('nmod').AsString := ElemData.NonElemental;
        {$WARNINGS ON}

        ExecSQL;
      end;

      if MobInfo.Child['attack'] = nil then
      begin
        for MobInfo in Root.Children do
          if (Pos('attack', MobInfo.Name) = 1) and (Length(MobInfo.Name) in [7, 8]) then
          begin
            SInfo := MobInfo.Child['info'];

            if not Assigned(SInfo) then
            begin
              //Writeln('WARNING: No attack-info for mob ' + Mob.Name);
              Continue;
            end;

            AttQ.ParamByName('id').AsInteger := NoIMG(Mob.Name);
            AttQ.ParamByName('aid').AsInteger := StrToInt(Copy(MobInfo.Name, 7, 2)) - 1;
            AttQ.ParamByName('mpc').AsInteger := SInfo.Get('conMP', 0);
            AttQ.ParamByName('mpb').AsInteger := SInfo.Get('mpBurn',0);
            AttQ.ParamByName('sid').AsInteger := SInfo.Get('disease', 0);
            if AttQ.ParamByName('sid').AsInteger = 0 then
              AttQ.ParamByName('sid').AsInteger := SInfo.Get('desease', 0);  // those noobs spelled disease incorrectly for one mob :O
            AttQ.ParamByName('lvl').AsInteger := SInfo.Get('level', 0);
            AttQ.ParamByName('aa').AsInteger := -1;
            AttQ.ParamByName('aac').AsInteger := 0;
            if SInfo.Get('deadlyAttack', 0) <> 0 then
              AttQ.ParamByName('flags').AsString := 'deadly'
            else
              AttQ.ParamByName('flags').AsString := '';

            AttQ.ExecSQL;
          end;
      end
      else   // info/attack, since v104
      begin
        AttQ.ParamByName('id').AsInteger := NoIMG(Mob.Name);
        for SInfo in MobInfo.Child['attack'].Children do
        begin
          if not CharInSet(SInfo.Name[1], ['0'..'9']) then
            Continue; // 'eva' value or structure fuckups
          AttQ.ParamByName('aid').AsString := SInfo.Name;
          AttQ.ParamByName('mpc').AsInteger := SInfo.Get('conMP', 0);
          AttQ.ParamByName('mpb').AsInteger := SInfo.Get('mpBurn',0);
          AttQ.ParamByName('sid').AsInteger := SInfo.Get('disease', 0);
          AttQ.ParamByName('lvl').AsInteger := SInfo.Get('level', 0);
          AttQ.ParamByName('aa').AsInteger := SInfo.Get('afterAttack', -1);
          AttQ.ParamByName('aac').AsInteger := SInfo.Get('afterAttackCount', 0);

          F := '';
          if SInfo.Get('deadlyAttack', 0) <> 0 then
            AddFlag(F, 'deadly');
          if SInfo.Get('onlyAfterAttack', 0) <> 0 then
            AddFlag(F, 'only_after_attack');
          AttQ.ParamByName('flags').AsString := F;

          AttQ.ExecSQL;
        end;
      end;
      Free;
    end;
  end;

  Data.Free;

  BanQ.Free;
  MobQ.Free;
  AttQ.Free;
  SkillQ.Free;
  SumQ.Free;
end;

procedure AddReactorTables(FDB: TDatabaseConnection);
var
  Q, EQ: TZQuery;
  Data: TWZArchive;
  Reactor: TWZFile;
  Ev: TWZImgEntry;
  i, c: Integer;
  Flags: string;
begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE  `reactor_data` (' +
  '`reactorid` int(11) NOT NULL, ' +
  '`max_states` tinyint(3) NOT NULL, ' +
  '`link` int(11) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''activate_by_touch'',''remove_in_fieldset'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`reactorid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Defines reactor information.'';';
  Q.ExecSQL;

  EQ := FDB.GetQuery;
  EQ.SQL.Text := 'CREATE TABLE `reactor_events` (' +
  '`reactorid` int(11) NOT NULL, ' +
  '`state` tinyint(3) NOT NULL, ' +
  '`event_type` enum(''none'',''plain_advance_state'',''hit_from_left'',''hit_from_right'',''hit_by_skill'',''type6'',''type7'',''harvest'',''click'',''touch_by_mob'',''hit_by_item'',''reset'') NOT NULL, ' +
  '`timeout` int(11) NOT NULL DEFAULT ''0'', ' +
  '`itemid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`quantity` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`ltx` smallint(6) NOT NULL, ' +
  '`lty` smallint(6) NOT NULL, ' +
  '`rbx` smallint(6) NOT NULL, ' +
  '`rby` smallint(6) NOT NULL, ' +
  '`next_state` tinyint(3) NOT NULL, ' +
  'PRIMARY KEY (`reactorid`,`state`,`next_state`), ' +
  'KEY `reactorid` (`reactorid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Defines player-reactor interactions and state advancement.'';';
  EQ.ExecSQL;

  Q.SQL.Text := 'INSERT INTO reactor_data VALUES (:id, :count, :link, :flags);';
  EQ.SQL.Text := 'INSERT INTO reactor_events VALUES (:id, :st, :type, :timeout, :item, :qty, :ltx, :lty, :rbx, :rby, :nst);';

  Data := LoadArchive('Reactor.wz');
  for Reactor in Data.Root.Files do
    with Data.ParseFile(Reactor) do
    begin
      Q.ParamByName('id').AsInteger := NoIMG(Reactor.Name);
      c := 0;
      for i := 0 to Root.Children.Count - 1 do
        if CharInSet(Root.Children[i].Name[1], ['0'..'9']) then
          Inc(c);
      Q.ParamByName('count').AsInteger := c;
      Q.ParamByName('link').AsInteger := Root.Get('info/link', 0);
      Flags := '';
      if Root.Get('info/removeInFieldSet', 0) = 1 then
        AddFlag(Flags, 'remove_in_fieldset');
      if Root.Get('info/activateByTouch', 0) = 1 then
        AddFlag(Flags, 'activate_by_touch');
      Q.ParamByName('flags').AsString := Flags;
      Q.ExecSQL;

      EQ.ParamByName('id').AsInteger := NoIMG(Reactor.Name);
      for i := 0 to c - 1 do
      begin
        EQ.ParamByName('st').AsInteger := i;
        Ev := Root.Child[IntToStr(i)];
        if Ev = nil then
          Continue;  // sucky reactors suck

        if Ev.Child['event'] = nil then
        begin
          EQ.ParamByName('type').AsString := 'none';
          EQ.ParamByName('timeout').AsInteger := 0;
          EQ.ParamByName('item').AsInteger := 0;
          EQ.ParamByName('qty').AsInteger := 0;
          EQ.ParamByName('ltx').AsInteger := 0;
          EQ.ParamByName('lty').AsInteger := 0;
          EQ.ParamByName('rbx').AsInteger := 0;
          EQ.ParamByName('rby').AsInteger := 0;
          if i < c - 1 then
            EQ.ParamByName('nst').AsInteger := i + 1
          else
            EQ.ParamByName('nst').AsInteger := 0;
        end
        else
          with Ev.Child['event'].Child['0'] do
          begin
            case Get('type', 0) of
              0: EQ.ParamByName('type').AsString := 'plain_advance_state';
              1: EQ.ParamByName('type').AsString := 'hit_from_left';
              2: EQ.ParamByName('type').AsString := 'hit_from_right';
              5: EQ.ParamByName('type').AsString := 'hit_by_skill';
              6: EQ.ParamByName('type').AsString := 'type6';
              7: EQ.ParamByName('type').AsString := 'type7';
              8: EQ.ParamByName('type').AsString := 'harvest';
              9: EQ.ParamByName('type').AsString := 'click';
              10: EQ.ParamByName('type').AsString := 'touch_by_mob';
              100: EQ.ParamByName('type').AsString := 'hit_by_item';
              101: EQ.ParamByName('type').AsString := 'reset';
              else Writeln(Format('Unknown type at %s/%d: %d', [Reactor.Name, i, Integer(Get('type', 0))]));
            end;
            EQ.ParamByName('timeout').AsInteger := Ev.Get('event/timeOut', 0);
            EQ.ParamByName('item').AsInteger := Get('0', 0);
            EQ.ParamByName('qty').AsInteger := Get('1', 0);
            if Child['lt'] <> nil then
            begin
              EQ.ParamByName('ltx').AsInteger := Child['lt'].Vector.X;
              EQ.ParamByName('lty').AsInteger := Child['lt'].Vector.Y;
              EQ.ParamByName('rbx').AsInteger := Child['rb'].Vector.X;
              EQ.ParamByName('rby').AsInteger := Child['rb'].Vector.Y;
            end
            else
            begin
              EQ.ParamByName('ltx').AsInteger := 0;
              EQ.ParamByName('lty').AsInteger := 0;
              EQ.ParamByName('rbx').AsInteger := 0;
              EQ.ParamByName('rby').AsInteger := 0;
            end;
            EQ.ParamByName('nst').AsInteger := Get('state', 0);
          end;

        EQ.ExecSQL;
      end;

      Free;
    end;

  Data.Free;
  Q.Free;
  EQ.Free;
end;

end.
