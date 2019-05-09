unit Maps;

interface

uses SysUtils, DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile, ZDataset,
     Generics.Collections;

procedure AddMapTables(FDB: TDatabaseConnection);

procedure CreateEscort(DB: TDatabaseConnection);
procedure AddEscortData(Info: TWZIMGEntry; DB: TDatabaseConnection; MapID: Integer);

implementation

function GetMapFlags(const Info: TWZIMGEntry): string;
begin
  Result := '';

  if Info.Get('town', 0) = 1 then
    AddFlag(Result, 'town');

  if TWZIMGEntry(Info.Parent).Child['clock'] <> nil then
    AddFlag(Result, 'clock');

  if Info.Get('swim', 0) = 1 then
    AddFlag(Result, 'swim');

  if Info.Get('fly', 0) = 1 then
    AddFlag(Result, 'fly');

  if Info.Get('everlast', 0) = 1 then
    AddFlag(Result, 'everlast');

  if Info.Get('blockPBossChange', 0) = 1 then
    AddFlag(Result, 'no_party_leader_pass');

  if Info.Get('personalShop', 0) = 1 then
    AddFlag(Result, 'shop');

  if Info.Get('scrollDisable', 0) = 1 then
    AddFlag(Result, 'scroll_disable');

  if Info.Get('reactorShuffle', 0) = 1 then
    AddFlag(Result, 'shuffle_reactors');

  if Info.Get('standAlone', 0) = 1 then
    AddFlag(Result, 'standalone');

  if Info.Get('partyStandAlone', 0) = 1 then
    AddFlag(Result, 'party_standalone');

  if Info.Get('noMapCmd', 0) = 1 then
    AddFlag(Result, 'no_commands');
end;

function GetMapLimits(Mask: Integer): string;
begin
  Result := '';
  if Mask = 0 then
    Exit;

  if Mask and 1 > 0 then
    AddFlag(Result, 'jump');
  if Mask and 2 > 0 then
    AddFlag(Result, 'movement_skills');
  if Mask and 4 > 0 then
    AddFlag(Result, 'summoning_bag');
  if Mask and 8 > 0 then
    AddFlag(Result, 'mystic_door');
  if Mask and $10 > 0 then
    AddFlag(Result, 'channel_switching');
  if Mask and $20 > 0 then
    AddFlag(Result, 'return_scroll');
  if Mask and $40 > 0 then
    AddFlag(Result, 'vip_rock');
  if Mask and $80 > 0 then
    AddFlag(Result, 'minigames');
  if Mask and $100 > 0 then
    AddFlag(Result, 'town_scroll');
  if Mask and $200 > 0 then
    AddFlag(Result, 'mount');
  if Mask and $400 > 0 then
    AddFlag(Result, 'potion_use');
  if Mask and $800 > 0 then
    AddFlag(Result, 'party_leader_pass');
  if Mask and $1000 > 0 then
    AddFlag(Result, 'mob_capacity');
  if Mask and $2000 > 0 then
    AddFlag(Result, 'wedding_invitation');
  if Mask and $4000 > 0 then
    AddFlag(Result, 'cash_weather');
  if Mask and $8000 > 0 then
    AddFlag(Result, 'pet');
  if Mask and $10000 > 0 then
    AddFlag(Result, 'anti_macro');
  if Mask and $20000 > 0 then
    AddFlag(Result, 'drop_down');
  if Mask and $40000 > 0 then
    AddFlag(Result, 'summon_npc');
  if Mask and $80000 > 0 then
    AddFlag(Result, 'regular_exp_loss');
  if Mask and $100000 > 0 then
    AddFlag(Result, 'fall_damage');
  if Mask and $200000 > 0 then
    AddFlag(Result, 'package_delivery');
  if Mask and $400000 > 0 then
    AddFlag(Result, 'chalkboard');
end;

const
  FIELDTYPE: array[1..31] of string = (
    'snowball', 'station', 'minigame_challenge', 'coconut', 'ox_quiz', 'time_limit',
    'waiting_room', 'guild_boss', 'limited_view', 'monster_carnival', 'monster_carnival_revive',
    'zakum', 'ariant_arena', 'dojo', 'monster_carnival2', 'monster_carnival_waiting_room',
    'cookie_house', 'balrog', 'wolf_and_sheep', 'space_gaga', 'witch_tower', 'tutorial',
    'survival', 'survival_result', 'partyraid', 'partyraid_boss', 'partyraid_result',
    'hide_dragon', 'dynamic_foothold', 'escort', 'escort_result');

  FIELDTYPE2: array[1000..1002] of string = ('protect_snowman', 'force_map_equip', 'tutorial2');

function GetMapType(T: Integer): string;
begin
  Result := '';
  if T = 0 then
    Exit;

  if T < Length(FIELDTYPE) then
    Result := FIELDTYPE[T]
  else if (T >= Low(FIELDTYPE2)) and (T <= High(FIELDTYPE2)) then
    Result := FIELDTYPE2[T];
end;

function GetMapTypeSQLStr: string;
var
  i: Integer;
begin
  Result := '''' + FIELDTYPE[1] + '''';
  for i := 2 to High(FIELDTYPE) do
    Result := Result + ',''' + FIELDTYPE[i] + '''';
  for i := Low(FIELDTYPE2) to High(FIELDTYPE2) do
    Result := Result + ',''' + FIELDTYPE2[i] + '''';
end;

procedure AddMapTables(FDB: TDatabaseConnection);
var
  MapQ, LifeQ, ScriptQ, FHQ, PortalQ, TimemobQ, StatQ: TZQuery;
  Data, Strings: TWZArchive;
  ContinentDirs, MapDir: TWZDirectory;
  Map: TWZFile;
  Life, Iter, Iter2, Iter3: TWZIMGEntry;
  MobNames, NPCNames: TWZIMGFile;
  Script, Flags: string;
begin
  MapQ := FDB.GetQuery;
  MapQ.SQL.Text := 'CREATE TABLE `map_data` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`flags` set(''town'',''clock'',''swim'',''fly'',''everlast'',''no_party_leader_pass'',''shop'',''scroll_disable'',''shuffle_reactors'',''standalone'',''party_standalone'',''no_commands'') NOT NULL DEFAULT '''', ' +
  '`shuffle_name` varchar(20) NOT NULL DEFAULT '''' COMMENT ''This map shuffles only a specific reactor name.'', ' +
  '`default_bgm` varchar(40) NOT NULL DEFAULT '''' COMMENT ''Background music.'', ' +
  '`min_level_limit` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`time_limit` int(11) NOT NULL DEFAULT ''0'' COMMENT ''The player may only stay on the map for this amount of time.'', ' +
  '`regen_rate` tinyint(3) NOT NULL DEFAULT ''1'' COMMENT ''The natural multiplier for the HP/MP a player gets every 10 seconds.'', ' +
  '`default_traction` double(20,15) NOT NULL DEFAULT ''1.000000000000000'' COMMENT ''Determines if a player "sticks" to the map when moving around.'', ' +
  '`map_ltx` smallint(6) NOT NULL DEFAULT ''0'' COMMENT ''Left top map dimension.'', ' +
  '`map_lty` smallint(6) NOT NULL DEFAULT ''0'' COMMENT ''Left top map dimension.'', ' +
  '`map_rbx` smallint(6) NOT NULL DEFAULT ''0'' COMMENT ''Right bottom map dimension.'', ' +
  '`map_rby` smallint(6) NOT NULL DEFAULT ''0'' COMMENT ''Right bottom map dimension.'', ' +
  '`return_map` int(11) NOT NULL DEFAULT ''999999999'' COMMENT ''Map that the player returns to upon using a return scroll or dies - typically a town.'', ' +
  '`forced_return_map` int(11) NOT NULL DEFAULT ''999999999'' COMMENT ''Where you go when you log out of a map with this property.'', ' +
  '`field_type` enum(' + GetMapTypeSQLStr + '), ' +
  '`field_limitations` set(''jump'',''movement_skills'',''summoning_bag'',''mystic_door'',''channel_switching'',''return_scroll'',''vip_rock'',''minigames'',' +
   '''town_scroll'',''mount'',''potion_use'',''party_leader_pass'',''mob_capacity'',''wedding_invitation'',''cash_weather'',''pet'',''anti_macro'',''drop_down'',' +
   '''summon_npc'',''regular_exp_loss'',''fall_damage'',''package_delivery'',''chalkboard'') ' +
  'NOT NULL COMMENT ''A series of flags that prevents the player from doing actions they normally can.'', ' +
  '`decrease_hp` tinyint(3) unsigned NOT NULL DEFAULT ''0'' COMMENT ''Does decrease_hp damage every decrease_hp seconds.'', ' +
  '`damage_per_second` smallint(6) unsigned NOT NULL DEFAULT ''0'', ' +
  '`protect_item` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Stops both decrease_hp and damage_per_second damage.'', ' +
  '`ship_kind` tinyint(3) NOT NULL DEFAULT ''-1'' COMMENT ''Specifies the type of ship that a map has.'', ' +
  '`mob_rate` double(13,11) NOT NULL DEFAULT ''0.00000000000'' COMMENT ''Affects spawn rate in some way, I think.'', ' +
  '`link` int(11) NOT NULL DEFAULT ''0'' COMMENT ''Map takes all portals/spawns/etc. from another map.'', ' +
  '`party_bonus_rate` smallint(6) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`mapid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  MapQ.ExecSQL;

  LifeQ := FDB.GetQuery;
  LifeQ.SQL.Text := 'CREATE TABLE `map_life` (' +
  '`id` bigint(21) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`mapid` int(11) NOT NULL, ' +
  '`life_type` enum(''npc'',''mob'',''reactor'') NOT NULL, ' +
  '`lifeid` int(11) NOT NULL, ' +
  '`life_name` varchar(50) DEFAULT NULL COMMENT ''For reactors, specifies a handle so scripts may interact with them; for NPC/mob, this field is useless'', ' +
  '`x_pos` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`y_pos` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`foothold` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`min_click_pos` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`max_click_pos` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`respawn_time` int(11) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''faces_left'',''hide'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`id`), ' +
  'KEY `lifetype` (`mapid`,`life_type`) ' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  LifeQ.ExecSQL;

  FHQ := FDB.GetQuery;
  FHQ.SQL.Text := 'CREATE TABLE `map_footholds` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`id` int(11) NOT NULL, ' +
  '`x1` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`y1` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`x2` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`y2` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`drag_force` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`previousid` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`nextid` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''forbid_downward_jump'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`mapid`,`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  FHQ.ExecSQL;

  PortalQ := FDB.GetQuery;
  PortalQ.SQL.Text := 'CREATE TABLE `map_portals` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`id` int(11) NOT NULL, ' +
  '`label` varchar(20) DEFAULT NULL, ' +
  '`x_pos` smallint(6) NOT NULL, ' +
  '`y_pos` smallint(6) NOT NULL, ' +
  '`destination` int(11) NOT NULL, ' +
  '`destination_label` varchar(20) DEFAULT NULL, ' +
  '`script` varchar(30) DEFAULT NULL, ' +
  '`portal_type` enum(''changeable'',''changeable_invisible'',''townportal_point''), ' +
  'PRIMARY KEY (`mapid`,`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  PortalQ.ExecSQL;

  TimemobQ := FDB.GetQuery;
  TimemobQ.SQL.Text := 'CREATE TABLE `map_time_mob` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`mobid` int(11) NOT NULL, ' +
  '`x_pos` smallint(6) NOT NULL, ' +
  '`y_pos` smallint(6) NOT NULL, ' +
  '`interval` smallint(6) NOT NULL, ' +
  '`message` varchar(120) NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`mapid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  TimemobQ.ExecSQL;
  TimemobQ.Free;

  StatQ := FDB.GetQuery;
  StatQ.SQL.Text := 'CREATE TABLE `map_forced_stats` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`job_category` smallint(6) NOT NULL, ' +
  '`strength` smallint(6) NOT NULL, ' +
  '`dexterity` smallint(6) NOT NULL, ' +
  '`intelligence` smallint(6) NOT NULL, ' +
  '`luck` smallint(6) NOT NULL, ' +
  '`weapon_attack` smallint(6) NOT NULL, ' +
  '`magic_attack` smallint(6) NOT NULL, ' +
  '`accuracy` smallint(6) NOT NULL, ' +
  '`avoidability` smallint(6) NOT NULL, ' +
  '`jump` smallint(6) NOT NULL, ' +
  '`speed` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`mapid`,`job_category`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  StatQ.ExecSQL;

  CreateEscort(FDB);

  MapQ.SQL.Text := 'INSERT INTO map_data VALUES (:id, :flags, :sname, :defbgm, :minlvl, :tlimit, :rrate, :deftraction, ' +
                   ':ltx, :lty, :rbx, :rby, :retmap, :fret, :ftype, :flimits, :dechp, :dps, :pitem, :ship, :mrate, :link, :pbr)';
  LifeQ.SQL.Text := 'INSERT INTO map_life VALUES (DEFAULT, :mid, :type, :lid, :lname, :x, :y, :fh, :mincp, :maxcp, :time, :flags)';
  FHQ.SQL.Text := 'INSERT INTO map_footholds VALUES (:mid, :id, :x1, :y1, :x2, :y2, :df, :prev, :next, :flags)';
  PortalQ.SQL.Text := 'INSERT INTO map_portals VALUES (:mid, :id, :name, :x, :y, :dest, :destname, :script, :ptype)';
  StatQ.SQL.Text := 'INSERT INTO map_forced_stats VALUES (:mid, :job, :str, :dex, :int, :luk, :watk, :matk, :acc, :avo, :jmp, :spd)';
  ScriptQ := FDB.GetQuery;
  ScriptQ.SQL.Text := SCRIPT_QUERY;

  Strings := LoadArchive('String.wz');
  MobNames := Strings.ParseFile(TWZFile(Strings.Root.Entry['Mob.img']));
  NPCNames := Strings.ParseFile(TWZFile(Strings.Root.Entry['Npc.img']));

  Data := LoadArchive('Map.wz');
  ContinentDirs := TWZDirectory(Data.Root.Entry['Map']);
  for MapDir in ContinentDirs.SubDirs do
    for Map in MapDir.Files do
      with Data.ParseFile(Map) do
      begin
        with Root.Child['info'], MapQ do
        begin
          try
            ParamByName('id').AsInteger := NoIMG(Map.Name);
          except
            Continue;  // New version? New fuck-ups!
          end;
          ParamByName('flags').AsString := GetMapFlags(Root.Child['info']);
          ParamByName('sname').AsString := Get('reactorShuffleName', '');
          ParamByName('defbgm').AsString := Get('bgm', '');
          ParamByName('minlvl').AsInteger := Get('lvLimit', 0);
          ParamByName('tlimit').AsInteger := Get('timeLimit', 0);
          ParamByName('rrate').AsInteger := Get('recovery', 1);
          ParamByName('deftraction').AsFloat := Get('fs', 1.0);
          ParamByName('ltx').AsInteger := Get('VRLeft', 0);
          ParamByName('lty').AsInteger := Get('VRTop', 0);
          ParamByName('rbx').AsInteger := Get('VRRight', 0);
          ParamByName('rby').AsInteger := Get('VRBottom', 0);
          ParamByName('retmap').AsInteger := Get('returnMap', 999999999);
          ParamByName('fret').AsInteger := Get('forcedReturn', 999999999);
          try
            ParamByName('ftype').AsString := GetMapType(Get('fieldType', 0));
            if ParamByName('ftype').AsString = '' then
              ParamByName('ftype').Clear;
          except // it has to be a fucking int you idiots
            ParamByName('ftype').Clear;
          end;
          ParamByName('flimits').AsString := GetMapLimits(Get('fieldLimit', 0));
          if Get('decHP', 0) <= 255 then
          begin
            ParamByName('dechp').AsInteger := Get('decHP', 0);
            ParamByName('dps').AsInteger := 0;
          end
          else
          begin
            ParamByName('dechp').AsInteger := 0;
            ParamByName('dps').AsInteger := Get('decHP', 0);
          end;
          ParamByName('pitem').AsInteger := Get('protectItem', 0);
          ParamByName('ship').AsInteger := Root.Get('shipObj/shipKind', -1);
          ParamByName('mrate').AsFloat := Get('mobRate', 0.0);
          ParamByName('link').AsInteger := Get('link', 0);
          ParamByName('pbr').AsInteger := Get('partyBonusR', 0);
          ExecSQL;
        end;

        Script := Root.Get('info/onUserEnter', '');
        if Script <> '' then
          ExecScriptQuery(ScriptQ, 'map_enter', Script, -1, NoIMG(Map.Name));
        Script := Root.Get('info/onFirstUserEnter', '');
        if Script <> '' then
          ExecScriptQuery(ScriptQ, 'map_first_enter', Script, -1, NoIMG(Map.Name));
        Script := Root.Get('info/fieldScript', '');
        if Script <> '' then
          ExecScriptQuery(ScriptQ, 'map', Script, -1, NoIMG(Map.Name));

        LifeQ.ParamByName('mid').AsInteger := NoIMG(Map.Name);
        if Root.Child['life'] <> nil then
          for Life in Root.Child['life'].Children do
            with Life, LifeQ do
            begin
              ParamByName('lid').AsInteger := Get('id', -1);
              if Get('type', '') = 'm' then
              begin
                ParamByName('type').AsString := 'mob';
                ParamByName('lname').AsString := DBStr(MobNames.Root.Get(Format('%d/name', [ParamByName('lid').AsInteger]), ''), 50);
              end
              else
              begin
                ParamByName('type').AsString := 'npc';
                ParamByName('lname').AsString := DBStr(NPCNames.Root.Get(Format('%d/name', [ParamByName('lid').AsInteger]), ''), 50);
              end;
              ParamByName('x').AsInteger := Get('x', 0);
              ParamByName('y').AsInteger := Get('cy', 0);
              ParamByName('fh').AsInteger := Get('fh', 0);
              ParamByName('mincp').AsInteger := Get('rx0', 0);
              ParamByName('maxcp').AsInteger := Get('rx1', 0);
              ParamByName('time').AsInteger := Get('mobTime', 0);
              Flags := '';
              if Get('f', 0) = 1 then
                AddFlag(Flags, 'faces_left');
              if Get('hide', 0) = 1 then
                AddFlag(Flags, 'hide');
              ParamByName('flags').AsString := Flags;
              ExecSQL;
            end;

        if Root.Child['reactor'] <> nil then
        begin
          LifeQ.ParamByName('type').AsString := 'reactor';
          LifeQ.ParamByName('fh').AsInteger := 0;
          LifeQ.ParamByName('mincp').AsInteger := 0;
          LifeQ.ParamByName('maxcp').AsInteger := 0;
          for Life in Root.Child['reactor'].Children do
            with Life, LifeQ do
            begin
              ParamByName('lid').AsInteger := Get('id', -1);
              ParamByName('lname').AsString := DBStr(Get('name', ''));
              ParamByName('x').AsInteger := Get('x', 0);
              ParamByName('y').AsInteger := Get('y', 0);
              ParamByName('time').AsInteger := Get('reactorTime', 0);
              if Get('f', 0) = 1 then
                ParamByName('flags').AsString := 'faces_left'
              else
                ParamByName('flags').AsString := '';
              ExecSQL;
            end;
        end;

        FHQ.ParamByName('mid').AsInteger := NoIMG(Map.Name);
        if Root.Child['foothold'] <> nil then
          for Iter in Root.Child['foothold'].Children do
            for Iter2 in Iter.Children do
              for Iter3 in Iter2.Children do
              begin
                FHQ.ParamByName('id').AsString := Iter3.Name;
                FHQ.ParamByName('x1').AsInteger := Iter3.Get('x1', 0);
                FHQ.ParamByName('y1').AsInteger := Iter3.Get('y1', 0);
                FHQ.ParamByName('x2').AsInteger := Iter3.Get('x2', 0);
                FHQ.ParamByName('y2').AsInteger := Iter3.Get('y2', 0);
                FHQ.ParamByName('df').AsInteger := Iter3.Get('force', 0);
                FHQ.ParamByName('prev').AsInteger := Iter3.Get('prev', 0);
                FHQ.ParamByName('next').AsInteger := Iter3.Get('next', 0);
                if Iter3.Get('forbidFallDown', 0) = 1 then
                  FHQ.ParamByName('flags').AsString := 'forbid_downward_jump'
                else
                  FHQ.ParamByName('flags').AsString := '';
                try
                  FHQ.ExecSQL;
                except
                  // GMST: Duplicate footholds
                end;
              end;

        PortalQ.ParamByName('mid').AsInteger := NoIMG(Map.Name);
        if Root.Child['portal'] <> nil then
          for Iter in Root.Child['portal'].Children do
          begin
            PortalQ.ParamByName('id').AsString := Iter.Name;
            PortalQ.ParamByName('name').AsString := Iter.Get('pn', '');
            PortalQ.ParamByName('x').AsInteger := Iter.Get('x', 0);
            PortalQ.ParamByName('y').AsInteger := Iter.Get('y', 0);
            PortalQ.ParamByName('dest').AsInteger := Iter.Get('tm', 0);
            PortalQ.ParamByName('destname').AsString := Iter.Get('tn', '');
            PortalQ.ParamByName('script').AsString := Iter.Get('script', '');

            PortalQ.ParamByName('ptype').AsString := '';
            case Iter.Get('pt', -1) of
              4: PortalQ.ParamByName('ptype').AsString := 'changeable';
              5: PortalQ.ParamByName('ptype').AsString := 'changeable_invisible';
              6: PortalQ.ParamByName('ptype').AsString := 'townportal_point';
            end;
            if PortalQ.ParamByName('ptype').AsString = '' then
              PortalQ.ParamByName('ptype').Clear;

            PortalQ.ExecSQL;
          end;

        if Root.Child['nodeInfo'] <> nil then
          AddEscortData(Root.Child['nodeInfo'], FDB, NoIMG(Map.Name));

        if Root.Child['user'] <> nil then
          for Iter in Root.Child['user'].Children do
            if Iter.Child['stat'] <> nil then
            begin
              Iter2 := Iter.Child['stat'];
              StatQ.ParamByName('mid').AsInteger := NoIMG(Map.Name);
              StatQ.ParamByName('job').AsInteger := Iter.Get('cond/jobCategory', -1);
              if StatQ.ParamByName('job').AsInteger = -1 then
                StatQ.ParamByName('job').AsInteger := Iter.Get('cond/job', -1);
              StatQ.ParamByName('str').AsInteger := Iter2.Get('str', -1);
              StatQ.ParamByName('dex').AsInteger := Iter2.Get('dex', -1);
              StatQ.ParamByName('int').AsInteger := Iter2.Get('int', -1);
              StatQ.ParamByName('luk').AsInteger := Iter2.Get('luk', -1);
              StatQ.ParamByName('watk').AsInteger := Iter2.Get('pad', -1);
              StatQ.ParamByName('matk').AsInteger := Iter2.Get('mad', -1);
              StatQ.ParamByName('acc').AsInteger := Iter2.Get('acc', -1);
              StatQ.ParamByName('avo').AsInteger := Iter2.Get('eva', -1);
              StatQ.ParamByName('jmp').AsInteger := Iter2.Get('jump', -1);
              StatQ.ParamByName('spd').AsInteger := Iter2.Get('speed', -1);
              try
                StatQ.ExecSQL;
              except
                // Just the same gender or so
              end;
            end;

        Free;
      end;

  Data.Free;
  Strings.Free;
  MapQ.Free;
  LifeQ.Free;
  ScriptQ.Free;
  FHQ.Free;
  PortalQ.Free;
  StatQ.Free;
end;

procedure CreateEscort(DB: TDatabaseConnection);
var
  Q, StopQ: TZQuery;
begin
  Q := DB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `map_escort_nodes` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`nodeid` smallint(6) NOT NULL, ' +
  '`x_pos` smallint(6) NOT NULL, ' +
  '`y_pos` smallint(6) NOT NULL, ' +
  '`attribute` enum('''',''rope'',''break'',''start'',''end'') NOT NULL DEFAULT '''', ' +
  '`edge0` smallint(6) NOT NULL, ' +
  '`edge1` smallint(6) NOT NULL, ' +
  '`edge2` smallint(6) NOT NULL DEFAULT ''-1'', ' +
  'PRIMARY KEY (`mapid`,`nodeid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  StopQ := DB.GetQuery;
  StopQ.SQL.Text := 'CREATE TABLE `map_escort_stops` (' +
  '`mapid` int(11) NOT NULL, ' +
  '`nodeid` smallint(6) NOT NULL, ' +
  '`action` varchar(10) NOT NULL, ' +
  '`flags` set(''weather'',''repeat'',''random'') NOT NULL DEFAULT '''', ' +
  '`duration` int(11) NOT NULL, ' +
  '`chat_balloon` int(11) NOT NULL DEFAULT ''0'', ' +
  '`message` varchar(150) NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`mapid`,`nodeid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  StopQ.ExecSQL;

  Q.Free;
  StopQ.Free;
end;

procedure AddEscortData(Info: TWZIMGEntry; DB: TDatabaseConnection; MapID: Integer);
var
  Q, StopQ: TZQuery;
  StartID, EndID: Integer;
  E: TWZIMGEntry;
  CurID: Integer;

  function MakeFlags: string;
  begin
    case E.Get('attr', 0) of
      0: Result := '';
      1: Result := 'rope';
      2: Result := 'break';
      else
        WriteLn('Unknown attribute, map ', MapID, ', node ', CurID);
        Result := '';
    end;

    if CurID = StartID then
      AddFlag(Result, 'start')
    else if CurID = EndID then
      AddFlag(Result, 'end');
  end;

  function MakeStopFlags: string;
  begin
    Result := '';
    if E.Get('stopInfo/isWeather', 0) = 1 then
      AddFlag(Result, 'weather');
    if E.Get('stopInfo/isRepeat', 0) = 1 then
      AddFlag(Result, 'repeat');
    if E.Get('stopInfo/isRandom', 0) = 1 then
      AddFlag(Result, 'random');
  end;

begin
  Q := DB.GetQuery;
  StopQ := DB.GetQuery;

  Q.SQL.Text := 'INSERT INTO map_escort_nodes VALUES (:mid, :nid, :x, :y, :flags, :edge0, :edge1, :edge2)';
  Q.ParamByName('mid').AsInteger := MapID;

  StopQ.SQL.Text := 'INSERT INTO map_escort_stops VALUES (:mid, :nid, :act, :flags, :len, :balloon, :msg)';
  StopQ.ParamByName('mid').AsInteger := MapID;

  StartID := Info.Get('start', -1);
  EndID := Info.Get('end', -1);

  if (StartID = -1) or (EndID = -1) then
    raise EArgumentException.Create('The given nodeInfo has no start or end node.');

  for E in Info.Children do
  begin
    if Length(E.Name) > 2 then
      Continue;

    CurID := E.Get('key', 0);
    Q.ParamByName('nid').AsInteger := CurID;
    Q.ParamByName('x').AsInteger := E.Get('x', 0);
    Q.ParamByName('y').AsInteger := E.Get('y', 0);
    Q.ParamByName('flags').AsString := MakeFlags;
    Q.ParamByName('edge0').AsInteger := E.Get('edge/0', -1);
    Q.ParamByName('edge1').AsInteger := E.Get('edge/1', -1);
    Q.ParamByName('edge2').AsInteger := E.Get('edge/2', -1);
    Q.ExecSQL;

    if Pos('break', Q.ParamByName('flags').AsString) > 0 then
    begin
      StopQ.ParamByName('nid').AsInteger := CurID;
      StopQ.ParamByName('act').AsString := E.Get('stopInfo/sayInfo/0/act', '');
      StopQ.ParamByName('flags').AsString := MakeStopFlags;
      StopQ.ParamByName('len').AsInteger := E.Get('stopInfo/stopDuration', 0) * 1000;
      StopQ.ParamByName('balloon').AsInteger := E.Get('stopInfo/chatBalloon', 0);
      StopQ.ParamByName('msg').AsString := DBStr(E.Get('stopInfo/sayInfo/0/say', ''));
      StopQ.ExecSQL;
    end;
  end;

  Q.Free;
  StopQ.Free;
end;

end.

