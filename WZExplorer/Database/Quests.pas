unit Quests;

interface

uses Windows, SysUtils, DBGeneral, DatabaseConnection, ZDataset, DB,
     WZArchive, WZDirectory, WZIMGFile, Math;

procedure AddQuestTables(FDB: TDatabaseConnection);

implementation

const
  JOB_TYPES: array[0..61] of string = (
    'beginner', 'warrior', 'magician', 'bowman', 'thief', 'pirate',
    '', '', 'mihile_beginner', 'gm',
    'noblesse', 'dawn_warrior', 'blaze_wizard', 'wind_archer', 'night_walker', 'thunder_breaker',
    'angelic_buster', 'legend_evan', 'legend_mercedes', 'legend_phantom', {Nexon fail, legend_luminous == legend_aran}
    'legend_aran', 'aran', 'evan', 'mercedes', 'phantom', 'shade', '', 'luminous', '', '',
    'citizen', 'demon', 'battle_mage', 'wild_hunter', '', 'mechanic', 'xenon', '', '', '',
    '', 'hayato', 'kanna', 'zero', 'beast_tamer', '', 'hayato_beginner', 'kanna_beginner', 'kaiser_beginner', 'angelic_buster_beginner',
    '', 'mihile', '', '', '', '', '', 'demon_slayer_beginner', '', '', '', 'kaiser'
  );

function GetJobTypes(const Mask: ULARGE_INTEGER): string;
var
  i: Integer;
begin
  Result := '';
  if Mask.QuadPart = 0 then
    Exit;

  for i := 0 to High(JOB_TYPES) do
    if (Mask.QuadPart and (UInt64(1) shl i) > 0) and (JOB_TYPES[i] <> '') then
      AddFlag(Result, JOB_TYPES[i]);
end;

function JoinTypes: string;
var
  i: Integer;
begin
  Result := '''' + JOB_TYPES[0] + '''';
  for i := 1 to High(JOB_TYPES) do
    if JOB_TYPES[i] <> '' then
      Result := Result + ',''' + JOB_TYPES[i] + '''';
end;

procedure ParseRequests(Q, Q2, QJob: TZQuery; E: TWZIMGEntry);
var
  Child, Iter: TWZIMGEntry;
  JobID: Integer;
begin
  Child := E.Child['item'];
  if Assigned(Child) then
  begin
    Q.ParamByName('rtype').AsString := 'item';
    for Iter in Child.Children do
    begin
      Q.ParamByName('oid').AsInteger := Iter.Get('id', 0);
      Q.ParamByName('count').AsInteger := Iter.Get('count', 0);
      Q.ExecSQL;
    end;
  end;

  Child := E.Child['quest'];
  if Assigned(Child) then
  begin
    Q.ParamByName('rtype').AsString := 'quest';
    for Iter in Child.Children do
    begin
      Q.ParamByName('oid').AsInteger := Iter.Get('id', 0);
      Q.ParamByName('count').AsInteger := Iter.Get('state', 0);
      Q.ExecSQL;
    end;
  end;

  Child := E.Child['infoex'];
  if Assigned(Child) then
  begin
    Q2.ParamByName('info').AsInteger := E.Get('infoNumber', Q2.ParamByName('id').AsInteger);
    for Iter in Child.Children do
    begin
      Q2.ParamByName('value').AsString := Iter.Get('value', '');
      Q2.ParamByName('cond').AsInteger := Iter.Get('cond', 0);
      if Iter.Child['exVariable'] <> nil then
        Q2.ParamByName('exVariable').AsString := Iter.Get('exVariable', '')
      else with Q2.ParamByName('exVariable') do
      begin
        DataType := TFieldType.ftString;
        Clear;
      end;
      Q2.ExecSQL;
    end;
  end;

  if E.Name = '1' then
  begin
    Child := E.Child['mob'];
    if Assigned(Child) then
    begin
      Q.ParamByName('rtype').AsString := 'mob';
      for Iter in Child.Children do
      begin
        Q.ParamByName('oid').AsInteger := Iter.Get('id', 0);
        Q.ParamByName('count').AsInteger := Iter.Get('count', 0);
        Q.ExecSQL;
      end;
    end;
  end
  else
  begin
    Child := E.Child['job'];
    if Assigned(Child) then
    begin
      for Iter in Child.Children do
      begin
        if Iter.DataType <> mdtString then
        begin
          QJob.ParamByName('job').AsInteger := Iter.Data;
          try
            QJob.ExecSQL;
          except // Didn't expect to find duplicates even here? Think again!
          end;
        end
        else if TryStrToInt(Iter.Data, JobID) then
        begin
          QJob.ParamByName('job').AsInteger := JobID;
          QJob.ExecSQL;
        end;
      end;
    end;
  end;
end;

procedure AddQuestTables(FDB: TDatabaseConnection);
var
  MainQ, ReqQ, RewQ, ScriptQ, InfoQ, JobQ: TZQuery;
  Data: TWZArchive;
  Act, Check, Info, Quest, CurCheck: TWZIMGEntry;
  QFlags, SScript, EScript: string;
  Child: TWZIMGEntry;
  uli: ULARGE_INTEGER;

  procedure GetRewards(Start: Boolean);
  const
    Gender: array[0..2] of AnsiString = ('male', 'female', 'both');
  var
    CurAct, Iter, JobIter: TWZIMGEntry;
  begin
    if Start then
    begin
      RewQ.ParamByName('state').AsString := 'start';
      CurAct := Act.Child[Quest.Name].Child['0'];
    end
    else
    begin
      RewQ.ParamByName('state').AsString := 'end';
      CurAct := Act.Child[Quest.Name].Child['1'];
    end;

    Child := CurAct.Child['skill'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'skill';
      for Iter in Child.Children do
      begin
        RewQ.ParamByName('oid').AsInteger := Iter.Get('id', 0);
        RewQ.ParamByName('count').AsInteger := Iter.Get('skillLevel', 0);
        RewQ.ParamByName('ml').AsInteger := Iter.Get('masterLevel', 1);
        RewQ.ParamByName('gender').Value := Gender[2];
        RewQ.ParamByName('jt').AsString := '';
        RewQ.ParamByName('prop').AsInteger := 0;

        if Iter.Get('onlyMasterLevel', 0) = 1 then
          RewQ.ParamByName('flags').AsString := 'only_master_level'
        else
          RewQ.ParamByName('flags').AsString := '';

        if Assigned(Iter.Child['job']) then
        begin
          for JobIter in Iter.Child['job'].Children do
          begin
            RewQ.ParamByName('job').AsInteger := JobIter.Data;
            RewQ.ExecSQL;
          end;
        end
        else
        begin
          RewQ.ParamByName('job').AsInteger := -1;
          RewQ.ExecSQL;
        end;
      end;
    end;

    RewQ.ParamByName('ml').AsInteger := 0;
    RewQ.ParamByName('job').AsInteger := -1;
    RewQ.ParamByName('flags').AsString := '';

    Child := CurAct.Child['item'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'item';
      for Iter in Child.Children do
      begin
        RewQ.ParamByName('oid').AsInteger := Iter.Get('id', 0);
        RewQ.ParamByName('count').AsInteger := Iter.Get('count', 0);
        RewQ.ParamByName('gender').Value := Gender[Integer(Iter.Get('gender', 2))];
        uli.LowPart := Iter.Get('job', 0);
        uli.HighPart := Iter.Get('jobEx', 0);
        RewQ.ParamByName('jt').AsString := GetJobTypes(uli);
        RewQ.ParamByName('prop').AsInteger := Iter.Get('prop', 0);
        RewQ.ExecSQL;
      end;
    end;

    RewQ.ParamByName('jt').AsString := '';
    RewQ.ParamByName('count').AsInteger := 0;
    RewQ.ParamByName('gender').Value := Gender[2];
    RewQ.ParamByName('prop').AsInteger := 0;

    Child := CurAct.Child['exp'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'exp';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;

    Child := CurAct.Child['money'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'mesos';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;

    Child := CurAct.Child['pop'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'fame';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;

    Child := CurAct.Child['buffItemID'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'buff';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;

    Child := CurAct.Child['pettameness'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'pet_closeness';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;

    Child := CurAct.Child['petspeed'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'pet_speed';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;

    Child := CurAct.Child['petskill'];
    if Assigned(Child) then
    begin
      RewQ.ParamByName('rtype').AsString := 'pet_skill';
      RewQ.ParamByName('oid').AsInteger := Child.Data;
      RewQ.ExecSQL;
    end;
  end;

begin
  MainQ := FDB.GetQuery;
  MainQ.SQL.Text := 'CREATE TABLE `quest_data` (' +
  '`questid` smallint(6) unsigned NOT NULL, ' +
  '`next_quest` smallint(6) unsigned NOT NULL DEFAULT ''0'', ' +
  '`quest_area` int(11) NOT NULL DEFAULT ''0'', ' +
  '`min_level` tinyint(3) unsigned NOT NULL DEFAULT ''1'', ' +
  '`max_level` tinyint(3) unsigned NOT NULL DEFAULT ''200'', ' +
  '`pet_closeness` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`taming_mob_level` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`repeat_wait` int(11) NOT NULL DEFAULT ''0'', ' +
  '`fame` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`time_limit` int(11) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''auto_accept'',''auto_start'',''selected_mob'',''blocked'',''self_start'',''self_complete'',''no_forfeit'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`questid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  MainQ.ExecSQL;

  ReqQ := FDB.GetQuery;
  ReqQ.SQL.Text := 'CREATE TABLE `quest_requests` (' +
  '`questid` int(11) NOT NULL, ' +
  '`quest_state` enum(''start'',''end'') NOT NULL, ' +
  '`request_type` enum(''mob'',''item'',''quest'') NOT NULL, ' +
  '`objectid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`quantity` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`questid`,`quest_state`,`objectid`), ' +
  'KEY `questid` (`questid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  ReqQ.ExecSQL;

  InfoQ := FDB.GetQuery;
  InfoQ.SQL.Text := 'CREATE TABLE `quest_info_requests` (' +
  '`id` int(11) NOT NULL AUTO_INCREMENT, ' +
  '`questid` int(11) NOT NULL, ' +
  '`quest_state` enum(''start'',''end'') NOT NULL, ' +
  '`infoid` int(11) NOT NULL, ' +
  '`value` varchar(30) NOT NULL, ' +
  '`condition_type` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`variable_name` varchar(30) DEFAULT NULL, ' +
  'PRIMARY KEY (`id`), ' +
  'KEY `questid` (`questid`) ' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  InfoQ.ExecSQL;

  JobQ := FDB.GetQuery;
  JobQ.SQL.Text := 'CREATE TABLE `quest_required_jobs` (' +
  '`questid` smallint(6) unsigned NOT NULL, ' +
  '`valid_jobid` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`questid`,`valid_jobid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  JobQ.ExecSQL;

  RewQ := FDB.GetQuery;
  RewQ.SQL.Text := 'CREATE TABLE `quest_rewards` (' +
  '`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`questid` int(11) NOT NULL, ' +
  '`quest_state` enum(''start'',''end'') NOT NULL, ' +
  '`reward_type` enum(''item'',''exp'',''mesos'',''fame'',''skill'',''buff'',''pet_closeness'',''pet_speed'',''pet_skill'') NOT NULL, ' +
  '`rewardid` int(11) NOT NULL, ' +
  '`quantity` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`master_level` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`gender` enum(''male'',''female'',''both'') NOT NULL, ' +
  '`job_tracks` set(' + JoinTypes + ') NOT NULL DEFAULT '''', ' +
  '`job` smallint(6) NOT NULL DEFAULT ''-1'', ' +
  '`prop` int(11) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''only_master_level'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`id`), ' +
  'KEY `questid` (`questid`) ' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  RewQ.ExecSQL;

  MainQ.SQL.Text := 'INSERT INTO quest_data VALUES (:id, :next, :area, :minlvl, :maxlvl, ' +
                    ':petc, :tml, :rw, :fame, :tl, :flags)';
  ReqQ.SQL.Text := 'INSERT INTO quest_requests VALUES (:id, :state, :rtype, :oid, ' +
                   ':count)';
  JobQ.SQL.Text := 'INSERT INTO quest_required_jobs VALUES (:qid, :job)';
  RewQ.SQL.Text := 'INSERT INTO quest_rewards VALUES (DEFAULT, :id, :state, :rtype, ' +
                   ':oid, :count, :ml, :gender, :jt, :job, :prop, :flags)';
  InfoQ.SQL.Text := 'INSERT INTO quest_info_requests VALUES (DEFAULT, :id, :state, :info, :value, :cond, :exVariable)';
  ScriptQ := FDB.GetQuery;
  ScriptQ.SQL.Text := SCRIPT_QUERY;

  Data := LoadArchive('Quest.wz');
  Act := Data.ParseFile(TWZFile(Data.Root.Entry['Act.img'])).Root;
  Check := Data.ParseFile(TWZFile(Data.Root.Entry['Check.img'])).Root;
  Info := Data.ParseFile(TWZFile(Data.Root.Entry['QuestInfo.img'])).Root;

  for Quest in Act.Children do
  begin
    CurCheck := Check.Child[Quest.Name]; // store this -- lookup is very costly!
    MainQ.ParamByName('id').AsString := Quest.Name;
    MainQ.ParamByName('next').AsInteger := Quest.Get('1/nextQuest', 0);
    MainQ.ParamByName('area').AsInteger := Info.Get(Quest.Name + '/area', 0);
    if Assigned(CurCheck) then
    begin
      MainQ.ParamByName('minlvl').AsInteger := Min(CurCheck.Get('0/lvmin', 1), 255);  // the fuckers set it to 100200 for a NLC quest
      MainQ.ParamByName('maxlvl').AsInteger := CurCheck.Get('0/lvmax', 200);
      MainQ.ParamByName('petc').AsInteger := CurCheck.Get('0/pettamenessmin', 0);
      MainQ.ParamByName('tml').AsInteger := CurCheck.Get('0/tamingmoblevelmin', 0);
      MainQ.ParamByName('rw').AsInteger := CurCheck.Get('0/interval', 0);
      MainQ.ParamByName('fame').AsInteger := CurCheck.Get('0/pop', 0);
      MainQ.ParamByName('tl').AsInteger := Info.Get(Quest.Name + '/timeLimit', 0);
    end
    else
    begin
      MainQ.ParamByName('minlvl').AsInteger := 1;
      MainQ.ParamByName('maxlvl').AsInteger := 250;
      MainQ.ParamByName('petc').AsInteger := 0;
      MainQ.ParamByName('tml').AsInteger := 0;
      MainQ.ParamByName('rw').AsInteger := 0;
      MainQ.ParamByName('fame').AsInteger := 0;
      MainQ.ParamByName('tl').AsInteger := 0;
    end;

    QFlags := '';
    Child := Info.Child[Quest.Name];
    if Child <> nil then
    begin
      if Child.Get('selectedMob', 0) = 1 then
        AddFlag(QFlags, 'selected_mob');
      if Child.Get('autoAccept', 0) = 1 then
        AddFlag(QFlags, 'auto_accept');
      if Child.Get('autoStart', 0) = 1 then
        AddFlag(QFlags, 'auto_start');
      if Child.Get('blocked', 0) = 1 then
        AddFlag(QFlags, 'blocked');
      if Child.Get('selfStart', 0) = 1 then
        AddFlag(QFlags, 'self_start');
      if Child.Get('selfComplete', 0) = 1 then
        AddFlag(QFlags, 'self_complete');
      if Child.Get('resignBlocked', 0) = 1 then
        AddFlag(QFlags, 'no_forfeit');
    end;
    MainQ.ParamByName('flags').AsString := QFlags;

    MainQ.ExecSQL;

    // Check.img / Requests
    if CurCheck <> nil then
    begin
      SScript := CurCheck.Get('0/startscript', '');
      EScript := CurCheck.Get('1/endscript', '');
      if SScript <> '' then
        ExecScriptQuery(ScriptQ, 'quest', SScript, 0, StrToInt(Quest.Name));
      if EScript <> '' then
        ExecScriptQuery(ScriptQ, 'quest', EScript, 1, StrToInt(Quest.Name));

      ReqQ.ParamByName('id').AsString := Quest.Name;
      InfoQ.ParamByName('id').AsString := Quest.Name;
      JobQ.ParamByName('qid').AsString := Quest.Name;

      ReqQ.ParamByName('state').AsString := 'start';
      InfoQ.ParamByName('state').AsString := 'start';
      ParseRequests(ReqQ, InfoQ, JobQ, CurCheck.Child['0']);
      ReqQ.ParamByName('state').AsString := 'end';
      InfoQ.ParamByName('state').AsString := 'end';
      ParseRequests(ReqQ, InfoQ, nil, CurCheck.Child['1']);
    end;

    // Act.img / Rewards
    RewQ.ParamByName('id').AsString := Quest.Name;
    GetRewards(True);     // Start
    GetRewards(False);    // End
  end;

  MainQ.Free;
  ReqQ.Free;
  RewQ.Free;
  ScriptQ.Free;
  Data.Free;
end;

end.
