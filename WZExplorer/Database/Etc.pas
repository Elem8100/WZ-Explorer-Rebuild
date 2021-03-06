unit Etc;

interface

uses SysUtils, DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile, ZDataset;

procedure AddEtcData(FDB: TDatabaseConnection);

implementation

var
  Data: TWZArchive;

procedure DumpCashData(FDB: TDatabaseConnection);
var
  Q: TZQuery;
  Iter, Iter2: TWZIMGEntry;
begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `cash_commodity_data` (' +
  '`serial_number` int(11) NOT NULL, ' +
  '`itemid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`quantity` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`price` int(11) NOT NULL DEFAULT ''0'', ' +
  '`expiration_days` smallint(6) NOT NULL DEFAULT ''90'', ' +
  '`priority` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`gender` enum(''male'',''female'',''both'') NOT NULL, ' +
  '`flags` set(''on_sale'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`serial_number`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'CREATE TABLE `cash_modified_data` (' +
  '`serial_number` int(10) unsigned NOT NULL, ' +
  '`on_sale` tinyint(3) unsigned NOT NULL, ' +
  'PRIMARY KEY (`serial_number`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'CREATE TABLE `cash_package_data` (' +
  '`id` bigint(21) NOT NULL AUTO_INCREMENT, ' +
  '`packageid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`serial_number` int(11) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO cash_commodity_data VALUES (:sn, :iid, :qty, :price, :expiration, :priority, :gender, :flags)';

  with Q, Data.GetImgFile('Commodity.img') do
  begin
    for Iter in Root.Children do
    begin
      ParamByName('sn').AsInteger := Iter.Get('SN', 0);
      ParamByName('iid').AsInteger := Iter.Get('ItemId', 0);
      ParamByName('qty').AsInteger := Iter.Get('Count', 1);
      ParamByName('price').AsInteger := Iter.Get('Price', 0);
      ParamByName('expiration').AsInteger := Iter.Get('Period', 0);
      ParamByName('priority').AsInteger := Iter.Get('Priority', 0);
      case Iter.Get('Gender', 2) of
        0: ParamByName('gender').AsString := 'male';
        1: ParamByName('gender').AsString := 'female';
        else ParamByName('gender').AsString := 'both';
      end;
      if Iter.Get('OnSale', 0) = 1 then
        ParamByName('flags').AsString := 'on_sale'
      else
        ParamByName('flags').AsString := '';
      ExecSQL;
    end;

    Free;
  end;

  Q.SQL.Text := 'INSERT INTO cash_package_data VALUES (DEFAULT, :pid, :sn)';

  with Data.GetImgFile('CashPackage.img'), Q do
  begin
    for Iter in Root.Children do
    begin
      ParamByName('pid').AsString := Iter.Name;
      for Iter2 in Iter.Child['SN'].Children do
      begin
        ParamByName('sn').AsInteger := Iter2.Data;
        ExecSQL;
      end;
    end;

    Free;
  end;
end;

procedure DumpCreationData(FDB: TDatabaseConnection);
var
  Q, W: TZQuery;
  Iter, Gender: TWZIMGEntry;
  i, SetChr: Integer;

  function TypeByID(ID: Integer): string;
  begin
    if (ID = 1562000) and (Q.ParamByName('gender').AsString = 'male') then
      Exit('shield');

    case ID div 10000 of
      2: Result := 'face';
      3: Result := 'hair';
      101: Result := 'accessory';
      104, 105: Result := 'top';
      106: Result := 'bottom';
      107: Result := 'shoes';
      109: Result := 'shield';
      110: Result := 'cape';
      120..159: Result := 'weapon';
      else Result := '';
    end;
  end;

  function TypeByName(const N: string): string;
  begin
    if N = 'pants' then
      Result := 'bottom'
    else if N = 'arms' then
      Result := 'weapon'
    else if (N = 'outfit') or (N = 'outfits') or (N = 'costumes') then
      Result := 'top'
    else if (N = 'face trim') or (N = 'acc.') then
      Result := 'accessory'
    else
      Result := N;
  end;

  procedure ParseEntry(E: TWZIMGEntry; Job: Integer);
  const
    OBJECT_TYPE: array['0'..'7'] of string = ('face', 'hair', 'haircolor', 'skin', 'top', 'bottom', 'shoes', 'weapon');
  var
    Info, Values, Hair, Colors: TWZIMGEntry;
  begin
    if E = nil then
      WriteLn('Entry is nil in DumpCreationData! Job: ', Job);

    Q.ParamByName('type').AsInteger := Job;
    Q.ParamByName('setid').AsInteger := 0;
    if Pos('Char', E.Name) > 0 then
    begin
      Q.ParamByName('gender').AsString := LowerCase(Copy(E.Name, Pos('Char', E.Name) + 4, 6));
    end
    else
    begin
      SetChr := Ord(E.Name[Length(E.Name)]);
      if (SetChr < $30) or (SetChr > $39) then
      begin
        Q.ParamByName('gender').AsString := E.Name;
      end
      else
      begin
        Q.ParamByName('gender').AsString := Copy(E.Name, 1, Length(E.Name) - 1);
        Q.ParamByName('setid').AsInteger := SetChr - $30;
      end;
    end;

    for Info in E.Children do
    begin
      Q.ParamByName('order').AsString := Info.Name;
      if Info.Child['name'] <> nil then
        Q.ParamByName('objtype').AsString := TypeByName(LowerCase(Info.Get('name', '')))
      else
        Q.ParamByName('objtype').AsString := TypeByID(TWZIMGEntry(Info.Children[0]).Data);
      if Q.ParamByName('objtype').AsString = '' then
        Q.ParamByName('objtype').AsString := OBJECT_TYPE[Info.Name[1]];

      Values := Info.Child['color'];
      if Values <> nil then
      begin
        for Hair in Values.Children do
          for Colors in Hair.Children do
          begin
            Q.ParamByName('objid').AsInteger := Colors.Data;
            Q.ExecSQL;
          end;
      end
      else
        for Values in Info.Children do
        begin
          if Values.Name <> 'name' then
          begin
            Q.ParamByName('objid').AsInteger := Values.Data;
            Q.ExecSQL;
          end;
        end;
    end;
  end;

begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `character_creation_data` (' +
  '`id` bigint(21) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`job` smallint(5) unsigned NOT NULL, ' +
  '`gender` enum(''male'',''female'') NOT NULL, ' +
  '`setid` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`order` tinyint(3) NOT NULL, ' +
  '`object_type` enum(''face'',''hair'',''haircolor'',''skin'',''top'',''bottom'',''shoes'',''weapon'',''shield'',''accessory'',''cape'') NOT NULL, ' +
  '`objectid` int(11) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO character_creation_data VALUES (DEFAULT, :type, :gender, :setid, :order, :objtype, :objid)';

  with Data.GetImgFile('MakeCharInfo.img') do
  begin
    if Root.Child['000'] <> nil then
    begin
      for Iter in Root.Children do
        if (TryStrToInt(Iter.Name, i)) or (Iter.Name = '000_1') then
        begin
          if Iter.Name = '000_1' then
            i := 1;

          for Gender in Iter.Children do
            if Gender.Name <> 'info' then
              ParseEntry(Gender, i);
        end;
    end
    else  // before Legend patch
    begin
      ParseEntry(Root.Get('Info/CharMale'), 0);
      ParseEntry(Root.Get('Info/CharFemale'), 0);
      ParseEntry(Root.Get('PremiumCharMale'), 1000);
      ParseEntry(Root.Get('PremiumCharFemale'), 1000);
      ParseEntry(Root.Get('OrientCharMale'), 2000);
      ParseEntry(Root.Get('OrientCharFemale'), 2000);
      ParseEntry(Root.Get('EvanCharMale'), 2001);
      ParseEntry(Root.Get('EvanCharFemale'), 2001);
      ParseEntry(Root.Get('ResistanceCharMale'), 3000);
      ParseEntry(Root.Get('ResistanceCharFemale'), 3000);
    end;
  end;

  // Change haircolor to skin for most jobs
  Q.SQL.Text := 'SELECT id FROM character_creation_data t WHERE t.object_type = ''haircolor'' AND (SELECT id FROM character_creation_data d WHERE d.job = t.job AND d.gender = t.gender AND d.object_type = ''hair'' AND d.objectid MOD 10 <> 0 LIMIT 1) IS NOT NULL ' + ' AND (SELECT id FROM character_creation_data d WHERE d.job = t.job AND d.gender = t.gender AND d.object_type = ''skin'' LIMIT 1) IS NULL';
  Q.Open;
  W := FDB.GetQuery;
  W.SQL.Text := 'UPDATE character_creation_data SET object_type = ''skin'' WHERE id = :id';
  while not Q.Eof  do
  begin
    W.ParamByName('id').AsInteger := Q.FieldByName('id').AsInteger;
    W.ExecSQL;
    Q.Next;
  end;
  W.Free;

  Q.Free;
end;

procedure DumpForbiddenNames(FDB: TDatabaseConnection);
var
  Q: TZQuery;
  Name: TWZIMGEntry;
begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `character_forbidden_names` (' +
  '`forbidden_name` varchar(15) NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`forbidden_name`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO character_forbidden_names VALUES (:name)';

  with Data.GetImgFile('ForbiddenName.img') do
  begin
    for Name in Root.Children do
    begin
      Q.ParamByName('name').AsString := Name.Data;
      try
        Q.ExecSQL;
      except
        // Stupid company - stupid duplicates
      end;
    end;

    Free;
  end;

  Q.SQL.Text := 'CREATE TABLE `curse_data` (' +
  '`word` varchar(70) NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`word`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO curse_data VALUES (:curse)';

  with Data.GetImgFile('Curse.img') do
  begin
    for Name in Root.Children do
    begin
      Q.ParamByName('curse').AsString := Name.Data;
      try
        Q.ExecSQL;
      except
      end;
    end;

    Free;
  end;

  Q.Free;
end;

procedure DumbMonsterCardSets(FDB: TDatabaseConnection);
var
  Q, CardQ: TZQuery;
  F: TWZIMGFile;
  Iter, Card: TWZIMGEntry;
begin
  F := Data.GetImgFile('MonsterBookSet.img');
  if F = nil then
    Exit;

  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `monster_card_sets` (' +
  '`setid` int(11) NOT NULL, ' +
  '`name` varchar(30) NOT NULL, ' +
  '`score` smallint(5) NOT NULL, ' +
  '`experience` int(11) NOT NULL, ' +
  '`potential_stat0` smallint(5) NOT NULL, ' +
  '`potential_stat1` smallint(5) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`setid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  CardQ := FDB.GetQuery;
  CardQ.SQL.Text := 'CREATE TABLE `monster_card_set_cards` (' +
  '`setid` int(11) NOT NULL, ' +
  '`cardid` int(11) NOT NULL, ' +
  'PRIMARY KEY (`setid`,`cardid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  CardQ.ExecSQL;

  Q.SQL.Text := 'INSERT INTO monster_card_sets VALUES (:id, :name, :score, :exp, :pot0, :pot1)';
  CardQ.SQL.Text := 'INSERT INTO monster_card_set_cards VALUES (:id, :card)';

  with F do
  begin
    for Iter in Root.Child['setList'].Children do
    begin
      Q.ParamByName('id').AsString := Iter.Name;
      Q.ParamByName('name').AsString := Iter.Get('setName', '');
      Q.ParamByName('score').AsInteger := Iter.Get('setScore', 0);
      Q.ParamByName('exp').AsInteger := Iter.Get('exp', 0);
      Q.ParamByName('pot0').AsInteger := Iter.Get('stats/potential/0', 0);
      Q.ParamByName('pot1').AsInteger := Iter.Get('stats/potential/1', 0);
      Q.ExecSQL;

      for Card in Iter.Child['cardList'].Children do
      begin
        CardQ.ParamByName('id').AsString := Iter.Name;
        CardQ.ParamByName('card').AsInteger := Card.Data;
        CardQ.ExecSQL;
      end;
    end;

    Free;
  end;

  Q.Free;
  CardQ.Free;
end;

procedure DumpItemSets(FDB: TDatabaseConnection);
var
  Q, ItemQ, PotQ: TZQuery;
  F: TWZIMGFile;
  Iter, EffIter, Item, Option: TWZIMGEntry;
begin
  F := Data.GetImgFile('SetItemInfo.img');
  if F = nil then
    Exit;

  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `item_set_data` (' +
  '`setid` int(11) NOT NULL, ' +
  '`equipped_item_count` tinyint(3) unsigned NOT NULL, ' +
  '`inc_str` smallint(6) NOT NULL, ' +
  '`inc_dex` smallint(6) NOT NULL, ' +
  '`inc_int` smallint(6) NOT NULL, ' +
  '`inc_luk` smallint(6) NOT NULL, ' +
  '`inc_all_stats` smallint(6) NOT NULL, ' +
  '`inc_hp` smallint(6) NOT NULL, ' +
  '`inc_mp` smallint(6) NOT NULL, ' +
  '`inc_hp_rate` tinyint(3) NOT NULL, ' +
  '`inc_mp_rate` tinyint(3) NOT NULL, ' +
  '`inc_watk` smallint(6) NOT NULL, ' +
  '`inc_matk` smallint(6) NOT NULL, ' +
  '`inc_wdef` smallint(6) NOT NULL, ' +
  '`inc_mdef` smallint(6) NOT NULL, ' +
  '`inc_acc` smallint(6) NOT NULL, ' +
  '`inc_avoid` smallint(6) NOT NULL, ' +
  '`inc_jump` smallint(6) NOT NULL, ' +
  '`inc_speed` smallint(6) NOT NULL, ' +
  '`inc_pvp_damage` smallint(6) NOT NULL, ' +
  '`skill` int(11) NOT NULL, ' +
  'PRIMARY KEY (`setid`,`equipped_item_count`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  ItemQ := FDB.GetQuery;
  ItemQ.SQL.Text := 'CREATE TABLE `item_set_items` (' +
  '`setid` int(11) NOT NULL, ' +
  '`group` tinyint(3) NOT NULL, ' +
  '`itemid` int(11) NOT NULL, ' +
  'PRIMARY KEY (`setid`,`group`,`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  ItemQ.ExecSQL;

  PotQ := FDB.GetQuery;
  PotQ.SQL.Text := 'CREATE TABLE `item_set_potentials` (' +
  '`id` int(11) NOT NULL AUTO_INCREMENT, ' +
  '`setid` int(11) NOT NULL, ' +
  '`equipped_item_count` tinyint(3) unsigned NOT NULL, ' +
  '`potentialid` int(11) NOT NULL, ' +
  '`potential_level` int(11) NOT NULL, ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  PotQ.ExecSQL;

  Q.SQL.Text := 'INSERT INTO item_set_data VALUES (:id, :count, :str, :dex, :int, :luk, ' +
  ':all, :hp, :mp, :hpr, :mpr, :watk, :matk, :wdef, :mdef, :acc, :avo, :jmp, :spd, :pvp, :skill)';
  ItemQ.SQL.Text := 'INSERT INTO item_set_items VALUES (:id, :group, :item)';
  PotQ.SQL.Text := 'INSERT INTO item_set_potentials VALUES (DEFAULT, :id, :count, :pot, :lv)';

  for Iter in F.Root.Children do
  begin
    Q.ParamByName('id').AsString := Iter.Name;
    for EffIter in Iter.Child['Effect'].Children do
    begin
      Q.ParamByName('count').AsString := EffIter.Name;
      Q.ParamByName('str').AsInteger := EffIter.Get('incSTR', 0);
      Q.ParamByName('dex').AsInteger := EffIter.Get('incDEX', 0);
      Q.ParamByName('int').AsInteger := EffIter.Get('incINT', 0);
      Q.ParamByName('luk').AsInteger := EffIter.Get('incLUK', 0);
      Q.ParamByName('all').AsInteger := EffIter.Get('incAllStat', 0);
      Q.ParamByName('hp').AsInteger := EffIter.Get('incMHP', 0);
      Q.ParamByName('mp').AsInteger := EffIter.Get('incMMP', 0);
      Q.ParamByName('hpr').AsInteger := EffIter.Get('incMHPr', 0);
      Q.ParamByName('mpr').AsInteger := EffIter.Get('incMMPr', 0);
      Q.ParamByName('watk').AsInteger := EffIter.Get('incPAD', 0);
      Q.ParamByName('matk').AsInteger := EffIter.Get('incMAD', 0);
      Q.ParamByName('wdef').AsInteger := EffIter.Get('incPDD', 0);
      Q.ParamByName('mdef').AsInteger := EffIter.Get('incMDD', 0);
      Q.ParamByName('acc').AsInteger := EffIter.Get('incACC', 0);
      Q.ParamByName('avo').AsInteger := EffIter.Get('incEVA', 0);
      Q.ParamByName('jmp').AsInteger := EffIter.Get('incJump', 0);
      Q.ParamByName('spd').AsInteger := EffIter.Get('incSpeed', 0);
      Q.ParamByName('pvp').AsInteger := EffIter.Get('incPVPDamage', 0);
      Q.ParamByName('skill').AsInteger := EffIter.Get('activeSkill/0/id', 0);
      Q.ExecSQL;

      if EffIter.Child['Option'] <> nil then
      begin
        PotQ.ParamByName('id').AsString := Iter.Name;
        PotQ.ParamByName('count').AsString := EffIter.Name;
        for Option in EffIter.Child['Option'].Children do
        begin
          PotQ.ParamByName('pot').AsInteger := Option.Get('option', 0);
          PotQ.ParamByName('lv').AsInteger := Option.Get('level', 0);
          PotQ.ExecSQL;
        end;
      end;
    end;

    ItemQ.ParamByName('id').AsString := Iter.Name;
    for Item in Iter.Child['ItemID'].Children do
    begin
      if Item.Children.Count > 0 then
      begin
        ItemQ.ParamByName('group').AsString := Item.Name;
        for Option in Item.Children do
        begin
          if Length(Option.Name) > 2 then
            Continue;

          ItemQ.ParamByName('item').AsInteger := Option.Data;
          ItemQ.ExecSQL;
        end;
      end
      else
      begin
        ItemQ.ParamByName('group').AsInteger := -1;
        ItemQ.ParamByName('item').AsInteger := Item.Data;
        try
          ItemQ.ExecSQL;
        except
          // SetID 32
        end;
      end;
    end;
  end;

  F.Free;
  Q.Free;
  ItemQ.Free;
  PotQ.Free;
end;

procedure DumpItemPots(FDB: TDatabaseConnection);
var
  Q: TZQuery;
  F: TWZIMGFile;
  Iter: TWZIMGEntry;
begin
  F := Data.GetImgFile('ItemPotLifeInfo.img');
  if F = nil then
    Exit;

  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `item_pot_data` (' +
  '`id` int(11) NOT NULL, ' +
  '`consume_item` int(11) unsigned NOT NULL, ' +
  '`type` tinyint(3) NOT NULL, ' +
  '`level_count` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO item_pot_data VALUES (:id, :cid, :t, :levels)';

  for Iter in F.Root.Children do
  begin
    Q.ParamByName('id').AsString := Iter.Name;
    Q.ParamByName('cid').AsInteger := Iter.Get('info/counsumeItem', 0);
    Q.ParamByName('t').AsInteger := Iter.Get('info/type', 0);
    if Iter.Child['level'] <> nil then
      Q.ParamByName('levels').AsInteger := Iter.Child['level'].Children.Count
    else
      Q.ParamByName('levels').AsInteger := 0;
    Q.ExecSQL;
  end;

  F.Free;
  Q.Free;
end;

procedure DumpCharacterCards(FDB: TDatabaseConnection);
var
  Q, DQ: TZQuery;
  F: TWZIMGFile;
  Iter, Folder, DeckCard: TWZIMGEntry;
begin
  F := Data.GetImgFile('CharacterCard.img');
  if F = nil then
    Exit;

  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `character_card_data` (' +
  '`id` int(11) NOT NULL, ' +
  '`skillid` int(11) NOT NULL, ' +
  '`rank` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''unique_effect'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  DQ := FDB.GetQuery;
  DQ.SQL.Text := 'CREATE TABLE `character_card_decks` (' +
  '`id` int(11) NOT NULL, ' +
  '`card` int(11) NOT NULL, ' +
  'PRIMARY KEY (`id`,`card`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  DQ.ExecSQL;

  Q.SQL.Text := 'INSERT INTO character_card_data VALUES (:id, :sid, :rank, :flags)';
  DQ.SQL.Text := 'INSERT INTO character_card_decks VALUES (:id, :card)';

  for Folder in F.Root.Children do
    for Iter in Folder.Children do
    begin
      Q.ParamByName('id').AsString := Iter.Name;
      Q.ParamByName('sid').AsInteger := Iter.Get('skillID', 0);
      case String(Iter.Get('reqRank', '_'))[1] of
        '_': Q.ParamByName('rank').AsInteger := 0;
        'B': Q.ParamByName('rank').AsInteger := 1;
        'A': Q.ParamByName('rank').AsInteger := 2;
        'S': if Iter.Get('reqRank', '') = 'S' then
               Q.ParamByName('rank').AsInteger := 3
             else
               Q.ParamByName('rank').AsInteger := 4;
      end;
      if Iter.Get('uniqueEffect', 0) = 1 then
        Q.ParamByName('flags').AsString := 'unique_effect'
      else
        Q.ParamByName('flags').AsString := '';
      Q.ExecSQL;

      if Iter.Child['reqCardID'] <> nil then
      begin
        DQ.ParamByName('id').AsString := Iter.Name;
        for DeckCard in Iter.Child['reqCardID'].Children do
        begin
          Assert(DeckCard.DataType in [mdtShort, mdtInt], 'DataType fault CharacterCard.img/Deck/' + Iter.Name + '/reqCardID/' + DeckCard.Name);
          DQ.ParamByName('card').AsInteger := DeckCard.Data;
          DQ.ExecSQL;
        end;
      end;
    end;

  F.Free;
  Q.Free;
  DQ.Free;
end;

procedure AddEtcData(FDB: TDatabaseConnection);
begin
  Data := LoadArchive('Etc.wz');
  try
    DumpCashData(FDB);
    DumpCreationData(FDB);
    DumpForbiddenNames(FDB);
    DumbMonsterCardSets(FDB);
    DumpItemSets(FDB);
    DumpItemPots(FDB);
    DumpCharacterCards(FDB);
  finally
    Data.Free;
  end;
end;

end.
