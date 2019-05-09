unit Items;

interface

uses SysUtils, DBGeneral, DatabaseConnection, WZArchive, WZDirectory, WZIMGFile,
     ZDataset, Math;

procedure AddItemTables(FDB: TDatabaseConnection);
procedure AddPetData(FDB: TDatabaseConnection; Data: TWZArchive);

implementation

function GetEquipFlagData(Info: TWZIMGEntry): string;
begin
  Result := '';
  if Info = nil then
    Exit;

  if Info.Get('equipTradeBlock', 0) = 1 then
    AddFlag(Result, 'wear_trade_block');

  if Info.Get('pickupMeso', 0) = 1 then
    AddFlag(Result, 'pet_loot_money');

  if Info.Get('pickupItem', 0) = 1 then
    AddFlag(Result, 'pet_loot_items');

  if Info.Get('consumeHP', 0) = 1 then
    AddFlag(Result, 'pet_auto_hp');

  if Info.Get('consumeMP', 0) = 1 then
    AddFlag(Result, 'pet_auto_mp');

  if Info.Get('sweepForDrop', 0) = 1 then
    AddFlag(Result, 'pet_sweep_for_drops');

  if Info.Get('longRange', 0) = 1 then
    AddFlag(Result, 'pet_big_loot_range');

  if Info.Get('pickupOthers', 0) = 1 then
    AddFlag(Result, 'pet_loot_others');

  if Info.Get('ignorePickup', 0) = 1 then
    AddFlag(Result, 'pet_loot_ignore');

  if Info.Get('jokerToSetItem', 0) = 1 then
    AddFlag(Result, 'set_joker');
end;

function GetEquipJob(Mask: Integer): string;
begin
  if Mask = -1 then
    Exit('beginner')
  else if Mask = 0 then
    Exit('beginner,warrior,magician,bowman,thief,pirate');

  Result := '';

  if Mask and 1 = 1 then
    AddFlag(Result, 'warrior');
  if Mask and 2 = 2 then
    AddFlag(Result, 'magician');
  if Mask and 4 = 4 then
    AddFlag(Result, 'bowman');
  if Mask and 8 = 8 then
    AddFlag(Result, 'thief');
  if Mask and 16 = 16 then
    AddFlag(Result, 'pirate');
end;

function GetEquipSlot(const WZSlot, DirName, Flags: string; ID: Integer): string;
begin
  Result := '';

  if (WZSlot = 'Cp') or (WZSlot = 'HrCp') then
    AddFlag(Result, 'helmet')
  else if WZSlot = 'Af' then
    AddFlag(Result, 'face_accessory')
  else if WZSlot = 'Ay' then
    AddFlag(Result, 'eye_accessory')
  else if WZSlot = 'Ae' then
    AddFlag(Result, 'earring')
  else if WZSlot = 'Pe' then
    AddFlag(Result, 'pendant')
  else if WZSlot = 'Be' then
    AddFlag(Result, 'belt')
  else if WZSlot = 'Me' then
    AddFlag(Result, 'medal')
  else if WZSlot = 'Sr' then
    AddFlag(Result, 'cape')
  else if WZSlot = 'Pn' then
    AddFlag(Result, 'bottom')
  else if WZSlot = 'Tm' then
    AddFlag(Result, 'taming_mob')
  else if WZSlot = 'Sd' then
    AddFlag(Result, 'saddle')
  else if Copy(WZSlot, 1, 2) = 'Ma' then
    AddFlag(Result, 'top')
  else if (WZSlot = 'Ri') (*or (ID div 10000 = 111)*) then
  begin
    AddFlag(Result, 'ring_1');
    AddFlag(Result, 'ring_2');
    AddFlag(Result, 'ring_3');
    AddFlag(Result, 'ring_4');
  end
  else if Copy(WZSlot, 1, 2) = 'Wp' then
    AddFlag(Result, 'weapon')
  else if WZSlot = 'So' then
    AddFlag(Result, 'shoe')
  else if WZSlot = 'Si' then
    AddFlag(Result, 'shield')
  else if DirName = 'Glove' then   // Some gloves have Cp instead of Gv in WZSlot, so.. o.o
    AddFlag(Result, 'glove')
  else if (ID >= 1802000) and (ID < 1802100) then
  begin
    AddFlag(Result, 'pet_equip_1');
    AddFlag(Result, 'pet_equip_2');
    AddFlag(Result, 'pet_equip_3');
  end
  else if ID = 1802100 then
    AddFlag(Result, 'pet_collar')
  else if Flags = 'pet_loot_money' then
  begin
    AddFlag(Result, 'pet_meso_magnet_1');
    AddFlag(Result, 'pet_meso_magnet_2');
    AddFlag(Result, 'pet_meso_magnet_3');
  end
  else if Flags = 'pet_loot_items' then
  begin
    AddFlag(Result, 'pet_item_pouch_1');
    AddFlag(Result, 'pet_item_pouch_2');
    AddFlag(Result, 'pet_item_pouch_3');
  end
  else if ((Flags = 'pet_auto_hp') or (Flags = 'pet_auto_mp')) and (ID < 1822000) then
    AddFlag(Result, Flags)
  else if Flags = 'pet_sweep_for_drops' then
  begin
    AddFlag(Result, 'pet_wing_boots_1');
    AddFlag(Result, 'pet_wing_boots_2');
    AddFlag(Result, 'pet_wing_boots_3');
  end
  else if Flags = 'pet_big_loot_range' then
  begin
    AddFlag(Result, 'pet_binoculars_1');
    AddFlag(Result, 'pet_binoculars_2');
    AddFlag(Result, 'pet_binoculars_3');
  end
  else if Flags = 'pet_loot_others' then
  begin
    AddFlag(Result, 'pet_magic_scales_1');
    AddFlag(Result, 'pet_magic_scales_2');
    AddFlag(Result, 'pet_magic_scales_3');
  end
  else if Flags = 'pet_loot_ignore' then
  begin
    AddFlag(Result, 'pet_item_ignore_1');
    AddFlag(Result, 'pet_item_ignore_2');
    AddFlag(Result, 'pet_item_ignore_3');
  end
  else if (ID >= 1822000) and (ID < 1832000) then
  begin
    AddFlag(Result, 'pet_label_ring_1');
    AddFlag(Result, 'pet_label_ring_2');
    AddFlag(Result, 'pet_label_ring_3');
  end
  else if (ID >= 1832000) and (ID < 1840000) then
  begin
    AddFlag(Result, 'pet_quote_ring_1');
    AddFlag(Result, 'pet_quote_ring_2');
    AddFlag(Result, 'pet_quote_ring_3');
  end
  else if WZSlot = 'Sh' then
    AddFlag(Result, 'toenail')
  else if WZSlot = 'Mb' then
    AddFlag(Result, 'monster_book')
  else if WZSlot = 'Po' then
    AddFlag(Result, 'pocket')
  else if WZSlot = 'Ba' then
    AddFlag(Result, 'badge')
  else if DirName = 'MonsterBattle' then
    AddFlag(Result, 'monster_battle')
  else if WZSlot = 'Bi' then
    AddFlag(Result, 'bits')
  else if WZSlot <> '' then
    Writeln('Unknown slot type: ' + WzSlot + ' | ' + DirName + ' | ' + IntToStr(ID));
end;

function GetItemFlagData(Info: TWZIMGEntry): string;
begin
  Result := '';
  if Info = nil then
    Exit;  // should not be the case but lol

  if Info.Get('tradeBlock', 0) = 1 then
    AddFlag(Result, 'no_trade');

  if Info.Get('notSale', 0) = 1 then
    AddFlag(Result, 'no_sale');

  if Info.Get('quest', 0) = 1 then
    AddFlag(Result, 'quest');

  if Info.Get('timeLimited', 0) = 1 then
    AddFlag(Result, 'time_limited');

  if Info.Get('pquest', 0) = 1 then
    AddFlag(Result, 'party_quest');

  if Info.Get('cash', 0) = 1 then
    AddFlag(Result, 'cash_item');

  if Info.Get('tradeAvailable', 0) = 1 then
    AddFlag(Result, 'karma_scissorable');

  if Info.Get('tradeAvailable', 0) = 2 then
    AddFlag(Result, 'premium_karma_scissorable');

  if Info.Get('expireOnLogout', 0) = 1 then
    AddFlag(Result, 'expire_on_logout');

  if Info.Get('pickUpBlock', 0) = 1 then
    AddFlag(Result, 'block_pickup');

  if Info.Get('accountSharable', 0) = 1 then
    AddFlag(Result, 'account_shareable');
end;

function GetItemConsumeFlags(Item: TWZIMGEntry): string;
begin
  Result := '';

  if (Item.Get('spec/consumeOnPickup', 0) = 1) or (Item.Get('specEx/consumeOnPickup', 0) = 1) then
    AddFlag(Result, 'auto_consume');

  if Item.Get('spec/runOnPickup', 0) = 1 then
    AddFlag(Result, 'auto_run');
end;

function GetItemCures(Item: TWZIMGEntry): string;
const
  DISEASES: array[0..4] of string = ('curse', 'darkness', 'poison', 'seal', 'weakness');
var
  i: Integer;
begin
  Result := '';

  if Item = nil then
    Exit;

  for i := 0 to High(DISEASES) do
    if Item.Get(DISEASES[i], 0) > 0 then
      AddFlag(Result, DISEASES[i]);
end;

function GetScrollFlags(Info: TWZIMGEntry): string;
begin
  Result := '';

  if Info.Get('preventslip', 0) = 1 then
    AddFlag(Result, 'prevent_slip');

  if Info.Get('warmsupport', 0) = 1 then
    AddFlag(Result, 'warm_support');

  if Info.Get('recover', 0) = 1 then
    AddFlag(Result, 'recover_slot');

  if Info.Get('randstat', 0) = 1 then
    AddFlag(Result, 'rand_stat');

  if Info.Get('incRandVol', 0) = 1 then
    AddFlag(Result, 'high_rand_range');

  if Info.Get('noNegative', 0) = 1 then
    AddFlag(Result, 'no_negative');

  if Info.Get('reset', 0) = 1 then
    AddFlag(Result, 'reset');
end;

function GetWeekdayCondition(const Day: string): Integer;
begin
  Result := -1;
  case Day[1] of
    'm': Result := 1;
    't': Result := IfThen(Day[2] = 'u', 2, 4);
    'w': Result := 3;
    'f': Result := 5;
    's': Result := IfThen(Day[2] = 'a', 6, 7);
  end;
end;

procedure AddItemTables(FDB: TDatabaseConnection);
var
  MainQ, EqQ, EqTQ, ConQ, MakerQ, RechQ, ScrollQ, ScrollTargetQ, SkillQ, SummonQ, ScriptQ: TZQuery;
  CatchQ, DrawQ, RewardQ, PotQ, CardQ, RecipeQ, LevelQ, LSkillQ, EqAdditionQ: TZQuery;
  Data, EqData: TWZArchive;
  Sub: TWZDirectory;
  Group: TWZFile;
  Item, Adv, Iter, Stat, Iter2: TWZIMGEntry;
  i, ID, A, B, C, D, E, F: Integer;
  FolderName: string;

  procedure GetData(Entry: TWZIMGEntry; IsEquipOrPet: Boolean = False);
  var
    s: string;
    p: Double;
    i: Integer;
    E: TWZIMGEntry;
  begin
    if Entry.Name = 'pack_ignore' then
      Exit;

    ID := NoIMG(Entry.Name);
    MainQ.ParamByName('id').AsInteger := ID;
    MainQ.ParamByName('price').AsInteger := Entry.Get('info/price', 0);
    if IsEquipOrPet or (ID = 5110000) then
      MainQ.ParamByName('msq').AsInteger := Entry.Get('info/slotMax', 1)
    else
      MainQ.ParamByName('msq').AsInteger := Entry.Get('info/slotMax', 100);
    MainQ.ParamByName('mpc').AsInteger := Entry.Get('info/only', 0);
    if IsEquipOrPet or (ID div 10000 = 207) then
      MainQ.ParamByName('minlvl').AsInteger := Entry.Get('info/reqLevel', 1)
    else
      MainQ.ParamByName('minlvl').AsInteger := Entry.Get('info/lvMin', 1);
    MainQ.ParamByName('maxlvl').AsInteger := Entry.Get('info/lvMax', 200);
    E := Entry.Get('info/exp');
    if Assigned(E) and (E.Children.Count = 0) then
      MainQ.ParamByName('exp').AsInteger := E.Data
    else
      MainQ.ParamByName('exp').AsInteger := 0;
    MainQ.ParamByName('money').AsInteger := Entry.Get('info/meso', 0);
    MainQ.ParamByName('sci').AsInteger := Entry.Get('info/stateChangeItem', 0);
    MainQ.ParamByName('lfm').AsInteger := Entry.Get('info/lv', 0);
    MainQ.ParamByName('npc').AsInteger := Entry.Get('spec/npc', 0);
    MainQ.ParamByName('flags').AsString := GetItemFlagData(Entry.Child['info']);
    MainQ.ExecSQL;

    p := Entry.Get('info/unitPrice', 0.0);
    i := Entry.Get('info/incPAD', 0);
    if ((not IsZero(p)) or (i <> 0)) and (ID div 10000 in [206, 207, 233]) then
    begin
      RechQ.ParamByName('id').AsInteger := ID;
      RechQ.ParamByName('price').AsFloat := p;
      RechQ.ParamByName('watk').AsInteger := i;
      RechQ.ExecSQL;
    end;

    s := Entry.Get('spec/script', '');
    if s <> '' then
      ExecScriptQuery(ScriptQ, 'item', s, -1, ID);

    Adv := Entry.Child['info'].Child['consumeItem'];
    if Adv <> nil then
    begin
      DrawQ.ParamByName('id').AsInteger := ID;
      for E in Adv.Children do
      begin
        if (Length(E.Name) > 2) or (E.Children.Count = 0) then
          Continue;   // other properties like consumeCount

        DrawQ.ParamByName('consume').AsInteger := E.Get('0', 0);
        DrawQ.ParamByName('inc').AsInteger := E.Get('1', 0);
        try
        DrawQ.ExecSQL;
        except
          // already in there. see 4220154
        end;
      end;
    end;

    Adv := Entry.Child['info'].Child['mob'];
    if Adv <> nil then
    begin
      CardQ.ParamByName('card').AsInteger := ID;
      CardQ.ParamByName('mob').AsInteger := Adv.Data;
      CardQ.ExecSQL;
    end;

    Adv := Entry.Child['reward'];
    if Adv <> nil then
    begin
      RewardQ.ParamByName('id').AsInteger := ID;
      for E in Adv.Children do
      begin
        RewardQ.ParamByName('rid').AsInteger := E.Get('item', 0);
        s := E.Get('prob', '0');
        if Pos('[R8]', s) = 1 then
          RewardQ.ParamByName('prob').AsFloat := StrToFloat(Copy(s, 5, 5))
        else
          RewardQ.ParamByName('prob').AsInteger := StrToInt(s);
        RewardQ.ParamByName('count').AsInteger := E.Get('count', 0);
        RewardQ.ParamByName('effect').AsString := E.Get('Effect', '');
        RewardQ.ParamByName('period').AsInteger := E.Get('period', 0);
        RewardQ.ParamByName('grade').AsInteger := E.Get('statGrade', 0);
        RewardQ.ExecSQL;
      end;
    end;

    i := Entry.Get('info/nickSkill', 0);
    if i > 0 then
    begin
      SkillQ.ParamByName('id').AsInteger := ID;
      SkillQ.ParamByName('type').AsString := 'title_skill';
      SkillQ.ParamByName('sid').AsInteger := i;
      SkillQ.ParamByName('slv').AsInteger := 1;
      SkillQ.ParamByName('mlv').AsInteger := 0;
      SkillQ.ParamByName('chance').AsInteger := 0;
      SkillQ.ExecSQL;
    end;

    if ID div 10000 <> 425 then
      Exit;

    MakerQ.ParamByName('id').AsInteger := ID;
    for i := 1 to MakerQ.Params.Count - 1 do
      MakerQ.Params[i].AsInteger := Entry.Get('info/' + MakerQ.Params[i].Name, 0);
    MakerQ.ExecSQL;
  end;

begin
  MainQ := FDB.GetQuery;
  MainQ.SQL.Text := 'CREATE TABLE `item_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`price` int(11) NOT NULL DEFAULT ''1'', ' +
  '`max_slot_quantity` smallint(6) NOT NULL DEFAULT ''1'', ' +
  '`max_possession_count` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`min_level` tinyint(3) unsigned NOT NULL DEFAULT ''1'', ' +
  '`max_level` tinyint(3) unsigned NOT NULL DEFAULT ''200'', ' +
  '`experience` int(11) NOT NULL DEFAULT ''0'', ' +
  '`money` int(11) NOT NULL DEFAULT ''0'', ' +
  '`state_change_item` int(11) NOT NULL DEFAULT ''0'', ' +
  '`level_for_maker` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`npc` int(11) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''time_limited'',''no_trade'',''no_sale'',''karma_scissorable'',''premium_karma_scissorable'',''expire_on_logout'',''block_pickup'',''quest'',''cash_item'',''party_quest'',''account_shareable'') NOT NULL DEFAULT '''', ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  MainQ.ExecSQL;

  EqQ := FDB.GetQuery;
  EqQ.SQL.Text := 'CREATE TABLE `item_equip_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`flags` set(''wear_trade_block'',''pet_big_loot_range'',''pet_auto_hp'',''pet_auto_mp'',''pet_sweep_for_drops'',''pet_loot_money'',''pet_loot_items'',''pet_loot_others'',''pet_loot_ignore'',''set_joker'') NOT NULL DEFAULT '''', ' +
  '`equip_slots` set(''helmet'',''face_accessory'',''eye_accessory'',''earring'',''top'',''bottom'',''shoe'',''glove'',''cape'',''shield'',''weapon'',' +
   '''ring_1'',''ring_2'',''pet_equip_1'',''ring_3'',''ring_4'',''pendant'',''taming_mob'',''saddle'',''pet_collar'',''pet_label_ring_1'',''pet_item_pouch_1'',''pet_meso_magnet_1'',''pet_auto_hp'',''pet_auto_mp'',''pet_wing_boots_1'',' +
   '''pet_binoculars_1'',''pet_magic_scales_1'',''pet_quote_ring_1'',''pet_equip_2'',''pet_label_ring_2'',''pet_quote_ring_2'',''pet_item_pouch_2'',''pet_meso_magnet_2'',''pet_wing_boots_2'',''pet_binoculars_2'',''pet_magic_scales_2'',''pet_equip_3'',' +
   '''pet_label_ring_3'',''pet_quote_ring_3'',''pet_item_pouch_3'',''pet_meso_magnet_3'',''pet_wing_boots_3'',''pet_binoculars_3'',''pet_magic_scales_3'',''pet_item_ignore_1'',''pet_item_ignore_2'',''pet_item_ignore_3'',''medal'',''belt'',''toenail'',' +
   '''monster_book'',''pocket'',''badge'',''monster_battle'',''bits'') NOT NULL DEFAULT '''', ' +
  '`attack_speed` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`heal_hp` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`scroll_slots` smallint(6) unsigned NOT NULL DEFAULT ''0'', ' +
  '`specialid` int(11) NOT NULL, ' +
  '`req_str` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`req_dex` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`req_int` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`req_luk` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`req_fame` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`req_job` set(''beginner'',''warrior'',''magician'',''bowman'',''thief'',''pirate'') NOT NULL, ' +
  '`hp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`mp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`hp_percentage` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`mp_percentage` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`strength` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`dexterity` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`intelligence` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`luck` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`hands` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`weapon_attack` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`weapon_defense` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`magic_attack` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`magic_defense` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`accuracy` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`avoid` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`jump` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`speed` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`traction` double NOT NULL DEFAULT ''1'', ' +
  '`recovery` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`knockback` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`taming_mob` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`inc_lightning_damage` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`inc_ice_damage` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`inc_fire_damage` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`inc_poison_damage` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`elemental_default` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`after_image` varchar(10) NOT NULL DEFAULT '''', ' +
  '`setid` int(11) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  EqQ.ExecSQL;

  EqTQ := FDB.GetQuery;
  EqTQ.SQL.Text := 'CREATE TABLE `item_equip_traits` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`ambition_exp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`insight_exp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`willpower_exp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`diligence_exp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`empathy_exp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`charm_exp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  EqTQ.ExecSQL;

  ConQ := FDB.GetQuery;
  ConQ.SQL.Text := 'CREATE TABLE `item_consume_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`flags` set(''auto_consume'',''auto_run'',''no_mouse_cancel'',''ignore_continent'',''party_item'',' +
    '''ghost'',''barrier'',''prevent_drowning'',''prevent_freezing'',''override_traction'',''meso_up'',''drop_up_for_party'',''ignore_physical_defense'',''ignore_magic_defense'') NOT NULL DEFAULT '''', ' +
  '`cure_ailments` set(''darkness'',''poison'',''curse'',''seal'',''weakness'') NOT NULL DEFAULT '''', ' +
  '`effect` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`hp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`mp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`hp_percentage` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`mp_percentage` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`move_to` int(11) NOT NULL DEFAULT ''0'', ' +
  '`decrease_hunger` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`decrease_fatigue` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`carnival_points` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`create_item` int(11) NOT NULL DEFAULT ''0'', ' +
  '`prob` tinyint(3) unsigned NOT NULL DEFAULT ''0'', ' +
  '`buff_time` int(11) NOT NULL DEFAULT ''0'', ' +
  '`weapon_attack` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`magic_attack` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`weapon_defense` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`magic_defense` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`accuracy` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`avoid` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`speed` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`jump` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`morph` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`strength` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`dexterity` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`intelligence` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`luck` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_watk` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_matk` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_maxhp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_maxmp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_maxhp2` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_maxmp2` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_speed` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_jump` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`indie_allstat` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`growth` smallint(6) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  ConQ.ExecSQL;

  RechQ := FDB.GetQuery;
  RechQ.SQL.Text := 'CREATE TABLE `item_projectile_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`unit_price` double(2,1) NOT NULL, ' +
  '`weapon_attack` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Contains properties that only projectiles have.'';';
  RechQ.ExecSQL;

  MakerQ := FDB.GetQuery;
  MakerQ.SQL.Text := 'CREATE TABLE `item_maker_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`inc_str` smallint(6) NOT NULL, ' +
  '`inc_dex` smallint(6) NOT NULL, ' +
  '`inc_int` smallint(6) NOT NULL, ' +
  '`inc_luk` smallint(6) NOT NULL, ' +
  '`inc_hp` smallint(6) NOT NULL, ' +
  '`inc_mp` smallint(6) NOT NULL, ' +
  '`inc_watk` smallint(6) NOT NULL, ' +
  '`inc_matk` smallint(6) NOT NULL, ' +
  '`inc_acc` smallint(6) NOT NULL, ' +
  '`inc_avoid` smallint(6) NOT NULL, ' +
  '`inc_jump` smallint(6) NOT NULL, ' +
  '`inc_speed` smallint(6) NOT NULL, ' +
  '`inc_req_level` tinyint(3) NOT NULL, ' +
  '`rand_option` tinyint(3) NOT NULL, ' +
  '`rand_stat` tinyint(3) NOT NULL, ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  MakerQ.ExecSQL;

  ScrollQ := FDB.GetQuery;
  ScrollQ.SQL.Text := 'CREATE TABLE `item_scroll_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`success` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`break_item` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`flags` set(''rand_stat'',''recover_slot'',''warm_support'',''prevent_slip'',''high_rand_range'',''no_negative'',''reset'') NOT NULL DEFAULT '''', ' +
  '`istr` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`idex` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`iint` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`iluk` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`ihp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`imp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`iwatk` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`imatk` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`iwdef` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`imdef` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`iacc` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`iavo` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`ijump` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`ispeed` smallint(6) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT=''Contains all the data revelant to scroll items.'';';
  ScrollQ.ExecSQL;

  ScrollTargetQ := FDB.GetQuery;
  ScrollTargetQ.SQL.Text := 'CREATE TABLE `item_scroll_targets` (' +
  '`id` bigint(21) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`scrollid` int(11) NOT NULL, ' +
  '`req_itemid` int(11) NOT NULL, ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  ScrollTargetQ.ExecSQL;

  SkillQ := FDB.GetQuery;
  SkillQ.SQL.Text := 'CREATE TABLE `item_skills` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`type` enum(''book'',''mob_skill'',''title_skill'') NOT NULL, ' +
  '`skillid` int(11) NOT NULL, ' +
  '`skill_level` tinyint(3) unsigned NOT NULL, ' +
  '`master_level` tinyint(3) unsigned NOT NULL, ' +
  '`chance` tinyint(3) NOT NULL, ' +
  'PRIMARY KEY (`itemid`,`skillid`,`master_level`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  SkillQ.ExecSQL;

  SummonQ := FDB.GetQuery;
  SummonQ.SQL.Text := 'CREATE TABLE `item_summons` (' +
  '`id` bigint(20) NOT NULL AUTO_INCREMENT, ' +
  '`itemid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`mobid` int(11) NOT NULL DEFAULT ''0'', ' +
  '`chance` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`id`), ' +
  'KEY `itemid` (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  SummonQ.ExecSQL;

  RewardQ := FDB.GetQuery;
  RewardQ.SQL.Text := 'CREATE TABLE `item_reward_data` (' +
  '`id` bigint(21) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`itemid` int(11) NOT NULL, ' +
  '`rewardid` int(11) NOT NULL, ' +
  '`prob` double unsigned NOT NULL, ' +
  '`quantity` smallint(6) NOT NULL, ' +
  '`expiration` int(11) NOT NULL DEFAULT ''0'', ' +
  '`stat_grade` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`effect` varchar(35) DEFAULT NULL, ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  RewardQ.ExecSQL;

  CatchQ := FDB.GetQuery;
  CatchQ.SQL.Text := 'CREATE TABLE `item_catch_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`mobid` int(11) NOT NULL, ' +
  '`createid` int(11) NOT NULL, ' +
  '`hp_percentage` tinyint(3) NOT NULL, ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  CatchQ.ExecSQL;

  DrawQ := FDB.GetQuery;
  DrawQ.SQL.Text := 'CREATE TABLE `item_draw_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`consumeid` int(11) NOT NULL, ' +
  '`status_increase` smallint(6) NOT NULL, ' +
  'PRIMARY KEY (`itemid`,`consumeid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  DrawQ.ExecSQL;

  PotQ := FDB.GetQuery;
  PotQ.SQL.Text := 'CREATE TABLE `item_potential_data` (' +
  '`statid` smallint(5) unsigned NOT NULL, ' +
  '`item_level` tinyint(3) unsigned NOT NULL, ' +
  '`property` varchar(30) NOT NULL, ' +
  '`value` int(11) NOT NULL, ' +
  'PRIMARY KEY (`statid`, `item_level`, `property`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  PotQ.ExecSQL;

  CardQ := FDB.GetQuery;
  CardQ.SQL.Text := 'CREATE TABLE `monster_card_data` (' +
  '`cardid` int(11) unsigned NOT NULL, ' +
  '`mobid` int(11) unsigned NOT NULL, ' +
  'PRIMARY KEY (`cardid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  CardQ.ExecSQL;

  RecipeQ := FDB.GetQuery;
  RecipeQ.SQL.Text := 'CREATE TABLE `item_recipe_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`recipe` int(11) NOT NULL, ' +
  '`req_skill` int(11) NOT NULL DEFAULT ''0'', ' +
  '`req_skill_level` tinyint(3) NOT NULL, ' +
  '`recipe_use_count` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`recipe_valid_day` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  RecipeQ.ExecSQL;

  LevelQ := FDB.GetQuery;
  LevelQ.SQL.Text := 'CREATE TABLE `item_level_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`item_level` tinyint(3) NOT NULL, ' +
  '`experience` smallint(6) NOT NULL, ' +
  '`str_min` smallint(6) NOT NULL, ' +
  '`str_max` smallint(6) NOT NULL, ' +
  '`dex_min` smallint(6) NOT NULL, ' +
  '`dex_max` smallint(6) NOT NULL, ' +
  '`int_min` smallint(6) NOT NULL, ' +
  '`int_max` smallint(6) NOT NULL, ' +
  '`luk_min` smallint(6) NOT NULL, ' +
  '`luk_max` smallint(6) NOT NULL, ' +
  '`speed_min` smallint(6) NOT NULL, ' +
  '`speed_max` smallint(6) NOT NULL, ' +
  '`jump_min` smallint(6) NOT NULL, ' +
  '`jump_max` smallint(6) NOT NULL, ' +
  '`weapon_defense_min` smallint(6) NOT NULL, ' +
  '`weapon_defense_max` smallint(6) NOT NULL, ' +
  '`weapon_attack_min` smallint(6) NOT NULL, ' +
  '`weapon_attack_max` smallint(6) NOT NULL, ' +
  '`magic_defense_min` smallint(6) NOT NULL, ' +
  '`magic_defense_max` smallint(6) NOT NULL, ' +
  '`magic_attack_min` smallint(6) NOT NULL, ' +
  '`magic_attack_max` smallint(6) NOT NULL, ' +
  '`hp_min` smallint(6) NOT NULL, ' +
  '`hp_max` smallint(6) NOT NULL, ' +
  '`mp_min` smallint(6) NOT NULL, ' +
  '`mp_max` smallint(6) NOT NULL, ' +
  '`accuracy_min` smallint(6) NOT NULL, ' +
  '`accuracy_max` smallint(6) NOT NULL, ' +
  '`avoidability_min` smallint(6) NOT NULL, ' +
  '`avoidability_max` smallint(6) NOT NULL, ' +
  ' PRIMARY KEY (`itemid`,`item_level`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  LevelQ.ExecSQL;

  LSkillQ := FDB.GetQuery;
  LSkillQ.SQL.Text := 'CREATE TABLE `item_level_skills` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`item_level` tinyint(3) NOT NULL, ' +
  '`skill_type` enum(''player_skill'',''equip_skill'') NOT NULL, ' +
  '`skillid` int(11) NOT NULL, ' +
  '`skill_level` tinyint(3) NOT NULL, ' +
  '`probability` tinyint(3) NOT NULL, ' +
  ' PRIMARY KEY (`itemid`,`item_level`,`skillid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  LSkillQ.ExecSQL;

  EqAdditionQ := FDB.GetQuery;
  EqAdditionQ.SQL.Text := 'CREATE TABLE `item_equip_additions` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`condition_type` enum(''none'',''job'',''weekday'',''level'',''mob_category'') NOT NULL, ' +
  '`conditionid` int(11) NOT NULL COMMENT ''Monday is 1, Sunday is 7.'', ' +
  '`hp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`mp` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`hp_percentage` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`mp_percentage` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`strength` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`dexterity` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`intelligence` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`luck` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`weapon_attack` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`weapon_defense` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`magic_attack` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`magic_defense` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`accuracy` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`avoid` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`jump` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`speed` smallint(6) NOT NULL DEFAULT ''0'', ' +
  '`critical_rate` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`critical_min_damage` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`damage` smallint(6) NOT NULL DEFAULT ''0'', ' +
  'PRIMARY KEY (`itemid`,`conditionid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  EqAdditionQ.ExecSQL;

  MainQ.SQL.Text := 'INSERT INTO item_data VALUES (:id, :price, :msq, :mpc, :minlvl, ' +
                    ':maxlvl, :exp, :money, :sci, :lfm, :npc, :flags)';
  EqQ.SQL.Text := 'INSERT INTO item_equip_data VALUES (:id, :flags, :slots, :aspd, ' +
                  ':hhp, :ss, :sid, :rstr, :rdex, :rint, :rluk, :rfame, :rjob, :hp, :mp, :hpr, :mpr, ' +
                  ':str, :dex, :int, :luk, :hands, :watk, :wdef, :matk, :mdef, :acc, ' +
                  ':avo, :jmp, :spd, :traction, :rcvry, :kb, :tmob, :ild, :iid, :ifd, :ipd, :edef, :aimg, :setid)';
  EqTQ.SQL.Text := 'INSERT INTO item_equip_traits VALUES (:id, :ambition, :insight, :willpower, :diligence, :empathy, :charm)';
  ConQ.SQL.Text := 'INSERT INTO item_consume_data VALUES (:id, :flags, :cures, 0, :hp, :mp, ' +
                   ':hpR, :mpR, :moveTo, :inc, :incFatigue, :cp, :create, :prob, :time, ' +
                   ':pad, :mad, :pdd, :mdd, :acc, :eva, :speed, :jump, :morph, ' +
                   ':str, :dex, :int, :luk, :indiePad, :indieMad, :indieMhp, :indieMmp, :imhp, :immp, ' +
                   ':indieSpeed, :indieJump, :indieAllStat, :inflation)';
  RechQ.SQL.Text := 'INSERT INTO item_projectile_data VALUES (:id, :price, :watk)';
  MakerQ.SQL.Text := 'INSERT INTO item_maker_data VALUES (:id, :incSTR, :incDEX, :incINT, :incLUK, ' +
                     ':incMaxHP, :incMaxMP, :incPAD, :incMAD, :incACC, :incEVA, ' +
                     ':incJump, :incSpeed, :incReqLevel, :randOption, :randStat)';
  ScrollQ.SQL.Text := 'INSERT INTO item_scroll_data VALUES (:id, :success, :cursed, :flags, ' +
                      ':incSTR, :incDEX, :incINT, :incLUK, :incMHP, :incMMP, :incPAD, :incMAD, ' +
                      ':incPDD, :incMDD, :incACC, :incEVA, :incJump, :incSpeed)';
  ScrollTargetQ.SQL.Text := 'INSERT INTO item_scroll_targets VALUES (DEFAULT, :id, :req)';
  SkillQ.SQL.Text := 'INSERT INTO item_skills VALUES (:id, :type, :sid, :slv, :mlv, :chance)';
  SummonQ.SQL.Text := 'INSERT INTO item_summons VALUES (DEFAULT, :id, :mob, :chance)';
  RewardQ.SQL.Text := 'INSERT INTO item_reward_data VALUES (DEFAULT, :id, :rid, :prob, :count, :period, :grade, :effect)';
  CatchQ.SQL.Text := 'INSERT INTO item_catch_data VALUES (:id, :mob, :create, :hp)';
  DrawQ.SQL.Text := 'INSERT INTO item_draw_data VALUES (:id, :consume, :inc)';
  PotQ.SQL.Text := 'INSERT INTO item_potential_data VALUES (:stat, :lv, :prop, :val)';
  CardQ.SQL.Text := 'INSERT INTO monster_card_data VALUES (:card, :mob)';
  RecipeQ.SQL.Text := 'INSERT INTO item_recipe_data VALUES (:id, :recipe, :skill, :slv, :count, :day)';
  LevelQ.SQL.Text := 'INSERT INTO item_level_data VALUES (:id, :lv, :exp, :incSTRMin, :incSTRMax, :incDEXMin, :incDEXMax, ' +
                     ':incINTMin, :incINTMax, :incLUKMin, :incLUKMax, :incSpeedMin, :incSpeedMax, :incJumpMin, :incJumpMax, ' +
                     ':incPDDMin, :incPDDMax, :incPADMin, :incPADMax, :incMDDMin, :incMDDMax, :incMADMin, :incMADMax, ' +
                     ':incMHPMin, :incMHPMax, :incMMPMin, :incMMPMax, :incACCMin, :incACCMax, :incEVAMin, :incEVAMax)';
  LSkillQ.SQL.Text := 'INSERT INTO item_level_skills VALUES (:id, :lv, :type, :sid, :slv, :prob)';
  EqAdditionQ.SQL.Text := 'INSERT INTO item_equip_additions VALUES (:id, :cond, :condid, :incMHP, :incMMP, :incMHPr, :incMMPr, ' +
                          ':incSTR, :incDEX, :incINT, :incLUK, :incPAD, :incPDD, :incMAD, :incMDD, :incACC, :incEVA, :incJump, :incSpeed, :crit, :critMin, :dmg)';
  ScriptQ := FDB.GetQuery;
  ScriptQ.SQL.Text := SCRIPT_QUERY;

  Data := LoadArchive('Item.wz');
  for Sub in Data.Root.SubDirs do     // Folders Consume, Install, Pet, Etc, Cash
    for Group in Sub.Files do         // 0201.img, 0417.img etc.
    begin
      if (Sub.Name = 'Special') or not TryStrToInt(ChangeFileExt(Group.Name, ''), i) then
        Continue;

      with Data.ParseFile(Group) do
      begin
        if Sub.Name <> 'Pet' then
        begin
          for Item in Root.Children do
            try
              GetData(Item)
            except
              Writeln('[Data fail] ', Item.Name, ' - duplicate');
            end;
        end
        else   // WZ/Pet/5xxxx.img/info -- not WZ/Blah/xxxx.img/xxxx/info
          GetData(Root, True);

        if (Sub.Name = 'Consume') or (Sub.Name = 'Cash') then
        begin
          for Item in Root.Children do
          begin
            if Item.Name = 'pack_ignore' then
              Continue;
            ID := NoIMG(Item.Name);
            if (ID div 1000000 = 5) and (Item.Child['spec'] = nil) then
              Continue;

            ConQ.ParamByName('id').AsInteger := ID;
            ConQ.ParamByName('flags').AsString := GetItemConsumeFlags(Item);
            ConQ.ParamByName('cures').AsString := GetItemCures(Item.Child['spec']);
            for i := 3 to ConQ.Params.Count - 1 do
              if ConQ.Params[i].Name <> 'incFatigue' then
                ConQ.Params[i].AsInteger := Item.Get('spec/' + ConQ.Params[i].Name, 0)
              else
                ConQ.Params[i].AsInteger := Abs(Int32(Item.Get('spec/' + ConQ.Params[i].Name, 0)));
            ConQ.ExecSQL;

            Adv := Item.Child['info'].Child['skill'];
            if Adv <> nil then
            begin
              SkillQ.ParamByName('id').AsInteger := ID;
              SkillQ.ParamByName('type').AsString := 'book';
              SkillQ.ParamByName('slv').AsInteger := Item.Get('info/reqSkillLevel', 0);
              SkillQ.ParamByName('mlv').AsInteger := Item.Get('info/masterLevel', 0);
              SkillQ.ParamByName('chance').AsInteger := Item.Get('info/success', 0);
              for Iter in Adv.Children do
              begin
                if Iter.DataType = mdtString then
                  SkillQ.ParamByName('sid').AsString := (Iter.Data)
                else
                  SkillQ.ParamByName('sid').AsInteger := Iter.Data;
                try
                SkillQ.ExecSQL;
                except
                  // ............
                  Writeln('[Data fail] ', ID, ' - duplicate ', Iter.Data);
                end;
              end;
            end;

            i := Item.Get('specEx/0/mobSkill', -1);
            if i > -1 then
            begin
              SkillQ.ParamByName('id').AsInteger := ID;
              SkillQ.ParamByName('type').AsString := 'mob_skill';
              SkillQ.ParamByName('sid').AsInteger := i;
              SkillQ.ParamByName('slv').AsInteger := Item.Get('specEx/0/level', 0);
              SkillQ.ParamByName('mlv').AsInteger := 0;
              SkillQ.ParamByName('chance').AsInteger := 0;
              SkillQ.ExecSQL;
            end;

            Adv := Item.Child['mob'];
            if Adv <> nil then
            begin
              SummonQ.ParamByName('id').AsInteger := ID;
              for Iter in Adv.Children do
              begin
                SummonQ.ParamByName('mob').AsInteger := Iter.Get('id', 0);
                SummonQ.ParamByName('chance').AsInteger := Iter.Get('prob', 100);
                SummonQ.ExecSQL;
              end;
            end;

            i := Item.Get('info/create', 0);
            if i > 0 then
            begin
              CatchQ.ParamByName('id').AsInteger := ID;
              CatchQ.ParamByName('mob').AsInteger := Item.Get('info/mob', 0);
              CatchQ.ParamByName('create').AsInteger := i;
              CatchQ.ParamByName('hp').AsInteger := Item.Get('info/mobHP', 100);
              try
                CatchQ.ExecSQL;
              except
                // 8000% HP? Fuck you.
              end;
            end;

            i := Item.Get('spec/recipe', 0);
            if i > 0 then
            begin
              RecipeQ.ParamByName('id').AsInteger := ID;
              RecipeQ.ParamByName('recipe').AsInteger := i;
              RecipeQ.ParamByName('skill').AsInteger := Item.Get('spec/reqSkill', 0);
              RecipeQ.ParamByName('slv').AsInteger := Item.Get('spec/reqSkillLevel', 0);
              RecipeQ.ParamByName('count').AsInteger := Item.Get('spec/recipeUseCount', 0);
              RecipeQ.ParamByName('day').AsInteger := Item.Get('spec/recipeValidDay', 0);
              RecipeQ.ExecSQL;
            end;

            case (ID div 10000 - 200) of
              4, 53, 57, 61, 64: ;
              else Continue;
            end;

            ScrollQ.ParamByName('id').AsInteger := ID;
            for i := 1 to ScrollQ.Params.Count - 1 do
              if i = 3 then
                ScrollQ.Params[i].AsString := GetScrollFlags(Item.Child['info'])
              else
              begin
                Adv := Item.Get('info/' + ScrollQ.Params[i].Name);
                if Adv = nil then
                begin
                  ScrollQ.Params[i].AsInteger := 0;
                  Continue;
                end;

                if Adv.DataType <> mdtString then
                  ScrollQ.Params[i].AsInteger := Adv.Data
                else // Nexon Europe... no comment
                  ScrollQ.Params[i].AsInteger := StrToInt(StringReplace((Adv.Data), '%', '', []));
              end;
            ScrollQ.ExecSQL;

            Adv := Item.Child['req'];
            if Adv <> nil then
            begin
              ScrollTargetQ.ParamByName('id').AsInteger := ID;
              for Iter in Adv.Children do
              begin
                ScrollTargetQ.ParamByName('req').AsInteger := Iter.Data;
                ScrollTargetQ.ExecSQL;
              end;
            end;
          end;
        end;

        Free;
      end;
    end;

  with Data.GetImgFile('ItemOption.img') do
  begin
    for Stat in Root.Children do
    begin
      PotQ.ParamByName('stat').AsInteger := StrToInt(Stat.Name);

      for Iter in Stat.Child['level'].Children do
      begin
        PotQ.ParamByName('lv').AsInteger := StrToInt(Iter.Name);
        for Iter2 in Iter.Children do
        begin
          if (Iter2.DataType = mdtString) and (not TryStrToInt((Iter2.Data), i)) then
            Continue;

          PotQ.ParamByName('prop').AsString := Iter2.Name;
          PotQ.ParamByName('val').AsInteger := Iter2.Data;
          PotQ.ExecSQL;
        end;
      end;
    end;

    Free;
  end;

  AddPetData(FDB, Data);

  CatchQ.Free;
  DrawQ.Free;
  MakerQ.Free;
  RechQ.Free;
  RewardQ.Free;
  ScriptQ.Free;
  ScrollQ.Free;
  ScrollTargetQ.Free;
  SkillQ.Free;
  SummonQ.Free;
  RecipeQ.Free;
  PotQ.Free;
  Data.Free;

  EqData := LoadArchive('Character.wz');
  for Sub in EqData.Root.SubDirs do
    if (Sub.Name <> 'Afterimage') and (Sub.Name <> 'Face') and (Sub.Name <> 'Hair') and (Sub.Name <> 'Familiar') then
      for Group in Sub.Files do
        with EqData.ParseFile(Group) do
        begin
          try
            GetData(Root, True);
          except
            // Welcome to AsiaSoft. We decided to put some shoes in the Coat directory, too.
            Free;
            Continue;
          end;

          Item := Root.Child['info'];
          with EqQ, Item do
          begin
            ParamByName('id').AsInteger := NoIMG(Root.Name);
            ParamByName('flags').AsString := GetEquipFlagData(Item);
            ParamByName('slots').AsString := GetEquipSlot(Get('islot', ''),
                Sub.Name, ParamByName('flags').AsString, NoIMG(Root.Name));
            ParamByName('aspd').AsInteger := Get('attackSpeed', 0);
            ParamByName('hhp').AsInteger := Get('incHP', 0);
            ParamByName('ss').AsInteger := Get('tuc', 0);
            ParamByName('sid').AsInteger := Get('specialID', 0);
            ParamByName('rstr').AsInteger := Get('reqSTR', 0);
            ParamByName('rdex').AsInteger := Get('reqDEX', 0);
            ParamByName('rint').AsInteger := Get('reqINT', 0);
            ParamByName('rluk').AsInteger := Get('reqLUK', 0);
            ParamByName('rfame').AsInteger := Get('reqPOP', 0);
            ParamByName('rjob').AsString := GetEquipJob(Get('reqJob', 0));
            ParamByName('hp').AsInteger := Get('incMHP', 0);
            ParamByName('mp').AsInteger := Get('incMMP', 0);
            ParamByName('hpr').AsInteger := Get('incMHPr', 0);
            ParamByName('mpr').AsInteger := Get('incMMPr', 0);
            ParamByName('str').AsInteger := Get('incSTR', 0);
            ParamByName('dex').AsInteger := Get('incDEX', 0);
            ParamByName('int').AsInteger := Get('incINT', 0);
            ParamByName('luk').AsInteger := Get('incLUK', 0);
            ParamByName('hands').AsInteger := Get('incCraft', 0);
            ParamByName('watk').AsInteger := Get('incPAD', 0);
            ParamByName('wdef').AsInteger := Get('incPDD', 0);
            ParamByName('matk').AsInteger := Get('incMAD', 0);
            ParamByName('mdef').AsInteger := Get('incMDD', 0);
            ParamByName('acc').AsInteger := Get('incACC', 0);
            ParamByName('avo').AsInteger := Get('incEVA', 0);
            ParamByName('jmp').AsInteger := Get('incJump', 0);
            ParamByName('spd').AsInteger := Get('incSpeed', 0);
            ParamByName('traction').AsInteger := Get('fs', 1);
            ParamByName('rcvry').AsInteger := Round(Get('recovery', 0));
            ParamByName('kb').AsInteger := Get('knockback', 0);
            ParamByName('tmob').AsInteger := Get('tamingMob', 0);
            ParamByName('ild').AsInteger := Get('incRMAL', 100);
            ParamByName('iid').AsInteger := Get('incRMAI', 100);
            ParamByName('ifd').AsInteger := Get('incRMAF', 100);
            ParamByName('ipd').AsInteger := Get('incRMAS', 100);
            ParamByName('edef').AsInteger := Get('elemDefault', 100);
            ParamByName('aimg').AsString := Get('afterImage', '');
            ParamByName('setid').AsInteger := Get('setItemID', 0);
            ExecSQL;
          end;

          with Item do
          begin
            A := Get('charismaEXP', 0);
            B := Get('insightEXP', 0);
            C := Get('willEXP', 0);
            D := Get('craftEXP', 0);
            E := Get('senseEXP', 0);
            F := Get('charmEXP', 0);
          end;
          if (A > 0) or (B > 0) or (C > 0) or (D > 0) or (E > 0) or (F > 0) then
          begin
            EqTQ.ParamByName('id').AsInteger := NoIMG(Root.Name);
            EqTQ.ParamByName('ambition').AsInteger := A;
            EqTQ.ParamByName('insight').AsInteger := B;
            EqTQ.ParamByName('willpower').AsInteger := C;
            EqTQ.ParamByName('diligence').AsInteger := D;
            EqTQ.ParamByName('empathy').AsInteger := E;
            EqTQ.ParamByName('charm').AsInteger := F;
            EqTQ.ExecSQL;
          end;

          Adv := Item.Child['level'];
          if (Adv <> nil) then
          begin
            LevelQ.ParamByName('id').AsInteger := NoIMG(Root.Name);
            for Iter in Adv.Child['info'].Children do
            begin
              LevelQ.ParamByName('lv').AsString := Iter.Name;
              LevelQ.ParamByName('exp').AsInteger := Iter.Get('exp', 0);
              for i := 3 to LevelQ.Params.Count - 1 do
                LevelQ.Params[i].AsInteger := Iter.Get(LevelQ.Params[i].Name, 0);
              LevelQ.ExecSQL;
            end;

            Adv := Adv.Child['case'];
            if Adv <> nil then
            begin
              for Iter in Adv.Children do
              begin
                Assert(Iter.Name <> '2', 'Unknown item_level_skills data!');
                for Iter2 in Iter.Children do
                begin
                  if Length(Iter2.Name) > 2 then
                    Continue;

                  // When Skill is nil, ItemSkill is not (v117) -- but nothing is known about it anyway, so we leave it out
                  if Iter2.Child['Skill'] <> nil then
                  begin
                    FolderName := 'Skill';
                    LSkillQ.ParamByName('type').AsString := 'player_skill';
                  end
                  else if Iter2.Child['EquipmentSkill'] <> nil then
                  begin
                    FolderName := 'EquipmentSkill';
                    LSkillQ.ParamByName('type').AsString := 'equip_skill';
                  end
                  else
                  begin
                    if Iter2.Child['ItemSkill'] = nil then
                      Writeln('Check ', Root.Name, ' for new item_level_skills data.');
                    Continue;
                  end;

                  LSkillQ.ParamByName('id').AsInteger := NoIMG(Root.Name);
                  LSkillQ.ParamByName('lv').AsString := Iter2.Name;
                  LSkillQ.ParamByName('prob').AsInteger := Iter.Get('prob', 0);
                  for Stat in Iter2.Child[FolderName].Children do
                  begin
                    LSkillQ.ParamByName('sid').AsInteger := Stat.Get('id', 0);
                    LSkillQ.ParamByName('slv').AsInteger := Stat.Get('level', 0);
                    LSkillQ.ExecSQL;
                  end;
                end;
              end;
            end;
          end;

          Adv := Item.Child['addition'];
          if Adv <> nil then
          begin
            EqAdditionQ.ParamByName('id').AsInteger := NoIMG(Root.Name);
            if Adv.Child['statinc'] <> nil then
            begin
              with Adv.Child['statinc'] do
              begin
                if Child['con'] = nil then
                  Stat := nil
                else
                  Stat := Child['con'].Children[0];

                if (Stat <> nil) and ((Child['con'].Children.Count <> 1) or ((Stat.Name <> 'job') and (Stat.Name <> 'weekDay') and (Stat.Name <> 'lv') and (Stat.Name <> 'level'))) then
                begin
                  Writeln(Root.Name)
                end
                else
                begin
                  if (Stat = nil) or (Stat.Name = 'lv') then  // No clue what lv is, the client doesn't check it. For most equips it equals the required level.
                    EqAdditionQ.ParamByName('cond').AsString := 'none'
                  else
                    EqAdditionQ.ParamByName('cond').AsString := LowerCase(Stat.Name);

                  if Stat = nil then
                  begin
                    Stat := TWZIMGEntry.Create(nil);
                    Stat.Name := '!';
                  end;
                  if Stat.Children.Count = 0 then
                    Stat.AddChild(TWZIMGEntry.Create(nil));

                  for Iter in Stat.Children do
                  begin
                    if (Stat.Name = '!') or (Stat.Name = 'lv') then
                      EqAdditionQ.ParamByName('condid').AsInteger := 0
                    else if Stat.Name = 'level' then
                      EqAdditionQ.ParamByName('condid').AsInteger := Get('con/level', 0)
                    else if Stat.Name <> 'weekDay' then
                      EqAdditionQ.ParamByName('condid').AsInteger := Stat.Get(Iter.Name, 0)
                    else
                      EqAdditionQ.ParamByName('condid').AsInteger := GetWeekdayCondition(Stat.Get(Iter.Name, ''));

                    // Default values
                    for i := 3 to EqAdditionQ.Params.Count - 1 do
                      EqAdditionQ.Params[i].AsInteger := 0;

                    // Set all normals stats like STR/DEX/INT/LUK
                    for Iter2 in Children do
                    begin
                      if Iter2.Name = 'con' then
                        Continue;

                      if EqAdditionQ.ParamByName(Iter2.Name) <> nil then
                        EqAdditionQ.ParamByName(Iter2.Name).AsInteger := Get(Iter2.Name, 0)
                      else
                        Writeln(Iter2.Name);
                    end;

                    // Check if there is a critical rate for this exact condition (it's a bit hackish, it assumes critical has the same conditions as statinc)
                    if (Adv.Child['critical'] <> nil) and (Adv.Child['critical'].Get('con/' + Stat.Name + '/' + Iter.Name) <> nil) and (Adv.Child['critical'].Get('con/' + Stat.Name + '/' + Iter.Name).Data = Iter.Data) then
                    begin
                      EqAdditionQ.ParamByName('crit').AsInteger := Adv.Child['critical'].Get('prob', 0);
                    end;

                    EqAdditionQ.ExecSQL;
                  end;

                  if Stat.Name = '!' then
                    Stat.Free;
                end;

              end;
            end;

            if Adv.Child['critical'] <> nil then
            begin
              with Adv.Child['critical'] do
              begin
                if Child['con'] = nil then
                  Stat := nil
                else
                  Stat := Child['con'].Children[0];

                if (Stat <> nil) and ((Child['con'].Children.Count <> 1) or ((Stat.Name <> 'job') and (Stat.Name <> 'weekDay') and (Stat.Name <> 'lv') and (Stat.Name <> 'level'))) then
                begin
                  Writeln(Root.Name)
                end
                else
                begin
                  if (Stat = nil) or (Stat.Name = 'lv') then
                    EqAdditionQ.ParamByName('cond').AsString := 'none'
                  else
                    EqAdditionQ.ParamByName('cond').AsString := LowerCase(Stat.Name);

                  if Stat = nil then
                  begin
                    Stat := TWZIMGEntry.Create(nil);
                    Stat.Name := '!';
                  end;
                  if Stat.Children.Count = 0 then
                    Stat.AddChild(TWZIMGEntry.Create(nil));

                  for Iter in Stat.Children do
                  begin
                    if (Stat.Name = '!') or (Stat.Name = 'lv') then
                      EqAdditionQ.ParamByName('condid').AsInteger := 0
                    else if Stat.Name = 'level' then
                      EqAdditionQ.ParamByName('condid').AsInteger := Get('con/level', 0)
                    else if Stat.Name <> 'weekDay' then
                      EqAdditionQ.ParamByName('condid').AsInteger := Stat.Get(Iter.Name, 0)
                    else
                      EqAdditionQ.ParamByName('condid').AsInteger := GetWeekdayCondition(Stat.Get(Iter.Name, ''));

                    for i := 3 to EqAdditionQ.Params.Count - 1 do
                      EqAdditionQ.Params[i].AsInteger := 0;
                    EqAdditionQ.ParamByName('crit').AsInteger := Get('prob', 0);
                    EqAdditionQ.ParamByName('critMin').AsInteger := Get('damage', 0);
                    try
                      EqAdditionQ.ExecSQL;
                    except
                      // The same condition has been handled above already
                    end;
                  end;

                  if Stat.Name = '!' then
                    Stat.Free;
                end;
              end;
            end;

            if Adv.Child['mobcategory'] <> nil then
            begin
              with Adv.Child['mobcategory'] do
              begin
                if Child['con'] <> nil then
                  Writeln('MOBCATEGORY CON!');

                EqAdditionQ.ParamByName('cond').AsString := 'mob_category';
                EqAdditionQ.ParamByName('condid').AsInteger := Get('category', -1);
                for i := 3 to EqAdditionQ.Params.Count - 1 do
                  EqAdditionQ.Params[i].AsInteger := 0;
                EqAdditionQ.ParamByName('dmg').AsInteger := Get('damage', 0);
                EqAdditionQ.ExecSQL;
              end;
            end;
          end;

          Free;
        end;

  EqData.Free;

  // FUUUUUUCK YOU
  MainQ.SQL.Text := 'DELETE FROM item_reward_data WHERE (SELECT id FROM item_data WHERE itemid = rewardid) IS NULL';
  MainQ.ExecSQL;
  MainQ.SQL.Text := 'INSERT INTO item_set_items SELECT setid, -1, itemid FROM item_equip_data e WHERE setid <> 0 AND (SELECT itemid FROM item_set_items si WHERE si.setid = e.setid AND (si.itemid = e.itemid OR `group` > -1) LIMIT 1) IS NULL';
  MainQ.ExecSQL;

  MainQ.Free;
  EqQ.Free;
  ConQ.Free;
  CardQ.Free;
end;

procedure AddPetData(FDB: TDatabaseConnection; Data: TWZArchive);
var
  Q, CmdQ, FoodQ: TZQuery;
  F: TWZFile;
  Strings: TWZArchive;
  PetNames, Iter, Iter2: TWZIMGEntry;
  Flags: string;
  Inc: Integer;
begin
  Q := FDB.GetQuery;
  Q.SQL.Text := 'CREATE TABLE `pet_data` (' +
  '`itemid` int(11) NOT NULL, ' +
  '`default_name` varchar(12) NOT NULL DEFAULT '''', ' +
  '`hunger` tinyint(3) NOT NULL DEFAULT ''0'', ' +
  '`life` int(11) NOT NULL COMMENT ''Number of days the pet should last'', ' +
  '`limited_life` int(11) NOT NULL COMMENT ''Number of seconds it should survive, given by a quest'', ' +
  '`evolution_item` int(11) NOT NULL, ' +
  '`req_level_for_evolution` tinyint(3) NOT NULL, ' +
  '`flags` set(''no_revive'',''no_move_to_cash_shop'',''auto_react'') NOT NULL DEFAULT '''', ' +
  '`setid` int(11) NOT NULL DEFAULT 0, ' +
  'UNIQUE KEY `id` (`itemid`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  Q.ExecSQL;

  CmdQ := FDB.GetQuery;
  CmdQ.SQL.Text := 'CREATE TABLE `pet_command_data` (' +
  '`id` int(10) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`pet_id` int(10) unsigned NOT NULL, ' +
  '`cmd` tinyint(3) unsigned NOT NULL, ' +
  '`prob` tinyint(3) unsigned NOT NULL, ' +
  '`inc` tinyint(3) unsigned NOT NULL, ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  CmdQ.ExecSQL;

  FoodQ := FDB.GetQuery;
  FoodQ.SQL.Text := 'CREATE TABLE `pet_food_data` (' +
  '`id` int(10) unsigned NOT NULL AUTO_INCREMENT, ' +
  '`food_id` int(10) unsigned NOT NULL, ' +
  '`inc` tinyint(3) unsigned NOT NULL, ' +
  '`pet_id` int(10) unsigned NOT NULL, ' +
  'PRIMARY KEY (`id`) ' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1;';
  FoodQ.ExecSQL;

  Q.SQL.Text := 'INSERT INTO pet_data VALUES (:iid, :name, :hunger, :life, :limlife, :evoitem, :reqlvevo, :flags, :setid)';
  CmdQ.SQL.Text := 'INSERT INTO pet_command_data VALUES (DEFAULT, :iid, :cmd, :prob, :inc)';
  FoodQ.SQL.Text := 'INSERT INTO pet_food_data VALUES (DEFAULT, :iid, :inc, :pet)';

  Strings := LoadArchive('String.wz');
  PetNames := Strings.GetImgFile('Pet.img').Root;

  for F in TWZDirectory(Data.Root.Entry['Pet']).Files do
    with Data.ParseFile(F) do
    begin
      Q.ParamByName('iid').AsInteger := NoIMG(F.Name);
      Q.ParamByName('name').AsString := DBStr(PetNames.Get(Q.ParamByName('iid').AsString + '/name', ''), 12);
      Q.ParamByName('hunger').AsInteger := Root.Get('info/hungry', 0);
      Q.ParamByName('life').AsInteger := Root.Get('info/life', 0);
      Q.ParamByName('limlife').AsInteger := Root.Get('info/limitedLife', 0);
      Q.ParamByName('evoitem').AsInteger := Root.Get('info/evolReqItemID', 0);
      Q.ParamByName('reqlvevo').AsInteger := Root.Get('info/evolReqPetLvl', 0);
      Q.ParamByName('setid').AsInteger := Root.Get('info/setItemID', 0);

      Flags := '';
      if Root.Get('info/noRevive', 0) > 0 then
        AddFlag(Flags, 'no_revive');
      if Root.Get('info/noMoveToLocker', 0) > 0 then
        AddFlag(Flags, 'no_move_to_cash_shop');
      if Root.Get('info/autoReact', 0) > 0 then
        AddFlag(Flags, 'auto_react');

      Q.ParamByName('flags').AsString := Flags;
      Q.ExecSQL;

      if Root.Child['interact'] <> nil then
      begin
        CmdQ.ParamByName('iid').AsInteger := NoIMG(F.Name);
        for Iter in Root.Child['interact'].Children do
        begin
          CmdQ.ParamByName('cmd').AsInteger := StrToInt(Iter.Name);
          CmdQ.ParamByName('prob').AsInteger := Iter.Get('prob', 0);
          CmdQ.ParamByName('inc').AsInteger := Iter.Get('inc', 0);
          CmdQ.ExecSQL;
        end;
      end;

      Free;
    end;

  with Data.GetImgFile('Cash/0524.img') do
  begin
    for Iter in Root.Children do
    begin
      FoodQ.ParamByName('iid').AsInteger := StrToInt(Iter.Name);
      Inc := Iter.Get('spec/incTameness', 0);
      for Iter2 in Iter.Child['spec'].Children do
      begin
        if Length(Iter2.Name) > 2 then
          Continue;

        FoodQ.ParamByName('inc').AsInteger := Inc;
        FoodQ.ParamByName('pet').AsInteger := Iter2.Data;
        FoodQ.ExecSQL;
      end;
    end;

    Free;
  end;

  Strings.Free;
  Q.Free;
  CmdQ.Free;
  FoodQ.Free;
end;

end.
