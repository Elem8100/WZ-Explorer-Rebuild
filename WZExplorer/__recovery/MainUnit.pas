unit MainUnit;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ImgList, ToolWin, VirtualTrees, ExtCtrls, Generics.Collections, Menus,
  PNGImage, Graphics, Tools,
  {$IFNDEF NOCRC}CRC32, xunit, {$ENDIF}Clipbrd, WZDirectory,
  WZArchive, BassHandler, XMLWriter, WZIMGFile, WZReader, KeyHandler,
  System.ImageList;

type
  TfrmWZExplorer = class(TForm)
    lblCredits: TLabel;
    ListImages: TImageList;
    NaviBar: TToolBar;
    btnOpenSingle: TToolButton;
    BarIcons: TImageList;
    btnSaveXML: TToolButton;
    VST: TVirtualStringTree;
    pnlInfo: TPanel;
    btnOpenFolder: TToolButton;
    Progress: TProgressBar;
    gbFileInfo: TGroupBox;
    lblFileIdentity: TLabel;
    lblFileSize: TLabel;
    lblHeaderSize: TLabel;
    lblCopyright: TLabel;
    lblVersion: TLabel;
    OpenWZ: TOpenDialog;
    pmRight: TPopupMenu;
    ExtractFile: TMenuItem;
    lblStatus: TLabel;
    TmrLive: TTimer;
    WZImage: TImage;
    cobKeys: TComboBox;
    SortList: TMenuItem;
    btnCreateMCDB: TToolButton;
    cbAutoNext: TCheckBox;
    PopupMenu1: TPopupMenu;
    Portals1: TMenuItem;
    Mobs1: TMenuItem;
    MobNames1: TMenuItem;
    fieldLimitscan1: TMenuItem;
    Skills1: TMenuItem;
    Search1: TMenuItem;
    BruteforceCRC: TMenuItem;
    CopyValue1: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure btnOpenSingleClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnSaveXMLClick(Sender: TObject);
    procedure btnCreateMCDBClick(Sender: TObject);
    procedure ExtractFileClick(Sender: TObject);
    procedure SortListClick(Sender: TObject);

    procedure TmrLiveTimer(Sender: TObject);

    procedure VSTDblClick(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTKeyPress(Sender: TObject; var Key: Char);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure Portals1Click(Sender: TObject);
    procedure Mobs1Click(Sender: TObject);
    procedure MobNames1Click(Sender: TObject);
    procedure pnlInfoDblClick(Sender: TObject);
    procedure fieldLimitscan1Click(Sender: TObject);
    procedure Skills1Click(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure CopyValue1Click(Sender: TObject);
    procedure BruteforceCRCClick(Sender: TObject);
    procedure cobKeysChange(Sender: TObject);
  private
    FOpenedArchives: TList<TWZArchive>;

    function LoadWZ(const FileName: string): TWZArchive;

    function GetRoot(Node: PVirtualNode): PVirtualNode;

    procedure XMLUpdate(Cur, Max: Integer; const CurFile: string);
    procedure UpdateInfoBox(WZ: TWZArchive);
    procedure VSTAddDirectory(Entry: TWZDirectory; Parent: PVirtualNode);
    procedure VSTAddIMGEntry(Entry: TWZIMGEntry; Parent: PVirtualNode);
  public
    ActiveBass: TBassHandler;

    property OpenedArchives: TList<TWZArchive> read FOpenedArchives;
  end;

var
  frmWZExplorer: TfrmWZExplorer;

implementation

uses Database, TextDB, VisualEscort, Search;

{$R *.dfm}

procedure TfrmWZExplorer.FormCreate(Sender: TObject);
begin
  VST.NodeDataSize := SizeOf(TWZDirectory);
  FormatSettings.DecimalSeparator := '.';

  FOpenedArchives := TList<TWZArchive>.Create;

  TWZReader.EncryptionIV := 0;

  {$IFNDEF NOCRC}
  BruteforceCRC.Visible := True;
  xunit.init(VST, pmRight);
  {$ENDIF}
end;

procedure TfrmWZExplorer.FormClose(Sender: TObject; var Action: TCloseAction);
var
  WZ: TWZArchive;
begin
  // Closing -> stop playing music
  if Assigned(ActiveBass) then
    FreeAndNil(ActiveBass);

  for WZ in FOpenedArchives do
    WZ.Free;

  FreeAndNil(FOpenedArchives);
end;

procedure TfrmWZExplorer.btnOpenSingleClick(Sender: TObject);
{ Open a single archive/patch }
var
  Ext: string;
begin
  if not OpenWZ.Execute then
    Exit;

  Ext := ExtractFileExt(OpenWZ.FileName);
  if Ext = '.wz' then    // Open a WZ file
    UpdateInfoBox(LoadWZ(OpenWZ.FileName))
  (*else if Ext = '.patch' then   // Open a WZ patch
    with TWZPatch.Create(OpenWZ.FileName) do
    begin
      ParseContents(ProcessZLib);
      Free;
    end *)
  else if Ext = '.img' then
  begin
    VSTAddIMGEntry(WZIMGFile.LoadStandalone(OpenWZ.FileName).Root, nil);
  end;
end;

procedure TfrmWZExplorer.btnOpenFolderClick(Sender: TObject);
{ Open a MapleStory folder containing WZ files }
var
  WZDir: string;
  Search: TSearchRec;
  Files: TStringList;
  i: Integer;
begin
  if not SelectDirectory('MapleStory Folder', WZDir) then
    Exit;

  WZDir := IncludeTrailingPathDelimiter(WZDir);

  if FindFirst(WZDir + '*.wz', faAnyFile, Search) = 0 then
  begin
    Files := TStringList.Create;

    repeat
      Files.Add(WZDir + Search.Name);
    until FindNext(Search) <> 0;

    FindClose(Search);
  end
  else
    Exit;

  for i := 0 to Files.Count - 1 do
    if i = 0 then
      UpdateInfoBox(LoadWZ(Files[i]))
    else
      LoadWZ(Files[i]);

  Files.Free;
end;

procedure TfrmWZExplorer.btnSaveXMLClick(Sender: TObject);
{ Save XML files of opened archives }
var
  w: TXMLWriter;
  i: integer;
  Path: string;
begin
  if FOpenedArchives.Count = 0 then
    raise Exception.Create('No WZ archives are loaded!');

  if not SelectDirectory('Output Folder', Path) then
    Exit;

  w := TXMLWriter.Create(XMLUpdate, IncludeTrailingPathDelimiter(Path));
  try
    for i := 0 to FOpenedArchives.Count - 1 do
      w.Dump(FOpenedArchives[i]);
  finally
    w.Free;
  end;

  lblStatus.Caption := '';
end;

procedure TfrmWZExplorer.cobKeysChange(Sender: TObject);
begin
  case cobKeys.ItemIndex of
    0: TWZReader.EncryptionIV := 0;
    1: TWZReader.EncryptionIV := GENERAL_IV;
    2: TWZReader.EncryptionIV := GMS_IV;
  end;
end;

procedure TfrmWZExplorer.CopyValue1Click(Sender: TObject);
var
  Entry: TWZEntry;
begin
  if VST.FocusedNode = nil then
    Exit;

  Entry := TWZEntry(VST.GetNodeData(VST.FocusedNode)^);
  if Entry is TWZIMGEntry then
  begin
    case TWZIMGEntry(Entry).DataType of
      mdtString, mdtUOL: Clipboard.AsText := (TWZIMGEntry(Entry).Data);
      mdtShort, mdtInt: Clipboard.AsText := IntToStr(TWZIMGEntry(Entry).Data);
      mdtDouble: Clipboard.AsText := FloatToStr(PDouble(@TWZIMGEntry(Entry).Data)^);
    end;
  end;
end;

procedure TfrmWZExplorer.BruteforceCRCClick(Sender: TObject);
begin
  {$IFNDEF NOCRC}
  CRC32.Bruteforce(XMLUpdate);
  {$ENDIF}
end;

procedure TfrmWZExplorer.btnCreateMCDBClick(Sender: TObject);
{ Create a Maple Community Database }
begin
  frmDatabase.Show;
end;

function TfrmWZExplorer.LoadWZ(const FileName: string): TWZArchive;
{ Parse a WZ archive and return it }
begin
  Result := TWZArchive.Create(FileName);
  FOpenedArchives.Add(Result);

  VST.BeginUpdate;
  try
    VSTAddDirectory(Result.Root, nil);
  finally
    VST.EndUpdate;
  end;
end;

procedure TfrmWZExplorer.MobNames1Click(Sender: TObject);
begin
  TextDB.DumpMobNamesForLinosal('C:\MobNames.txt');
end;

procedure TfrmWZExplorer.Mobs1Click(Sender: TObject);
begin
  TextDB.DumpMobDataToFile('C:\Mobs.txt');
end;

procedure TfrmWZExplorer.pnlInfoDblClick(Sender: TObject);
begin
  form1.show;
end;

procedure TfrmWZExplorer.Portals1Click(Sender: TObject);
begin
  TextDB.DumpPortalDataToFile('C:\Portals.txt');
end;

function TfrmWZExplorer.GetRoot(Node: PVirtualNode): PVirtualNode;
{ Returns the root node of a node, which is the WZArchive }
begin
  Result := Node;

  while VST.GetNodeLevel(Result) > 0 do
    Result := Result.Parent;
end;

procedure TfrmWZExplorer.UpdateInfoBox(WZ: TWZArchive);
{ Fills the File-Information box with information }
begin
  gbFileInfo.Caption := WZ.Root.Name;
  lblFileIdentity.Caption := 'File-Identity: ' + WZ.PKG;
  lblFileSize.Caption := 'Filesize: ' + IntToStr(WZ.FileSize) + ' Byte';
  lblHeaderSize.Caption := 'Headersize: ' + IntToStr(WZ.HeaderSize);
  lblCopyright.Caption := WZ.Copyright;
  lblVersion.Caption := 'Version: ' + IntToStr(WZ.Version);
end;

procedure TfrmWZExplorer.TmrLiveTimer(Sender: TObject);
{ Updates the progress bar when playing MP3s }
var
  N: PVirtualNode;
begin
  if Assigned(ActiveBass) then
    Progress.Position := Trunc(ActiveBass.Position);

  // Song finished? Stop updating then
  if Progress.Position = Progress.Max then
  begin
    TmrLive.Enabled := False;

    if not cbAutoNext.Checked then
      Exit;

    // Focus next node and play it
    N := VST.GetNextSibling(VST.FocusedNode);
    if Assigned(N) then
    begin
      VST.FocusedNode := N;
      VSTDblClick(VST);
    end;
  end;
end;

procedure TfrmWZExplorer.XMLUpdate(Cur, Max: Integer; const CurFile: string);
{ This is called by XMLWriter instances to update the progress bar }
begin
  Progress.Max := Max;
  Progress.Position := Cur;
  lblStatus.Caption := CurFile;

  Application.ProcessMessages;
end;

procedure TfrmWZExplorer.Search1Click(Sender: TObject);
begin
  frmSearch.Show;
end;

procedure TfrmWZExplorer.Skills1Click(Sender: TObject);
begin
  TextDB.DumpSkillsForLinosal('C:\Skills.txt');
end;

procedure TfrmWZExplorer.SortListClick(Sender: TObject);
begin
  VST.SortTree(0, sdAscending);
end;

procedure TfrmWZExplorer.ExtractFileClick(Sender: TObject);
var
  Entry: TWZEntry;
  MP3: TMemoryStream;
begin
  if VST.FocusedNode = nil then
    Exit;

  Entry := TWZEntry(VST.GetNodeData(VST.FocusedNode)^);

  if Entry is TWZIMGEntry then
  begin
    if TWZIMGEntry(Entry).DataType = mdtSound then
    begin
      MP3 := TWZIMGEntry(Entry).Sound.Dump;
      try
        MP3.SaveToFile(ExtractFilePath(ParamStr(0)) + TWZIMGEntry(Entry).Name + '.mp3');
      finally
        MP3.Free;
      end;
    end;

    if TWZIMGEntry(Entry).DataType = mdtCanvas then
      TWZIMGEntry(Entry).Canvas.DumpPng.SaveToFile(ExtractFilePath(ParamStr(0)) + TWZIMGEntry(Entry).Name + '.png');
  end;
end;

procedure TfrmWZExplorer.fieldLimitscan1Click(Sender: TObject);
var
  MapWZ: TWZArchive;
  ContinentDirs, MapDir: TWZDirectory;
  Map: TWZFile;
  FL: Cardinal;
begin
  MapWZ := TWZArchive.Create('D:\Spiele\MapleStory Global\Map.wz');
  AllocConsole;
  try
    ContinentDirs := TWZDirectory(MapWZ.Root.Entry['Map']);
    for MapDir in ContinentDirs.SubDirs do
      for Map in MapDir.Files do
        with MapWZ.ParseFile(Map) do
        begin
          lblStatus.Caption := 'Scan: ' + Map.Name;
          Application.ProcessMessages;

          FL := Root.Get('info/fieldLimit', 0);
          if FL and $10000000 = $10000000 then
            WriteLn(Map.Name);

          Free;
        end;
  finally
    MapWZ.Free;
  end;
  Readln;
  FreeConsole;
end;

procedure TfrmWZExplorer.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data, Data2: TWZEntry;
  V, V2, E, E2: Integer;
begin
  Data := TWZEntry(VST.GetNodeData(Node1)^);
  Data2 := TWZEntry(VST.GetNodeData(Node2)^);

  if (Data is TWZFile) and (Data2 is TWZFile) then
  begin
    Result := CompareStr(Data.Name, Data2.Name);
  end;

  if (Data is TWZIMGEntry) and (Data2 is TWZIMGEntry) then
  begin
    Val(Data.Name, V, E);
    Val(Data2.Name, V2, E2);
    if (E = 0) and (E2 = 0) then
      Result := V - V2
    else
      Result := CompareStr(Data.Name, Data2.Name);
  end;
end;

procedure TfrmWZExplorer.VSTDblClick(Sender: TObject);
var
  Entry: TWZEntry;
  WZ: TWZArchive;
  PNG: TPNGImage;
  IE: TWZIMGEntry absolute Entry;
begin
  if VST.FocusedNode = nil then
    Exit;

  Entry := TWZEntry(VST.GetNodeData(VST.FocusedNode)^);
  WZ := TWZDirectory(VST.GetNodeData(GetRoot(VST.FocusedNode))^).Archive as TWZArchive;

  if Entry is TWZFile then
  begin
    if not Assigned(TWZFile(Entry).IMGFile) then
    begin
      VST.BeginUpdate;
      try
        for Entry in WZ.ParseFile(TWZFile(Entry)).Root.Children do
          VSTAddIMGEntry(IE, VST.FocusedNode);
      finally
        VST.EndUpdate;
      end;
    end;
  end
  else if Entry is TWZIMGEntry then
  begin
    if IE.DataType = mdtSound then
    begin
      // Already playing, stop that first
      if Assigned(ActiveBass) then
        FreeAndNil(ActiveBass);

      lblStatus.Caption := Format('WZlive! - %s.mp3', [Entry.Name]);

      // Direct playing from the archive, faster than first dumping the Media
      ActiveBass := TBassHandler.Create(WZ.Reader.Stream, IE.Sound.Offset, IE.Sound.DataLength);
      ActiveBass.Play;
      Progress.Max := Trunc(ActiveBass.Length);
      TmrLive.Enabled := True;
    end;

    if IE.DataType = mdtCanvas then
    begin
      if IE.Child['source'] <> nil then
        IE := WZ.ResolveFullPath(IE.Get('source', ''));
      PNG := IE.Canvas.DumpPng;
      WZImage.Picture.Assign(PNG);
      if PNG <> nil then
        WZImage.Width := PNG.Width + 8;   // + 8 so it doesn't look so tight
    end;
  end;
end;

procedure TfrmWZExplorer.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Entry: TWZEntry;
begin
  if Column > 0 then
    Exit;

  Entry := TWZEntry(VST.GetNodeData(Node)^);

  if (Entry is TWZDirectory) or (Entry is TWZFile) then
    ImageIndex := 1
  else
    ImageIndex := 0;
end;

procedure TfrmWZExplorer.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: TWZEntry;
begin
  Data := TWZEntry(VST.GetNodeData(Node)^);

  if Column = 0 then
  begin
    CellText := Data.Name;
    Exit;
  end;

  if (Column = 1) and (Data is TWZIMGEntry) then
  begin
    case TWZIMGEntry(Data).DataType of
      mdtShort, mdtInt, mdtInt64: CellText := IntToStr(TWZIMGEntry(Data).Data);
      mdtFloat: CellText := FloatToStrF(PSingle(@TWZIMGEntry(Data).Data)^, ffGeneral, 7, 8);
      mdtDouble: CellText := FloatToStr(PDouble(@TWZIMGEntry(Data).Data)^);
      mdtString: CellText := (TWZIMGEntry(Data).Data);
    end;

    if TWZIMGEntry(Data).DataType in [mdtIMG_0x00, mdtProperty, mdtExtended] then
      CellText := '';

    if TWZIMGEntry(Data).DataType = mdtVector then
      CellText := Format('Vector - X: %d | Y: %d', [TWZIMGEntry(Data).Vector.X,
                                           TWZIMGEntry(Data).Vector.Y]);

    if TWZIMGEntry(Data).DataType = mdtCanvas then
      CellText := Format('Canvas - Width: %d | Height: %d | Format: %d', [TWZIMGEntry(Data).Canvas.Width,
                                                            TWZIMGEntry(Data).Canvas.Height, TWZIMGEntry(Data).Canvas.Format]);

    if TWZIMGEntry(Data).DataType = mdtSound then
      CellText := 'MP3-Sound - double-click to play';

    if TWZIMGEntry(Data).DataType = mdtUOL then
      CellText := 'UOL - ' + (TWZIMGEntry(Data).Data);

    Exit;
  end;

  if (Column = 1) then
  begin
    CellText := IntToStr(Data.Offset) + ' | ' + IntToStr(Data.Size);
    Exit;
  end;

  CellText := '';
end;

procedure TfrmWZExplorer.VSTHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column = 0 then
    SortList.Click;
end;

procedure TfrmWZExplorer.VSTKeyPress(Sender: TObject; var Key: Char);
begin
  // User pressed return key?
  if Ord(Key) = VK_RETURN then
    VSTDblClick(VST);    // Same handler
end;

procedure TfrmWZExplorer.VSTAddDirectory(Entry: TWZDirectory; Parent: PVirtualNode);
var
  Root: PVirtualNode;
  D: TWZDirectory;
  F: TWZFile;
begin
  Root := VST.AddChild(Parent, Entry);

  for D in Entry.SubDirs do
    VSTAddDirectory(D, Root);

  for F in Entry.Files do
    VST.AddChild(Root, F);

  if Parent = nil then
    VST.Expanded[Root] := True;
end;

procedure TfrmWZExplorer.VSTAddIMGEntry(Entry: TWZIMGEntry; Parent: PVirtualNode);
var
  Root: PVirtualNode;
  E: TWZIMGEntry;
begin
  Root := VST.AddChild(Parent, Entry);

  for E in Entry.Children do
    VSTAddIMGEntry(E, Root);
end;

end.
