unit Database;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, DatabaseConnection, Generics.Collections, WZArchive,
  WZDirectory, WZIMGFile, ZDataset, TypInfo, Math, StrUtils, DBGeneral,
  ExtCtrls, ImgList, {$WARNINGS OFF}FileCtrl, System.ImageList{$WARNINGS ON};

type
  TfrmDatabase = class(TForm)
    gbDatabase: TGroupBox;
    lblSQLHost: TLabel;
    lblSQLPort: TLabel;
    lblSQLUser: TLabel;
    lblPassword: TLabel;
    edtSQLHost: TEdit;
    seSQLPort: TSpinEdit;
    edtSQLUser: TEdit;
    edtSQLPW: TEdit;
    lblDBName: TLabel;
    edtDBName: TEdit;
    lblOverwrite: TLabel;
    btnConnect: TButton;
    edtPath: TButtonedEdit;
    Icons: TImageList;
    procedure btnConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtPathRightButtonClick(Sender: TObject);
  private
    FDumpStart: Cardinal;

    function time(): Cardinal;
   // procedure RunDropQuery;
   // procedure RunShopQuery;
  end;

  TDBTask = procedure(DB: TDatabaseConnection);
  TTaskPair = TPair<TDBTask, string>;

var
  frmDatabase: TfrmDatabase;

implementation

uses Strings, Life, Items, Skills, Quests, Maps, Etc;

{$R *.dfm}

procedure TfrmDatabase.btnConnectClick(Sender: TObject);
var
  Tasks: TList<TTaskPair>;
begin
  edtPath.Enabled := False;
  AllocConsole;
  SetConsoleTitle('Dumping to database');
  FDumpStart := GetTickCount;
  DBGeneral.Init;
  WriteLn(time, ': Initialized.');

  Tasks := TList<TTaskPair>.Create;
  Tasks.Add(TTaskPair.Create(AddEtcData, 'Etc'));
  Tasks.Add(TTaskPair.Create(AddItemTables, 'Items'));
  Tasks.Add(TTaskPair.Create(AddMapTables, 'Maps'));
  Tasks.Add(TTaskPair.Create(AddMobTables, 'Mobs'));
  Tasks.Add(TTaskPair.Create(AddNPCTable, 'NPCs'));
  Tasks.Add(TTaskPair.Create(AddQuestTables, 'Quests'));
  Tasks.Add(TTaskPair.Create(AddReactorTables, 'Reactors'));
  Tasks.Add(TTaskPair.Create(AddSkillTable, 'Skills'));
  Tasks.Add(TTaskPair.Create(AddStringsTable, 'Strings'));

  TThread.CreateAnonymousThread(procedure
  var
    P: TTaskPair;
    DB: TDatabaseConnection;
  begin
    DB := GetConnection;
    try
      for P in Tasks do
      begin
        DB.Con.StartTransaction;
        try
          P.Key(DB);
          DB.Con.Commit;
          WriteLn(time(), ': ', P.Value, ' done.');
        except
          WriteLn(P.Value, ': ' + Exception(ExceptObject).Message);
        end;
      end;

      WriteLn('Done. Operation took ', time div 1000, ' seconds.');
      WriteLn('Press enter to close ...');
      ReadLn;
      FreeConsole;
    finally
      DB.Free;
      Tasks.Free;
      edtPath.Enabled := True;
    end;
  end).Start;
end;

function TfrmDatabase.time(): Cardinal;
begin
  Result := GetTickCount - FDumpStart;
end;

procedure TfrmDatabase.edtPathRightButtonClick(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('MapleStory Folder', '', Dir) then
    edtPath.Text := Dir;
end;

procedure TfrmDatabase.FormShow(Sender: TObject);
begin
  edtSQLPW.SetFocus;
end;

(*procedure TfrmDatabase.RunDropQuery;
begin
  with FDB.GetQuery do
  begin
    SQL.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'SQL\Drop Query.sql');
    ExecSQL;

    Free;
  end;
end;

procedure TfrmDatabase.RunShopQuery;
begin
  with FDB.GetQuery do
  begin
    SQL.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'SQL\Shop Query.sql');
    ExecSQL;

    Free;
  end;
end;     *)

end.
