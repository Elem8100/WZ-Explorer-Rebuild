program WZExplorer;

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses

  Forms,
  MainUnit in 'MainUnit.pas' {frmWZExplorer},
  XMLWriter in 'output\XMLWriter.pas',
  BassHandler in 'output\BassHandler.pas',
  DatabaseConnection in 'DatabaseConnection.pas',
  Database in 'Database.pas' {frmDatabase},
  Tools in 'Tools.pas',
  TextDB in 'output\TextDB.pas',
  DBGeneral in 'Database\DBGeneral.pas',
  Strings in 'Database\Strings.pas',
  Items in 'Database\Items.pas',
  Skills in 'Database\Skills.pas',
  Quests in 'Database\Quests.pas',
  Maps in 'Database\Maps.pas',
  Life in 'Database\Life.pas',
  Etc in 'Database\Etc.pas',
  VisualEscort in 'VisualEscort.pas' {Form1},
  Search in 'Search.pas' {frmSearch};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'WZ Explorer';
  Application.CreateForm(TfrmWZExplorer, frmWZExplorer);
  Application.CreateForm(TfrmDatabase, frmDatabase);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmSearch, frmSearch);
  Application.Run;
end.
