unit Search;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, MainUnit,
  WZArchive, WZDirectory, WZIMGFile, TypInfo, Vcl.Samples.Spin;

type
  TfrmSearch = class(TForm)
    LVRes: TListView;
    edtQuery: TEdit;
    btnSearch: TButton;
    cbFullMatch: TCheckBox;
    edtVal: TEdit;
    cbBoth: TCheckBox;
    cbChildCount: TCheckBox;
    seChildCount: TSpinEdit;
    procedure btnSearchClick(Sender: TObject);
    procedure LVResColumnClick(Sender: TObject; Column: TListColumn);
  private
    FCmpName, FCmpVal: string;
    FCmpIVal: Integer;
    FHasIVal: Boolean;

    procedure AddResult(IE: TWZIMGEntry);
    procedure ScanFile(WZ: TWZArchive; F: TWZFile);
    procedure ScanDirectory(WZ: TWZArchive; D: TWZDirectory);
    procedure ScanIMGEntry(IE: TWZIMGEntry);
  public
    { Public-Deklarationen }
  end;

var
  frmSearch: TfrmSearch;

implementation

{$R *.dfm}

procedure TfrmSearch.AddResult(IE: TWZIMGEntry);
var
  Path: string;
  E: TWZEntry;
begin
  Path := IE.Name;
  E := IE.Parent;
  while E <> nil do
  begin
    Path := E.Name + '/' + Path;
    E := E.Parent;
  end;

  with LVRes.Items.Add do
  begin
    Caption := Path;
    SubItems.Add(GetEnumName(TypeInfo(TMapleDataType), Int32(IE.DataType)));
    if IE.DataType in [mdtShort, mdtInt] then
      SubItems.Add(IntToStr(IE.Data))
    else if IE.DataType = mdtString then
      SubItems.Add((IE.Data));
  end;
end;

procedure TfrmSearch.btnSearchClick(Sender: TObject);
var
  WZ, MyWZ: TWZArchive;
begin
  if (Length(edtQuery.Text) = 0) and (Length(edtVal.Text) = 0) then
    Exit;

  FCmpName := LowerCase(edtQuery.Text);
  FCmpVal := LowerCase(edtVal.Text);
  FHasIVal := TryStrToInt(FCmpVal, FCmpIVal);
  for WZ in frmWZExplorer.OpenedArchives do
  begin
    MyWZ := TWZArchive.Create(WZ.Reader.FileName);
    ScanDirectory(MyWZ, MyWZ.Root);
    MyWZ.Free;
  end;
  Caption := 'Search';
end;

procedure TfrmSearch.LVResColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = 0 then
    LVRes.SortType := stText;
end;

procedure TfrmSearch.ScanDirectory(WZ: TWZArchive; D: TWZDirectory);
var
  F: TWZFile;
  Dir: TWZDirectory;
begin
  for F in D.Files do
    ScanFile(WZ, F);

  for Dir in D.SubDirs do
    ScanDirectory(WZ, Dir);
end;

procedure TfrmSearch.ScanFile(WZ: TWZArchive; F: TWZFile);
var
  IMG: TWZIMGFile;
begin
  Caption := 'Scan: ' + F.Name;
  Application.ProcessMessages;

  IMG := WZ.ParseFile(F);
  ScanIMGEntry(IMG.Root);
  IMG.Free;
end;

procedure TfrmSearch.ScanIMGEntry(IE: TWZIMGEntry);
var
  C: TWZIMGEntry;
  IsName: Boolean;
begin
  IsName := (FCmpName <> '') and ((LowerCase(IE.Name) = FCmpName) or (not cbFullMatch.Checked and (Pos(FCmpName, LowerCase(IE.Name)) > 0)));
  if IsName and not cbBoth.Checked then
  begin
    if not cbChildCount.Checked or (IE.Children.Count > seChildCount.Value) then
      AddResult(IE);
  end;

  if (FCmpVal <> '') and (not cbBoth.Checked or IsName) then
  begin
    case IE.DataType of
      mdtShort, mdtInt: if FHasIVal and (IE.Data = FCmpIVal) then AddResult(IE);
      mdtString, mdtUOL:
      begin
        if LowerCase((IE.Data)) = FCmpVal then
          AddResult(IE)
        else if not cbFullMatch.Checked and (Pos(FCmpVal, LowerCase((IE.Data))) > 0) then
          AddResult(IE);
      end;
    end;
  end;

  for C in IE.Children do
    ScanIMGEntry(C);
end;

end.
