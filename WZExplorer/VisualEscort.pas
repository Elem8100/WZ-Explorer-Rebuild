unit VisualEscort;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WZIMGFile, Generics.Collections, WZarchive, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormShow(Sender: TObject);
  private
    procedure AddEscortData(Info: TWZIMGEntry);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddEscortData(Info: TWZIMGEntry);
var
  StartID, EndID: Integer;
  Nodes: TDictionary<Integer, TPoint>;
  Edges: TList<TPair<Integer, Integer>>;
  P: TPair<Integer, Integer>;
  E: TWZIMGEntry;
  N: TPoint;
  i, j: Integer;
begin
  StartID := Info.Get('start', -1);
  EndID := Info.Get('end', -1);

  if (StartID = -1) or (EndID = -1) then
    raise EArgumentException.Create('The given nodeInfo has no start or end node.');

  Nodes := TDictionary<Integer, TPoint>.Create;
  for E in Info.Children do
  begin
    if Length(E.Name) > 2 then
      Continue;

    i := E.Get('x', 0) div 2 + 200;
    j := E.Get('y', 0) div 2 + 600;
    Image1.Canvas.Rectangle(i, j, i + 20, j + 20);
    Image1.Canvas.TextOut(i + 5, j + 5, E.Get('key', ''));
    Image1.Canvas.TextOut(i - 5, j + 25, E.Get('edge/0', ''));
    Image1.Canvas.TextOut(i + 10, j + 25, E.Get('edge/2', ''));
    Image1.Canvas.TextOut(i + 25, j + 25, E.Get('edge/1', ''));
    Image1.Canvas.TextOut(i + 9, j - 15, E.Get('attr', ''));

    N.X := i;
    N.Y := j;
    Nodes.Add(E.Get('key', 0), N);
  end;

  Image1.Canvas.TextOut(Nodes[StartID].X, Nodes[StartID].Y - 50, 'Start');

  Edges := TList<TPair<Integer, Integer>>.Create;
  for i := 0 to Info.Child['edgeInfo'].Children.Count - 1 do
  begin
    E := Info.Child['edgeInfo'].Children[i];
    Image1.Canvas.TextOut(i * 50, 900, IntToStr(E.Get('node1', -1)) + ' | ' + IntToStr(E.Get('node2', -1)));
    Edges.Add(TPair<Integer, Integer>.Create(E.Get('node1', -1), E.Get('node2', -1)));
  end;

  for P in Edges do
  begin
    if P.Key < 0 then
      continue;
    Image1.Canvas.MoveTo(Nodes[P.Key].X, Nodes[P.Key].Y);
    Image1.Canvas.LineTo(Nodes[P.Value].X, Nodes[P.Value].Y);
    Image1.Canvas.Rectangle(Nodes[P.Key].X,  Nodes[P.Key].Y,Nodes[P.Key].X + 5 + random(5), Nodes[P.Key].Y + 5 + random(5));
  end;
 (*
  Writeln('Parsing ', MapID);
  Counter := 0;
  N := Nodes[StartID];
  repeat


    Prev := N;
    Inc(Counter);
    if Counter > 50 then
      raise exception.Create('overflow');
  until (not Nodes.TryGetValue(Next, N)) or (Prev.ID = EndID);      *)
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  AddEscortData(TWZArchive.Create('D:\Spiele\MapleStory Global\Map.wz').GetImgFile('Map/Map9/921120200.img').Root.Get('nodeInfo'));
 //AddEscortData(TWZArchive.Create('D:\Spiele\MapleStory Global\Map.wz').GetImgFile('Map/Map9/921120100.img').Root.Get('nodeInfo'));
end;

end.
