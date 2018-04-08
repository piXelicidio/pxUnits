unit pxSqrTrees;

interface

type



  {SqrTree of bytes}
  TSqrTree = class
  private
    FLevelsList :array of array of byte;
    FMaxLevel   :integer;
  protected
  public
    constructor create(Levels:byte);
    property  MaxLevel:integer read FMaxLevel;
    function LevelWidth(level:byte):integer;
    procedure SetValue(level:byte; x, y :integer; value:byte);
    function GetValue(level:byte; x,y   :integer):byte;
  end;



implementation

{ TSqrTree }

constructor TSqrTree.create;
var
  i:integer;
begin
  FMaxLevel:=levels-1;
  SetLength(FLevelsList,levels);
  for i:=0 to MaxLevel do
    SetLength(FLevelsList[i], (LevelWidth(i)*LevelWidth(i))   );
end;

function TSqrTree.GetValue(level: byte; x, y: integer): byte;
begin
  Result:=FLevelsList[level, LevelWidth(level) * y + x ];
end;

function TSqrTree.LevelWidth;
begin
  result:=(1 shl level); {2^i};
end;

procedure TSqrTree.SetValue(level: byte; x, y: integer; value: byte);
begin
  FLevelsList[level, LevelWidth(level) * y + x ]:=value;
end;

end.
