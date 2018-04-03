unit pxUtiles;

interface
  uses SysUtils, Classes;



type
  TSetOfChar = set of char;

procedure FillDWord(var MemVar; count:cardinal; value:cardinal);
procedure xchg(var a,b :integer);
function FixOutBounds( value, LowLimit, HighLimit : integer):integer;
function IsInsideRect( xpoint, ypoint: integer; xrect, yrect, rectWidth, rectHeight:integer):boolean;

function ExtractChainsOf( str:string; chainChars:TSetOfChar):TStrings;
function ExtractNumericsChains( str:string):TStrings;
function IntToStrFixed(val, FixLen: integer):string;

implementation

function ExtractNumericsChains( str:string):TStrings;
begin
  Result := ExtractChainsOf( str, ['0'..'9','-','.']);
end;

function ExtractChainsOf( str:string; chainChars:TSetOfChar):TStrings;
var
  s :string;
  i :integer;
begin
  Result := TStringList.Create;
  s:='';
  for i:=1 to Length(str) do
  begin
    if str[i] in ChainChars then
    begin
      s:=s+str[i];
      if i=Length(str) then Result.Add(s);
    end else if length(s)>0 then
    begin
      Result.Add(s);
      s:='';
    end;
  end;
end;

function IsInsideRect( xpoint, ypoint: integer; xrect, yrect, rectWidth, rectHeight:integer):boolean;
begin
  Result := (xpoint >= xrect) and
            (xpoint < xrect+rectWidth) and
            (ypoint >= yrect) and
            (ypoint < yrect+rectHeight);
end;

function FixOutBounds( value, LowLimit, HighLimit : integer):integer;
begin
  if value<lowlimit then Result:=lowlimit else
    if value>highlimit then Result:=HighLimit else Result := value;
end;

function IntToStrFixed(val, FixLen: integer): string;
var
  s:string;
begin
  s:=IntToStr(val);
  if length(s)<FixLen then
  begin
    s:=StringOfChar('0', FixLen - Length(s))+s;
  end;
  Result := s;
end;

procedure xchg;
var
  t:integer;
begin
  t:=a;
  a:=b;
  b:=t;
end;

procedure       FillDWord;
asm
{     ->EAX     Pointer to destination
       EDX     count
       ECX     value   }

        PUSH    EDI

        MOV     EDI,EAX // Point EDI to destination
        MOV     EAX,ECX // 32 bit value para EAX
        MOV     ECX,EDX // y para ECX el count
        REP     STOSD   // llena con  dwords

        POP     EDI
end;



end.
