unit pxCopyProtection;
{
  TSerialNumber
  Denys Almaral Rodríguez
  junio-2004

  nota 2009:
  Hacerle pruebas
  puede que tenga problemas con
  character sets diferentes, region or system language settings
  afectaban los cálculos.
}

interface
uses SysUtils;

type
  TMAD  = record
    case integer of
    0: (data       :int64);
    1: (random,
        dependent  :cardinal);
    end;

  TSerialNumbers =   class
  public
    constructor Create;
  private
    fProductKey :int64;
  protected
    function Func( aValue :cardinal ):cardinal;
    function GenerateMad:TMAD;virtual;
    function FormatSN(s:string):string;
    function UnFormatSN(s:string):string;
  public
    property ProductKey:int64 read fProductKey;
    procedure SetProductKey( aValue :int64 );
    function  Generate:string;
    function  CheckString( SN :string ):boolean;
  end;

implementation

{ TSerialNumbers }

function TSerialNumbers.CheckString(SN: string): boolean;
var
  code  :integer;
  v     :int64;
  mad   :TMAD;
  c     :cardinal;
begin
  try
    SN:=UnFormatSN(sn);
    code:=0;
    try v:=StrToInt64(sn) except code:=1 end;
    if code=0 then
    begin
      mad.data:=v;
      c:= Func(mad.random);
      Result:= c = Mad.dependent;
    end else Result:=false;
    except
     Result:=false;
  end;
end;

constructor TSerialNumbers.Create;
begin

end;

function TSerialNumbers.FormatSN(s: string): string;
var
  negative  :boolean;
  i         :integer;
begin
  negative:= s[1]='-';
  if negative then  delete(s,1,1);
  if Length(s)<23 then s:= StringOfChar('0', 23-Length(s)) +s;
  Result:='00';
  if negative then Result:=Result+'X'
              else Result:=Result+'Y';
  Result:= Result + s[1];
  for i:=2 to length(s) do
  begin
    if ((i+2) mod 4)=0 then Result:=Result+'-';
    Result:=Result+s[i];
  end;
end;

function TSerialNumbers.Func(aValue: cardinal): cardinal;
var
  i:integer;
  ck  :int64;
begin
  Result:=aValue;
  ck:=abs(ProductKey)+$cacaf0f0;
  for i:=0  to ((ck) mod 177)+177 do
  begin
    Result:= Result * aValue;
    if (i mod 7)=3 then Result:=Result+7;
  end;
  ck:=ck div 255;
  for i:=0 to (ck mod 255) do
  begin
    Result:=Result*ck;
  end;
  ck:=ck div 255;
  Result:=Result + (aValue xor ck);
end;

function TSerialNumbers.Generate: string;
var
  mad :TMAD;
  s   :string;
begin
  mad:=GenerateMad;
  {conveting to string}
  s:=IntToStr(mad.data);
  {formating}
  Result:=FormatSN(s);
end;

function TSerialNumbers.GenerateMad: TMAD;
var
  i :integer;
begin
  randomize;
  Result.random:=Random($ffffffff);
  Result.dependent:=Func(Result.random);
end;

procedure TSerialNumbers.SetProductKey(aValue: int64);
begin
  fProductKey := aValue;
end;

function TSerialNumbers.UnFormatSN(s: string): string;
var
  negative  :boolean;
  i         :integer;
begin
  if Length(s)>5 then
  begin
    delete(s,1,2);// 00
    for i:=1 to Length(s) do s[i]:=upcase(s[i]);
    negative:= s[1]='X';
    delete(s,1,1);
    for i:=1 to length(s)
      do if s[i]<>'-' then Result:=Result+s[i];
    if negative then Result:='-' + Result;
  end else
  begin
    s:='error';
  end;
end;

end.
