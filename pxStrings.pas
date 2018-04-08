unit pxStrings;

interface


const
  ccDigitos   = ['0'..'9'];
  ccSignos    = ['+','-'];
  ccLetras    = ['a'..'z'];
  ccVocales   = ['a','e','i','o','u'];
  ccVocalesCT = ['á','é','í','ó','ú'];
  ccPunto     = ['.'];
  ccComa      = [','];

type
{ MultiByte Character Set (MBCS) byte type }
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);
var
  LeadBytes: set of Char = [];

function ExtractDecimal(n:byte; s:string):string;
function ExtractStr(OpenKey,CloseKey, s:string):string;
function CmpStrs(const s1,s2 :string; CaseSensitive:boolean=true):boolean;
function CharCompleteLeft( s:string; MaxLength:integer; Ch:char):string;

function StrScan(const Str: PChar; Chr: Char): PChar; assembler;
function ByteTypeTest(P: PChar; Index: Integer): TMbcsByteType;
function ByteType(const S: string; Index: Integer): TMbcsByteType;
function UpperCase(const S: string): string;

function TieneEspacios(const s: string):boolean;
function MakeParamStr(const s:string):string;

implementation

function TieneEspacios(const s: string):boolean;
begin
  Result:=( Pos(' ', s) > 0 );
end;

function MakeParamStr(const s:string):string;
begin
  if TieneEspacios(s) then Result:='"'+s+'"' else Result:=s;
end;

function CmpStrs(const s1,s2 :string; CaseSensitive:boolean=true):boolean;
begin
  if CaseSensitive then
    Result:=(s1=s2)
  else
    Result:=(UpperCase(s1)=UpperCase(s2));
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;


function ExtractStr(OpenKey,CloseKey, s:string):string;
var
  posi :integer;
begin
  posi:=pos(OpenKey, s);
  s:=Copy(s,posi+length(openkey), 1+length(s)-(posi+length(Openkey)));
  posi:=pos(CloseKey,s);
  Result:=Copy(s,1,posi-1);
end;


function ExtractDecimal(n:byte; s:string):string;
{
 Extrae el enesimo decimal de la cadena s
}
var
cont1   :byte;
cont2,i :byte;
code    :integer;
xx      :string;
sal     :boolean;

begin

     cont1:=0;
     sal:=false;
     i:=0;
     repeat
      inc(i);
          if s[i] in (ccSignos + ccDigitos + ccPunto) then
          begin
               inc(cont1);
               cont2:=i;
               while s[cont2] in (ccSignos + ccDigitos + ccPunto) do inc(cont2);
               if cont1=n then
               begin
                    xx:=copy(s,i,cont2-i);
                    result:=xx;
                    sal:=true;
               end;
               i:=cont2;
          end;
     until (i=Length(s)) or sal;
end;


function ByteTypeTest(P: PChar; Index: Integer): TMbcsByteType;
var
  I: Integer;
begin
  Result := mbSingleByte;
  if (P = nil) or (P[Index] = #$0) then Exit;
  if (Index = 0) then
  begin
    if P[0] in LeadBytes then Result := mbLeadByte;
  end
  else
  begin
    I := Index - 1;
    while (I >= 0) and (P[I] in LeadBytes) do Dec(I);
    if ((Index - I) mod 2) = 0 then Result := mbTrailByte
    else if P[Index] in LeadBytes then Result := mbLeadByte;
  end;
end;

function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
  if {SysLocale.FarEast} false then
    Result := ByteTypeTest(PChar(S), Index-1);
end;


function StrScan(const Str: PChar; Chr: Char): PChar; assembler;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        POP     EDI
        MOV     AL,Chr
        REPNE   SCASB
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        DEC     EAX
@@1:    POP     EDI
end;

{}
function CharCompleteLeft( s:string; MaxLength:integer; Ch:char):string;
begin
  if Length(s)<MaxLength then
      Result:=StringOfChar(ch, MaxLength - Length(s))+ s
    Else
      Result:=s;
end;

end.
