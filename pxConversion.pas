unit pxConversion;
{
  Desde base 2 hasta base 36
  By Denys
}

interface
  uses math,SysUtils;

const
  DIGITS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';


  procedure SymbToVal(symb:string; var v:extended; var code:integer; Base:byte);
  function  ValToSymb(v:extended; base:byte):string;
  function SystemName(base:byte):string;

implementation


function  ValToSymb(v:extended; base:byte):string;
var
  i,rest:Int64;
N:BOOLEAN;
begin
  result:='';
  n:=(v<0);
  if n then v:=-v;
  if base=10 then result:=FloatToStr(v) else
  begin
    try
      i:=round(v);
      except
        result:='ERROR: Valor muy grande para representar en: '+SystemName(base);
        exit;
    end;
    repeat
      rest:=(i mod base);
      i:=i div base;
      result:=DIGITS[rest+1]+result;
    until i=0;
  end;
  if n then result:='-'+result;
end;

procedure SymbToVal(symb:string; var v:extended; var code:integer; Base:byte);
var
  i:integer;
  c:char;
  subDigits:string;
  digit:byte;
  posicion:integer;
  FloatingPoint  :boolean;
  FloatPointPos  :integer;
  negative       :boolean;
begin
  code:=0;
  v:=0;
  {negative}
  if symb='' then
  begin
    code:=1;
    exit;
  end;
  negative:=(symb[1]='-');
  if negative then delete(symb, 1, 1);
  {}
  FloatPointPos:=pos('.',symb);
  FloatingPoint:=(FloatPointPos>0);
  if Base=10 then begin val(symb,v,code); exit; end;
  if (Base in[2..36]) and (symb<>'') then
  begin
      {aqui viene la conversion para bases diferentes de 10}
      subDigits:=copy(DIGITS,1,Base);
      if FloatingPoint then posicion:=FloatPointPos-2
                      else posicion:=Length(symb)-1;
      for i:=1 to Length(symb) do
      begin
        c:=upcase(symb[i]);     {digito char}
        digit:=pos(c,subDigits);{digito natural + 1}
        if digit=0 then {no es un digito de esta notacion}
        begin
          if (c='.') and FloatingPoint then {y si es el punto}
          begin
            FloatingPoint:=false; {ya no pueden haber mas}
            Posicion:=0; {ajuste de -1 a 0}
          end else
          begin
            {ERROR}
            code:=1;
            EXIT;
          end;
        end else
        begin {es un digito normal elevar base^posicion}
          digit:=digit-1;  {lo que le sobraba}
          v:=v+digit*power(base,posicion);
        end;
      posicion:=posicion-1;
      end
  end;
  if negative then v:=-v;
end;

function SystemName(base:byte):string;
begin
  result:='Base '+IntToStr(base);
  case base of
  2:result:='Binario';
  8:result:='Octal';
  10:result:='Décimal';
  16:result:='Hexadecimal';
  end;
end;





end.

