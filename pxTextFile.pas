unit pxTextFile;

interface

uses classes;

type

TTextFile = class
 public
  Constructor Create(FileName:string);
  Destructor Destroy;override;
 private
  ff              :text;
  fCurrLine       :string;
  fLastLine       :boolean;
  fCaseSensitive  :boolean;
  fLineNumber     :integer;
 protected
   function StrCase(s:string):string;
 public
   procedure GoNext;
   procedure GoFirst;
   function Search(Strs:TStrings):string;overload;
   function Search(s:String):string;overload;
   property LogFile:Text read ff ;
   property CurrLine:string read fCurrLine;
   property LastLine:boolean read fLastLine;
   property CaseSensitive:boolean read fCaseSensitive write fCaseSensitive;
   property LineNumber:integer read fLineNumber ;
 end;

implementation

uses SysUtils, pxStrings;

constructor TTextFile.Create(FileName:string);
begin
  try
  AssignFile(ff,FileName);
  Reset(ff);
  except
    free;
    raise;
  end;
  fLastLine:=false;
  fCaseSensitive:=true;
  fLineNumber:=0;
  GoNext;
end;

destructor TTextFile.Destroy;
begin
  try
    CloseFile(ff);
  except
  end;
  inherited;
end;

procedure TTextFile.GoFirst;
begin
  Reset(ff);
  fLineNumber:=0;
  fLastLine:=false;
  GoNext;
end;

procedure TTextFile.GoNext;
begin
  if not fLastLine then readln(ff,fCurrLine);
  fLastLine:=EOF(ff);
  inc(fLineNumber);
end;
{si lo ecuentra devuelve quien encontro
y en currline se queda la linea que lo contiene}
function TTextFile.Search(Strs: TStrings): string;
var
  found:boolean;

  procedure test;
  var
    i:integer;
  begin
    found:=false;
    for i:=0 to strs.count-1 do
    begin
      found:=Pos(StrCase(strs.strings[i]), StrCase(CurrLine))<>0;
      if found then
      begin
        Result:=Strs.Strings[i];
        break;
      end;
    end;
  end;
begin
  Result:='';
  if Strs<>nil then
  begin
    test;
    while (not found) and (not LastLine) do
    begin
      GoNext;
      Test;
    end;
  end;
end;

function TTextFile.Search(s: String): string;
var
  strs:TStrings;
begin
  strs:=TStringList.Create;
  strs.Add(s);
  Result:=Search(strs);
end;

function TTextFile.StrCase(s:string): string;
begin
  if fCaseSensitive then result:=s else result:=UpperCase(s);
end;

end.
