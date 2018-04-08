Unit pxSearcher;

interface
uses pxHardApiTypes;

type

  TSearcher = class
    public
    	constructor create;
    private
    protected
    	fKey	:TByteArray;
    	fFound :boolean;
      fCont :integer;
    public
      procedure Reset;
    	procedure NextByte(const b:Byte);
    	property Found:boolean read fFound;
    	property Key:TByteArray  read fKey write fkey;
  end;

implementation

constructor TSearcher.create;
begin
	fFound:=false;
  fCont:=0;
end;

procedure TSearcher.NextByte(const b: Byte);
begin
  if fFound then fFound:=false;
  if b=fKey[fCont] then inc(fCont) else fCont:=0;
  if fCont=length(fKey) then
  begin
    fFound:=true;
    fCont:=0;
  end;
end;

procedure TSearcher.Reset;
begin
  fCont:=0;
end;

end.