Unit pxLoadDll;

interface
Uses
  sysutils, windows;

Type
 EInvalidLibrary = class(Exception)
      ExErrorCode : WORD;
 end;

TDLL = class
  private
    hdll :HINST;
  public
    constructor create(Nombre:Pchar);
    function GetprocAdd(Nombre:Pchar):Pointer;
    destructor Destroy; Override;
    property Handle : HINST read hdll;
end;
implementation

function TDLL.GetprocAdd(nombre:Pchar):pointer;
begin
     Result:=GetProcAddress(hdll, Nombre); //obtiene la direccion de la funcion
end;

constructor TDLL.create;
var
  e: EInvalidLibrary;
begin
	hdll:=LoadLibrary(nombre); //Carga la DLL en memoria y obtiene el handle
	if hdll=0 then
	begin
		e.ExErrorCode:=GetLastError;
		raise EinvalidLibrary.create(nombre);
  end;
end;  

destructor TDLL.destroy;
begin
	FreeLibrary(hdll);     //Descarga la DLL de memoria
	inherited  destroy;
end;

end.