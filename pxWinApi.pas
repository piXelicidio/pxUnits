unit pxWinApi;

interface

  Uses Windows, ShellApi, pxStrings;

type
 TByteArray = array[0..0] of byte;
 PByteArray = ^TByteArray;
 TStrArr    = array of string;


function  EjecutaExe( FileName, CmdLine:string):cardinal;  //crea process return hprocess
function  ShellAbrirFichero( FileName:string):cardinal;    //return hprocess
procedure ShowMecha( str:string);
function  EstaEjecutandose (FicheroExe:string): boolean;      //creating e file
procedure Xorrea( var data; count:integer; mask:byte);
function Cript( str:string; mask:byte):string;overload;
function Cript( str:string ):string;overload;
function GetPathOf(what:string):String;                    //shell folders
function GetStrsFromRunMRU:TStrArr;                        //Win+Run strings
function Win32Platform:integer;                            //ver si es NT o 9X
function RenameFile(const OldName, NewName: string): Boolean;
function ErrorString(err:dword):string;

implementation

function ErrorString(err:dword):string;
var
  buf:pchar;
begin
 getmem(buf, 500);
 FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
                 nil, err, 0, buf, 500, nil);
 Result:=string(buf);
 freemem(buf,500);
end;

function EstaEjecutandose (FicheroExe:string):boolean;
begin

end;


function RenameFile(const OldName, NewName: string): Boolean;
begin
  Result := MoveFile(PChar(OldName), PChar(NewName));
end;

function  ShellAbrirFichero( FileName:string):cardinal;
var
   Info:TShellExecuteInfo;
   pInfo:PShellExecuteInfo;
begin
     {Puntero a Info}
     {Pointer to Info}
     pInfo:=@Info;
     {Rellenamos Info}
     {Fill info}
     with  Info do
     begin
      cbSize:=SizeOf(Info);
      fMask:=SEE_MASK_NOCLOSEPROCESS;
      lpVerb:=nil;
      wnd:=0;
      lpFile:=PChar(FileName);
      {Parametros al ejecutable}
      {Executable parameters}
      lpParameters:=nil;
      lpDirectory:=nil;
      nShow:=SW_SHOW;
      hInstApp:=0;
     end;
     {Ejecutamos}
     {Execute}
     ShellExecuteEx(pInfo);
     result:=info.hprocess;
end;



function Win32Platform:integer;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  result:=OSVersionInfo.dwPlatformId;
end;

function GetStrsFromRunMRU:TStrArr;
var
  arr: TStrArr;
  str, Val: PChar;
  i, j: integer;
  Key: HKey;
begin
  if (RegOpenKeyEx(HKEY_CURRENT_USER,
    'Software\Microsoft\Windows\CurrentVersion\Explorer\RunMRU',
    0, KEY_QUERY_VALUE, Key)= ERROR_SUCCESS) then
    begin
    i:= 255;
    GetMem(str, i);
    GetMem(Val, 2);
    fillchar(val^,2,0);
    RegQueryValueEx(Key, 'MRUList', nil, nil, PByte(str), @i);
    dec(i);
    SetLength(arr, i);
    dec(i);
    for i:= 0 to i do
      begin
          Val[0]:= str[i];
          RegQueryValueEx(Key, Val, nil, nil, nil, @j);
          SetLength(arr[i], j);
          RegQueryValueEx(Key, Val, nil, nil, @arr[i][1], @j);
          SetLength(arr[i],j-3);
      end;
    FreeMem(Val);
    FreeMem(str);
    RegCloseKey(Key);
    end;
    result:=arr;
end;


function GetPathOf(what:string):String;
var
  j: integer;
  Key: HKey;
begin
  if (RegOpenKeyEx(HKEY_CURRENT_USER,
    'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
    0, KEY_QUERY_VALUE, Key)= ERROR_SUCCESS) then
    begin
      if RegQueryValueEx(Key, PChar(what), nil, nil, nil, @j)=ERROR_SUCCESS
      then
      begin
        SetLength(Result,j);
        RegQueryValueEx(Key, PChar(what), nil, nil, @Result[1], @j);
        SetLength(Result,j-1);
        RegCloseKey(Key);
      end else Result:='';
    end;
end;

function Cript( str:string; mask:byte):string;overload;
begin
  Xorrea( str[1], length(str), mask);
  result:=str;
end;

function Cript( str:string ):string;overload;
begin
  result:=Cript(str,1);
end;

procedure Xorrea( var data; count:integer; mask:byte);
var
  p:^Byte;
  i:integer;
begin
  p:=@data;
  for i:=0 to count-1 do
  begin
    p^:=p^ xor mask;
    inc(p);
  end;
end;

procedure ShowMecha( str:string);
begin
  MessageBox(0, PChar(str), PChar('/-<(o)>\-( )-/<(o)>-\ (i can see you))'),0);
end;

function EjecutaExe(FileName, CmdLine:string):cardinal;
var
  kk1       :_STARTUPINFOA;
  kk2       :_PROCESS_INFORMATION;
begin
    fillchar(kk1, SizeOf(kk1), 0);
    kk1.CB:=SizeOf(kk1);
    if CreateProcess(nil, PChar(FileName+' '+CmdLine), nil, nil, false, 0, nil, nil, kk1, KK2 )
    then result:=kk2.hProcess else result:=0;
end;



end.
