unit pxHardApiTools;
{esta unit no puede usar nada de la VCL}
interface

  Uses Windows, ShellApi, TLHelp32, pxStrings,
       pxFileNames, pxHardApiTypes, pxSearcher;

const
  rc3_StockIcon = 0;
  rc3_Icon = 1;
  rc3_Cursor = 2;
Type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;
PIconRec = ^TIconRec;
TIconRec = packed record
  Width: Byte;
  Height: Byte;
  Colors: Word;
  Reserved1: Word;
  Reserved2: Word;
  DIBSize: Longint;
  DIBOffset: Longint;
end;

function TempPath:string;
function GetProcessNames:TStrArray;
function  EjecutaExe( FileName, CmdLine:string; Hidden :boolean = false):cardinal;  //crea process return hprocess
function  ShellAbrirFichero( FileName:string):cardinal;    //return hprocess
procedure ShowMecha( str:string);
function  EstaEjecutandose (FicheroExe:string; CaseSensitive:boolean=true): boolean;      //creating e file
procedure Xorrea( var data; count:integer; mask:byte);
function Cript( str:string; mask:byte):string;overload;
function Cript( str:string ):string;overload;
function GetPathOf(what:string):String;                    //shell folders
function GetStrsFromRunMRU:TStrArray;                        //Win+Run strings
function Win32Platform:integer;
function IsNTPlatform:boolean;                         //ver si es NT o 9X
function RenameFile(const OldName, NewName: string): Boolean;
function ErrorString(err:dword):string;
function AppExeName:string;

procedure WriteIcon(var buffer:TByteArray; Icon: HICON );
procedure WriteToFile(FileName:string; var buff; Size:integer);
procedure WriteToArray(var target:TByteArray; var Buff; size:integer);
function  ReplaceExeIcon(TargetExeFile :string; OrdinaryName :byte;
                      GroupName :string; Memory :TByteArray ):boolean;
function  ReplaceExeIconWin9x(TargetExeFile :string; const Memory:TByteArray):boolean;
function  SearchPos(const Key:TByteArray; const FileName:string):integer;
function  ExtractRCDATA( RCName:string; var Buff:TByteArray):boolean;
function  GetStrsFromKey(Rootkey:HKEY; SubKey:string ):TStrArray;
function  GetStrsFromRun:TStrArray;
function  ResolvePath(fileName: string): string;


implementation

function ResolvePath(fileName: string): string;
{Simelo(C) tested....}
const
  mask: string= '.exe';
var
  i: cardinal;
  param: PChar;
begin
  result:= '';
  i:= SearchPath(nil, PChar(fileName), PChar(mask),	0, nil, param);
  if (i> 0) then
    begin
    SetLength(result, i- 1);
    SearchPath(nil, PChar(fileName), PChar(mask), i, @result[1], param);
    end;
end;

function TempPath:string;
var
  len :integer;
begin
  len:=GetTempPath(0, nil);  {temp path}
  SetLength(Result,len);
  GetTempPath(len, PChar(Result));
  SetLength(Result,len-1);
end;

function AppExeName:string;
begin
  Result:=ParamStr(0);
end;

function ErrorString(err:dword):string;
var
  buf   :pchar;
  l     :integer;
begin
 getmem(buf, 500);
 l:=FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
                 nil, err, 0, buf, 500, nil);
 Result:=string(buf);
 freemem(buf,500);
 if l=0 then Result:='Erroneous error identifier value.';
end;

function GetProcessNames:TStrArray;
var
  hss   :cardinal;
  rec   :TPROCESSENTRY32;
  more  :boolean;
begin
  setLength(Result,0);
  hss:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  rec.dwSize:=SizeOf(rec);
  more:=Process32First(hss, rec);
  while more do
  begin
    {catch this.... baby}
    setLength(Result, Length(Result)+1);
    rec.dwSize:=SizeOf(rec);
    Result[Length(Result)-1]:=String(rec.szExeFile);
    more:=Process32Next(hss, rec);
  end;
  CloseHandle(hss);
end;

function EstaEjecutandose(FicheroExe:string; CaseSensitive:boolean=true):boolean;
var
  strs :TStrArray;
  cont :integer;
begin
  cont:=-1;
  strs:=GetProcessNames;
  FicheroExe:=ExtractFileName(FicheroExe);
  repeat
    inc(cont);
  until CmpStrs(FicheroExe, ExtractFileName(strs[cont]), CaseSensitive) or (cont=Length(strs)-1);
  Result:=( cont<Length(strs)-1 );
  if not Result then Result:=(CmpStrs(FicheroExe,ExtractFileName(strs[cont]),CaseSensitive));
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

function GetStrsFromRunMRU:TStrArray;
var
  arr: TStrArray;
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

{
  Get Path Of Windows user folder
  What: especifica la carpeta
  ejemplos:
    Cache        [temporales de internet]
    Desktop
    My Pictures
    Personal     [My Documents]
    Programs
    Start Menu
    Startup
}
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
    p^:=p^ xOR mask;
    inc(p);
  end;
end;

procedure ShowMecha( str:string);
begin
  MessageBox(0, PChar(str), PChar('/-<(o)>\-( )-/<(o)>-\ (i can see you))'),0);
end;

{
 Retorna un Handle que puede ser usado
 con WaitForObject para esperar  a que
 se termine la ejecución del proceso
}
function EjecutaExe(FileName, CmdLine:string; Hidden :boolean = false ):cardinal;
var
  kk1       :_STARTUPINFOA;
  kk2       :_PROCESS_INFORMATION;
begin
    fillchar(kk1, SizeOf(kk1), 0);
    kk1.CB:=SizeOf(kk1);
    if Hidden then
    begin
      kk1.wShowWindow := SW_HIDE;
    end;
    if CreateProcess(nil, PChar(FileName+' '+CmdLine), nil, nil, false, 0, nil, nil, kk1, KK2 )
    then result:=kk2.hProcess else result:=0;
end;


function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  Colors: Integer);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  {if Bytes = 0 then InvalidBitmap
  else}
  if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(sizeof(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case Colors of
    2: BI.biBitCount := 1;
    3..16:
      begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;
    17..256:
      begin
        BI.biBitCount := 8;
        BI.biClrUsed := Colors;
      end;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
end;


procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD; Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, Colors);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * (1 shl BI.biBitCount)
    else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * BI.biClrUsed;
  ImageSize := BI.biSizeImage;
end;


function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; Colors: Integer): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
      TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    if OldPal <> 0 then SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

procedure WriteToArray(var target:TByteArray; var Buff; size:integer);
var
  len:integer;
begin
  len:=Length(target);
  setlength(target, Len+size);
  move(buff, target[len], size);
end;


procedure WriteIcon(var buffer:TByteArray; Icon: HICON );
var
  IconInfo: TIconInfo;
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
  CI: TCursorOrIcon;
  List: TIconRec;
//  Length: Longint;
begin
  FillChar(CI, SizeOf(CI), 0);
  FillChar(List, SizeOf(List), 0);
  GetIconInfo(Icon, IconInfo);
  try
    InternalGetDIBSizes(IconInfo.hbmMask, MonoInfoSize, MonoBitsSize, 2);
    InternalGetDIBSizes(IconInfo.hbmColor, ColorInfoSize, ColorBitsSize, 16);
    MonoInfo := nil;
    MonoBits := nil;
    ColorInfo := nil;
    ColorBits := nil;
    try
      //MonoInfo := AllocMem(MonoInfoSize);
      GetMem(MonoInfo, MonoInfoSize);
      FillChar(MonoInfo^, MonoInfoSize, 0);
      //MonoBits := AllocMem(MonoBitsSize);
      GetMem(MonoBits, MonoBitsSize);
      FillChar(MonoBits^, MonoBitsSize, 0);
      //ColorInfo := AllocMem(ColorInfoSize);
      GetMem(ColorInfo, ColorInfoSize);
      FillChar(ColorInfo^, ColorInfoSize, 0);
      //ColorBits := AllocMem(ColorBitsSize);
      GetMem(ColorBits, ColorBitsSize);
      FillChar(ColorBits^, ColorBitsSize, 0);
      InternalGetDIB(IconInfo.hbmMask, 0, MonoInfo^, MonoBits^, 2);
      InternalGetDIB(IconInfo.hbmColor, 0, ColorInfo^, ColorBits^, 16);
{      if WriteLength then
      begin
        Length := SizeOf(CI) + SizeOf(List) + ColorInfoSize +
          ColorBitsSize + MonoBitsSize;
        Stream.Write(Length, SizeOf(Length));
      end;}
      with CI do
      begin
        CI.wType := RC3_ICON;
        CI.Count := 1;
      end;
      SetLength(buffer,0);
      //Stream.Write(CI, SizeOf(CI));
      WriteToArray(buffer, CI, SizeOf(CI));
      with List, PBitmapInfoHeader(ColorInfo)^ do
      begin
        Width := biWidth;
        Height := biHeight;
        Colors := biPlanes * biBitCount;
        DIBSize := ColorInfoSize + ColorBitsSize + MonoBitsSize;
        DIBOffset := SizeOf(CI) + SizeOf(List);
      end;
      //Stream.Write(List, SizeOf(List));
      WritetoArray(buffer, List, SizeOf(List));
      with PBitmapInfoHeader(ColorInfo)^ do
        Inc(biHeight, biHeight); { color height includes mono bits }
      //Stream.Write(ColorInfo^, ColorInfoSize);
      //Stream.Write(ColorBits^, ColorBitsSize);
      //Stream.Write(MonoBits^, MonoBitsSize);
      WriteToArray(Buffer, colorInfo^, ColorInfoSize);
      WriteToArray(Buffer, colorBits^, ColorBitsSize);
      WriteToArray(Buffer, MonoBits^, MonoBitsSize);
    finally
      FreeMem(ColorInfo, ColorInfoSize);
      FreeMem(ColorBits, ColorBitsSize);
      FreeMem(MonoInfo, ColorInfoSize);
      FreeMem(MonoBits, MonoBitsSize);
    end;
  finally
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
  end;
end;

procedure WriteToFile(FileName:string; var buff; Size:integer);
var
  f:File;
begin
  AssignFile(f, FileName);
  ReWrite(f,1);
  if IOResult=0 then
  begin
    BlockWrite(f, buff, size);
  end;
  CloseFile(f);
end;

{16colors icons}
function ReplaceExeIcon(TargetExeFile :string; OrdinaryName :byte;
                      GroupName :string; Memory :TByteArray ):boolean;

var
  HUpDate :cardinal;
begin
    Hupdate:=BeginUpdateResource(pchar(TargetExeFile),false);
    Result:=Hupdate<>0;
    if Result then
    begin
      Memory[18]:=OrdinaryName;
      Memory[10]:=$01; {mmm?????}
      Memory[12]:=$04; {colors}
      {pone lo nuevo }
      UpDateResource(HUpdate, RT_GROUP_ICON,
                     PChar(GroupName),1033, @memory[0], 20);

      UpDateResource(HUpdate, RT_ICON,
                     PChar(OrdinaryName),1033, @memory[22], Length(memory)-22);
      EndUpdateResource(HUpdate, False);
    end;
end;
{
nota los nombres de los iconos de los resources pueden ser string o numeros, se diferencian
Use este procedimiento un no pregunto muchos por que?
}

function SearchPos(const Key:TByteArray; const FileName:string):integer;
{Dice la posicion de una cadena en un fichero}
var
  f :file;
  b :byte;
  Searcher  :TSearcher;
  count,limit :integer;
begin
  Result:=-1;
  Assignfile(f, fileName);
  Reset(f,1);
  Searcher:=TSearcher.create;
  Searcher.key:=Key;
  count:=0; Limit:=FileSize(f);
  repeat
    inc(count);
    BlockRead(f, b, 1);
    Searcher.NextByte(b);
  until Searcher.Found or (count=Limit);
  if Searcher.Found then Result:=Count-Length(Key);
  CloseFile(f);
end;

function  ReplaceExeIconWin9x(TargetExeFile :string; const Memory:TByteArray):boolean;
var
  ActualIconMem :TByteArray;
  wrd           :word;
  key           :TByteArray;
  f             :file;
  posi          :integer;
begin
  Result:=false;
  {icono que tiene ahora}
  WriteIcon(ActualIconMem,
    ExtractAssociatedIcon(hinstance, PChar(TargetExeFile), wrd) );
  {donde lo tiene}
  SetLength(key, 200);
  Move(ActualIconMem[22], key[0], Length(Key));
  posi:=SearchPos(key, TargetExeFile);
  if Posi<>-1 then
  begin
    AssignFile(f, TargetExeFile); Reset(f, 1);
    Seek(f, Posi);
    BlockWrite(f, Memory[22], Length(Memory) - 22 );
    CloseFile(f);
  end;
end;

function ExtractRCDATA( RCName:string; var Buff:TByteArray):boolean;
var
  s1, s2    :pchar;
  hresinfo  :HRSRC;
  rdata     :HGLOBAL;
  p         :Pointer;
  size      :cardinal;
begin
  {Busca el Resource binario}
  s1:=PChar( RCName );
  S2:=RT_RCDATA;
  HResInfo:=FindResource(0, S1, S2);
  {existe}
  Result:=(HResInfo<>0);
  if Result then
  begin
    {carda el recurso (el cangurito)}
    rdata:=LoadResource(0, hresinfo);
    size:=SizeOfResource(0, hresinfo);
    {coge un puntero }
    p:=LockResource(rdata);
    SetLength(Buff, size);
    Move(P^, Buff[0], size);
    UnlockResource(rdata);
  end;
end;

function IsNTPlatform:boolean;
begin
  Result:=(Win32Platform = VER_PLATFORM_WIN32_NT);
end;


function GetStrsFromRun:TStrArray;
begin
  Result:=GetStrsFromKey(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run');
end;

function GetStrsFromKey(Rootkey:HKEY; SubKey:string ):TStrArray;
var
  Key    : HKey;
  len    : integer;
  value,
  ValueName  : string;
  vsize,
  vnsize     : cardinal;
  cont   : integer;
begin
  if (RegOpenKeyEx(RootKey, PChar( SubKey ),
    0, KEY_QUERY_VALUE, Key)= ERROR_SUCCESS) then
    begin
      SetLength(result,0);
      cont:=0;
      SetLength(valuename,500); vnsize:=500;
      SetLength(value,500);      vsize:=500;
      while RegEnumValue(key, cont, PChar(ValueName), vnsize, nil, nil, @Byte(Value[1]), @vsize)=ERROR_SUCCESS do
      begin
        len:=Length(Result);
        SetLength(Result, len+2);
        Result[len]:=copy(valueName, 1, vnsize);
        Result[len+1]:=copy(value, 1, vsize-1);
        inc(cont);
      end;
      RegCloseKey(Key);
    end;
end;

{}

end.
