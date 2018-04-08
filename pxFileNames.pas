unit pxFileNames;
{estas unit no debe usar ninguna unit vcl}
interface
  uses pxHardApiTypes;

function ChangeFileExt(const FileName, Extension: string): string;
function ExtractFilePath(const FileName: string): string;
function ExtractFileDir(const FileName: string): string;
function ExtractFileDrive(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;
function ExpandFileName(const FileName: string): string;
function FileExists(const FileName: string): boolean;
Function SearchForFiles(DirPath:string; Comodines:string):TStrArray;
Function GetNewFileName(propuesta:string):string;

implementation
  uses pxStrings, Windows;

function FileExists(const FileName:string): boolean;
var
  wfd    :_WIN32_FIND_DATAA;
  hfind  :cardinal;
begin
  hfind:=FindFirstFile(pchar(FileName),wfd);
  Result:=(hfind<>INVALID_HANDLE_VALUE);
  if result then FindClose(hfind);
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := Length(S);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
      if (ByteType(S, Result) = mbTrailByte) then
        Dec(Result)
      else
        Exit;
    Dec(Result);
  end;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:',Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDir(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:',Filename);
  if (I > 1) and (FileName[I] = '\') and
    (not (FileName[I - 1] in ['\', ':']) or
    (ByteType(FileName, I-1) = mbTrailByte)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: string): string;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = ':') then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = '\') and
    (FileName[2] = '\') then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = '\' then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = '\' then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

function ExtractFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function ExpandFileName(const FileName: string): string;
var
  FName: PChar;
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, Buffer, GetFullPathName(PChar(FileName), SizeOf(Buffer),
    Buffer, FName));
end;

Function SearchForFiles(DirPath:string; Comodines:string):TStrArray;
var
  hfind:cardinal;
  wfd       :_WIN32_FIND_DATAA;
  filename  :string;
  len       :integer;
begin
  Setlength(Result,0);
  hfind:=FindFirstFile(pchar(Dirpath+Comodines),wfd);
  if hfind<>INVALID_HANDLE_VALUE then
  begin
    repeat
      filename:=string(wfd.cFileName);
      Len:=Length(Result);
      Setlength(Result, len+1);
      Result[len]:=ExtractFilePath(DirPath)+filename;
    until not FindNextFile(hfind, wfd);
    FindClose(hfind);
  end;
end;

Function GetNewFileName(propuesta:string):string;
{Prueba que la propuesta exista y busca uno nuevo
 añadiendo _A  _B .. etc etc}
var
  DifStr  :string;
  ch      :char;
  Path,
  Name,
  Ext     :string;
begin
  DifStr:='';
  ch:='A';
  Path:=ExtractFilePath(propuesta);
  Name:=ChangeFileExt(ExtractFileName(propuesta),'');
  Ext:=ExtractFileExt(propuesta);
  while (FileExists(Path+Name+DifStr+Ext)) or (ch='Z') do
  begin
    DifStr:='_'+ch;
    inc(ch);
  end;
  Result:=Path+Name+DifStr+Ext;
end;



end.
