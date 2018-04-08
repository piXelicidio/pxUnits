unit pxFilePack;
{
    Unit pxFilePack
    Denys A.R. (piXel)
    May 2009

    Para manejar ficheros empaquetados en un solo
    archivo, similar a los .BIG, .WAD, etc..

    NO PUBLICAR: Este debe ser un formato privado.

    Formato PFP1 - px File Pack
    Pos -> 0
    header record - TfpHeader;
      ID :4xCHAR   --File ID  = PFP1
      DirCount     --Cantidad de entradas del directorio
      DirPos       --Posicion en fichero donde comienza el directorio
      Reserved     --DWORD

    Pos -> TfpHeader.DirPos
    Todas las entradas del directorio (TfpHeader.DirCount entradas)
      Entrada(0..N-1)
        Pos       :LongWord
        Size      :LongWord
        NameSize  :LongWord
        FullName  :Array[1..NameSize] of char

    Pos -> Entrada[N].Pos
      Datos de fichero del tamaño Entrada[N].Size

    NOTA: Encriptación simple:
      Tanto los Nombres de entradas como los datos de fichero pueden
      estar encriptados con un XOR byte por byte predeterminado que solo
      conoce la aplicación.
}
interface

uses classes, Windows, pxFileSearcher, SysUtils;

type

    TfpHeader = packed record
      ID          :array[0..3] of char;
      DirCount    :LongWord;
      DirPos      :LongWord;
      RESERVED    :LongWord;
    end;

    TfpEntry  = packed record
      Pos         :LongWord;
      Size        :LongWord;
      NameSize    :LongWord;
      Name        :string;
      FullName    :string;        //este no se almacena en el Pack
    end;

    
    TFilePackBuilder = class
    public
      constructor create;
      destructor destroy;override;
    private
    function GetEntry(i: integer): TfpEntry;
    function GetEntriesCount: integer;
    protected
      fErrorMsgs  :TStrings;
      fHeader     :TfpHeader;
      fEntries    :array of TfpEntry;
      fCurrRootFolder :string;
      procedure myFileProc( FileName :string; var Stop:boolean);
      procedure WriteHeader( fs :TStream; xorreo:byte);
      procedure WriteEntries( fs :TStream; xorreo:byte);
      procedure PackFileEntries( fs:TStream; xorreo:byte);
    public
      procedure SetRootDirectory( RootFolder:String);
      procedure AddEntry( aFullFileName:string);
      procedure ScanDirectory;
      procedure BuildPack( packName:string; xorreo:byte = 0 );
      property ErrorMsgs:TStrings read fErrorMsgs;
      property Entries[i:integer]:TfpEntry read GetEntry;
      property EntriesCount:integer read GetEntriesCount;
    end;

    TFilePackReader = class
    public
      constructor create;
      destructor destroy;override;
    private
    function GetEntriesCount: integer;
    function GetEntry(i: integer): TfpEntry;
    protected
      pf          :TStream;
      fHeader     :TfpHeader;
      fEntries    :array of TfpEntry;
      fXorreo     :byte;
      fLastFoundIdx  :integer;
      procedure ReadHeader( fs :TStream );
      procedure ReadEntries( fs :TStream );
    public
      function  Open( const aFileName:string; xorreo:byte = 0; LoadToMemory:boolean = False ):boolean;
      function  OpenFromResource(Instance :THandle; RCDATA_Name:string; xorreo:byte=0  ):boolean;
      procedure SaveEntryToStream( idx:integer; aStream:TStream );
      function  CreateMemStreamFromEntry( idx:integer ):TMemoryStream;
      procedure ReadEntry(idx:integer; var buf);
      function  GetEntryByName( aName :string ):integer;  //low optimization  - aprobechar el TStrings SORTED!
      property  Entries[i:integer]:TfpEntry read GetEntry;
      property  EntriesCount:integer read GetEntriesCount;
    end;


    procedure xorBuf( var buf; count:integer; xr :byte);

implementation

type
  PByte = ^byte;

procedure xorBuf(var buf; count:integer; xr:byte);
var
  p :PByte;
  i :integer;
begin
  p := @buf;
  for i:=0 to count-1 do
  begin
    p^ := p^ xor xr;
    inc(p);
  end;
end;

{ TFilePackBuilder }


procedure TFilePackBuilder.AddEntry(aFullFileName: string);
var
  s, fname:string;
  len :integer;
begin
  fname := aFullFileName;
  s:=copy(fname, 1, Length(fCurrRootFolder));
  if fCurrRootFolder = s then
  begin
    delete(fname, 1, Length(fCurrRootFolder));
  end else
  begin
    fname := ExtractFileName(fname);
  end;
  len := Length(fEntries);
  SetLength(fEntries, len+1);

  fEntries[len].Pos:=0;
  fEntries[len].Size:=0;
  fEntries[len].NameSize := Length(fName);
  fEntries[len].Name := Uppercase(fName);
  fEntries[len].FullName := aFullFileName;
  //add entry data
end;

procedure TFilePackBuilder.BuildPack(packName: string;  xorreo: byte );
var
  pf  :TFileStream;

begin
  pf := TFileStream.Create(packName, fmCreate);
  //Escribe el header y los entries por primera vez sin actualizar
  WriteHeader( pf, 0 );
  WriteEntries( pf, 0 );

  //Ahora empaqueta los ficheros
  PackFileEntries( pf, xorreo);

  //Actualizandos los entreis y el header... Write Again...
  fHeader.ID[0] := 'R'; //le puse Ral! ahora para confundir
  fHeader.ID[1] := 'a';
  fHeader.ID[2] := 'l';
  fHeader.ID[3] := '!';
  fHeader.DirCount := EntriesCount;
  fHeader.DirPos := SizeOf( fHeader );
  fHeader.RESERVED := 0;

  pf.Position :=0;

  WriteHeader(pf, xorreo);
  WriteEntries(pf, xorreo);
  {Listo!}

  pf.Free;
end;

constructor TFilePackBuilder.create;
begin
  fCurrRootFolder := '';
  fErrorMsgs := TStringList.create;
end;

destructor TFilePackBuilder.destroy;
begin
  SetLength(fEntries,0);
  fErrorMsgs.Free;
  inherited;
end;

function TFilePackBuilder.GetEntriesCount: integer;
begin
  Result := Length(fEntries);
end;

function TFilePackBuilder.GetEntry(i: integer): TfpEntry;
begin
  Result := fEntries[i];
end;

procedure TFilePackBuilder.myFileProc(FileName: string; var Stop: boolean);
begin
  AddEntry(FileName);
  stop := false;
end;

procedure TFilePackBuilder.PackFileEntries(fs: TStream; xorreo: byte);
var
  i :integer;
  e :TfpEntry;
  ms  :TMemoryStream;
begin
  ms := TMemoryStream.Create;
  for i:=0 to EntriesCount-1 do
  begin
    e := fEntries[i];
    try
      ms.LoadFromFile(e.FullName);
      except
        fErrorMsgs.Add('READ ERROR: '+e.FullName);
        e.Name[1] := '$';                              //mark problematic file
        e.Size := 0;
        e.Pos  := 0;
        fEntries[i] := e;
    end;

    XorBuf(ms.Memory^, ms.size, xorreo);
    e.Size := ms.Size;
    e.Pos := fs.Position;
    fEntries[i] := e;
    ms.SaveToStream( fs );
  end;
  ms.Free;
end;

procedure TFilePackBuilder.ScanDirectory;
begin
 SearchFiles(fCurrRootFolder, myFileProc, '*.*');
end;

procedure TFilePackBuilder.SetRootDirectory(RootFolder: String);
begin
  fCurrRootFolder := RootFolder;
  SetLength(fEntries, 0);
end;

procedure TFilePackBuilder.WriteEntries(fs: TStream; xorreo: byte);
var
  i :integer;
  e :TfpEntry;
begin
  for i:=0 to EntriesCount-1 do
  begin
    e := fEntries[i];
    //xorBuf(e, 3 * SizeOf(LongWord), xorreo ); solo sorreo en el name
    xorBuf(e.Name[1], Length(e.Name), xorreo);
    fs.WriteBuffer(e, 3 * SizeOf(longWord));;
    fs.WriteBuffer(e.Name[1], Length(e.name));
  end;
end;

procedure TFilePackBuilder.WriteHeader(fs: TStream; xorreo: byte);
var
  h:TfpHeader;
begin
  h := fHeader;
  //  xorBuf(h,sizeOf(h), xorreo); no al xorreo en el header
  fs.WriteBuffer(h, SizeOf(h));
end;

{ TFilePackReader }

constructor TFilePackReader.create;
begin
  pf := nil;
end;

function TFilePackReader.CreateMemStreamFromEntry(
  idx: integer): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.SetSize(fEntries[idx].Size);
  ReadEntry(idx,  Result.memory^);
end;

destructor TFilePackReader.destroy;
begin

  inherited;
end;

function TFilePackReader.GetEntriesCount: integer;
begin
  Result := Length(fEntries);
end;

function TFilePackReader.GetEntry(i: integer): TfpEntry;
begin
  Result := fEntries[i];
end;

function TFilePackReader.GetEntryByName(aName: string): integer;
var
  found:boolean;
begin
  //Busqueda rotatoria..
  //se empieza la búsiqueda a partir del siguiente del último encontrado
  //(Mejorar esta búsqueda en el futuro ordenando primero la lista
  // y haciendo búsqueda binaria... you know)
  Result := fLastFoundIdx;
  aName := upperCase(aName);
  repeat
    inc(Result);
    if Result = Length(fEntries) then Result:=0;
    found := (aName = fEntries[Result].Name);
  until found or (Result=fLastFoundIdx);
  if found then fLastFoundIdx:=Result else Result:=-1;
end;

function TFilePackReader.Open(const aFileName: string; xorreo: byte; LoadToMemory:boolean): boolean;
var
  oldpf:TStream;
begin
  oldPf:=pf;
  Result := FileExists(aFileName);
  if Result then
  begin
    try
      fXorreo := xorreo;
      if LoadToMemory then
      begin
        pf := TMemoryStream.Create;
        (pf as TMemoryStream).LoadFromFile(aFileName);
      end else
      begin
        pf := TFileStream.Create(aFileName, fmOpenRead );
      end;
      ReadHeader( pf );
      ReadEntries( pf );
      oldpf.Free;
      except
        Result:=false;
    end;
  end;
end;

function TFilePackReader.OpenFromResource(Instance: THandle;
  RCDATA_Name: string; xorreo:byte=0): boolean;
var
 newpf :TResourceStream;
begin
  try
    fXorreo := xorreo;
    newpf := TResourceStream.Create(Instance, RCDATA_Name, RT_RCDATA );
    if pf<>nil then pf.Free;
    pf := newpf;
    ReadHeader( pf );
    ReadEntries( pf );
    except;
      Result := false;
  end;
end;

procedure TFilePackReader.ReadEntries(fs: TStream);
var
  i:integer;
  e:TfpEntry;
begin
  fs.Position := fHeader.DirPos;
  SetLength(fEntries, fHeader.DirCount);
  fLastFoundIdx := Length(fEntries)-1;
  for i:=0 to fHeader.DirCount-1 do
  begin
    fs.ReadBuffer(e, 3*SizeOf(LongWord));
    SetLength(e.Name, e.NameSize);
    fs.ReadBuffer(e.Name[1], e.NameSize);
    xorBuf(e.Name[1], e.NameSize ,fXorreo);
    e.FullName := '';
    fEntries[i] := e;
  end;
end;

procedure TFilePackReader.ReadEntry(idx: integer; var buf);
var
  size  :integer;
begin
  size := fEntries[idx].Size;
  pf.Position := fEntries[idx].Pos;
  pf.ReadBuffer(buf, size);
  if fXorreo<>0 then xorBuf(buf, size, fXorreo);

end;

procedure TFilePackReader.ReadHeader(fs: TStream);
begin
  fs.Position:=0;
  fs.ReadBuffer(fHeader, SizeOf(fHeader));
end;

procedure TFilePackReader.SaveEntryToStream(idx: integer;
  aStream: TStream);
var
  p :Pointer;
  size  :integer;
begin
  size := fEntries[idx].Size;
  GetMem( p, size);
  ReadEntry(idx, p^);
  aStream.WriteBuffer(p^, size);
  FreeMem( p, size);
end;

end.
