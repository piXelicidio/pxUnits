unit pxVirtualFileMgr;

interface
  uses classes, SysUtils, pxFilePack;

type


  TVFileMgr = class
  public
    constructor create;
    destructor destroy;override;
  private
  protected
    fWorkingFolder    :string;
    fPackedFilesMode  :boolean;
    fPacks            :TStringList; //nombres de paquete y packetes clases TFilePackReader
                                //el nombre del pack actua como RootFolder del los nombres de fichero, así hasta se pueden tener varios packs
                               //si en el folder root especificado no coincide con el packname(s) entonces el fichero se lee de disco!
  public
    procedure SetWorkingFolder(const s:string );
    function ReadFile(const aFileName:string ):TStream;         //FileName relativo; Stream debes destruirlo tu
    procedure AddPackFile( const aFileName:string; xorreo:byte=0; LoadToMemory:boolean= false );            //relative to working folder too
    procedure AddPackResource( Instance:THandle; const RCDATA_Name:string; xorreo:byte=0);
    procedure SetPackedFilesMode( value :boolean );
    property WorkingFolder:string read fWorkingFolder write SetWorkingFolder;
    property PackedFilesMode:boolean read fPackedFilesMode;
  end;


implementation

{ TVFileMgr }

constructor TVFileMgr.create;

begin
  fPacks := TStringList.Create;
end;

destructor TVFileMgr.destroy;
var
  i :integer;
  p :TFilePackReader;
begin
  for i:=0 to fpacks.Count-1 do
  begin
    p := fPacks.Objects[i] as TFilePackReader;
    p.Free;
  end;
  inherited;
end;

function TVFileMgr.ReadFile(const aFileName: string): TStream;
var
  posi  :integer;
  pname :string;
  fname :string;
  idx :integer;
  p   :TFilePackReader;
begin
  Result := nil;
  if fPackedFilesMode then
  begin
    //Get File from packs
    //el primer Folder es el nombre del paquete
    posi := pos('\', aFileName);
    if posi<>0 then
    begin
      pname := Copy(afileName, 1, posi-1);
      fname := aFileName;
      delete(fname, 1, posi);
      idx := fPacks.IndexOf(pname);
      if idx<>-1 then
      begin
        p := fPacks.objects[idx] as TFilePackReader;
        idx := p.GetEntryByName(fname);
        if idx=-1 then
        begin
          //no se encontro el Entry;
          beep;
          asm nop end;
        end else
        begin
          Result := p.CreateMemStreamFromEntry(idx);
        end;
      end;
    end;
    if Result=nil then //no se encontró el paquete o el archivo...
    begin //abrirlo de disco entonces..
      Result := TFileStream.Create( fWorkingFolder+aFileName, fmOpenRead );
    end;
  end else
  begin
    Result := TFileStream.Create( fWorkingFolder+aFileName, fmOpenRead );
  end;
end;


procedure TVFileMgr.SetPackedFilesMode(value: boolean);
begin
  fPackedFilesMode := value;
end;

procedure TVFileMgr.AddPackFile(const aFileName: string; xorreo:byte; loadToMemory:boolean);
var
  p :TFilePackReader;
  s,e :string;
begin
  p := TFilePackReader.create;
  p.Open(fWorkingFolder + aFileName, xorreo, loadtomemory);
  s := ExtractFileName(aFileName);
  e := ExtractFileExt(s);
  delete(s, Length(s)-Length(e)+1,Length(e));
  fPacks.AddObject(s, p);
end;

procedure TVFileMgr.SetWorkingFolder(const s: string);
begin
  fWorkingFolder := s;
end;

procedure TVFileMgr.AddPackResource(Instance: THandle;
  const RCDATA_Name: string; xorreo: byte);
var
  p :TFilePackReader;
  s,e :string;
begin
  p := TFilePackReader.create;
  p.OpenFromResource(instance, RCDATA_Name, xorreo);
  fPacks.AddObject(RCDATA_Name, p);
end;


end.
