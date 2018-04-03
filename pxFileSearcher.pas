unit pxFileSearcher;
{ver variables globales}
interface

uses SysUtils;

type
  TFileProc = procedure( FileName :string; var Stop:boolean) of object;

Var
  DirChanged  :boolean;
  SearchRec   :TSearchRec;




Procedure SearchFiles( const InitPath:string; const FileProc:TFileProc; const Comodines:string);

implementation


Procedure SearchFiles( const InitPath:string; const FileProc:TFileProc; const Comodines:string);
var
  Stop  :boolean;

  Procedure SDir ( SPath : String);
  Var
    S     : TSearchRec;
    Error : integer;
  Begin
    DirChanged:=true;
    Error:=FindFirst (SPath + comodines, faAnyFile Xor faVolumeID Xor faDirectory, S);
    While (Error = 0) and not stop Do
    Begin
      SearchRec:=s;
      FileProc(SPath + S. Name, stop);
      DirChanged:=false;
      Error:=FindNext (S);
    End;
    FindClose(S);
    Error:=FindFirst (SPath + '*.*', faAnyFile Xor faVolumeID, S);
    While (Error = 0) and not stop Do
    Begin
      if (S.Attr and faDirectory)=faDirectory then
      begin
        if S.Name ='.'then FindNext(s)
          else SDir(SPath + S. Name+'\');
      end;
      Error:=FindNext (S);
    End;
    FindClose(S);
  End;
begin
  Stop:=false;
  SDir(InitPath);
end;

end.
