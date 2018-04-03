unit pxGridMap;

interface
  uses Classes, tmVectMat;

type


  TMapObject = class
  public
    constructor create;
  private
    fIndex      :integer;
    fActualPos  :TPt;
  public
    property ActualPos:TPt read fActualPos;
  end;

  TGridList = class(TList)
  public
    constructor Create;
  private
  protected
  public
    procedure Add(const obj:TMapObject );
    procedure Delete(const obj:TMapObject );
  end;

   TGridMap = class
   public
        constructor Create( aWidth, aHeight, aGridSizeX, aGridSizeY:integer);
        destructor Destroy;override;
   private
    function GetMapObjects(j, i: integer): TGridList;
   protected
        fMap    :array of array of TGridList;
        fOutOfMap :TGridList;
        fWidth,
        fHeight  :integer;
        fGridSizeX,
        fGridSizeY :integer;
        procedure Resize(NewWidth, NewHeight :integer);
   public
        function IsInside(x,y:integer):Boolean;
        procedure MoveObj( obj:TMapObject; x,y:integer); overload;
        procedure ObjectsNear(obj:TMapObject; GridsBorder:integer; List:TList);overload;
        procedure ObjectsNear(ax,ay:integer; GridsBorder: integer; List: TList);overload;
        procedure DeleteObject( obj:TMapObject);
        property MapObjects[j,i:integer]:TGridList read GetMapObjects;
   end;

implementation

{ TGridList }

procedure TGridList.Add(const obj: TMapObject);
begin
  obj.fIndex:=inherited Add(obj);
end;

constructor TGridList.Create;
begin
  inherited;
end;

procedure TGridList.Delete(const obj: TMapObject);
var
  i :integer;
  o :TMapObject;
begin
  inherited Delete(Obj.fIndex);
  for i:=Obj.fIndex to Count-1 do
    dec((TObject(Items[i]) as  TMapObject).fIndex);
end;

{ TMapObject }

constructor TMapObject.create;
begin
  fIndex:=-1; //-1 indica que no estaba en ningúna lista
  fActualPos.x:=-2;
  fActualPos.y:=-2;
end;

{ TMap }

constructor TGridMap.Create(aWidth, aHeight: integer; aGridSizeX, aGridSizeY:integer);
begin
  Resize(aWidth, aHeight);
  fOutOfMap := TGridList.Create;
  fWidth:=aWidth;
  fHeight:=aHeight;
  fGridSizeX:=aGridSizeX;
  fGridSizeY:=aGridSizeY;
end;

procedure TGridMap.DeleteObject(obj: TMapObject);
begin
  fMap[Obj.fActualPos.Y, Obj.fActualPos.X].Delete(obj);;
end;

destructor TGridMap.Destroy;
var
 i,j:integer;
begin
  inherited;
  for j:=0 to fHeight-1 do
    for i:=0 to fWidth-1 do
    begin
      fMap[j,i].Free;
    end;
end;

(*procedure TMap.Draw;
var
  j,i,k:integer;
  o :TMapObject;
begin
  fscr.PutImage2(0,0, fFondo);
  for j:=0 to fHeight-1 do
  begin
    for i:=0 to fWidth-1 do
    begin
      for k:=0 to fMap[j,i].fList.Count-1 do
      begin
        o:=fMap[j,i].fList[k];
        o.Draw(fScr);
        //Debug
        {fScr.Canvas.Font.Color:=clGreen;
        fScr.Canvas.Brush.Style:=bsClear;
        fScr.Canvas.TextOut(round(i*fGridSizeX),
                            round(j*fGridSizeY), IntToStr(fMap[j,i].fList.Count));
         }
      end;
    end;
  end;
end;
*)
procedure TGridMap.ObjectsNear(obj: TMapObject; GridsBorder: integer; List: TList);
var i,j,k:integer;
begin
  for j:=Obj.fActualPos.y-GridsBorder to Obj.fActualPos.Y+GridsBorder do
    if (j>0) and (j<fHeight) then
    for i:=Obj.fActualPos.x-GridsBorder to Obj.fActualPos.x+GridsBorder do
      if (i>0) and (i<fWidth) then
        for k:=0 to fMap[j,i].Count-1 do
        begin
          List.Add(fMap[j,i][k]);
        end;
end;

function TGridMap.IsInside(x, y: integer): Boolean;
begin
  x:=x div fGridSizeX;
  y:=y div fGridSizeY;
  Result:=(x>=0) and (x<fWidth) and (y>=0) and (y<fHeight)
end;

procedure TGridMap.ObjectsNear(ax,ay:integer; GridsBorder: integer; List: TList);
var
  i,j,k:integer;
  x,y:integer;
begin
  x:=ax div fGridSizeX;
  y:=ay div fGridSizeY;
  for j:=y-GridsBorder to Y+GridsBorder do
    if (j>0) and (j<fHeight) then
    for i:=x-GridsBorder to x+GridsBorder do
      if (i>0) and (i<fWidth) then
        for k:=0 to fMap[j,i].Count-1 do
        begin
          List.Add(fMap[j,i][k]);
        end;
end;

procedure TGridMap.Resize(NewWidth, NewHeight: integer);
var
  i,j:integer;
begin

  SetLength(fMap, NewHeight);
  for j:=0 to NewHeight-1 do
  begin
    SetLength(fMap[j], NewWidth);
    for i:=0 to NewWidth-1 do
    begin
      fMap[j,i] := TGridList.Create;
    end;
  end;
end;

//Para la primera vez Obj.fActualPos.x = -1
//Si x o y se salen de las dimensiones de la matrix.... kacharro total
//arreglar esto en el futuro poniendo una lista
//para objetos fuera de lugar..
procedure TGridMap.MoveObj(obj: TMapObject; x, y: integer);
var
  NewPos:TPt;
begin
  NewPos.x:=x div fGridSizeX;
  if x<0 then Dec(NewPos.x);
  NewPos.y:=y div fGridSizeY;
  if y<0 then dec(NewPos.y);
  if (NewPos.x <> Obj.fActualPos.X) or (NewPos.Y <> Obj.fActualPos.Y) then
  begin
    if  (Obj.fIndex<>-1) then
    begin
      if Obj.fActualPos.x<>-1 then fMap[Obj.fActualPos.y, Obj.fActualPos.x].Delete(Obj)
        else fOutOfMap.Delete(obj);
    end;
    if (NewPos.x>=0) and (NewPos.x<fWidth) and (NewPos.y>=0) and (NewPos.y<fHeight) then
    begin
      fMap[NewPos.y, NewPos.x].Add(obj);
      Obj.fActualPos.X:=NewPos.X;
      Obj.fActualPos.Y:=NewPos.Y;
    end else
    begin
      fOutOfMap.Add(obj);
      Obj.fActualPos.X:=-1;
      Obj.fActualPos.Y:=-1;
    end;

  end;
end;

function TGridMap.GetMapObjects(j, i: integer): TGridList;
begin
  Result := fMap[j,i];
end;

end.
