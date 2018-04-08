unit pxListas;
{
  Denys Almaral Rodríguez
  septiembre - 2001



}
interface

  type

    PLstNode = ^TLstNode;
    TLstNode = Record
      info  :pointer;
      next  :PLstNode;
    end;

    TWriteInfo=procedure(aInfo:pointer; var KillThis, Stop :boolean) of object;
    TWriteInfo2=procedure(aInfo:pointer; var KillThis, Stop :boolean);
    TFastWriteInfo=procedure(aInfo:pointer) of object;
    TFastWriteInfo2=procedure(aInfo:pointer);

    TSimpleList = class(TObject)
      private
        FHead   :PLstNode; {FHead es puntero a un nodo de cabecera}
      public
        constructor create;
        destructor destroy;override;
        procedure AddNode(aInfo:pointer);virtual;
        procedure recorre(aProc:TWriteInfo);overload;
        procedure recorre(aProc:TWriteInfo2);overload;
        procedure recorre(aProc:TFastWriteInfo);overload;
        procedure recorre(aProc:TFastWriteInfo2);overload;
        function DeleteNext(antNode :PLstNode):pointer; {mucho cuidado con antNode,
                                                 nodo anterior al que se quiere borrar}
        function vacia:boolean;
    end;

    TPila = class(TSimpleList)
      public
        function sacar:pointer;
    end;

    TCola = class(TPila)
      private
        FFirst  :PLstNode;
      public
        procedure AddNode(aInfo:pointer);override;
    end;



implementation

{ TSimpleList }

function TSimpleList.DeleteNext;
var
  tmp:PLstNode;
begin
  tmp:=antNode^.next;
  antNode^.next:=tmp^.next;
  result:=tmp^.info;
  dispose(tmp);
end;

procedure TSimpleList.AddNode(aInfo: pointer);
var
  tmp:PLstNode;
begin
  new(tmp);
  tmp^.info:=aInfo;
  tmp^.next:=FHead^.next;
  FHead^.next:=tmp;
end;

constructor TSimpleList.create;
begin
  inherited;
  new(FHead);       {FHead^.next es el primer nodo de la lista}
  FHead^.info:=nil; {FHead^.info reservado }
  FHead^.next:=nil;
end;

destructor TSimpleList.destroy;
var
  tmp:PLstNode;
begin
  inherited;
  while FHead<>nil do
  begin
    tmp:=FHead;
    FHead:=FHead^.next;
    dispose(tmp);
  end;
end;

procedure TSimpleList.recorre(aProc:TWriteInfo);
var
  curr,ant       :PLstNOde;
  killthis, stop :Boolean;
begin
  Killthis:=false;
  stop:=false;
  ant:=FHead;
  curr:=FHead^.next;
  while (curr<>nil) and not stop  do
  begin
    aProc(curr^.info, Killthis, stop);
    if killthis then
    begin
      if (self is TCola) and (curr=TCola(self).FFirst) then
      begin
        TCola(self).FFirst:=ant;
      end;
      ant^.next:=curr^.next;
      dispose(curr);
      curr:=ant^.next;
      killthis:=false;
    end else
    begin
       ant:=curr;
       curr:=curr^.next
    end;
  end;
end;


procedure TSimpleList.recorre(aProc:TFastWriteInfo);
var
  curr       :PLstNOde;
begin
  curr:=FHead^.next;
  while (curr<>nil)  do
  begin
    aProc(curr^.info);
    curr:=curr^.next
  end;
end;


function TSimpleList.vacia: boolean;
begin
  result:=(FHead^.next=nil);
end;

procedure TSimpleList.recorre(aProc: TWriteInfo2);
var
  curr,ant       :PLstNOde;
  killthis, stop :Boolean;
begin
  Killthis:=false;
  stop:=false;
  ant:=FHead;
  curr:=FHead^.next;
  while (curr<>nil) and not stop  do
  begin
    aProc(curr^.info, Killthis, stop);
    if killthis then
    begin
      if (self is TCola) and (curr=TCola(self).FFirst) then
      begin
        TCola(self).FFirst:=ant;
      end;
      ant^.next:=curr^.next;
      dispose(curr);
      curr:=ant^.next;
      killthis:=false;
    end else
    begin
       ant:=curr;
       curr:=curr^.next
    end;
  end;
end;

procedure TSimpleList.recorre(aProc: TFastWriteInfo2);
var
  curr       :PLstNOde;
begin
  curr:=FHead^.next;
  while (curr<>nil)  do
  begin
    aProc(curr^.info);
    curr:=curr^.next
  end;
end;

{ TPila }

function TPila.sacar: pointer;
var
  tmp:PLstNode;
begin
  result:=FHead^.next;
  if result<>nil then
  begin
    result:=PLstNode(result)^.info;
    tmp:=FHead^.next;
    FHead^.next:=tmp^.next;
    dispose(tmp);
  end;
end;

{ TCola }

procedure TCola.AddNode(aInfo: pointer);
var
  tmp:PLstNode;
begin
  new(tmp);
  tmp^.Info:=aInfo;
  tmp^.next:=nil;
  if vacia then FHead^.next:=tmp
           else FFirst^.next:=tmp;
  FFirst:=tmp;
end;

end.
