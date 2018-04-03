unit pxPolygonTools;

interface
  uses tmVectMat, Math;
type

  TIndexArray = array of Integer;

  Function ConvexAroundPoly( Vertices:TVectorArray):TVectorArray;  //2D
  Procedure RoundingBox( const Vertices:TVectorArray; out MinBox, MaxBox :TVector);
  Procedure RoundingSphere( Vertices:TVectorArray; out Radio:single; out Center:TVector);
  Procedure RoundingCilinderGenetic(const Vertices:TVectorArray; out Radio:single; out Center:TVector); //2D
  Function MaxDist(const Vertices:TVectorArray; const Center:TVector):single;
  Function MinDist(const Vertices:TVectorArray; const Center:TVector):single;
  Function ShadowRays( const Poly1, Poly2:TVectorArray; out p1R, p2R, p1L, p2L :integer):integer; //2D
  Function SideOf(const vPos, vDir:TVector; const point:TVector ):single;overload; //2D
  Function SideOf(const vPos, vDir:TVector; const points:TVectorArray ):integer;overload; //2D
  Function Intercept( a1, a2:TVector; b1, b2:TVector; out point:TVector):boolean;//2D
  Function InsideOf( v1, v2 :TVector; point:TVector):boolean; //2D
  Function InsideOfSegm( v1, v2 :TVector; point:TVector):boolean; //2D
  Function MiddlePoint( v1, v2 :TVector ):TVector;
  Function InsideOfPoly( Poly:TVectorArray; point:TVector; ClockWise:boolean):boolean; //2D Convex poly;
  Function CollisionPolyPoly( Poly1, Poly2:TVectorArray; ClockWise:boolean):boolean; //2D by vertexes

  //Aquellas funciones marcadas con //2D estan hechas para trabajar en el plano X Y ignorando la Z

implementation

//punto medio entre dos posiciones
Function MiddlePoint( v1, v2 :TVector ):TVector;
begin
  Result := SubVects(v2,v1);
  RealProd( Result, 0.5);
  Result := AddVects(v1, Result);
end;

//si punto point dentro del rectangle 2D v1-v2 retorna true se ignora Z
Function InsideOf( v1, v2 :TVector; point:TVector):boolean;
begin
  Result :=
    (point.x >= Min( v1.x, v2.x)) and
    (point.x <= Max( v1.x, v2.x)) and
    (point.y >= Min( v1.y, v2.y)) and
    (point.y <= Max( v1.y, v2.y));
end;

//2D se ignora z
//si punto que intercepta recta v1-v2 esta dentro del segmento v1-v2 retorna true se ignora Z
Function InsideOfSegm( v1, v2 :TVector; point:TVector):boolean;
begin
  {if (v1.y = v2.y) then
  begin
    Result :=
    (point.x >= Min( v1.x, v2.x)) and
    (point.x < Max( v1.x, v2.x)) ;
  end else if (v1.x = v2.x) then
  begin
    Result :=
    (point.y >= Min( v1.y, v2.y)) and
    (point.y < Max( v1.y, v2.y));
  end else
  begin
    Result :=
    (point.x >= Min( v1.x, v2.x)) and
    (point.x < Max( v1.x, v2.x)) and
    (point.y >= Min( v1.y, v2.y)) and
    (point.y < Max( v1.y, v2.y));
  end; }
  if abs(v1.y - v2.y)<abs(v1.x - v2.x) then
  begin
     Result :=
    (point.x >= Min( v1.x, v2.x)) and
    (point.x <= Max( v1.x, v2.x)) ;
  end else
  begin
    Result :=
    (point.y >= Min( v1.y, v2.y)) and
    (point.y <= Max( v1.y, v2.y));
  end;
end;


function MaxDist(const Vertices:TVectorArray; const Center:TVector):single;
var
  r:single;
  i:integer;
begin
  Result:=0;
  For i:=0 to Length(Vertices)-1 do
  begin
     r:=Module( SubVects(Vertices[i], Center) );
     if r>Result then
     begin
        Result := r;
     end;
  end;
end;

function MinDist(const Vertices:TVectorArray; const Center:TVector):single;
var
  r:single;
  i:integer;
begin
  Result:=Module( SubVects(Vertices[0], Center) );;
  For i:=0 to Length(Vertices)-1 do
  begin
     r:=Module( SubVects(Vertices[i], Center) );
     if r<Result then
     begin
        Result := r;
     end;
  end;
end;

Procedure RoundingBox( const Vertices:TVectorArray; out MinBox, MaxBox :TVector);
var
  i:integer;
begin
  MinBox:=Vertices[0];
  MaxBox:=Vertices[0];
  for i:=0 to Length(Vertices)-1 do
  begin
    MinBox.x := Min(Vertices[i].x, MinBox.x);
    MinBox.y := Min(Vertices[i].y, MinBox.y);
    MinBox.z := Min(Vertices[i].z, MinBox.z);
    MaxBox.x := Max(Vertices[i].x, MaxBox.x);
    MaxBox.y := Max(Vertices[i].y, MaxBox.y);
    MaxBox.z := Max(Vertices[i].z, MaxBox.z);
  end;
end;

{ Halla una Esfera que contine a todo los vértices }

Procedure RoundingSphere( Vertices:TVectorArray; out Radio:single; out Center:TVector);
var
 min,max  :TVector;
begin
  Radio:=0;
  Center:=MakeVector(0,0,0);
  RoundingBox(Vertices, min, max);
  center := AddVects(min, max);
  RealProd(Center, 1/2);
  
  Radio := MaxDist( Vertices, Center);
end;

{
  Crea un poligono convexo a partir de un grupo de vertices regados
  O sea un Polígono que rodea todos los vétices exteriores en el plano XY
  se ingnora la componente Z, y se iguala a 0
}
Function ConvexAroundPoly ( Vertices:TVectorArray):TVectorArray;
var
  Minv  :integer;  //vértice de menor x, se comienza por ese
  vAnt  :TVector;  //vértice anterior
  NewV  :TVector;  //nuevo vertice
  curr  :integer;  //current vertex
  next  :integer;  //next vertex
  ang   :single;   //angulo entre un segmento y otro
  maxAng:single;   //máximo ángulo de todos
  i     :integer;  //contador
  len :integer;    //temp var
  Verts :TVectorArray;
begin
  SetLength(Verts, Length(Vertices));
  Move(Vertices[0], Verts[0], Length(Vertices)*SizeOf(TVector));
  for i:=0 to Length(Verts)-1 do
  begin
    Verts[i].z := 0;
  end;
  {buscando vertice de menor x}
  Minv := 0;
  for i:=1 to Length(Verts)-1 do
  begin
    if Verts[i].x <= Verts[Minv].x then
    begin  //TODO: PROBLEMA PUNTOS COLINEALES, Si las x son iguales coger la de y máxima
      if abs(Verts[i].x - Verts[Minv].x)<1 then
      begin
        if Verts[i].y < Verts[Minv].y then
        begin
          Minv := i;
        end
      end else
      begin
        Minv:=i;
      end;
    end;
  end;
  Next := 0;//NOP
  Curr := Minv;
  SetLength(Result, 1);
  Result[0] := Verts[Curr];
  vAnt := MakeVector(-1, -1, 0); //
  repeat
    {busca el segmento con el que se forma el mayor coseno, menor ang con el
    segmento actual}
    MaxAng:=-1;
    for i:=0 to Length(Verts)-1 do
    begin
      if i<>Curr then
      begin
        NewV := SubVects( Verts[i], Verts[curr] );
        if (Module(vAnt)<>0) and (Module(NewV)<>0) then
        begin
          ang := CosAng( vAnt, NewV);
          if ang > MaxAng then
          begin
            MaxAng := ang;
            Next := i;
          end;
        end;
      end;
    end;
    {lo añade a la lista}
    len:=Length(Result);
    SetLength(Result, len+1);
    Result[len]:=Verts[Next];
    vAnt := SubVects(Verts[Next],Verts[Curr]);
    curr := next;
  until (EqualVects(Result[0], Result[len])) or (len>=Length(Verts));
  SetLength( Result, len);
end;

{  2D
  Halla una circunferencia que mas se ajuste al polygono
  con un algoritmo genético simple (random)
}
Procedure RoundingCilinderGenetic(const Vertices:TVectorArray; out Radio:single; out Center:TVector);
var
 i        :integer;
 prom     :TVector;
 MinRadio :single;
 c        :TVector;
 r, MinR  :single;
 ProperCenter :TVector;
 Dif  :single;
begin
  Radio:=0;
  Center:=MakeVector(0,0,0);
  prom:=MakeVector(0,0,0);
  for i:=0 to Length(Vertices)-1 do
  begin
    prom:=AddVects(prom, Vertices[i]);
  end;
  RealProd(prom, 1/Length(Vertices));
  Center:=prom;
  Radio := MaxDist( Vertices, center);
  MinRadio := MinDist (Vertices, center);
  Dif := Radio - MinRadio;
  MinR := Radio;
    ProperCenter:=Center;
    For i:=0 to 500 do
    begin
      c := AddVects( Center, MakeVector(random*6.28, random*dif));
      r := MaxDist( Vertices, c);
      if r < MinR then
      begin
        MinR := r;
        ProperCenter := c;
      end;
    end;
    Center := ProperCenter;
    Radio := MinR;
end;

{ 2D
  determina de que lado está un punto con respecto a un vector
  result < 0    IZQUIERDA
  result > 0    DERECHA
  result = 0    Sobre el vector;
}
Function SideOf( const vPos, vDir:TVector; const point:TVector ):single; //2D
begin
  Result := CrossProd( vDir, SubVects( point, vPos)).z;
end;

{ 2D
  determina de que lado está un conjunto de puntos con respecto a un vector
  Result = -1      Todos a la derecha
  Result =  1      Todos a la izquerda
  Result =  0      Ambos lados
}
Function SideOf(const vPos, vDir:TVector; const points:TVectorArray ):integer;overload;
var
  positives   :integer;
  negatives,i :integer;
begin
  Positives :=0;
  Negatives :=0;
  for i:=0 to Length(points)-1 do
  begin
    if SideOf( vPos, vDir, Points[i])> 0 then inc(Positives);
    if SideOf( vPos, vDir, Points[i])< 0 then inc(Negatives);
  end;
  Result:=0;
  if (Positives=0) and (Negatives>0) then Result:=  -1;
  if (Positives>0) and (Negatives=0) then Result:=  1;
end;

{  2D
  Halla a partir de dos polígonos Poly1 y Poly2, dos rayos
  uno por la derecha y otro por la izquierda,
  para cada rayo los dos polígonos deben quedar completamente a un solo lado
  y los rayos pasan por un vertice

  El procedimiento devuelve el índice de cada vertice para formar los dos rayos
  p1R, p2R  -> Rayo Derecho
  p1L, p2L  -> Rayo Izquierdo

  Devuelve la cantidad de rayos hallados, supuestamente debe ser siempre 2
  pero en situaciones donde los polígonos se mezclan hay más
  si se hallan 0 rayos es que un polígono está completamente dentro del otro.
}
Function ShadowRays( const Poly1, Poly2:TVectorArray; out p1R, p2R, p1L, p2L :integer):integer;
var
  i,j :integer;
  v   :TVector;
begin
  {Left Rays}
  p1L:=0;
  p2L:=0;
  Result:=0;
  for i:=0 to Length(Poly1)-1 do
  begin
    for j:=0 to Length(Poly2)-1 do
    begin
      v := SubVects( Poly2[j], Poly1[i] );
      if (SideOf( Poly1[i], v, Poly1) > 0) and (SideOf( Poly1[i], v, Poly2) > 0) then
      begin
        p1L := i;
        p2L := j;
        Inc(Result);
      end;
    end;
  end;
  {Right Rays}
  p1R:=0;
  p2R:=0;
  for i:=0 to Length(Poly1)-1 do
  begin
    for j:=0 to Length(Poly2)-1 do
    begin
      v := SubVects( Poly2[j], Poly1[i] );
      if (SideOf( Poly1[i], v, Poly1) < 0) and (SideOf( Poly1[i], v, Poly2) < 0) then
      begin
        p1R := i;
        p2R := j;
        Inc(Result);
      end;
    end;
  end;
end;

{
  Intercepta dos rectas definidas por
  recta 1: puntos a1 y a2
  recta 2: puntos b1 y b2
  El punto de intercección se retorna en point:TVector
  La función devuelve False si las rectas son paralelas
  y no se pueden iterceptar;
}
Function Intercept( a1, a2:TVector; b1, b2:TVector; out point:TVector):boolean;
var
  m1, m2: single;
  n1, n2: single;
  X,Y   : single;
  indef_m1  :boolean;
  indef_m2  :boolean;

begin
  Result:=false;
  m1:=0; m2:=0; n1:=0; n2:=0; X:=0;
  indef_m1:=false;
  if (a2.x - a1.x)<>0 then
    begin
      m1 := (a2.y - a1.y) / (a2.x - a1.x);
      n1 := a1.y - m1 * a1.x;
    end else begin
      X:=a2.x;
      indef_m1 := true;
    end;

  indef_m2:=false;
  if (b2.x - b1.x)<>0 then
    begin
      m2 := (b2.y - b1.y) / (b2.x - b1.x);
      n2 := b1.y - m2 * b1.x;
    end else begin
      X:=b2.x;
      indef_m2 := true
    end;

  if (not indef_m1) and (not indef_m2) then
  begin
    if (m1=m2) then exit //'Son paralelas no se cortan'
    else
    begin
      X := (n2 - n1) / (m1 - m2);
    end;
  end else
  if (indef_m1 and indef_m2) then exit; {'Son parelelas no se cortan'};


  if not indef_m1 then
  begin
    Y := m1 * X + n1;
  end else
  begin
    Y := m2 * X + n2;
  end;
  Result:=true;
  point:=MakeVector(X, Y, 0);

end;

Function InsideOfPoly( Poly:TVectorArray; point:TVector; ClockWise:boolean):boolean;
var
  pos :TVector;
  dir :TVector;
  i :integer;
begin
  Result := true;
  for i:=0 to High(Poly) do
  begin
    pos := Poly[i];
    dir := Poly[(i+1) mod Length(Poly)];
    dir := SubVects(dir, pos);
    if (SideOf(pos, dir, point)>0) xor ClockWise then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function CollisionPolyPoly( Poly1, Poly2:TVectorArray; ClockWise:boolean):boolean;
var
  Count :integer;
begin
  Count := 0;
  repeat
    Result := InsideOfPoly( Poly2, Poly1[Count], ClockWise);
    Inc(Count);
  until Result or (Count = Length(Poly1));
  Count := 0;
  if not Result then
    repeat
      Result := InsideOfPoly( Poly1, Poly2[Count], ClockWise);
      Inc(Count);
    until Result or (Count = Length(Poly2));
end;

end.
